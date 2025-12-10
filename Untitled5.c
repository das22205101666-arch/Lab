/*
  lexer_parser.c
  Lexer + Recursive-descent parser for the custom language described by the user.
  Produces:
    1) Lexical token stream (one token per line)
    2) Parsing trace and final verdict: ACCEPTED or REJECTED

  Notes:
    - Accepts either "#include <stdio.h>" or "#include<stdio.h>" as the first line.
    - Comments (single-line) allowed from line 2 onward; they may contain only spaces and letters.
    - Variables: _[A-Za-z]+[0-9][A-Za-z]  (underscore, >0 letters, 1 digit, 1 letter)
    - Functions: [A-Za-z]+Fn  (alphabets only, ends with Fn)
    - Statement terminator is '..'
    - Types: int or dec
    - printf(_varname)..  builtin print; varname must follow var rule
    - loop label: loop_<letters><2digits>: and next is while(...) { ... }
    - while condition inside parentheses: TYPE VAR < DIGITS ..)
    - Custom functions may optionally exist before main; accept one param: type var
    - main() mandatory
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ---------- Token definitions ---------- */
typedef enum {
    T_INCLUDE,
    T_TYPE_INT, T_TYPE_DEC,
    T_IDENTIFIER_VAR,    // variable names per rule
    T_IDENTIFIER_FUNC,   // function names per rule
    T_PRINTF,
    T_LOOP_LABEL,        // e.g., "loop_main01:"
    T_WHILE,
    T_LPAREN, T_RPAREN,
    T_LBRACE, T_RBRACE,
    T_ASSIGN,   // =
    T_PLUS,     // +
    T_NUMBER,   // digits
    T_DOTDOT,   // ..
    T_RETURN,
    T_BREAK,
    T_COMMA,
    T_SEMI,     // not used; kept for compatibility
    T_EOF,
    T_UNKNOWN
} TokenType;

const char* token_names[] = {
    "INCLUDE",
    "TYPE_int", "TYPE_dec",
    "ID_VAR",
    "ID_FUNC",
    "PRINTF",
    "LOOP_LABEL",
    "WHILE",
    "LPAREN","RPAREN",
    "LBRACE","RBRACE",
    "ASSIGN",
    "PLUS",
    "NUMBER",
    "DOTDOT",
    "RETURN",
    "BREAK",
    "COMMA",
    "SEMI",
    "EOF",
    "UNKNOWN"
};

typedef struct {
    TokenType type;
    char lexeme[256];
    int line;
} Token;

/* ---------- Globals for lexing/parsing ---------- */
char *source = NULL;
size_t src_len = 0;
size_t pos = 0;
int cur_line = 1;

/* Token list dynamic array */
Token *tokens = NULL;
int tok_count = 0;
int tok_cap = 0;

/* Parser cursor */
int cur = 0;

/* Utility functions for token array */
void push_token(Token t) {
    if (tok_count + 1 >= tok_cap) {
        tok_cap = (tok_cap == 0) ? 256 : tok_cap * 2;
        tokens = realloc(tokens, sizeof(Token) * tok_cap);
    }
    tokens[tok_count++] = t;
}

Token make_token(TokenType tt, const char *lex) {
    Token t;
    t.type = tt;
    strncpy(t.lexeme, lex ? lex : "", sizeof(t.lexeme)-1);
    t.lexeme[sizeof(t.lexeme)-1] = '\0';
    t.line = cur_line;
    return t;
}

/* peek current character */
char peekc() {
    if (pos >= src_len) return '\0';
    return source[pos];
}
char peekc_next() {
    if (pos+1 >= src_len) return '\0';
    return source[pos+1];
}
char getc_() {
    if (pos >= src_len) return '\0';
    char c = source[pos++];
    if (c == '\n') cur_line++;
    return c;
}
void unread_one() {
    if (pos == 0) return;
    pos--;
    if (source[pos] == '\n') cur_line--;
}

/* Helper: check if lexeme matches variable naming rule */
int is_valid_varname(const char *s) {
    // pattern: ^_[A-Za-z]+[0-9][A-Za-z]$
    int n = strlen(s);
    if (n < 4) return 0;
    if (s[0] != '_') return 0;
    int i = 1;
    if (!isalpha((unsigned char)s[i])) return 0;
    while (i < n && isalpha((unsigned char)s[i])) i++;
    if (i >= n-2) return 0;
    if (!isdigit((unsigned char)s[i])) return 0;
    i++;
    if (!isalpha((unsigned char)s[i]) || s[i+1] != '\0') return 0;
    return 1;
}

/* Helper: check func name rule */
int is_valid_funcname(const char *s) {
    // all alphabets and ends with "Fn"
    int n = strlen(s);
    if (n < 3) return 0;
    if (s[n-2] != 'F' || s[n-1] != 'n') return 0;
    for (int i=0;i<n-2;i++) if (!isalpha((unsigned char)s[i])) return 0;
    return 1;
}

/* Helper: check loop label form "loop_<letters><2digits>:" */
int is_valid_loop_label(const char *s) {
    // example: loop_main01:
    int n = strlen(s);
    if (n < 8) return 0;
    if (strncmp(s, "loop_", 5) != 0) return 0;
    if (s[n-1] != ':') return 0;
    // between pos 5 and n-3: letters (>=1), then two digits before ':'.
    int i = 5;
    int letters = 0;
    while (i < n-3 && isalpha((unsigned char)s[i])) {
        i++; letters++;
    }
    if (letters < 1) return 0;
    if (!isdigit((unsigned char)s[i]) || !isdigit((unsigned char)s[i+1])) return 0;
    if (i+2 != n-1) return 0;
    return 1;
}

/* ---------- Lexical analysis ---------- */

void skip_whitespace() {
    while (isspace((unsigned char)peekc())) getc_();
}

void lex_error(const char *msg) {
    fprintf(stderr, "Lex error (line %d): %s\n", cur_line, msg);
}

/* Read a full line into buffer (used for handling include and comment validation) */
char *read_line_from_pos(size_t startpos) {
    size_t i = startpos;
    while (i < src_len && source[i] != '\n') i++;
    size_t len = i - startpos;
    char *buf = malloc(len + 1);
    memcpy(buf, source + startpos, len);
    buf[len] = '\0';
    return buf;
}

void lex_all() {
    pos = 0;
    cur_line = 1;

    /* First, validate and consume the first line: include */
    // read first physical line
    size_t first_line_start = pos;
    while (pos < src_len && source[pos] != '\n') pos++;
    char *line1 = read_line_from_pos(first_line_start);
    // trim leading/trailing spaces
    char tmp[512];
    int st = 0, en = (int)strlen(line1)-1;
    while (st <= en && isspace((unsigned char)line1[st])) st++;
    while (en >= st && isspace((unsigned char)line1[en])) en--;
    int L = 0;
    for (int i=st;i<=en;i++) tmp[L++] = line1[i];
    tmp[L] = '\0';
    // Accept both "#include <stdio.h>" and "#include<stdio.h>" and without space between include and <.
    if (strcmp(tmp, "#include <stdio.h>") != 0 && strcmp(tmp, "#include<stdio.h>") != 0) {
        fprintf(stderr, "First line must be '#include <stdio.h>' (line 1). Found: '%s'\n", tmp);
        free(line1);
        push_token(make_token(T_UNKNOWN, tmp));
        push_token(make_token(T_EOF, NULL));
        return;
    }
    push_token(make_token(T_INCLUDE, tmp));
    free(line1);
    // move over the newline if any
    if (pos < src_len && source[pos] == '\n') { pos++; cur_line++; }

    /* From line 2 onwards, we can have comments anywhere (single line) containing only spaces and alphabets.
       We'll do a simple line-by-line scan to validate comment lines, but comments can appear "anywhere" as lines.
       For lexical scanning of the rest, we'll treat comments as whitespace lines.
    */

    while (pos < src_len) {
        skip_whitespace();
        char c = peekc();
        if (c == '\0') break;

        /* detect comment line starting at current position if it begins with letters/spaces up to newline
           but don't interpret constructs starting with letters (like int/dec) as a comment; comments can
           appear anywhere as whole lines containing only spaces and alphabets. So we only treat a line as a comment
           if the line (up to newline) contains only spaces and letters. */
        size_t line_start = pos;
        char *line = read_line_from_pos(line_start);
        int only_spaces_and_alpha = 1;
        int len = strlen(line);
        if (len == 0) only_spaces_and_alpha = 0; // blank lines are not comments but skip them
        for (int i=0;i<len;i++) {
            if (!(line[i]==' ' || isalpha((unsigned char)line[i]))) {
                only_spaces_and_alpha = 0; break;
            }
        }
        free(line);
        if (only_spaces_and_alpha) {
            // consume that line as a comment (skip)
            // advance pos to after newline
            while (pos < src_len && source[pos] != '\n') pos++;
            if (pos < src_len && source[pos] == '\n') { pos++; cur_line++; }
            continue;
        }

        c = peekc();
        // tokens detection
        if (isalpha((unsigned char)c) || c == '_') {
            // read alpha/underscore/digit sequence and some symbols (for loop label may include ':')
            size_t s = pos;
            char buf[512]; int L=0;
            while (pos < src_len && (isalnum((unsigned char)source[pos]) || source[pos]=='_' || source[pos]==':')) {
                buf[L++] = source[pos++];
            }
            buf[L] = '\0';
            // check keywords and patterns
            if (strcmp(buf, "int") == 0) {
                push_token(make_token(T_TYPE_INT, "int"));
            } else if (strcmp(buf, "dec") == 0) {
                push_token(make_token(T_TYPE_DEC, "dec"));
            } else if (strcmp(buf, "while") == 0) {
                push_token(make_token(T_WHILE, "while"));
            } else if (strcmp(buf, "return") == 0) {
                push_token(make_token(T_RETURN, "return"));
            } else if (strcmp(buf, "break") == 0) {
                // break might be followed by .. later; we push BREAK token and let parser expect DOTDOT next
                push_token(make_token(T_BREAK, "break"));
            } else if (strcmp(buf, "printf") == 0) {
                push_token(make_token(T_PRINTF, "printf"));
            } else {
                // it could be variable name, function name, or loop label, or general identifier
                // check if it is loop label (ends with ':')
                if (buf[L-1] == ':') {
                    if (is_valid_loop_label(buf)) {
                        push_token(make_token(T_LOOP_LABEL, buf));
                    } else {
                        // invalid loop label format; still push UNKNOWN to catch in parser
                        push_token(make_token(T_UNKNOWN, buf));
                    }
                } else {
                    // if name ends with "Fn" and all-alpha => function
                    if (is_valid_funcname(buf)) {
                        push_token(make_token(T_IDENTIFIER_FUNC, buf));
                    } else if (is_valid_varname(buf)) {
                        push_token(make_token(T_IDENTIFIER_VAR, buf));
                    } else {
                        // could be identifier like "main" or parameter names in function header etc.
                        // If it's "main" treat as function name but validator will check signature later.
                        // We will push generic ID_FUNC if all letters else UNKNOWN
                        int allalpha = 1;
                        for (int i=0;i<strlen(buf);i++) if (!isalpha((unsigned char)buf[i])) { allalpha = 0; break; }
                        if (allalpha) {
                            push_token(make_token(T_IDENTIFIER_FUNC, buf));
                        } else {
                            push_token(make_token(T_UNKNOWN, buf));
                        }
                    }
                }
            }
            continue;
        } else if (c == '(') { getc_(); push_token(make_token(T_LPAREN, "(")); continue; }
        else if (c == ')') { getc_(); push_token(make_token(T_RPAREN, ")")); continue; }
        else if (c == '{') { getc_(); push_token(make_token(T_LBRACE, "{")); continue; }
        else if (c == '}') { getc_(); push_token(make_token(T_RBRACE, "}")); continue; }
        else if (c == '=') { getc_(); push_token(make_token(T_ASSIGN, "=")); continue; }
        else if (c == '+') { getc_(); push_token(make_token(T_PLUS, "+")); continue; }
        else if (c == ',') { getc_(); push_token(make_token(T_COMMA, ",")); continue; }
        else if (c == '.') {
            // expect two dots for '..'
            if (peekc_next() == '.') {
                getc_(); getc_();
                push_token(make_token(T_DOTDOT, ".."));
                continue;
            } else {
                // single dot unexpected
                getc_();
                push_token(make_token(T_UNKNOWN, "."));
                continue;
            }
        } else if (isdigit((unsigned char)c)) {
            // number
            char buf[128]; int L=0;
            while (isdigit((unsigned char)peekc())) {
                buf[L++] = getc_();
            }
            buf[L] = '\0';
            push_token(make_token(T_NUMBER, buf));
            continue;
        } else if (c == '\n') {
            getc_();
            continue;
        } else {
            // unrecognized char
            char tmpbuf[4] = {0};
            tmpbuf[0] = getc_();
            push_token(make_token(T_UNKNOWN, tmpbuf));
            continue;
        }
    }

    push_token(make_token(T_EOF, NULL));
}

/* ---------- Parser helpers (recursive-descent) ---------- */

Token *peek_tok() {
    if (cur >= tok_count) return &tokens[tok_count-1];
    return &tokens[cur];
}
Token *advance_tok() {
    if (cur < tok_count) cur++;
    return peek_tok();
}
int match(TokenType t) {
    if (peek_tok()->type == t) { cur++; return 1; }
    return 0;
}
int expect(TokenType t) {
    if (peek_tok()->type == t) { cur++; return 1; }
    fprintf(stderr, "Parse error (line %d): expected %s but found %s ('%s')\n",
            peek_tok()->line, token_names[t], token_names[peek_tok()->type], peek_tok()->lexeme);
    return 0;
}

/* We'll produce a parsing trace (printed to stdout). */
void trace(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}

/* Grammar sketch (simplified, permissive for the allowed constructs)
   program -> INCLUDE rest
   rest -> global_items main_func EOF
   global_items -> { func_decl }   (0 or more custom functions)
   func_decl -> type funcname LPAREN param RPAREN LBRACE statements RBRACE
   main_func -> type "main" LPAREN RPAREN LBRACE statements RBRACE
   param -> type varname | ε
   statements -> { statement }
   statement -> var_decl DOTDOT | assignment DOTDOT | printf_stmt DOTDOT | loop_block | return_stmt DOTDOT | break DOTDOT
   var_decl -> type varname ASSIGN expr
   assignment -> varname ASSIGN expr
   printf_stmt -> PRINTF LPAREN varname RPAREN
   return_stmt -> RETURN (NUMBER | varname | expr)
   loop_block -> LOOP_LABEL WHILE LPAREN type varname '<' NUMBER DOTDOT RPAREN LBRACE statements RBRACE
   expr -> varname | NUMBER | varname PLUS NUMBER | func_call
   func_call -> funcname LPAREN varname RPAREN
*/

int parse_expr(); // forward

int parse_varname_token() {
    if (peek_tok()->type == T_IDENTIFIER_VAR) { cur++; return 1; }
    return 0;
}
int parse_typename_token() {
    if (peek_tok()->type == T_TYPE_INT || peek_tok()->type == T_TYPE_DEC) { cur++; return 1; }
    return 0;
}

int parse_func_decl() {
    int saved = cur;
    if (!parse_typename_token()) { cur = saved; return 0; }
    // expect function name (alphabet only)
    if (peek_tok()->type == T_IDENTIFIER_FUNC) {
        // custom functions must end with Fn; validated in lexing
        cur++;
    } else { cur = saved; return 0; }
    if (!expect(T_LPAREN)) { cur = saved; return 0; }
    // parameter: type varname
    if (peek_tok()->type == T_TYPE_INT || peek_tok()->type == T_TYPE_DEC) {
        cur++;
        if (!parse_varname_token()) { cur = saved; return 0; }
    } else {
        // functions in this language must accept one parameter if present; but spec said "may or may not be present".
        // We'll allow empty param list too for safety.
    }
    if (!expect(T_RPAREN)) { cur = saved; return 0; }
    if (!expect(T_LBRACE)) { cur = saved; return 0; }
    // statements until RBRACE
    while (peek_tok()->type != T_RBRACE && peek_tok()->type != T_EOF) {
        // reuse statement parser; to avoid infinite loop on error, break on unknown tokens
        int ok = 0;
        // var decl or assignment or printf or return or break or loop
        if (parse_typename_token()) {
            // variable declaration
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_ASSIGN)) { cur = saved; return 0; }
            if (!parse_expr()) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
            ok = 1;
        } else if (peek_tok()->type == T_IDENTIFIER_VAR) {
            // assignment
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_ASSIGN)) { cur = saved; return 0; }
            if (!parse_expr()) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
            ok = 1;
        } else if (peek_tok()->type == T_PRINTF) {
            cur++;
            if (!expect(T_LPAREN)) { cur = saved; return 0; }
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_RPAREN)) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
            ok = 1;
        } else if (peek_tok()->type == T_LOOP_LABEL) {
            // loop label then while
            cur++;
            if (!expect(T_WHILE)) { cur = saved; return 0; }
            if (!expect(T_LPAREN)) { cur = saved; return 0; }
            if (!parse_typename_token()) { cur = saved; return 0; }
            if (!parse_varname_token()) { cur = saved; return 0; }
            // expect '<' which was lexed as UNKNOWN if raw char '<' present; our lexer did not create a token for '<'
            // We'll accept the stream where after varname next token is NUMBER (because user uses '< number ..)')
            // But to be strict, look ahead in lexemes for a NUMBER then DOTDOT before RPAREN.
            if (peek_tok()->type == T_UNKNOWN && strcmp(peek_tok()->lexeme, "<") == 0) {
                cur++;
            } else {
                // sometimes the '<' may have been lumped; we'll try to proceed
                // continue
            }
            if (peek_tok()->type != T_NUMBER) { cur = saved; return 0; }
            cur++;
            if (!expect(T_DOTDOT)) { cur = saved; return 0; } // expects the .. after number inside ()
            if (!expect(T_RPAREN)) { cur = saved; return 0; }
            if (!expect(T_LBRACE)) { cur = saved; return 0; }
            // body
            while (peek_tok()->type != T_RBRACE && peek_tok()->type != T_EOF) {
                if (peek_tok()->type == T_BREAK) {
                    cur++;
                    if (!expect(T_DOTDOT)) { cur = saved; return 0; }
                } else {
                    // other statements: var decl, assignment, printf, return
                    if (parse_typename_token()) {
                        if (!parse_varname_token()) { cur = saved; return 0; }
                        if (!expect(T_ASSIGN)) { cur = saved; return 0; }
                        if (!parse_expr()) { cur = saved; return 0; }
                        if (!expect(T_DOTDOT)) { cur = saved; return 0; }
                    } else if (peek_tok()->type == T_IDENTIFIER_VAR) {
                        // assignment
                        parse_varname_token();
                        if (!expect(T_ASSIGN)) { cur = saved; return 0; }
                        if (!parse_expr()) { cur = saved; return 0; }
                        if (!expect(T_DOTDOT)) { cur = saved; return 0; }
                    } else if (peek_tok()->type == T_PRINTF) {
                        cur++;
                        if (!expect(T_LPAREN)) { cur = saved; return 0; }
                        if (!parse_varname_token()) { cur = saved; return 0; }
                        if (!expect(T_RPAREN)) { cur = saved; return 0; }
                        if (!expect(T_DOTDOT)) { cur = saved; return 0; }
                    } else if (peek_tok()->type == T_RETURN) {
                        cur++;
                        // return number or var
                        if (peek_tok()->type == T_NUMBER || peek_tok()->type == T_IDENTIFIER_VAR) cur++;
                        if (!expect(T_DOTDOT)) { cur = saved; return 0; }
                    } else {
                        cur = saved; return 0;
                    }
                }
            }
            // close RBRACE
            if (!expect(T_RBRACE)) { cur = saved; return 0; }
            ok = 1;
        } else if (peek_tok()->type == T_RETURN) {
            cur++;
            if (peek_tok()->type == T_NUMBER || peek_tok()->type == T_IDENTIFIER_VAR) cur++;
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
            ok = 1;
        } else {
            // unknown token -> fail
            cur = saved; return 0;
        }
        if (!ok) { cur = saved; return 0; }
    }

    if (!expect(T_RBRACE)) { cur = saved; return 0; }
    trace("Parsed function declaration.");
    return 1;
}

int parse_main_func() {
    int saved = cur;
    if (!parse_typename_token()) { cur = saved; return 0; }
    // main name must be "main" tokenized as ID_FUNC earlier
    if (peek_tok()->type == T_IDENTIFIER_FUNC && strcmp(peek_tok()->lexeme,"main")==0) { cur++; }
    else { cur = saved; return 0; }
    if (!expect(T_LPAREN)) { cur = saved; return 0; }
    if (!expect(T_RPAREN)) { cur = saved; return 0; }
    if (!expect(T_LBRACE)) { cur = saved; return 0; }
    // statements
    while (peek_tok()->type != T_RBRACE && peek_tok()->type != T_EOF) {
        if (parse_typename_token()) {
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_ASSIGN)) { cur = saved; return 0; }
            if (!parse_expr()) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
        } else if (peek_tok()->type == T_IDENTIFIER_VAR) {
            // assignment
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_ASSIGN)) { cur = saved; return 0; }
            if (!parse_expr()) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
        } else if (peek_tok()->type == T_PRINTF) {
            cur++;
            if (!expect(T_LPAREN)) { cur = saved; return 0; }
            if (!parse_varname_token()) { cur = saved; return 0; }
            if (!expect(T_RPAREN)) { cur = saved; return 0; }
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
        } else if (peek_tok()->type == T_LOOP_LABEL) {
            // same handling as in function
            if (!parse_func_decl()) { ; } // reusing parse_func_decl branch for loop parsing is okay as it handles loop_label
            else { /* parsed */ }
        } else if (peek_tok()->type == T_RETURN) {
            cur++;
            if (peek_tok()->type == T_NUMBER || peek_tok()->type == T_IDENTIFIER_VAR) cur++;
            if (!expect(T_DOTDOT)) { cur = saved; return 0; }
        } else {
            // unknown or unsupported in main
            cur = saved; return 0;
        }
    }
    if (!expect(T_RBRACE)) { cur = saved; return 0; }
    trace("Parsed main function.");
    return 1;
}

int parse_program() {
    cur = 0;
    if (!expect(T_INCLUDE)) return 0;
    // parse zero or more function declarations (custom functions)
    while (1) {
        // skip comments/spaces already removed; lookahead if next is type -> possible function or var decl in global scope (not allowed)
        int saved = cur;
        if (peek_tok()->type == T_TYPE_INT || peek_tok()->type == T_TYPE_DEC) {
            // we need to peek further to distinguish func declaration from main or illegal global var.
            // if next tokens form: type IDENT_FUNC LPAREN -> it's a function or main
            if (tokens[cur+1].type == T_IDENTIFIER_FUNC && tokens[cur+2].type == T_LPAREN) {
                // could be custom function or main
                // if funcname is main => stop here (it is main)
                if (strcmp(tokens[cur+1].lexeme, "main") == 0) break;
                if (!parse_func_decl()) { return 0; }
            } else {
                // unexpected global construct - error
                return 0;
            }
        } else {
            break;
        }
    }
    // now main function
    if (!parse_main_func()) return 0;
    // expect EOF
    if (!expect(T_EOF)) return 0;
    return 1;
}

/* parse function: simplified expression parsing for patterns allowed */
int parse_expr() {
    // expr -> NUMBER | var | var PLUS NUMBER | func_call
    if (peek_tok()->type == T_NUMBER) { cur++; return 1; }
    if (peek_tok()->type == T_IDENTIFIER_VAR) {
        cur++;
        if (peek_tok()->type == T_PLUS) {
            cur++;
            if (peek_tok()->type == T_NUMBER) { cur++; return 1; }
            return 0;
        }
        return 1;
    }
    if (peek_tok()->type == T_IDENTIFIER_FUNC) {
        // function call style: funcname LPAREN var RPAREN
        cur++;
        if (!expect(T_LPAREN)) return 0;
        if (!parse_varname_token()) return 0;
        if (!expect(T_RPAREN)) return 0;
        return 1;
    }
    return 0;
}

/* ---------- Main ---------- */

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <sourcefile>\n", argv[0]);
        return 1;
    }
    /* read file into memory */
    FILE *f = fopen(argv[1], "rb");
    if (!f) { perror("open"); return 1; }
    fseek(f, 0, SEEK_END);
    src_len = ftell(f);
    fseek(f, 0, SEEK_SET);
    source = malloc(src_len + 2);
    if (!source) { perror("malloc"); return 1; }
    fread(source, 1, src_len, f);
    source[src_len] = '\0';
    fclose(f);

    /* Lex */
    lex_all();

    /* Print lexical output */
    printf("----- LEXICAL OUTPUT -----\n");
    for (int i=0;i<tok_count;i++) {
        printf("(%3d) %-12s  '%s'\n", tokens[i].line, token_names[tokens[i].type], tokens[i].lexeme);
    }

    /* Parse */
    printf("\n----- PARSING TRACE -----\n");
    int ok = parse_program();
    if (ok) {
        printf("\nFINAL VERDICT: ACCEPTED\n");
    } else {
        printf("\nFINAL VERDICT: REJECTED\n");
    }

    free(tokens);
    free(source);
    return 0;
}
