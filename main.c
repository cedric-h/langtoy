#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

static void bail(char *str) {
  puts(str);
  assert(0);
}
static void bailf(const char* format, ... ) {
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    putchar('\n'); /* trigger stdout flush */
    assert(0);
}

/* https://en.wikipedia.org/wiki/X_Macro */
#define KEYWORDS \
  ENTRY(PRINT, "print") \
  ENTRY(  VAR, "var"  ) \
  ENTRY(  AND, "and"  ) ENTRY(    OR, "or"    ) \
  ENTRY(  FUN, "fun"  ) ENTRY(RETURN, "return") \
  ENTRY( ELSE, "else" ) ENTRY(    IF, "if"    ) \
  ENTRY(WHILE, "while") ENTRY(   FOR, "for"   ) \
  ENTRY(  NIL, "nil"  ) ENTRY(  TRUE, "true"  ) ENTRY(FALSE, "false") \
  ENTRY( THIS, "this" ) ENTRY( CLASS, "class" ) ENTRY(SUPER, "super") \

typedef enum {
  LexKind_NULL, /* not the same as NIL, used to detect zero-initialization */

  // keywords
#define ENTRY(variant, text) LexKind_##variant,
  KEYWORDS
#undef ENTRY

  // 1 char tokens.
  LexKind_LEFT_PAREN, LexKind_RIGHT_PAREN,
  LexKind_LEFT_BRACE, LexKind_RIGHT_BRACE,
  LexKind_COMMA,
  LexKind_DOT,
  LexKind_MINUS, LexKind_PLUS,
  LexKind_SLASH, LexKind_STAR,
  LexKind_SEMICOLON,

  // true/false ops
  LexKind_BANG,    LexKind_BANG_EQUAL,
  LexKind_EQUAL,   LexKind_EQUAL_EQUAL,
  LexKind_GREATER, LexKind_GREATER_EQUAL,
  LexKind_LESS,    LexKind_LESS_EQUAL,

  // literals
  LexKind_IDENTIFIER, LexKind_STRING, LexKind_NUMBER,

  LexKind_EOF
} LexKind;
typedef struct {
  LexKind kind;
  uint32_t char_from, char_to;
  union {
    uint64_t hash; /* valid for LexKind_IDENTIFIER */
    char *str; /* valid for LexKind_STRING */
    double num; /* valid for LexKind_NUMBER */
  };
} Lex;

static void lex_print(char *src, Lex *lex) {
  printf("%.*s", lex->char_to - lex->char_from, src + lex->char_from);
}

static int is_numeric(char c) { return '0' <= c && c <= '9'; }
static int is_letter(char c) { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'); }

static uint64_t hash_strn(char *p, int n_bytes) {
  uint64_t h = 14695981039346656037ul;
  for (int i = 0; i < n_bytes; i++)
    h = (h * 1099511628211) ^ (uint8_t)p[i];
  return h;
}
static uint64_t hash_str(char *str) {
  uint64_t h = 14695981039346656037ul;
  for (; *str; str++)
    h = (h * 1099511628211) ^ (uint8_t)*str;
  return h;
}

static Lex *lex(char *src) {
  char *rdr = src;
  /* number of lexemes will be at most 1 per char, plus EOF */
  Lex *res = calloc(strlen(src) + 1, sizeof(Lex)), *wtr = res;

  uint64_t keyword_hashes[] = {
#define ENTRY(variant, text) [LexKind_##variant] = hash_str(text),
    KEYWORDS 
#undef ENTRY
  };

  while (*rdr) {
    Lex out = { .char_from = rdr - src };
    char *start = rdr;
    switch (*rdr) {
      case ' ': case '\n': case '\t': rdr++; continue;
      case '(': out.kind = LexKind_LEFT_PAREN , rdr++; break;
      case ')': out.kind = LexKind_RIGHT_PAREN, rdr++; break;
      case '{': out.kind = LexKind_LEFT_BRACE , rdr++; break;
      case '}': out.kind = LexKind_RIGHT_BRACE, rdr++; break;
      case '-': out.kind = LexKind_MINUS      , rdr++; break;
      case '+': out.kind = LexKind_PLUS       , rdr++; break;
      case '*': out.kind = LexKind_STAR       , rdr++; break;
      case '/': out.kind = LexKind_SLASH      , rdr++; break;
      case ';': out.kind = LexKind_SEMICOLON  , rdr++; break;
      case '>': {
        if (rdr[1] == '=') out.kind = LexKind_GREATER_EQUAL, rdr += 2;
        else               out.kind = LexKind_GREATER      , rdr++   ;
      } break;
      case '<': {
        if (rdr[1] == '=') out.kind = LexKind_LESS_EQUAL   , rdr += 2;
        else               out.kind = LexKind_LESS         , rdr++   ;
      } break;
      case '=': {
        if (rdr[1] == '=') out.kind = LexKind_EQUAL_EQUAL  , rdr += 2;
        else               out.kind = LexKind_EQUAL        , rdr++   ;
      } break;
      case '!': {
        if (rdr[1] == '=') out.kind = LexKind_BANG_EQUAL   , rdr += 2;
        else               out.kind = LexKind_BANG         , rdr++   ;
      } break;
      case '"': {
        rdr++;
        while (*rdr++ != '"');
        /* out.hash = hash_strn(start, rdr - start); */
        int len = (rdr - start) - 2;
        out.str = malloc(len);
        memcpy(out.str, start + 1, len);
        out.kind = LexKind_STRING;
      } break;
      default: {
        if (is_numeric(*rdr)) {
          out.kind = LexKind_NUMBER;
          char *end = NULL;
          out.num = strtod(rdr, &end);
          rdr = end;
        } else if (is_letter( *rdr)) {
          out.kind = LexKind_IDENTIFIER;
          for (char c = *rdr;
               is_letter(c) || is_numeric(c) || c == '_';
               c = *++rdr);
          out.hash = hash_strn(start, rdr - start);
          for (int i = 1; i < sizeof(keyword_hashes)/sizeof(uint64_t); i++)
            if (out.hash == keyword_hashes[i])
              out.kind = i;
        } else {
          bailf("Unexpected character: %c", *rdr);
          rdr++;
          continue;
        }
      } break;
    }
    out.char_to = rdr - src;
    *wtr++ = out;
  }
  *wtr++ = (Lex) { LexKind_EOF };
  return res;
}

typedef enum {
  ValKind_NIL,
  ValKind_NUM,
  ValKind_STR,
  ValKind_BOOL,
} ValKind;
typedef struct {
  ValKind kind;
  union {
    double num;
    char *str;
    int bool;
  };
} Val;

static void val_print(Val val) {
  switch (val.kind) {
    case ValKind_NIL:  printf("nil");                              break;
    case ValKind_BOOL: fputs(val.bool ? "true" : "false", stdout); break;
    case ValKind_NUM:  printf("%f", val.num);                      break;
    case ValKind_STR:  printf("\"%s\"", val.str);                  break;
  };
}

typedef enum {
  ExprKind_NULL,
  ExprKind_BINARY,
  ExprKind_UNARY,
  ExprKind_GROUPING,
  ExprKind_LITERAL,
  ExprKind_VARIABLE,
} ExprKind;
typedef struct Expr Expr;
struct Expr {
  ExprKind kind;
  Lex op;
  union {
    struct { Expr *lhs, *rhs; }; /* valid for ExprKind_BINARY */
    Expr *expr; /* valid for ExprKind_UNARY, ExprKind_GROUPING */
    Val val; /* valid for ExprKind_BINARY */
    struct { uint32_t scope, scope_var; }; /* valid for ExprKind_VARIABLE */
  };
};

typedef enum {
  StmtKind_NULL,
  StmtKind_DECL,
  StmtKind_EXPR,
  StmtKind_BLOCK,
  StmtKind_PRINT,
} StmtKind;
typedef struct Stmt Stmt;
struct Stmt {
  StmtKind kind;
  Lex op;
  union {
    Expr *expr; /* valid for StmtKind_EXPR, StmtKind_PRINT, StmtKind_DECL */
    Stmt *last_stmt; /* valid for StmtKind_BLOCK */
  };
  uint32_t scope; /* intended for StmtKind_DECL, StmtKind_BLOCK */
  union {
    uint32_t scope_var; /* valid for StmtKind_DECL */
    uint32_t var_count; /* valid for StmtKind_BLOCK */
  };
};

typedef struct {
  uint32_t scopes, scope_vars;
  Lex *in;
  Expr *expr_start, *expr_out; /* last expression output; to initialize, malloc and subtract 1 */
  Stmt *stmt_start, *stmt_out;
  char *src;
} ParseState;

static Expr *ps_push_expr(ParseState *ps, Expr expr) {
  *ps->expr_out = expr;
  return ps->expr_out++;
}
static Stmt *ps_push_stmt(ParseState *ps, Stmt stmt) {
  *ps->stmt_out = stmt;
  return ps->stmt_out++;
}

static Expr *parse_expr(ParseState *ps);

static Expr *parse_primary(ParseState *ps) {
  Val val = {0};
  switch (ps->in->kind) {
    case LexKind_NIL:    val = (Val) { ValKind_NIL                       }; break;
    case LexKind_TRUE:   val = (Val) { ValKind_BOOL, .bool = 1           }; break;
    case LexKind_FALSE:  val = (Val) { ValKind_BOOL, .bool = 0           }; break;
    case LexKind_NUMBER: val = (Val) { ValKind_NUM,  .num  = ps->in->num }; break;
    case LexKind_STRING: val = (Val) { ValKind_STR,  .str  = ps->in->str }; break;
    case LexKind_IDENTIFIER: {
      for (Stmt *stmt = ps->stmt_out; stmt != ps->stmt_start; stmt--)
        if (stmt->kind != StmtKind_DECL  &&
            stmt->scope == ps->scopes    &&
            stmt->op.hash == ps->in->hash) 
          return ps_push_expr(ps, (Expr) {
            ExprKind_VARIABLE,
            *ps->in++,
            .scope = ps->scopes,
            .scope_var = stmt->scope_var
          });
      bailf("Couldn't find declaration for identifier %d", ps->in->hash);
    } break;
    case LexKind_LEFT_PAREN: {
      ps->in++;
      Expr *expr = parse_expr(ps);
      if (ps->in->kind != LexKind_RIGHT_PAREN) bail("unmatched paren!?");
      return ps_push_expr(ps, (Expr) { ExprKind_GROUPING, *ps->in++, .expr = expr });
    } break;
    default: bail("Unexpected token!"); break;
  }
  return ps_push_expr(ps, (Expr) { ExprKind_LITERAL, *ps->in++, .val = val });
}

static Expr *parse_unary(ParseState *ps) {
  if (ps->in->kind == LexKind_BANG || ps->in->kind == LexKind_MINUS) {
    return ps_push_expr(ps, (Expr) { ExprKind_UNARY, *ps->in++, .expr = parse_unary(ps) });
  }
  return parse_primary(ps);
}

static Expr *parse_factor(ParseState *ps) {
  Expr *expr = parse_unary(ps);

  while (ps->in->kind == LexKind_SLASH || ps->in->kind == LexKind_STAR)
    expr = ps_push_expr(ps, (Expr) { ExprKind_BINARY, *ps->in++, {{ expr, parse_unary(ps) }} });

  return expr;
}

static Expr *parse_term(ParseState *ps) {
  Expr *expr = parse_factor(ps);

  while (ps->in->kind == LexKind_PLUS || ps->in->kind == LexKind_MINUS)
    expr = ps_push_expr(ps, (Expr) { ExprKind_BINARY, *ps->in++, {{ expr, parse_factor(ps) }} });

  return expr;
}

static Expr *parse_comparison(ParseState *ps) {
  Expr *expr = parse_term(ps);

  while (ps->in->kind == LexKind_LESS          ||
         ps->in->kind == LexKind_LESS_EQUAL    ||
         ps->in->kind == LexKind_GREATER       ||
         ps->in->kind == LexKind_GREATER_EQUAL)
    expr = ps_push_expr(ps, (Expr) { ExprKind_BINARY, *ps->in++, {{ expr, parse_term(ps) }} });

  return expr;
}

static Expr *parse_equality(ParseState *ps) {
  Expr *expr = parse_comparison(ps);

  while (ps->in->kind == LexKind_BANG_EQUAL || ps->in->kind == LexKind_EQUAL_EQUAL)
    expr = ps_push_expr(ps, (Expr) { ExprKind_BINARY, *ps->in++, {{ expr, parse_comparison(ps) }} });

  return expr;
}

static Expr *parse_expr(ParseState *ps) {
  return parse_equality(ps);
}

static Stmt *parse_stmt(ParseState *ps) {
  Stmt *stmt = NULL;
  switch (ps->in->kind) {
    case LexKind_VAR: {
      Lex ident = ps->in[1];
      assert(    ident.kind == LexKind_IDENTIFIER &&
             ps->in[2].kind == LexKind_EQUAL      );
      ps->in += 3;
      stmt = ps_push_stmt(ps, (Stmt) {
        .kind = StmtKind_DECL,
        .op = ident,
        .expr = parse_expr(ps),
        .scope = ps->scopes,
        .scope_var = ps->scope_vars++,
      });
    } break;
    case LexKind_LEFT_BRACE: {
      uint32_t temp = ps->scope_vars;
      ps->scope_vars = 0;
      ps->scopes++;
      Stmt *last = NULL;
      Lex lbl = *ps->in++;
      while (ps->in->kind != LexKind_RIGHT_BRACE)
        last = parse_stmt(ps);
      ps->in++;
      stmt = ps_push_stmt(ps, (Stmt) {
        StmtKind_BLOCK,
        lbl,
        .last_stmt = last,
        .scope = ps->scopes,
        .var_count = ps->scope_vars,
      });
      ps->scopes--;
      ps->scope_vars = temp;
    } break;
    case LexKind_PRINT: {
      Lex pl = *ps->in++;
      stmt = ps_push_stmt(ps, (Stmt) { StmtKind_PRINT, pl, .expr = parse_expr(ps) });
    } break;
    default: {
      Expr *e = parse_expr(ps);
      stmt = ps_push_stmt(ps, (Stmt) { StmtKind_EXPR, *ps->in, .expr = e });
    } break;
  }
  assert(ps->in++->kind == LexKind_SEMICOLON);
  return stmt;
}

static void parse_program(ParseState *ps) {
  while (ps->in->kind != LexKind_EOF)
    parse_stmt(ps);
}

static void print_expr(char *src, Expr *expr) {
  switch (expr->kind) {
    case ExprKind_NULL: bail("Unexpected NULL expr?"); break;
    case ExprKind_BINARY: {
      putchar('(');
      lex_print(src, &expr->op);
      putchar(' ');
      print_expr(src, expr->lhs);
      putchar(' ');
      print_expr(src, expr->rhs);
      putchar(')');
    } break;
    case ExprKind_UNARY: {
      putchar('(');
      lex_print(src, &expr->op);
      putchar(' ');
      print_expr(src, expr->expr);
      putchar(')');
    } break;
    case ExprKind_LITERAL: val_print(expr->val); break;
    case ExprKind_VARIABLE: printf("%ld", expr->op.hash); break;
    case ExprKind_GROUPING: putchar('('); print_expr(src, expr->expr); putchar(')'); break;
  }
}

static void print_stmts(char *src, Stmt *stmt, Stmt *end) {
  for (; stmt->kind && stmt != end; stmt++)
    switch (stmt->kind) {
      case StmtKind_NULL: bail("Unexpected null statement!");                               break;
      case StmtKind_DECL: printf("var %ld = ", stmt->op.hash); print_expr(src, stmt->expr); break;
      case StmtKind_EXPR: print_expr(src, stmt->expr);                                      break;
      case StmtKind_PRINT: printf("print "); print_expr(src, stmt->expr);                   break;
      case StmtKind_BLOCK: {
        printf("{\n");
        print_stmts(src, stmt+1, stmt->last_stmt);
        printf("}\n");
      } break;
    }
  printf(";\n");
}

typedef struct { Expr *expr; Stmt *stmt; uint32_t global_var_count; } ParseRes;
static ParseRes parse(char *str) {
  Lex *lex_in = lex(str), *lex_end = lex_in;

  int stmt_n = 0; /* approximate number of statements lexed */
  for (; lex_end->kind; lex_end++)
    stmt_n += lex_end->kind == LexKind_SEMICOLON;

  if (stmt_n == 0) bail("Expected at least one statement");

  ParseState ps = {
    .src = str,
    .in = lex_in,
    .expr_start = calloc(lex_end - lex_in, sizeof(Expr)),
    .stmt_start = calloc(          stmt_n, sizeof(Stmt)),
  };
  ps.expr_out = ps.expr_start;
  ps.stmt_out = ps.stmt_start;
  parse_program(&ps);

  print_stmts(str, ps.stmt_start, NULL);
  free(lex_in);
  return (ParseRes) {
    .expr = ps.expr_start,
    .stmt = ps.stmt_start
    .global_var_count = ps.scope_vars,
  };
}

typedef struct {
  uint32_t *scope_stack; /* stores where each scope's variables start */
  Val *stack; /* pointer to next available Val in stack_mem */
  Val *stack_mem;
} EvalState;

static Val *es_var(EvalState *es, uint32_t scope, uint32_t scope_var) {
  return es->stack_mem + es->scope_stack[scope] + scope_var;
}

static Val eval_expr(EvalState *es, Expr *expr);

static Val eval_binary(EvalState *es, Expr *expr) {
  Val lhs = eval_expr(es, expr->lhs),
      rhs = eval_expr(es, expr->rhs);
  if (lhs.kind == ValKind_NUM && rhs.kind == ValKind_NUM)
    switch (expr->op.kind) {
      case LexKind_MINUS:         return (Val) { ValKind_NUM,  .num  = lhs.num - rhs.num  };
      case LexKind_PLUS:          return (Val) { ValKind_NUM,  .num  = lhs.num + rhs.num  };
      case LexKind_SLASH:         return (Val) { ValKind_NUM,  .num  = lhs.num / rhs.num  };
      case LexKind_STAR:          return (Val) { ValKind_NUM,  .num  = lhs.num * rhs.num  };
      case LexKind_GREATER:       return (Val) { ValKind_BOOL, .bool = lhs.num >  rhs.num };
      case LexKind_LESS:          return (Val) { ValKind_BOOL, .bool = lhs.num <  rhs.num };
      case LexKind_GREATER_EQUAL: return (Val) { ValKind_BOOL, .bool = lhs.num >= rhs.num };
      case LexKind_LESS_EQUAL:    return (Val) { ValKind_BOOL, .bool = lhs.num <= rhs.num };
      case LexKind_EQUAL_EQUAL:   return (Val) { ValKind_BOOL, .bool = lhs.num == rhs.num };
      default: bail("Operator not supported between two numbers"); break;
    };
  if (lhs.kind == ValKind_BOOL && rhs.kind == ValKind_BOOL) {
    if (expr->op.kind == LexKind_EQUAL_EQUAL)
      return (Val) { ValKind_BOOL, .bool = lhs.bool == rhs.bool };
    else
      bail("Operator not supported between two bools.");
  }
  if (lhs.kind == ValKind_STR && rhs.kind == ValKind_STR) {
    switch (expr->op.kind) {
      case LexKind_EQUAL_EQUAL: return (Val) { ValKind_BOOL, .bool = strcmp(lhs.str, rhs.str) == 0 };
      case LexKind_PLUS: {
        int lhs_len = strlen(lhs.str),
            rhs_len = strlen(rhs.str);
        char *str = malloc(lhs_len + rhs_len);
        memcpy(str          , lhs.str, lhs_len);
        memcpy(str + lhs_len, rhs.str, rhs_len);
        free(lhs.str); free(rhs.str);
        return (Val) { ValKind_STR, .str = str };
      }
      default: bail("Operator not supported between two strings.");
    }
  }
  bail("Operator not supported for given types.");
  return (Val) {0};
}

static Val eval_unary(EvalState *es, Expr *expr) {
  Val inr = eval_expr(es, expr->expr);
  switch (expr->op.kind) {
    case (LexKind_MINUS): {
      if (inr.kind == ValKind_NUM)
        return (Val) { .kind = ValKind_NUM, .num = -inr.num };
      else
        bail("The unary - operator only takes a number");
    } break;
    case (LexKind_BANG): {
      if (inr.kind == ValKind_BOOL)
        return (Val) { .kind = ValKind_BOOL, .bool = !inr.bool };
      else
        bail("The unary ! operator only takes a boolean");
    } break;
    default: bail("Unknown unary operator");
  }
  return (Val) {0};
}

static Val eval_expr(EvalState *es, Expr *expr) {
  switch (expr->kind) {
    case ExprKind_NULL:     bail("Unexpected NULL expr?");                    break;
    case ExprKind_BINARY:   return eval_binary(es, expr);                     break;
    case ExprKind_UNARY:    return eval_unary(es, expr);                      break;
    case ExprKind_LITERAL:  return expr->val;                                 break;
    case ExprKind_VARIABLE: return *es_var(es, expr->scope, expr->scope_var); break;
    case ExprKind_GROUPING: return eval_expr(es, expr->expr);                 break;
  }
  return (Val) {0};
}

static void eval_stmt(EvalState *es, Stmt *stmt) {
  switch (stmt->kind) {
    case StmtKind_PRINT: val_print(eval_expr(es, stmt->expr)); putchar('\n'); break;
    case StmtKind_DECL: {
      *es_var(es, stmt->scope, stmt->scope_var) = eval_expr(es, stmt->expr);
    } break;
    case StmtKind_BLOCK: {
      for (Stmt *loop_st = stmt; loop_st++ != stmt->last_stmt; )
        eval_stmt(es, loop_st);
    } break;
    default: bail("Unhandled StmtKind"); break;
  }
}

static void eval(char *str) {
  ParseRes pr = parse(str);
  EvalState es = {
    .stack_mem = calloc(1 << 10, sizeof(Val)),
    .scope_stack = calloc(1 << 5, sizeof(uint32_t))
  };
  es.stack = es.stack_mem + pr.global_var_count;

  Stmt *wtr = pr.stmt;
  for (; wtr->kind; wtr++)
    eval_stmt(&es, wtr);

  free(pr.expr);
  free(pr.stmt);

  free(es.stack_mem);
  free(es.scope_stack);
}

static void repl() {
  puts("Welcome to CedLox REPL");
  char *line = NULL;
  size_t size = 0;
  for (;;) {
    printf("> ");
    ssize_t chars = getline(&line, &size, stdin);
    if (chars == -1) puts("Couldn't read input");
    else eval(line);
  }
  free(line);
}

int main(int argc, char *argv[]) {
  if (argc == 2) puts(argv[1]);
  else if (argc == 1) repl();
  else {
    puts("Usage: cedlox [script]");
    puts("runs [script] if provided, otherwise opens REPL");
  }
}
