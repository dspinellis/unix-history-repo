#include "itran.h"
#include "lex.h"
#include "token.h"

/*
 * Token table - contains an entry for each token type
 * with printable name of token, token type, and flags
 * for semicolon insertion.
 */

struct toktab toktab[] = {
/*  token		token type	flags */

   /* primitives */
   "identifier",      IDENT,         BEGINNER+ENDER,    /*   0 */
   "integer-literal", INTLIT,        BEGINNER+ENDER,    /*   1 */
   "real-literal",    REALLIT,       BEGINNER+ENDER,    /*   2 */
   "string-literal",  STRINGLIT,     BEGINNER+ENDER,    /*   3 */
   "cset-literal",    CSETLIT,       BEGINNER+ENDER,    /*   4 */
   "end-of-file",     EOFX,          0,                 /*   5 */

   /* reserved words */
   "break",           BREAK,         BEGINNER+ENDER,    /*   6 */
   "by",              BY,            0,                 /*   7 */
   "case",            CASE,          BEGINNER,          /*   8 */
   "create",          CREATE,        BEGINNER,          /*   9 */
   "default",         DEFAULT,       BEGINNER,          /*  10 */
   "do",              DO,            0,                 /*  11 */
   "dynamic",         DYNAMIC,       BEGINNER,          /*  12 */
   "else",            ELSE,          0,                 /*  13 */
   "end",             END,           BEGINNER,          /*  14 */
   "every",           EVERY,         BEGINNER,          /*  15 */
   "fail",            FAIL,          BEGINNER+ENDER,    /*  16 */
   "global",          GLOBAL,        0,                 /*  17 */
   "if",              IF,            BEGINNER,          /*  18 */
   "initial",         INITIAL,       BEGINNER,          /*  19 */
   "link",            LINK,          0,                 /*  20 */
   "local",           LOCAL,         BEGINNER,          /*  21 */
   "next",            NEXT,          BEGINNER+ENDER,    /*  22 */
   "not",             NOT,           BEGINNER,          /*  23 */
   "of",              OF,            0,                 /*  24 */
   "procedure",       PROCEDURE,     0,                 /*  25 */
   "record",          RECORD,        0,                 /*  26 */
   "repeat",          REPEAT,        BEGINNER,          /*  27 */
   "return",          RETURN,        BEGINNER+ENDER,    /*  28 */
   "static",          STATIC,        BEGINNER,          /*  29 */
   "suspend",         SUSPEND,       BEGINNER+ENDER,    /*  30 */
   "then",            THEN,          0,                 /*  31 */
   "to",              TO,            0,                 /*  32 */
   "until",           UNTIL,         BEGINNER,          /*  33 */
   "while",           WHILE,         BEGINNER,          /*  34 */

   /* operators */
   ":=",              ASSIGN,        0,                 /*  35 */
   "@",               AT,            BEGINNER,          /*  36 */
   "@:=",             AUGACT,        0,                 /*  37 */
   "&:=",             AUGAND,        0,                 /*  38 */
   "=:=",             AUGEQ,         0,                 /*  39 */
   "===:=",           AUGEQV,        0,                 /*  40 */
   ">=:=",            AUGGE,         0,                 /*  41 */
   ">:=",             AUGGT,         0,                 /*  42 */
   "<=:=",            AUGLE,         0,                 /*  43 */
   "<:=",             AUGLT,         0,                 /*  44 */
   "~=:=",            AUGNE,         0,                 /*  45 */
   "~===:=",          AUGNEQV,       0,                 /*  46 */
   "==:=",            AUGSEQ,        0,                 /*  47 */
   ">>=:=",           AUGSGE,        0,                 /*  48 */
   ">>:=",            AUGSGT,        0,                 /*  49 */
   "<<=:=",           AUGSLE,        0,                 /*  50 */
   "<<:=",            AUGSLT,        0,                 /*  51 */
   "~==:=",           AUGSNE,        0,                 /*  52 */
   "\\",              BACKSLASH,     BEGINNER,          /*  53 */
   "!",               BANG,          BEGINNER,          /*  54 */
   "|",               BAR,           BEGINNER,          /*  55 */
   "^",               CARET,         BEGINNER,          /*  56 */
   "^:=",             CARETASGN,     0,                 /*  57 */
   ":",               COLON,         0,                 /*  58 */
   ",",               COMMA,         0,                 /*  59 */
   "||",              CONCAT,        BEGINNER,          /*  60 */
   "||:=",            CONCATASGN,    0,                 /*  61 */
   "&",               CONJUNC,       BEGINNER,          /*  62 */
   ".",               DOT,           BEGINNER,          /*  63 */
   "--",              DIFF,          BEGINNER,          /*  64 */
   "--:=",            DIFFASGN,      0,                 /*  65 */
   "===",             EQUIV,         BEGINNER,          /*  66 */
   "**",              INTER,         BEGINNER,          /*  67 */
   "**:=",            INTERASGN,     0,                 /*  68 */
   "{",               LBRACE,        BEGINNER,          /*  69 */
   "[",               LBRACK,        BEGINNER,          /*  70 */
   "|||",             LCONCAT,       BEGINNER,          /*  71 */
   "|||:=",           LCONCATASGN,   BEGINNER,          /*  72 */
   "==",              LEXEQ,         BEGINNER,          /*  73 */
   ">>=",             LEXGE,         0,                 /*  74 */
   ">>",              LEXGT,         0,                 /*  75 */
   "<<=",             LEXLE,         0,                 /*  76 */
   "<<",              LEXLT,         0,                 /*  77 */
   "~==",             LEXNE,         BEGINNER,          /*  78 */
   "(",               LPAREN,        BEGINNER,          /*  79 */
   "-:",              MCOLON,        0,                 /*  80 */
   "-",               MINUS,         BEGINNER,          /*  81 */
   "-:=",             MINUSASGN,     0,                 /*  82 */
   "%",               MOD,           0,                 /*  83 */
   "%:=",             MODASGN,       0,                 /*  84 */
   "~===",            NOTEQUIV,      BEGINNER,          /*  85 */
   "=",               NUMEQ,         BEGINNER,          /*  86 */
   ">=",              NUMGE,         0,                 /*  87 */
   ">",               NUMGT,         0,                 /*  88 */
   "<=",              NUMLE,         0,                 /*  89 */
   "<",               NUMLT,         0,                 /*  90 */
   "~=",              NUMNE,         BEGINNER,          /*  91 */
   "+:",              PCOLON,        0,                 /*  92 */
   "+",               PLUS,          BEGINNER,          /*  93 */
   "+:=",             PLUSASGN,      0,                 /*  94 */
   "?",               QMARK,         BEGINNER,          /*  95 */
   "<-",              REVASSIGN,     0,                 /*  96 */
   "<->",             REVSWAP,       0,                 /*  97 */
   "}",               RBRACE,        ENDER,             /*  98 */
   "]",               RBRACK,        ENDER,             /*  99 */
   ")",               RPAREN,        ENDER,             /* 100 */
   ";",               SEMICOL,       0,                 /* 101 */
   "?:=",             SCANASGN,      0,                 /* 102 */
   "/",               SLASH,         BEGINNER,          /* 103 */
   "/:=",             SLASHASGN,     0,                 /* 104 */
   "*",               STAR,          BEGINNER,          /* 105 */
   "*:=",             STARASGN,      0,                 /* 106 */
   ":=:",             SWAP,          0,                 /* 107 */
   "~",               TILDE,         BEGINNER,          /* 108 */
   "++",              UNION,         BEGINNER,          /* 109 */
   "++:=",            UNIONASGN,     0,                 /* 110 */
   "end-of-file",     0,             0,
   };

/*
 * restab[c] points to the first keyword in toktab which
 * begins with the letter c.
 */

struct toktab *restab[] = {
                             NULL       , NULL       , /*   _` */
   NULL,        &toktab[ 6], &toktab[ 8], &toktab[10], /* abcd */
   &toktab[13], &toktab[16], &toktab[17], NULL,        /* efgh */
   &toktab[18], NULL,        NULL,        &toktab[20], /* ijkl */
   NULL,        &toktab[22], &toktab[24], &toktab[25], /* mnop */
   NULL,        &toktab[26], &toktab[29], &toktab[31], /* qrst */
   &toktab[33], NULL,        &toktab[34], NULL,        /* uvwx */
   NULL,        NULL,                                  /* yz */
   };
