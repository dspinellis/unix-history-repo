
# line 32 "./m2-exp.y"
#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "frame.h"
#include "expression.h"
#include "language.h"
#include "value.h"
#include "parser-defs.h"
#include "bfd.h"
#include "symfile.h"
#include "objfiles.h"

/* These MUST be included in any grammar file!!!! Please choose unique names!
   Note that this are a combined list of variables that can be produced
   by any one of bison, byacc, or yacc. */
#define	yymaxdepth m2_maxdepth
#define	yyparse	m2_parse
#define	yylex	m2_lex
#define	yyerror	m2_error
#define	yylval	m2_lval
#define	yychar	m2_char
#define	yydebug	m2_debug
#define	yypact	m2_pact
#define	yyr1	m2_r1
#define	yyr2	m2_r2
#define	yydef	m2_def
#define	yychk	m2_chk
#define	yypgo	m2_pgo
#define	yyact	m2_act
#define	yyexca	m2_exca
#define yyerrflag m2_errflag
#define yynerrs	m2_nerrs
#define	yyps	m2_ps
#define	yypv	m2_pv
#define	yys	m2_s
#define	yy_yys	m2_yys
#define	yystate	m2_state
#define	yytmp	m2_tmp
#define	yyv	m2_v
#define	yy_yyv	m2_yyv
#define	yyval	m2_val
#define	yylloc	m2_lloc
#define yyss	m2_yyss		/* byacc */
#define	yyssp	m2_yysp		/* byacc */
#define	yyvs	m2_yyvs		/* byacc */
#define	yyvsp	m2_yyvsp	/* byacc */

#if 0
static char *
make_qualname PARAMS ((char *, char *));
#endif

static int
parse_number PARAMS ((int));

static int
yylex PARAMS ((void));

static void
yyerror PARAMS ((char *));

int
yyparse PARAMS ((void));

/* The sign of the number being parsed. */
int number_sign = 1;

/* The block that the module specified by the qualifer on an identifer is
   contained in, */
struct block *modblock=0;

/* #define	YYDEBUG	1 */

# line 112 "./m2-exp.y"
typedef union 
  {
    LONGEST lval;
    unsigned LONGEST ulval;
    double dval;
    struct symbol *sym;
    struct type *tval;
    struct stoken sval;
    int voidval;
    struct block *bval;
    enum exp_opcode opcode;
    struct internalvar *ivar;

    struct type **tvec;
    int *ivec;
  } YYSTYPE;
# define INT 257
# define HEX 258
# define ERROR 259
# define UINT 260
# define M2_TRUE 261
# define M2_FALSE 262
# define CHAR 263
# define FLOAT 264
# define STRING 265
# define NAME 266
# define BLOCKNAME 267
# define IDENT 268
# define VARNAME 269
# define TYPENAME 270
# define SIZE 271
# define CAP 272
# define ORD 273
# define HIGH 274
# define ABS 275
# define MIN_FUNC 276
# define MAX_FUNC 277
# define FLOAT_FUNC 278
# define VAL 279
# define CHR 280
# define ODD 281
# define TRUNC 282
# define INC 283
# define DEC 284
# define INCL 285
# define EXCL 286
# define COLONCOLON 287
# define LAST 288
# define REGNAME 289
# define INTERNAL_VAR 290
# define ABOVE_COMMA 291
# define ASSIGN 292
# define LEQ 293
# define GEQ 294
# define NOTEQUAL 295
# define IN 296
# define OROR 297
# define ANDAND 298
# define DIV 299
# define MOD 300
# define UNARY 301
# define DOT 302
# define NOT 303
# define QID 304

# line 181 "./m2-exp.y"
/* Ensure that if the generated parser contains any calls to malloc/realloc,
   that they get mapped to xmalloc/xrealloc.  We have to do this here
   rather than earlier in the file because this is the first point after
   the place where the SVR4 yacc includes <malloc.h>, and if we do it
   before that, then the remapped declarations in <malloc.h> will collide
   with the ones in "defs.h". */

#define malloc	xmalloc
#define realloc	xrealloc
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 673 "./m2-exp.y"


#if 0  /* FIXME! */
int
overflow(a,b)
   long a,b;
{
   return (MAX_OF_TYPE(builtin_type_m2_int) - b) < a;
}

int
uoverflow(a,b)
   unsigned long a,b;
{
   return (MAX_OF_TYPE(builtin_type_m2_card) - b) < a;
}
#endif /* FIXME */

/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/*** Needs some error checking for the float case ***/

static int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register LONGEST n = 0;
  register LONGEST prevn = 0;
  register int c,i,ischar=0;
  register int base = input_radix;
  register int len = olen;
  int unsigned_p = number_sign == 1 ? 1 : 0;

  if(p[len-1] == 'H')
  {
     base = 16;
     len--;
  }
  else if(p[len-1] == 'C' || p[len-1] == 'B')
  {
     base = 8;
     ischar = p[len-1] == 'C';
     len--;
  }

  /* Scan the number */
  for (c = 0; c < len; c++)
  {
    if (p[c] == '.' && base == 10)
      {
	/* It's a float since it contains a point.  */
	yylval.dval = atof (p);
	lexptr += len;
	return FLOAT;
      }
    if (p[c] == '.' && base != 10)
       error("Floating point numbers must be base 10.");
    if (base == 10 && (p[c] < '0' || p[c] > '9'))
       error("Invalid digit \'%c\' in number.",p[c]);
 }

  while (len-- > 0)
    {
      c = *p++;
      n *= base;
      if( base == 8 && (c == '8' || c == '9'))
	 error("Invalid digit \'%c\' in octal number.",c);
      if (c >= '0' && c <= '9')
	i = c - '0';
      else
	{
	  if (base == 16 && c >= 'A' && c <= 'F')
	    i = c - 'A' + 10;
	  else
	     return ERROR;
	}
      n+=i;
      if(i >= base)
	 return ERROR;
      if(!unsigned_p && number_sign == 1 && (prevn >= n))
	 unsigned_p=1;		/* Try something unsigned */
      /* Don't do the range check if n==i and i==0, since that special
	 case will give an overflow error. */
      if(RANGE_CHECK && n!=i && i)
      {
	 if((unsigned_p && (unsigned)prevn >= (unsigned)n) ||
	    ((!unsigned_p && number_sign==-1) && -prevn <= -n))
	    range_error("Overflow on numeric constant.");
      }
	 prevn=n;
    }

  lexptr = p;
  if(*p == 'B' || *p == 'C' || *p == 'H')
     lexptr++;			/* Advance past B,C or H */

  if (ischar)
  {
     yylval.ulval = n;
     return CHAR;
  }
  else if ( unsigned_p && number_sign == 1)
  {
     yylval.ulval = n;
     return UINT;
  }
  else if((unsigned_p && (n<0))) {
     range_error("Overflow on numeric constant -- number too large.");
     /* But, this can return if range_check == range_warn.  */
  }
  yylval.lval = n;
  return INT;
}


/* Some tokens */

static struct
{
   char name[4];
   int token;
} tokentab2[] =
{
    { {'<', '>'},    NOTEQUAL 	},
    { {':', '='},    ASSIGN	},
    { {'<', '='},    LEQ	},
    { {'>', '='},    GEQ	},
    { {':', ':'},    COLONCOLON },

};

/* Some specific keywords */

struct keyword {
   char keyw[10];
   int token;
};

static struct keyword keytab[] =
{
    {"OR" ,   OROR	 },
    {"IN",    IN         },/* Note space after IN */
    {"AND",   ANDAND     },
    {"ABS",   ABS	 },
    {"CHR",   CHR	 },
    {"DEC",   DEC	 },
    {"NOT",   NOT	 },
    {"DIV",   DIV    	 },
    {"INC",   INC	 },
    {"MAX",   MAX_FUNC	 },
    {"MIN",   MIN_FUNC	 },
    {"MOD",   MOD	 },
    {"ODD",   ODD	 },
    {"CAP",   CAP	 },
    {"ORD",   ORD	 },
    {"VAL",   VAL	 },
    {"EXCL",  EXCL	 },
    {"HIGH",  HIGH       },
    {"INCL",  INCL	 },
    {"SIZE",  SIZE       },
    {"FLOAT", FLOAT_FUNC },
    {"TRUNC", TRUNC	 },
};


/* Read one token, getting characters through lexptr.  */

/* This is where we will check to make sure that the language and the operators used are
   compatible  */

static int
yylex ()
{
  register int c;
  register int namelen;
  register int i;
  register char *tokstart;
  register char quote;

 retry:

  tokstart = lexptr;


  /* See if it is a special token of length 2 */
  for( i = 0 ; i < sizeof tokentab2 / sizeof tokentab2[0] ; i++)
     if(!strncmp(tokentab2[i].name, tokstart, 2))
     {
	lexptr += 2;
	return tokentab2[i].token;
     }

  switch (c = *tokstart)
    {
    case 0:
      return 0;

    case ' ':
    case '\t':
    case '\n':
      lexptr++;
      goto retry;

    case '(':
      paren_depth++;
      lexptr++;
      return c;

    case ')':
      if (paren_depth == 0)
	return 0;
      paren_depth--;
      lexptr++;
      return c;

    case ',':
      if (comma_terminates && paren_depth == 0)
	return 0;
      lexptr++;
      return c;

    case '.':
      /* Might be a floating point number.  */
      if (lexptr[1] >= '0' && lexptr[1] <= '9')
	break;			/* Falls into number code.  */
      else
      {
	 lexptr++;
	 return DOT;
      }

/* These are character tokens that appear as-is in the YACC grammar */
    case '+':
    case '-':
    case '*':
    case '/':
    case '^':
    case '<':
    case '>':
    case '[':
    case ']':
    case '=':
    case '{':
    case '}':
    case '#':
    case '@':
    case '~':
    case '&':
      lexptr++;
      return c;

    case '\'' :
    case '"':
      quote = c;
      for (namelen = 1; (c = tokstart[namelen]) != quote && c != '\0'; namelen++)
	if (c == '\\')
	  {
	    c = tokstart[++namelen];
	    if (c >= '0' && c <= '9')
	      {
		c = tokstart[++namelen];
		if (c >= '0' && c <= '9')
		  c = tokstart[++namelen];
	      }
	  }
      if(c != quote)
	 error("Unterminated string or character constant.");
      yylval.sval.ptr = tokstart + 1;
      yylval.sval.length = namelen - 1;
      lexptr += namelen + 1;

      if(namelen == 2)  	/* Single character */
      {
	   yylval.ulval = tokstart[1];
	   return CHAR;
      }
      else
	 return STRING;
    }

  /* Is it a number?  */
  /* Note:  We have already dealt with the case of the token '.'.
     See case '.' above.  */
  if ((c >= '0' && c <= '9'))
    {
      /* It's a number.  */
      int got_dot = 0, got_e = 0;
      register char *p = tokstart;
      int toktype;

      for (++p ;; ++p)
	{
	  if (!got_e && (*p == 'e' || *p == 'E'))
	    got_dot = got_e = 1;
	  else if (!got_dot && *p == '.')
	    got_dot = 1;
	  else if (got_e && (p[-1] == 'e' || p[-1] == 'E')
		   && (*p == '-' || *p == '+'))
	    /* This is the sign of the exponent, not the end of the
	       number.  */
	    continue;
	  else if ((*p < '0' || *p > '9') &&
		   (*p < 'A' || *p > 'F') &&
		   (*p != 'H'))  /* Modula-2 hexadecimal number */
	    break;
	}
	toktype = parse_number (p - tokstart);
        if (toktype == ERROR)
	  {
	    char *err_copy = (char *) alloca (p - tokstart + 1);

	    memcpy (err_copy, tokstart, p - tokstart);
	    err_copy[p - tokstart] = 0;
	    error ("Invalid number \"%s\".", err_copy);
	  }
	lexptr = p;
	return toktype;
    }

  if (!(c == '_' || c == '$'
	|| (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')))
    /* We must have come across a bad character (e.g. ';').  */
    error ("Invalid character '%c' in expression.", c);

  /* It's a name.  See how long it is.  */
  namelen = 0;
  for (c = tokstart[namelen];
       (c == '_' || c == '$' || (c >= '0' && c <= '9')
	|| (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
       c = tokstart[++namelen])
    ;

  /* The token "if" terminates the expression and is NOT
     removed from the input stream.  */
  if (namelen == 2 && tokstart[0] == 'i' && tokstart[1] == 'f')
    {
      return 0;
    }

  lexptr += namelen;

  /* Handle the tokens $digits; also $ (short for $0) and $$ (short for $$1)
     and $$digits (equivalent to $<-digits> if you could type that).
     Make token type LAST, and put the number (the digits) in yylval.  */

  if (*tokstart == '$')
    {
      register int negate = 0;
      c = 1;
      /* Double dollar means negate the number and add -1 as well.
	 Thus $$ alone means -1.  */
      if (namelen >= 2 && tokstart[1] == '$')
	{
	  negate = 1;
	  c = 2;
	}
      if (c == namelen)
	{
	  /* Just dollars (one or two) */
	  yylval.lval = - negate;
	  return LAST;
	}
      /* Is the rest of the token digits?  */
      for (; c < namelen; c++)
	if (!(tokstart[c] >= '0' && tokstart[c] <= '9'))
	  break;
      if (c == namelen)
	{
	  yylval.lval = atoi (tokstart + 1 + negate);
	  if (negate)
	    yylval.lval = - yylval.lval;
	  return LAST;
	}
    }

  /* Handle tokens that refer to machine registers:
     $ followed by a register name.  */

  if (*tokstart == '$') {
    for (c = 0; c < NUM_REGS; c++)
      if (namelen - 1 == strlen (reg_names[c])
	  && !strncmp (tokstart + 1, reg_names[c], namelen - 1))
	{
	  yylval.lval = c;
	  return REGNAME;
	}
    for (c = 0; c < num_std_regs; c++)
     if (namelen - 1 == strlen (std_regs[c].name)
	 && !strncmp (tokstart + 1, std_regs[c].name, namelen - 1))
       {
	 yylval.lval = std_regs[c].regnum;
	 return REGNAME;
       }
  }


  /*  Lookup special keywords */
  for(i = 0 ; i < sizeof(keytab) / sizeof(keytab[0]) ; i++)
     if(namelen == strlen(keytab[i].keyw) && !strncmp(tokstart,keytab[i].keyw,namelen))
	   return keytab[i].token;

  yylval.sval.ptr = tokstart;
  yylval.sval.length = namelen;

  /* Any other names starting in $ are debugger internal variables.  */

  if (*tokstart == '$')
    {
      yylval.ivar = (struct internalvar *) lookup_internalvar (copy_name (yylval.sval) + 1);
      return INTERNAL_VAR;
    }


  /* Use token-type BLOCKNAME for symbols that happen to be defined as
     functions.  If this is not so, then ...
     Use token-type TYPENAME for symbols that happen to be defined
     currently as names of types; NAME for other symbols.
     The caller is not constrained to care about the distinction.  */
 {


    char *tmp = copy_name (yylval.sval);
    struct symbol *sym;

    if (lookup_partial_symtab (tmp))
      return BLOCKNAME;
    sym = lookup_symbol (tmp, expression_context_block,
			 VAR_NAMESPACE, 0, NULL);
    if (sym && SYMBOL_CLASS (sym) == LOC_BLOCK)
      return BLOCKNAME;
    if (lookup_typename (copy_name (yylval.sval), expression_context_block, 1))
      return TYPENAME;

    if(sym)
    {
       switch(sym->class)
       {
       case LOC_STATIC:
       case LOC_REGISTER:
       case LOC_ARG:
       case LOC_REF_ARG:
       case LOC_REGPARM:
       case LOC_LOCAL:
       case LOC_LOCAL_ARG:
       case LOC_CONST:
       case LOC_CONST_BYTES:
	  return NAME;

       case LOC_TYPEDEF:
	  return TYPENAME;

       case LOC_BLOCK:
	  return BLOCKNAME;

       case LOC_UNDEF:
	  error("internal:  Undefined class in m2lex()");

       case LOC_LABEL:
	  error("internal:  Unforseen case in m2lex()");
       }
    }
    else
    {
       /* Built-in BOOLEAN type.  This is sort of a hack. */
       if(!strncmp(tokstart,"TRUE",4))
       {
	  yylval.ulval = 1;
	  return M2_TRUE;
       }
       else if(!strncmp(tokstart,"FALSE",5))
       {
	  yylval.ulval = 0;
	  return M2_FALSE;
       }
    }

    /* Must be another type of name... */
    return NAME;
 }
}

#if 0		/* Unused */
static char *
make_qualname(mod,ident)
   char *mod, *ident;
{
   char *new = xmalloc(strlen(mod)+strlen(ident)+2);

   strcpy(new,mod);
   strcat(new,".");
   strcat(new,ident);
   return new;
}
#endif  /* 0 */

static void
yyerror(msg)
     char *msg;	/* unused */
{
   printf("Parsing:  %s\n",lexptr);
   if (yychar < 256)
     error("Invalid syntax in expression near character '%c'.",yychar);
   else
     error("Invalid syntax in expression");
}

/* Table of operators and their precedences for printing expressions.  */

const static struct op_print m2_op_print_tab[] = {
    {"+",   BINOP_ADD, PREC_ADD, 0},
    {"+",   UNOP_PLUS, PREC_PREFIX, 0},
    {"-",   BINOP_SUB, PREC_ADD, 0},
    {"-",   UNOP_NEG, PREC_PREFIX, 0},
    {"*",   BINOP_MUL, PREC_MUL, 0},
    {"/",   BINOP_DIV, PREC_MUL, 0},
    {"DIV", BINOP_INTDIV, PREC_MUL, 0},
    {"MOD", BINOP_REM, PREC_MUL, 0},
    {":=",  BINOP_ASSIGN, PREC_ASSIGN, 1},
    {"OR",  BINOP_OR, PREC_OR, 0},
    {"AND", BINOP_AND, PREC_AND, 0},
    {"NOT", UNOP_ZEROP, PREC_PREFIX, 0},
    {"=",   BINOP_EQUAL, PREC_EQUAL, 0},
    {"<>",  BINOP_NOTEQUAL, PREC_EQUAL, 0},
    {"<=",  BINOP_LEQ, PREC_ORDER, 0},
    {">=",  BINOP_GEQ, PREC_ORDER, 0},
    {">",   BINOP_GTR, PREC_ORDER, 0},
    {"<",   BINOP_LESS, PREC_ORDER, 0},
    {"^",   UNOP_IND, PREC_PREFIX, 0},
    {"@",   BINOP_REPEAT, PREC_REPEAT, 0},
};

/* The built-in types of Modula-2.  */

struct type *builtin_type_m2_char;
struct type *builtin_type_m2_int;
struct type *builtin_type_m2_card;
struct type *builtin_type_m2_real;
struct type *builtin_type_m2_bool;

struct type ** const (m2_builtin_types[]) = 
{
  &builtin_type_m2_char,
  &builtin_type_m2_int,
  &builtin_type_m2_card,
  &builtin_type_m2_real,
  &builtin_type_m2_bool,
  0
};

const struct language_defn m2_language_defn = {
  "modula-2",
  language_m2,
  m2_builtin_types,
  range_check_on,
  type_check_on,
  m2_parse,			/* parser */
  m2_error,			/* parser error function */
  &builtin_type_m2_int,		/* longest signed   integral type */
  &builtin_type_m2_card,		/* longest unsigned integral type */
  &builtin_type_m2_real,		/* longest floating point type */
  "0%XH", "0%", "XH",		/* Hex   format string, prefix, suffix */
  "%oB",  "%",  "oB",		/* Octal format string, prefix, suffix */
  m2_op_print_tab,		/* expression operators for printing */
  LANG_MAGIC
};

/* Initialization for Modula-2 */

void
_initialize_m2_exp ()
{
  /* Modula-2 "pervasive" types.  NOTE:  these can be redefined!!! */
  builtin_type_m2_int =
    init_type (TYPE_CODE_INT, TARGET_INT_BIT / TARGET_CHAR_BIT,
	       0,
	       "INTEGER", (struct objfile *) NULL);
  builtin_type_m2_card =
    init_type (TYPE_CODE_INT, TARGET_INT_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "CARDINAL", (struct objfile *) NULL);
  builtin_type_m2_real =
    init_type (TYPE_CODE_FLT, TARGET_FLOAT_BIT / TARGET_CHAR_BIT,
	       0,
	       "REAL", (struct objfile *) NULL);
  builtin_type_m2_char =
    init_type (TYPE_CODE_CHAR, TARGET_CHAR_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "CHAR", (struct objfile *) NULL);
  builtin_type_m2_bool =
    init_type (TYPE_CODE_BOOL, TARGET_INT_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "BOOLEAN", (struct objfile *) NULL);

  TYPE_NFIELDS(builtin_type_m2_bool) = 2;
  TYPE_FIELDS(builtin_type_m2_bool) = 
     (struct field *) malloc (sizeof (struct field) * 2);
  TYPE_FIELD_BITPOS(builtin_type_m2_bool,0) = 0;
  TYPE_FIELD_NAME(builtin_type_m2_bool,0) = (char *)malloc(6);
  strcpy(TYPE_FIELD_NAME(builtin_type_m2_bool,0),"FALSE");
  TYPE_FIELD_BITPOS(builtin_type_m2_bool,1) = 1;
  TYPE_FIELD_NAME(builtin_type_m2_bool,1) = (char *)malloc(5);
  strcpy(TYPE_FIELD_NAME(builtin_type_m2_bool,1),"TRUE");

  add_language (&m2_language_defn);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 40,
	287, 75,
	-2, 78,
-1, 119,
	292, 0,
	-2, 63,
	};
# define YYNPROD 83
# define YYLAST 1409
int yyact[]={

    26,    95,    98,     5,    39,     4,   144,   143,    96,    93,
   138,    92,   138,    89,   171,   155,    93,   159,    93,   101,
   172,    21,   153,   138,   152,    88,    87,    86,    85,    82,
    81,    80,    79,    49,    84,    51,    55,     5,    56,     4,
    52,    59,    78,    77,    65,    76,    49,   162,    51,    55,
   163,    56,    59,    52,    75,    65,    49,    49,   160,    51,
    55,   161,    56,   170,    52,    74,    62,    57,    63,    97,
    50,    73,    72,   145,   100,     6,    68,    62,    57,    63,
    40,    50,    42,    24,    48,    33,    38,    45,     1,     3,
     0,   168,    92,   137,   139,     0,    49,    48,    51,    92,
    45,    92,     0,    52,     0,     0,   140,    48,    48,     0,
    45,    45,     0,     0,     0,   147,    59,    24,     0,    65,
    38,    49,   183,    51,    55,     0,    56,    59,    52,     0,
    65,     0,    49,   182,    51,    55,     0,    56,     0,    52,
     0,    62,    57,    63,     0,    50,     0,    48,     0,    39,
    45,     0,    62,    57,    63,    59,    50,     0,    65,     0,
    49,   181,    51,    55,     0,    56,     0,    52,     0,     0,
     0,     0,    48,     0,     0,    45,     0,     0,     0,     0,
    62,    57,    63,    48,    50,     0,    45,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    48,     0,     0,    45,     0,     0,    29,     0,     0,
    30,    27,    28,    31,    32,    36,    43,    44,     0,     0,
    39,    18,     7,     8,    10,     9,    11,    12,    13,    14,
    15,    16,    17,    19,    20,    22,    23,     0,    34,    35,
    41,    29,     0,     0,    30,    27,    28,    31,    32,    36,
    43,    44,     0,    37,    39,    18,     7,     8,    10,     9,
    11,    12,    13,    14,    15,    16,    17,    19,    20,    22,
    23,     0,    34,    35,    41,     0,     0,     0,     0,     0,
     0,     0,    53,    54,     0,    46,     0,    37,    67,    60,
    61,    58,    47,    66,    64,    53,    54,     0,    46,    67,
    60,    61,    58,    47,    66,    64,    53,    54,    46,    46,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    59,     0,     0,
    65,     0,    49,   180,    51,    55,     0,    56,     0,    52,
     0,     0,     0,     0,     0,    53,    54,     0,    46,     0,
     0,     0,    62,    57,    63,     0,    50,     0,     0,     0,
     0,     0,     0,    67,    60,    61,    58,    47,    66,    64,
    53,    54,     0,    46,    67,    60,    61,    58,    47,    66,
    64,    53,    54,    48,    46,     0,    45,     0,    59,     0,
     0,    65,     0,    49,   179,    51,    55,     0,    56,     0,
    52,     0,    67,    60,    61,    58,    47,    66,    64,    53,
    54,     0,    46,    62,    57,    63,    59,    50,     0,    65,
     0,    49,   169,    51,    55,     0,    56,    59,    52,     0,
    65,     0,    49,     0,    51,    55,   165,    56,     0,    52,
     0,    62,    57,    63,    48,    50,     0,    45,     0,     0,
     0,     0,    62,    57,    63,    59,    50,     0,    65,     0,
    49,     0,    51,    55,   164,    56,     0,    52,     0,     0,
     0,     0,    48,     0,     0,    45,     0,     0,     0,     0,
    62,    57,    63,    48,    50,     0,    45,     0,     0,     0,
    59,     0,     0,    65,     0,    49,   158,    51,    55,     0,
    56,     0,    52,     0,     0,     0,     0,     0,     0,     0,
     0,    48,     0,     0,    45,    62,    57,    63,    59,    50,
     0,    65,     0,    49,   157,    51,    55,     0,    56,     0,
    52,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    62,    57,    63,    48,    50,     0,    45,
     0,    70,    25,    59,     0,     0,    65,     0,    49,   156,
    51,    55,     0,    56,     0,    52,     0,     0,     0,     0,
     0,     0,     0,     0,    48,     0,    90,    45,    62,    57,
    63,     0,    50,     0,    67,    60,    61,    58,    47,    66,
    64,    53,    54,     0,    46,     0,     0,     0,    59,    99,
     0,    65,     0,    49,   154,    51,    55,     0,    56,    48,
    52,     0,    45,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    62,    57,    63,     0,    50,   125,   126,
     0,   128,     0,     0,     0,     0,   132,     0,     0,     0,
     0,     0,     0,     0,     0,    67,    60,    61,    58,    47,
    66,    64,    53,    54,    48,    46,     0,    45,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    67,    60,    61,    58,    47,    66,    64,
    53,    54,     0,    46,    67,    60,    61,    58,    47,    66,
    64,    53,    54,     0,    46,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    67,    60,    61,    58,    47,    66,    64,    53,
    54,     0,    46,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    59,     0,     0,    65,     0,    49,   151,
    51,    55,     0,    56,     0,    52,     0,    67,    60,    61,
    58,    47,    66,    64,    53,    54,     0,    46,    62,    57,
    63,     0,    50,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    67,    60,    61,    58,    47,
    66,    64,    53,    54,     0,    46,     0,     0,     0,    48,
     0,     0,    45,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    67,    60,    61,    58,    47,    66,    64,    53,    54,    59,
    46,     0,    65,     0,    49,   150,    51,    55,    65,    56,
    49,    52,    51,    55,     0,    56,     0,    52,     0,     0,
     0,     0,     0,     0,    62,    57,    63,     0,    50,     0,
     0,     0,     0,     0,    50,    67,    60,    61,    58,    47,
    66,    64,    53,    54,    59,    46,     0,    65,     0,    49,
   149,    51,    55,     0,    56,    48,    52,     0,    45,     0,
     0,    48,     0,     0,    45,     0,     0,     0,     0,    62,
    57,    63,    59,    50,     0,    65,     0,    49,   148,    51,
    55,     0,    56,    59,    52,     0,    65,     0,    49,   142,
    51,    55,     0,    56,     0,    52,     0,    62,    57,    63,
    48,    50,     0,    45,     0,     0,     0,     0,    62,    57,
    63,    65,    50,    49,     0,    51,    55,     0,    56,     0,
    52,     0,     0,     0,     0,     0,     0,     0,    48,     0,
     0,    45,     0,    59,     0,     0,    65,    50,    49,    48,
    51,    55,    45,    56,     0,    52,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    62,    57,
    63,     0,    50,     0,    48,     0,     0,    45,     0,     0,
    67,    60,    61,    58,    47,    66,    64,    53,    54,     0,
    46,     0,     0,    59,     0,     0,    65,     0,    49,    48,
    51,    55,    45,    56,    49,    52,    51,    55,     0,    56,
     0,    52,     0,     0,     0,     0,     0,     0,    62,    57,
    63,     0,    50,     0,     0,     0,     0,     0,    50,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
     0,     0,    45,     0,     0,    48,     0,     0,    45,     0,
     0,     0,     0,     0,     0,     0,    67,    60,    61,    58,
    47,    66,    64,    53,    54,     0,    46,    66,    64,    53,
    54,     0,    46,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    67,    60,    61,    58,    47,    66,    64,    53,    54,
     0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
    60,    61,    58,    47,    66,    64,    53,    54,     0,    46,
    67,    60,    61,    58,    47,    66,    64,    53,    54,     0,
    46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    64,    53,    54,     0,    46,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    67,    60,    61,    58,    47,    66,    64,    53,    54,     0,
    46,     0,     0,     0,     0,     0,    91,     2,     0,     0,
     0,     0,    69,    71,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    83,     0,     0,     0,     0,
     0,     0,     0,    94,     0,     0,     0,     0,     0,     0,
     0,    60,    61,    58,    47,    66,    64,    53,    54,     0,
    46,     0,     0,    53,    54,     0,    46,   102,   103,   104,
   105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
   115,   116,   117,   118,   119,   120,     0,     0,     0,   121,
   122,   123,   124,     0,     0,   127,     0,   129,   130,   131,
     0,    94,   133,   134,   135,   136,     0,     0,     0,     0,
   141,     0,     0,     0,     0,     0,     0,   146,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   166,   167,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   173,     0,     0,     0,     0,     0,   174,     0,
   175,   176,   177,     0,     0,     0,     0,     0,   178 };
int yypact[]={

   -40, -1000,   928, -1000, -1000,   -40,   -40,    32,    31,    25,
    14,     5,     3,     2,    -8,    -9,   -10,   -11,    -6,   -12,
   -13, -1000,   -14,   -15,   -40,   -22,   -40, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,  -286, -1000, -1000, -1000,  -258,  -121, -1000, -1000,
   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,
   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,   -40,    16,
   -22,    16,   -40,   -40,   -40,   -40,  -266,  -266,   -40,  -266,
   -40,   -40,   -40,    16,   -40,   -40,   -40,   -40,   -40,   -32,
   -31,   928,   -40,   -40,   878,  -260, -1000, -1000,   -40,  -112,
   -40,   -40,    -7,    16,    16,    16,    16,    56,    56,   800,
   800,   800,   800,   800,   800,   800,   984,   984,   903,   978,
    16,   867,   839,   794,   708,   -17,   -19,   573,   -29,   528,
   493,   465,   -24,    17,     6,   430,   402, -1000,   -40,   -40,
   -34,   391, -1000, -1000, -1000,   -30,   928,   -21, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000,   -40, -1000, -1000, -1000, -1000,
 -1000,   -40, -1000,   -40,   -40,   -40,   928,    16, -1000, -1000,
 -1000,   -40, -1000,   363,   302,   120,    92,    81,   928, -1000,
 -1000, -1000, -1000, -1000 };
int yypgo[]={

     0,  1236,    89,    88,    21,    85,   561,    82,    80,    76,
    75,    13,    74,    73,    19 };
int yyr1[]={

     0,     3,     3,     2,     1,     9,     1,     1,     1,    10,
    10,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     4,     4,    12,     1,    14,     1,    11,    11,
    11,    13,    13,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     7,     8,     8,     5,     5,
     5,     5,     6 };
int yyr2[]={

     0,     2,     2,     3,     5,     1,     7,     5,     5,     2,
     2,     9,     9,     9,     9,     9,     9,     9,    13,     9,
     9,     9,     5,     9,    13,     9,    13,     7,     2,     7,
    13,    13,     7,     9,     1,    11,     1,    11,     0,     3,
     7,     3,     7,     9,     9,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     3,     3,     3,     3,     3,     3,
     2,     3,     3,     9,     3,     3,     3,     7,     3,     3,
     7,     3,     3 };
int yychk[]={

 -1000,    -3,    -1,    -2,    45,    43,   -10,   272,   273,   275,
   274,   276,   277,   278,   279,   280,   281,   282,   271,   283,
   284,    -4,   285,   286,   123,    -6,    40,   261,   262,   257,
   260,   263,   264,    -5,   288,   289,   265,   303,   126,   270,
    -8,   290,    -7,   266,   267,    94,   302,   296,    91,    40,
    64,    42,    47,   299,   300,    43,    45,    61,   295,    35,
   293,   294,    60,    62,   298,    38,   297,   292,    -9,    -1,
    -6,    -1,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    -1,    40,    40,    40,    40,    40,   -11,
    -6,    -1,   123,    40,    -1,   287,   266,    -4,   123,    -6,
   -12,   -14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -6,    -6,    -1,    -6,    -1,
    -1,    -1,    -6,    -1,    -1,    -1,    -1,   125,    44,   125,
   -11,    -1,    41,   267,   266,   -13,    -1,   -11,    41,    41,
    41,    41,    41,    41,    41,    44,    41,    41,    41,    41,
    41,    44,    41,    44,    44,    44,    -1,    -1,   125,    41,
    93,    44,    41,    -1,    -1,    -1,    -1,    -1,    -1,    41,
    41,    41,    41,    41 };
int yydef[]={

     0,    -2,     1,     2,     5,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    28,     0,     0,    38,     3,     0,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    74,     9,    10,    82,
    -2,    79,     0,    81,    76,     4,     0,     0,    34,    36,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     7,
     0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    22,     0,     0,     0,     0,     0,     0,
     0,    39,    38,     0,     0,     0,    27,    29,    38,     0,
     0,    38,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    62,    -2,
     6,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    32,     0,     0,
     0,     0,    45,    77,    80,     0,    41,     0,    11,    12,
    13,    14,    15,    16,    17,     0,    19,    20,    21,    73,
    23,     0,    25,     0,     0,     0,    40,    43,    33,    44,
    35,     0,    37,     0,     0,     0,     0,     0,    42,    18,
    24,    26,    30,    31 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"INT",	257,
	"HEX",	258,
	"ERROR",	259,
	"UINT",	260,
	"M2_TRUE",	261,
	"M2_FALSE",	262,
	"CHAR",	263,
	"FLOAT",	264,
	"STRING",	265,
	"NAME",	266,
	"BLOCKNAME",	267,
	"IDENT",	268,
	"VARNAME",	269,
	"TYPENAME",	270,
	"SIZE",	271,
	"CAP",	272,
	"ORD",	273,
	"HIGH",	274,
	"ABS",	275,
	"MIN_FUNC",	276,
	"MAX_FUNC",	277,
	"FLOAT_FUNC",	278,
	"VAL",	279,
	"CHR",	280,
	"ODD",	281,
	"TRUNC",	282,
	"INC",	283,
	"DEC",	284,
	"INCL",	285,
	"EXCL",	286,
	"COLONCOLON",	287,
	"LAST",	288,
	"REGNAME",	289,
	"INTERNAL_VAR",	290,
	",",	44,
	"ABOVE_COMMA",	291,
	"ASSIGN",	292,
	"<",	60,
	">",	62,
	"LEQ",	293,
	"GEQ",	294,
	"=",	61,
	"NOTEQUAL",	295,
	"#",	35,
	"IN",	296,
	"OROR",	297,
	"ANDAND",	298,
	"&",	38,
	"@",	64,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"DIV",	299,
	"MOD",	300,
	"UNARY",	301,
	"^",	94,
	"DOT",	302,
	"[",	91,
	"(",	40,
	"NOT",	303,
	"~",	126,
	"QID",	304,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : exp",
	"start : type_exp",
	"type_exp : type",
	"exp : exp '^'",
	"exp : '-'",
	"exp : '-' exp",
	"exp : '+' exp",
	"exp : not_exp exp",
	"not_exp : NOT",
	"not_exp : '~'",
	"exp : CAP '(' exp ')'",
	"exp : ORD '(' exp ')'",
	"exp : ABS '(' exp ')'",
	"exp : HIGH '(' exp ')'",
	"exp : MIN_FUNC '(' type ')'",
	"exp : MAX_FUNC '(' type ')'",
	"exp : FLOAT_FUNC '(' exp ')'",
	"exp : VAL '(' type ',' exp ')'",
	"exp : CHR '(' exp ')'",
	"exp : ODD '(' exp ')'",
	"exp : TRUNC '(' exp ')'",
	"exp : SIZE exp",
	"exp : INC '(' exp ')'",
	"exp : INC '(' exp ',' exp ')'",
	"exp : DEC '(' exp ')'",
	"exp : DEC '(' exp ',' exp ')'",
	"exp : exp DOT NAME",
	"exp : set",
	"exp : exp IN set",
	"exp : INCL '(' exp ',' exp ')'",
	"exp : EXCL '(' exp ',' exp ')'",
	"set : '{' arglist '}'",
	"set : type '{' arglist '}'",
	"exp : exp '['",
	"exp : exp '[' non_empty_arglist ']'",
	"exp : exp '('",
	"exp : exp '(' arglist ')'",
	"arglist : /* empty */",
	"arglist : exp",
	"arglist : arglist ',' exp",
	"non_empty_arglist : exp",
	"non_empty_arglist : non_empty_arglist ',' exp",
	"exp : '{' type '}' exp",
	"exp : type '(' exp ')'",
	"exp : '(' exp ')'",
	"exp : exp '@' exp",
	"exp : exp '*' exp",
	"exp : exp '/' exp",
	"exp : exp DIV exp",
	"exp : exp MOD exp",
	"exp : exp '+' exp",
	"exp : exp '-' exp",
	"exp : exp '=' exp",
	"exp : exp NOTEQUAL exp",
	"exp : exp '#' exp",
	"exp : exp LEQ exp",
	"exp : exp GEQ exp",
	"exp : exp '<' exp",
	"exp : exp '>' exp",
	"exp : exp ANDAND exp",
	"exp : exp '&' exp",
	"exp : exp OROR exp",
	"exp : exp ASSIGN exp",
	"exp : M2_TRUE",
	"exp : M2_FALSE",
	"exp : INT",
	"exp : UINT",
	"exp : CHAR",
	"exp : FLOAT",
	"exp : variable",
	"exp : LAST",
	"exp : REGNAME",
	"exp : SIZE '(' type ')'",
	"exp : STRING",
	"block : fblock",
	"fblock : BLOCKNAME",
	"fblock : block COLONCOLON BLOCKNAME",
	"variable : fblock",
	"variable : INTERNAL_VAR",
	"variable : block COLONCOLON NAME",
	"variable : NAME",
	"type : TYPENAME",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 3:
# line 199 "./m2-exp.y"
{ write_exp_elt_opcode(OP_TYPE);
		  write_exp_elt_type(yypvt[-0].tval);
		  write_exp_elt_opcode(OP_TYPE);
		} break;
case 4:
# line 208 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_IND); } break;
case 5:
# line 211 "./m2-exp.y"
{ number_sign = -1; } break;
case 6:
# line 213 "./m2-exp.y"
{ number_sign = 1;
			  write_exp_elt_opcode (UNOP_NEG); } break;
case 7:
# line 218 "./m2-exp.y"
{ write_exp_elt_opcode(UNOP_PLUS); } break;
case 8:
# line 222 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_ZEROP); } break;
case 11:
# line 230 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_CAP); } break;
case 12:
# line 234 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_ORD); } break;
case 13:
# line 238 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_ABS); } break;
case 14:
# line 242 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_HIGH); } break;
case 15:
# line 246 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_MIN);
			  write_exp_elt_type (yypvt[-1].tval);
			  write_exp_elt_opcode (UNOP_MIN); } break;
case 16:
# line 252 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_MAX);
			  write_exp_elt_type (yypvt[-1].tval);
			  write_exp_elt_opcode (UNOP_MIN); } break;
case 17:
# line 258 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_FLOAT); } break;
case 18:
# line 262 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_VAL);
			  write_exp_elt_type (yypvt[-3].tval);
			  write_exp_elt_opcode (BINOP_VAL); } break;
case 19:
# line 268 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_CHR); } break;
case 20:
# line 272 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_ODD); } break;
case 21:
# line 276 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_TRUNC); } break;
case 22:
# line 280 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_SIZEOF); } break;
case 23:
# line 285 "./m2-exp.y"
{ write_exp_elt_opcode(UNOP_PREINCREMENT); } break;
case 24:
# line 289 "./m2-exp.y"
{ write_exp_elt_opcode(BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode(BINOP_ADD);
			  write_exp_elt_opcode(BINOP_ASSIGN_MODIFY); } break;
case 25:
# line 295 "./m2-exp.y"
{ write_exp_elt_opcode(UNOP_PREDECREMENT);} break;
case 26:
# line 299 "./m2-exp.y"
{ write_exp_elt_opcode(BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode(BINOP_SUB);
			  write_exp_elt_opcode(BINOP_ASSIGN_MODIFY); } break;
case 27:
# line 305 "./m2-exp.y"
{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (STRUCTOP_STRUCT); } break;
case 29:
# line 314 "./m2-exp.y"
{ error("Sets are not implemented.");} break;
case 30:
# line 318 "./m2-exp.y"
{ error("Sets are not implemented.");} break;
case 31:
# line 322 "./m2-exp.y"
{ error("Sets are not implemented.");} break;
case 32:
# line 325 "./m2-exp.y"
{ error("Sets are not implemented.");} break;
case 33:
# line 327 "./m2-exp.y"
{ error("Sets are not implemented.");} break;
case 34:
# line 336 "./m2-exp.y"
{ start_arglist(); } break;
case 35:
# line 338 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_MULTI_SUBSCRIPT);
			  write_exp_elt_longcst ((LONGEST) end_arglist());
			  write_exp_elt_opcode (BINOP_MULTI_SUBSCRIPT); } break;
case 36:
# line 346 "./m2-exp.y"
{ start_arglist (); } break;
case 37:
# line 348 "./m2-exp.y"
{ write_exp_elt_opcode (OP_FUNCALL);
			  write_exp_elt_longcst ((LONGEST) end_arglist ());
			  write_exp_elt_opcode (OP_FUNCALL); } break;
case 39:
# line 357 "./m2-exp.y"
{ arglist_len = 1; } break;
case 40:
# line 361 "./m2-exp.y"
{ arglist_len++; } break;
case 41:
# line 366 "./m2-exp.y"
{ arglist_len = 1; } break;
case 42:
# line 371 "./m2-exp.y"
{ arglist_len++; } break;
case 43:
# line 376 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_MEMVAL);
			  write_exp_elt_type (yypvt[-2].tval);
			  write_exp_elt_opcode (UNOP_MEMVAL); } break;
case 44:
# line 382 "./m2-exp.y"
{ write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type (yypvt[-3].tval);
			  write_exp_elt_opcode (UNOP_CAST); } break;
case 45:
# line 388 "./m2-exp.y"
{ } break;
case 46:
# line 396 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_REPEAT); } break;
case 47:
# line 400 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_MUL); } break;
case 48:
# line 404 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_DIV); } break;
case 49:
# line 408 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_INTDIV); } break;
case 50:
# line 412 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_REM); } break;
case 51:
# line 416 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_ADD); } break;
case 52:
# line 420 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_SUB); } break;
case 53:
# line 424 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_EQUAL); } break;
case 54:
# line 428 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_NOTEQUAL); } break;
case 55:
# line 430 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_NOTEQUAL); } break;
case 56:
# line 434 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_LEQ); } break;
case 57:
# line 438 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_GEQ); } break;
case 58:
# line 442 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_LESS); } break;
case 59:
# line 446 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_GTR); } break;
case 60:
# line 450 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_AND); } break;
case 61:
# line 454 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_AND); } break;
case 62:
# line 458 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_OR); } break;
case 63:
# line 462 "./m2-exp.y"
{ write_exp_elt_opcode (BINOP_ASSIGN); } break;
case 64:
# line 469 "./m2-exp.y"
{ write_exp_elt_opcode (OP_BOOL);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].ulval);
			  write_exp_elt_opcode (OP_BOOL); } break;
case 65:
# line 475 "./m2-exp.y"
{ write_exp_elt_opcode (OP_BOOL);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].ulval);
			  write_exp_elt_opcode (OP_BOOL); } break;
case 66:
# line 481 "./m2-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_int);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LONG); } break;
case 67:
# line 488 "./m2-exp.y"
{
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_card);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].ulval);
			  write_exp_elt_opcode (OP_LONG);
			} break;
case 68:
# line 497 "./m2-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_char);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].ulval);
			  write_exp_elt_opcode (OP_LONG); } break;
case 69:
# line 505 "./m2-exp.y"
{ write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_m2_real);
			  write_exp_elt_dblcst (yypvt[-0].dval);
			  write_exp_elt_opcode (OP_DOUBLE); } break;
case 71:
# line 516 "./m2-exp.y"
{ write_exp_elt_opcode (OP_LAST);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LAST); } break;
case 72:
# line 522 "./m2-exp.y"
{ write_exp_elt_opcode (OP_REGISTER);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_REGISTER); } break;
case 73:
# line 528 "./m2-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH (yypvt[-1].tval));
			  write_exp_elt_opcode (OP_LONG); } break;
case 74:
# line 535 "./m2-exp.y"
{ write_exp_elt_opcode (OP_M2_STRING);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (OP_M2_STRING); } break;
case 75:
# line 542 "./m2-exp.y"
{ yyval.bval = SYMBOL_BLOCK_VALUE(yypvt[-0].sym); } break;
case 76:
# line 546 "./m2-exp.y"
{ struct symbol *sym
			    = lookup_symbol (copy_name (yypvt[-0].sval), expression_context_block,
					     VAR_NAMESPACE, 0, NULL);
			  yyval.sym = sym;} break;
case 77:
# line 555 "./m2-exp.y"
{ struct symbol *tem
			    = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					     VAR_NAMESPACE, 0, NULL);
			  if (!tem || SYMBOL_CLASS (tem) != LOC_BLOCK)
			    error ("No function \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));
			  yyval.sym = tem;
			} break;
case 78:
# line 567 "./m2-exp.y"
{ write_exp_elt_opcode(OP_VAR_VALUE);
			  write_exp_elt_sym (yypvt[-0].sym);
			  write_exp_elt_opcode (OP_VAR_VALUE); } break;
case 79:
# line 574 "./m2-exp.y"
{ write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern (yypvt[-0].ivar);
			  write_exp_elt_opcode (OP_INTERNALVAR); } break;
case 80:
# line 581 "./m2-exp.y"
{ struct symbol *sym;
			  sym = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					       VAR_NAMESPACE, 0, NULL);
			  if (sym == 0)
			    error ("No symbol \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));

			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_sym (sym);
			  write_exp_elt_opcode (OP_VAR_VALUE); } break;
case 81:
# line 595 "./m2-exp.y"
{ struct symbol *sym;
			  int is_a_field_of_this;

 			  sym = lookup_symbol (copy_name (yypvt[-0].sval),
					       expression_context_block,
					       VAR_NAMESPACE,
					       &is_a_field_of_this,
					       NULL);
			  if (sym)
			    {
			      switch (sym->class)
				{
				case LOC_REGISTER:
				case LOC_ARG:
				case LOC_LOCAL:
				case LOC_REF_ARG:
				case LOC_REGPARM:
				case LOC_LOCAL_ARG:
				  if (innermost_block == 0 ||
				      contained_in (block_found,
						    innermost_block))
				    innermost_block = block_found;
				  break;

				case LOC_UNDEF:
				case LOC_CONST:
				case LOC_STATIC:
				case LOC_TYPEDEF:
				case LOC_LABEL:	/* maybe should go above? */
				case LOC_BLOCK:
				case LOC_CONST_BYTES:
				  /* These are listed so gcc -Wall will reveal
				     un-handled cases.  */
				  break;
				}
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      write_exp_elt_sym (sym);
			      write_exp_elt_opcode (OP_VAR_VALUE);
			    }
			  else
			    {
			      struct minimal_symbol *msymbol;
			      register char *arg = copy_name (yypvt[-0].sval);

			      msymbol = lookup_minimal_symbol (arg,
					  (struct objfile *) NULL);
			      if (msymbol != NULL)
				{
				  write_exp_elt_opcode (OP_LONG);
				  write_exp_elt_type (builtin_type_int);
				  write_exp_elt_longcst ((LONGEST) msymbol -> address);
				  write_exp_elt_opcode (OP_LONG);
				  write_exp_elt_opcode (UNOP_MEMVAL);
				  if (msymbol -> type == mst_data ||
				      msymbol -> type == mst_bss)
				    write_exp_elt_type (builtin_type_int);
				  else if (msymbol -> type == mst_text)
				    write_exp_elt_type (lookup_function_type (builtin_type_int));
				  else
				    write_exp_elt_type (builtin_type_char);
				  write_exp_elt_opcode (UNOP_MEMVAL);
				}
			      else if (!have_full_symbols () && !have_partial_symbols ())
				error ("No symbol table is loaded.  Use the \"symbol-file\" command.");
			      else
				error ("No symbol \"%s\" in current context.",
				       copy_name (yypvt[-0].sval));
			    }
			} break;
case 82:
# line 668 "./m2-exp.y"
{ yyval.tval = lookup_typename (copy_name (yypvt[-0].sval),
						expression_context_block, 0); } break;
	}
	goto yystack;		/* reset registers in driver code */
}
