
# line 30 "./c-exp.y"

#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "frame.h"
#include "expression.h"
#include "parser-defs.h"
#include "value.h"
#include "language.h"
#include "bfd.h"
#include "symfile.h"
#include "objfiles.h"

/* These MUST be included in any grammar file!!!! Please choose unique names!
   Note that this are a combined list of variables that can be produced
   by any one of bison, byacc, or yacc. */
#define	yymaxdepth c_maxdepth
#define	yyparse	c_parse
#define	yylex	c_lex
#define	yyerror	c_error
#define	yylval	c_lval
#define	yychar	c_char
#define	yydebug	c_debug
#define	yypact	c_pact	
#define	yyr1	c_r1			
#define	yyr2	c_r2			
#define	yydef	c_def		
#define	yychk	c_chk		
#define	yypgo	c_pgo		
#define	yyact	c_act		
#define	yyexca	c_exca
#define yyerrflag c_errflag
#define yynerrs	c_nerrs
#define	yyps	c_ps
#define	yypv	c_pv
#define	yys	c_s
#define	yy_yys	c_yys
#define	yystate	c_state
#define	yytmp	c_tmp
#define	yyv	c_v
#define	yy_yyv	c_yyv
#define	yyval	c_val
#define	yylloc	c_lloc
#define yyss	c_yyss		/* byacc */
#define	yyssp	c_yysp		/* byacc */
#define	yyvs	c_yyvs		/* byacc */
#define	yyvsp	c_yyvsp		/* byacc */

int
yyparse PARAMS ((void));

int
yylex PARAMS ((void));

void
yyerror PARAMS ((char *));

/* #define	YYDEBUG	1 */


# line 97 "./c-exp.y"
typedef union 
  {
    LONGEST lval;
    unsigned LONGEST ulval;
    double dval;
    struct symbol *sym;
    struct type *tval;
    struct stoken sval;
    struct ttype tsym;
    struct symtoken ssym;
    int voidval;
    struct block *bval;
    enum exp_opcode opcode;
    struct internalvar *ivar;

    struct type **tvec;
    int *ivec;
  } YYSTYPE;

# line 117 "./c-exp.y"
/* YYSTYPE gets defined by %union */
static int
parse_number PARAMS ((char *, int, int, YYSTYPE *));
# define INT 257
# define CHAR 258
# define UINT 259
# define FLOAT 260
# define STRING 261
# define NAME 262
# define TYPENAME 263
# define NAME_OR_INT 264
# define NAME_OR_UINT 265
# define STRUCT 266
# define CLASS 267
# define UNION 268
# define ENUM 269
# define SIZEOF 270
# define UNSIGNED 271
# define COLONCOLON 272
# define TEMPLATE 273
# define ERROR 274
# define SIGNED_KEYWORD 275
# define LONG 276
# define SHORT 277
# define INT_KEYWORD 278
# define CONST_KEYWORD 279
# define VOLATILE_KEYWORD 280
# define LAST 281
# define REGNAME 282
# define VARIABLE 283
# define ASSIGN_MODIFY 284
# define THIS 285
# define ABOVE_COMMA 286
# define OROR 287
# define ANDAND 288
# define EQUAL 289
# define NOTEQUAL 290
# define LEQ 291
# define GEQ 292
# define LSH 293
# define RSH 294
# define UNARY 295
# define INCREMENT 296
# define DECREMENT 297
# define ARROW 298
# define BLOCKNAME 299

# line 197 "./c-exp.y"
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

# line 946 "./c-exp.y"


/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/*** Needs some error checking for the float case ***/

static int
parse_number (p, len, parsed_float, putithere)
     register char *p;
     register int len;
     int parsed_float;
     YYSTYPE *putithere;
{
  register LONGEST n = 0;
  register LONGEST prevn = 0;
  register int i;
  register int c;
  register int base = input_radix;
  int unsigned_p = 0;

  if (parsed_float)
    {
      /* It's a float since it contains a point or an exponent.  */
      putithere->dval = atof (p);
      return FLOAT;
    }

  /* Handle base-switching prefixes 0x, 0t, 0d, 0 */
  if (p[0] == '0')
    switch (p[1])
      {
      case 'x':
      case 'X':
	if (len >= 3)
	  {
	    p += 2;
	    base = 16;
	    len -= 2;
	  }
	break;

      case 't':
      case 'T':
      case 'd':
      case 'D':
	if (len >= 3)
	  {
	    p += 2;
	    base = 10;
	    len -= 2;
	  }
	break;

      default:
	base = 8;
	break;
      }

  while (len-- > 0)
    {
      c = *p++;
      if (c >= 'A' && c <= 'Z')
	c += 'a' - 'A';
      if (c != 'l' && c != 'u')
	n *= base;
      if (c >= '0' && c <= '9')
	n += i = c - '0';
      else
	{
	  if (base > 10 && c >= 'a' && c <= 'f')
	    n += i = c - 'a' + 10;
	  else if (len == 0 && c == 'l')
	    ;
	  else if (len == 0 && c == 'u')
	    unsigned_p = 1;
	  else
	    return ERROR;	/* Char not a digit */
	}
      if (i >= base)
	return ERROR;		/* Invalid digit in this base */
      /* Portably test for overflow (only works for nonzero values, so make
	 a second check for zero).  */
      if((prevn >= n) && n != 0)
	 unsigned_p=1;		/* Try something unsigned */
      /* If range checking enabled, portably test for unsigned overflow.  */
      if(RANGE_CHECK && n!=0)
      {	
	 if((unsigned_p && (unsigned)prevn >= (unsigned)n))
	    range_error("Overflow on numeric constant.");	 
      }
      prevn=n;
    }

  if (unsigned_p)
    {
      putithere->ulval = n;
      return UINT;
    }
  else
    {
      putithere->lval = n;
      return INT;
    }
}

struct token
{
  char *operator;
  int token;
  enum exp_opcode opcode;
};

const static struct token tokentab3[] =
  {
    {">>=", ASSIGN_MODIFY, BINOP_RSH},
    {"<<=", ASSIGN_MODIFY, BINOP_LSH}
  };

const static struct token tokentab2[] =
  {
    {"+=", ASSIGN_MODIFY, BINOP_ADD},
    {"-=", ASSIGN_MODIFY, BINOP_SUB},
    {"*=", ASSIGN_MODIFY, BINOP_MUL},
    {"/=", ASSIGN_MODIFY, BINOP_DIV},
    {"%=", ASSIGN_MODIFY, BINOP_REM},
    {"|=", ASSIGN_MODIFY, BINOP_LOGIOR},
    {"&=", ASSIGN_MODIFY, BINOP_LOGAND},
    {"^=", ASSIGN_MODIFY, BINOP_LOGXOR},
    {"++", INCREMENT, BINOP_END},
    {"--", DECREMENT, BINOP_END},
    {"->", ARROW, BINOP_END},
    {"&&", ANDAND, BINOP_END},
    {"||", OROR, BINOP_END},
    {"::", COLONCOLON, BINOP_END},
    {"<<", LSH, BINOP_END},
    {">>", RSH, BINOP_END},
    {"==", EQUAL, BINOP_END},
    {"!=", NOTEQUAL, BINOP_END},
    {"<=", LEQ, BINOP_END},
    {">=", GEQ, BINOP_END}
  };

/* Read one token, getting characters through lexptr.  */

int
yylex ()
{
  register int c;
  register int namelen;
  register unsigned i;
  register char *tokstart;

 retry:

  tokstart = lexptr;
  /* See if it is a special token of length 3.  */
  for (i = 0; i < sizeof tokentab3 / sizeof tokentab3[0]; i++)
    if (!strncmp (tokstart, tokentab3[i].operator, 3))
      {
	lexptr += 3;
	yylval.opcode = tokentab3[i].opcode;
	return tokentab3[i].token;
      }

  /* See if it is a special token of length 2.  */
  for (i = 0; i < sizeof tokentab2 / sizeof tokentab2[0]; i++)
    if (!strncmp (tokstart, tokentab2[i].operator, 2))
      {
	lexptr += 2;
	yylval.opcode = tokentab2[i].opcode;
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

    case '\'':
      /* We either have a character constant ('0' or '\177' for example)
	 or we have a quoted symbol reference ('foo(int,int)' in C++
	 for example). */
      lexptr++;
      c = *lexptr++;
      if (c == '\\')
	c = parse_escape (&lexptr);
      yylval.lval = c;
      c = *lexptr++;
      if (c != '\'')
	{
	  namelen = skip_quoted (tokstart) - tokstart;
	  if (namelen > 2)
	    {
	      lexptr = tokstart + namelen;
	      namelen -= 2;
	      tokstart++;
	      goto tryname;
	    }
	  error ("Invalid character constant.");
	}
      return CHAR;

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
      if (lexptr[1] < '0' || lexptr[1] > '9')
	goto symbol;		/* Nope, must be a symbol. */
      /* FALL THRU into number case.  */

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      {
	/* It's a number.  */
	int got_dot = 0, got_e = 0, toktype;
	register char *p = tokstart;
	int hex = input_radix > 10;

	if (c == '0' && (p[1] == 'x' || p[1] == 'X'))
	  {
	    p += 2;
	    hex = 1;
	  }
	else if (c == '0' && (p[1]=='t' || p[1]=='T' || p[1]=='d' || p[1]=='D'))
	  {
	    p += 2;
	    hex = 0;
	  }

	for (;; ++p)
	  {
	    if (!hex && !got_e && (*p == 'e' || *p == 'E'))
	      got_dot = got_e = 1;
	    else if (!hex && !got_dot && *p == '.')
	      got_dot = 1;
	    else if (got_e && (p[-1] == 'e' || p[-1] == 'E')
		     && (*p == '-' || *p == '+'))
	      /* This is the sign of the exponent, not the end of the
		 number.  */
	      continue;
	    /* We will take any letters or digits.  parse_number will
	       complain if past the radix, or if L or U are not final.  */
	    else if ((*p < '0' || *p > '9')
		     && ((*p < 'a' || *p > 'z')
				  && (*p < 'A' || *p > 'Z')))
	      break;
	  }
	toktype = parse_number (tokstart, p - tokstart, got_dot|got_e, &yylval);
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

    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '|':
    case '&':
    case '^':
    case '~':
    case '!':
    case '@':
    case '<':
    case '>':
    case '[':
    case ']':
    case '?':
    case ':':
    case '=':
    case '{':
    case '}':
    symbol:
      lexptr++;
      return c;

    case '"':
      for (namelen = 1; (c = tokstart[namelen]) != '"'; namelen++)
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
      yylval.sval.ptr = tokstart + 1;
      yylval.sval.length = namelen - 1;
      lexptr += namelen + 1;
      return STRING;
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

  tryname:
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
  /* Catch specific keywords.  Should be done with a data structure.  */
  switch (namelen)
    {
    case 8:
      if (!strncmp (tokstart, "unsigned", 8))
	return UNSIGNED;
      if (current_language->la_language == language_cplus
	  && !strncmp (tokstart, "template", 8))
	return TEMPLATE;
      if (!strncmp (tokstart, "volatile", 8))
	return VOLATILE_KEYWORD;
      break;
    case 6:
      if (!strncmp (tokstart, "struct", 6))
	return STRUCT;
      if (!strncmp (tokstart, "signed", 6))
	return SIGNED_KEYWORD;
      if (!strncmp (tokstart, "sizeof", 6))      
	return SIZEOF;
      break;
    case 5:
      if (current_language->la_language == language_cplus
	  && !strncmp (tokstart, "class", 5))
	return CLASS;
      if (!strncmp (tokstart, "union", 5))
	return UNION;
      if (!strncmp (tokstart, "short", 5))
	return SHORT;
      if (!strncmp (tokstart, "const", 5))
	return CONST_KEYWORD;
      break;
    case 4:
      if (!strncmp (tokstart, "enum", 4))
	return ENUM;
      if (!strncmp (tokstart, "long", 4))
	return LONG;
      if (current_language->la_language == language_cplus
	  && !strncmp (tokstart, "this", 4))
	{
	  static const char this_name[] =
				 { CPLUS_MARKER, 't', 'h', 'i', 's', '\0' };

	  if (lookup_symbol (this_name, expression_context_block,
			     VAR_NAMESPACE, 0, NULL))
	    return THIS;
	}
      break;
    case 3:
      if (!strncmp (tokstart, "int", 3))
	return INT_KEYWORD;
      break;
    default:
      break;
    }

  yylval.sval.ptr = tokstart;
  yylval.sval.length = namelen;

  /* Any other names starting in $ are debugger internal variables.  */

  if (*tokstart == '$')
    {
      yylval.ivar =  lookup_internalvar (copy_name (yylval.sval) + 1);
      return VARIABLE;
    }

  /* Use token-type BLOCKNAME for symbols that happen to be defined as
     functions or symtabs.  If this is not so, then ...
     Use token-type TYPENAME for symbols that happen to be defined
     currently as names of types; NAME for other symbols.
     The caller is not constrained to care about the distinction.  */
  {
    char *tmp = copy_name (yylval.sval);
    struct symbol *sym;
    int is_a_field_of_this = 0;
    int hextype;

    sym = lookup_symbol (tmp, expression_context_block,
			 VAR_NAMESPACE,
			 current_language->la_language == language_cplus
			 ? &is_a_field_of_this : NULL,
			 NULL);
    if ((sym && SYMBOL_CLASS (sym) == LOC_BLOCK) ||
        lookup_partial_symtab (tmp))
      {
	yylval.ssym.sym = sym;
	yylval.ssym.is_a_field_of_this = is_a_field_of_this;
	return BLOCKNAME;
      }
    if (sym && SYMBOL_CLASS (sym) == LOC_TYPEDEF)
        {
	  yylval.tsym.type = SYMBOL_TYPE (sym);
	  return TYPENAME;
        }
    if ((yylval.tsym.type = lookup_primitive_typename (tmp)) != 0)
	return TYPENAME;

    /* Input names that aren't symbols but ARE valid hex numbers,
       when the input radix permits them, can be names or numbers
       depending on the parse.  Note we support radixes > 16 here.  */
    if (!sym && 
        ((tokstart[0] >= 'a' && tokstart[0] < 'a' + input_radix - 10) ||
         (tokstart[0] >= 'A' && tokstart[0] < 'A' + input_radix - 10)))
      {
 	YYSTYPE newlval;	/* Its value is ignored.  */
	hextype = parse_number (tokstart, namelen, 0, &newlval);
	if (hextype == INT)
	  {
	    yylval.ssym.sym = sym;
	    yylval.ssym.is_a_field_of_this = is_a_field_of_this;
	    return NAME_OR_INT;
	  }
	if (hextype == UINT)
	  {
	    yylval.ssym.sym = sym;
	    yylval.ssym.is_a_field_of_this = is_a_field_of_this;
	    return NAME_OR_UINT;
	  }
      }

    /* Any other kind of symbol */
    yylval.ssym.sym = sym;
    yylval.ssym.is_a_field_of_this = is_a_field_of_this;
    return NAME;
  }
}

void
yyerror (msg)
     char *msg;
{
  error (msg ? msg : "Invalid syntax in expression.");
}

/* Table mapping opcodes into strings for printing operators
   and precedences of the operators.  */

const static struct op_print c_op_print_tab[] =
  {
    {",",  BINOP_COMMA, PREC_COMMA, 0},
    {"=",  BINOP_ASSIGN, PREC_ASSIGN, 1},
    {"||", BINOP_OR, PREC_OR, 0},
    {"&&", BINOP_AND, PREC_AND, 0},
    {"|",  BINOP_LOGIOR, PREC_LOGIOR, 0},
    {"&",  BINOP_LOGAND, PREC_LOGAND, 0},
    {"^",  BINOP_LOGXOR, PREC_LOGXOR, 0},
    {"==", BINOP_EQUAL, PREC_EQUAL, 0},
    {"!=", BINOP_NOTEQUAL, PREC_EQUAL, 0},
    {"<=", BINOP_LEQ, PREC_ORDER, 0},
    {">=", BINOP_GEQ, PREC_ORDER, 0},
    {">",  BINOP_GTR, PREC_ORDER, 0},
    {"<",  BINOP_LESS, PREC_ORDER, 0},
    {">>", BINOP_RSH, PREC_SHIFT, 0},
    {"<<", BINOP_LSH, PREC_SHIFT, 0},
    {"+",  BINOP_ADD, PREC_ADD, 0},
    {"-",  BINOP_SUB, PREC_ADD, 0},
    {"*",  BINOP_MUL, PREC_MUL, 0},
    {"/",  BINOP_DIV, PREC_MUL, 0},
    {"%",  BINOP_REM, PREC_MUL, 0},
    {"@",  BINOP_REPEAT, PREC_REPEAT, 0},
    {"-",  UNOP_NEG, PREC_PREFIX, 0},
    {"!",  UNOP_ZEROP, PREC_PREFIX, 0},
    {"~",  UNOP_LOGNOT, PREC_PREFIX, 0},
    {"*",  UNOP_IND, PREC_PREFIX, 0},
    {"&",  UNOP_ADDR, PREC_PREFIX, 0},
    {"sizeof ", UNOP_SIZEOF, PREC_PREFIX, 0},
    {"++", UNOP_PREINCREMENT, PREC_PREFIX, 0},
    {"--", UNOP_PREDECREMENT, PREC_PREFIX, 0},
    /* C++  */
    {"::", BINOP_SCOPE, PREC_PREFIX, 0},
};

/* These variables point to the objects
   representing the predefined C data types.  */

struct type *builtin_type_void;
struct type *builtin_type_char;
struct type *builtin_type_short;
struct type *builtin_type_int;
struct type *builtin_type_long;
struct type *builtin_type_long_long;
struct type *builtin_type_signed_char;
struct type *builtin_type_unsigned_char;
struct type *builtin_type_unsigned_short;
struct type *builtin_type_unsigned_int;
struct type *builtin_type_unsigned_long;
struct type *builtin_type_unsigned_long_long;
struct type *builtin_type_float;
struct type *builtin_type_double;
struct type *builtin_type_long_double;
struct type *builtin_type_complex;
struct type *builtin_type_double_complex;

struct type ** const (c_builtin_types[]) = 
{
  &builtin_type_int,
  &builtin_type_long,
  &builtin_type_short,
  &builtin_type_char,
  &builtin_type_float,
  &builtin_type_double,
  &builtin_type_void,
  &builtin_type_long_long,
  &builtin_type_signed_char,
  &builtin_type_unsigned_char,
  &builtin_type_unsigned_short,
  &builtin_type_unsigned_int,
  &builtin_type_unsigned_long,
  &builtin_type_unsigned_long_long,
  &builtin_type_long_double,
  &builtin_type_complex,
  &builtin_type_double_complex,
  0
};

const struct language_defn c_language_defn = {
  "c",				/* Language name */
  language_c,
  c_builtin_types,
  range_check_off,
  type_check_off,
  c_parse,
  c_error,
  &BUILTIN_TYPE_LONGEST,	 /* longest signed   integral type */
  &BUILTIN_TYPE_UNSIGNED_LONGEST,/* longest unsigned integral type */
  &builtin_type_double,		/* longest floating point type */ /*FIXME*/
  "0x%x", "0x%", "x",		/* Hex   format, prefix, suffix */
  "0%o",  "0%",  "o",		/* Octal format, prefix, suffix */
  c_op_print_tab,		/* expression operators for printing */
  LANG_MAGIC
};

const struct language_defn cplus_language_defn = {
  "c++",				/* Language name */
  language_cplus,
  c_builtin_types,
  range_check_off,
  type_check_off,
  c_parse,
  c_error,
  &BUILTIN_TYPE_LONGEST,	 /* longest signed   integral type */
  &BUILTIN_TYPE_UNSIGNED_LONGEST,/* longest unsigned integral type */
  &builtin_type_double,		/* longest floating point type */ /*FIXME*/
  "0x%x", "0x%", "x",		/* Hex   format, prefix, suffix */
  "0%o",  "0%",  "o",		/* Octal format, prefix, suffix */
  c_op_print_tab,		/* expression operators for printing */
  LANG_MAGIC
};

void
_initialize_c_exp ()
{
  builtin_type_void =
    init_type (TYPE_CODE_VOID, 1,
	       0,
	       "void", (struct objfile *) NULL);
  builtin_type_char =
    init_type (TYPE_CODE_INT, TARGET_CHAR_BIT / TARGET_CHAR_BIT,
	       0,
	       "char", (struct objfile *) NULL);
  builtin_type_signed_char =
    init_type (TYPE_CODE_INT, TARGET_CHAR_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_SIGNED,
	       "signed char", (struct objfile *) NULL);
  builtin_type_unsigned_char =
    init_type (TYPE_CODE_INT, TARGET_CHAR_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "unsigned char", (struct objfile *) NULL);
  builtin_type_short =
    init_type (TYPE_CODE_INT, TARGET_SHORT_BIT / TARGET_CHAR_BIT,
	       0,
	       "short", (struct objfile *) NULL);
  builtin_type_unsigned_short =
    init_type (TYPE_CODE_INT, TARGET_SHORT_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "unsigned short", (struct objfile *) NULL);
  builtin_type_int =
    init_type (TYPE_CODE_INT, TARGET_INT_BIT / TARGET_CHAR_BIT,
	       0,
	       "int", (struct objfile *) NULL);
  builtin_type_unsigned_int =
    init_type (TYPE_CODE_INT, TARGET_INT_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "unsigned int", (struct objfile *) NULL);
  builtin_type_long =
    init_type (TYPE_CODE_INT, TARGET_LONG_BIT / TARGET_CHAR_BIT,
	       0,
	       "long", (struct objfile *) NULL);
  builtin_type_unsigned_long =
    init_type (TYPE_CODE_INT, TARGET_LONG_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "unsigned long", (struct objfile *) NULL);
  builtin_type_long_long =
    init_type (TYPE_CODE_INT, TARGET_LONG_LONG_BIT / TARGET_CHAR_BIT,
	       0,
	       "long long", (struct objfile *) NULL);
  builtin_type_unsigned_long_long = 
    init_type (TYPE_CODE_INT, TARGET_LONG_LONG_BIT / TARGET_CHAR_BIT,
	       TYPE_FLAG_UNSIGNED,
	       "unsigned long long", (struct objfile *) NULL);
  builtin_type_float =
    init_type (TYPE_CODE_FLT, TARGET_FLOAT_BIT / TARGET_CHAR_BIT,
	       0,
	       "float", (struct objfile *) NULL);
  builtin_type_double =
    init_type (TYPE_CODE_FLT, TARGET_DOUBLE_BIT / TARGET_CHAR_BIT,
	       0,
	       "double", (struct objfile *) NULL);
  builtin_type_long_double =
    init_type (TYPE_CODE_FLT, TARGET_LONG_DOUBLE_BIT / TARGET_CHAR_BIT,
	       0,
	       "long double", (struct objfile *) NULL);
  builtin_type_complex =
    init_type (TYPE_CODE_FLT, TARGET_COMPLEX_BIT / TARGET_CHAR_BIT,
	       0,
	       "complex", (struct objfile *) NULL);
  builtin_type_double_complex =
    init_type (TYPE_CODE_FLT, TARGET_DOUBLE_COMPLEX_BIT / TARGET_CHAR_BIT,
	       0,
	       "double complex", (struct objfile *) NULL);

  add_language (&c_language_defn);
  add_language (&cplus_language_defn);
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 47,
	272, 66,
	-2, 130,
-1, 131,
	272, 95,
	-2, 126,
-1, 180,
	272, 67,
	-2, 68,
	};
# define YYNPROD 131
# define YYLAST 1050
int yyact[]={

     9,   183,   167,   182,   110,     7,   109,    15,   115,     6,
   201,   115,     8,   184,   181,   111,   104,   106,   107,   108,
   178,   122,   123,   116,   112,   113,   116,    96,    96,    98,
    98,    95,    95,   192,   160,   102,    78,    59,    94,    49,
    55,   176,    57,     9,    99,    55,    53,    58,     7,   200,
    88,    53,     6,   105,    34,     8,   173,    39,    40,    41,
    42,    78,    38,   100,    44,    31,    43,    36,    37,    35,
    45,    46,    96,   185,    98,    49,    95,   206,   168,   214,
   101,   101,   199,   208,    78,   193,   203,     9,   188,   204,
    14,    54,   198,    10,    15,   199,    54,    78,   164,   165,
    92,     2,    49,   210,   114,    59,    70,   101,    55,   211,
    57,    60,   197,    61,    53,    58,    78,   189,   129,   133,
   136,   162,   168,    30,    33,   101,   191,    28,    68,    76,
    69,    75,    56,    14,   169,   170,    10,   174,   104,   106,
   107,   108,   171,    97,    59,    22,     1,    55,   121,    57,
    60,     3,    61,    53,    58,   135,     0,     0,     0,    54,
     0,   172,    71,     0,    59,    70,     0,    55,     0,    57,
    60,    56,    61,    53,    58,   105,     0,    14,     0,     0,
    10,     0,     0,     0,   179,     0,     0,    68,    76,    69,
    75,    56,    72,     0,     0,     0,     0,     0,    54,     0,
     0,     0,    59,     0,     0,    55,   167,    57,    60,     0,
    61,    53,    58,     0,     0,     0,     0,     0,    54,     0,
     0,    71,     0,     0,    16,    20,    18,    21,    26,    48,
    34,    17,    19,    39,    40,    41,    42,    13,    38,    32,
    44,     0,    43,    36,    37,    35,    45,    46,    23,    24,
    25,    72,    27,   213,     0,     0,    54,     0,     0,     0,
     0,   163,    93,    11,    12,     0,    47,    16,    20,    18,
    21,    26,    48,    34,    17,    19,    39,    40,    41,    42,
    13,    38,    32,    44,     0,    43,    36,    37,    35,    45,
    46,    23,    24,    25,     0,    27,    50,    51,    52,     0,
     0,    50,    51,    52,     0,     0,    11,    12,     0,    47,
     0,    16,    20,    18,    21,    26,    48,    34,    17,    19,
    39,    40,    41,    42,    13,    38,    32,    44,   134,    43,
    36,    37,    35,    45,    46,    23,    24,    25,     0,    27,
     0,     0,   104,   106,   107,   108,     0,     0,     0,     0,
    11,    12,    77,    47,     0,    74,    73,    64,    65,    66,
    67,    62,    63,     0,    50,    51,    52,     0,     0,     0,
     0,     0,    59,    70,     0,    55,     0,    57,    60,   105,
    61,    53,    58,    59,    70,     0,    55,     0,    57,    60,
     0,    61,    53,    58,     0,    68,     0,    69,    75,    56,
    62,    63,     0,    50,    51,    52,    68,     0,    69,     0,
    56,    77,     0,     0,    74,    73,    64,    65,    66,    67,
    62,    63,     0,    50,    51,    52,    54,   130,     0,    71,
     0,     0,     0,    59,    70,     0,    55,    54,    57,    60,
    71,    61,    53,    58,    59,    70,     0,    55,     0,    57,
    60,     0,    61,    53,    58,     0,    68,     0,    69,    72,
    56,    50,    51,    52,     0,     0,     0,    68,     0,    69,
    72,    56,    59,    70,     0,    55,     0,    57,    60,     0,
    61,    53,    58,    59,   212,     0,    55,    54,    57,    60,
    71,    61,    53,    58,     0,    68,     0,    69,    54,    56,
     0,    71,     0,     0,     0,     0,    68,     0,    69,     0,
    56,     0,     0,    59,     0,     0,    55,     0,    57,    60,
    72,    61,    53,    58,   175,     0,    54,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    68,    54,    69,     0,
    56,     0,     0,     0,     0,     0,     0,     0,   104,   131,
   107,   108,    39,    40,    41,    42,     0,    38,     0,    44,
     0,    43,    36,    37,    35,    45,    46,    54,    96,     0,
    98,   175,    95,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   105,     0,    59,     0,     0,
    55,     0,    57,    60,     0,    61,    53,    58,     0,     0,
     0,     0,   177,     5,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    56,     0,     0,    89,    91,     0,
     0,   101,    74,    73,    64,    65,    66,    67,    62,    63,
     0,    50,    51,    52,    73,    64,    65,    66,    67,    62,
    63,    54,    50,    51,    52,     0,     0,   104,   131,   107,
   108,    39,    40,    41,    42,     0,    38,     0,    44,     0,
    43,    36,    37,    35,    45,    46,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   105,    64,    65,    66,    67,    62,
    63,   161,    50,    51,    52,     0,    64,    65,    66,    67,
    62,    63,     0,    50,    51,    52,    34,     0,     0,    39,
    40,    41,    42,     0,    38,     0,    44,     0,    43,    36,
    37,    35,    45,    46,    64,    65,    66,    67,    62,    63,
     0,    50,    51,    52,     0,    64,    65,    66,    67,    62,
    63,     0,    50,    51,    52,     0,    34,     0,     0,    39,
    40,    41,    42,     0,    38,     0,    44,     0,    43,    36,
    37,    35,    45,    46,     0,     0,     0,    66,    67,    62,
    63,     0,    50,    51,    52,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   166,     0,   202,     0,
     0,     0,     0,    34,     0,     0,    39,    40,    41,    42,
     0,    38,   207,    44,     4,    43,    36,    37,    35,    45,
    46,    79,    81,    82,    83,    84,    85,    86,    87,   103,
     0,     0,     0,     0,     0,     0,   117,   118,   119,   120,
     0,   124,     0,     0,     0,     0,     0,    80,    29,   128,
   132,     0,     0,     0,     0,     0,    50,    51,    52,     0,
     0,     0,    90,    29,   127,     0,     0,     0,     0,     0,
     0,   137,   138,   139,   140,   141,   142,   143,   144,   145,
   146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
   156,   157,   158,   125,   126,     0,     0,     0,     0,   180,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   159,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    29,     0,     0,     0,
     0,     0,     0,     0,     0,   186,    90,     0,     0,   187,
     0,   190,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   196,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   194,     0,   195,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   205,     0,   195,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   209,
     0,    90,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    90,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    90,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    90 };
int yypact[]={

   -33, -1000,    31, -1000,   127,    76,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,    10,  -209,   -33, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,   -10,
  -237, -1000,  -246, -1000, -1000, -1000,  -272,  -263,  -252,  -246,
  -246,  -246,  -246,  -255,  -246,  -209,  -209, -1000, -1000,   -33,
 -1000, -1000,   385,   286,   -33, -1000,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,  -209,     5,
  -238,     5,     5,     5,     5,     5,     5,     5,   -33,    -4,
   -11,    57,    58,    80, -1000,    34,    34,    16,   530, -1000,
 -1000,   -73,  -246, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -264, -1000,  -275,  -265, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,    13, -1000, -1000,   127, -1000, -1000,
   -33, -1000, -1000, -1000,   -33,    -5,   -33,   165,     5,     5,
     5,     0,     0,   550,   550,   476,   476,   107,   107,   107,
   107,   446,   435,   407,   396,   346,    68,   127,   127,  -239,
  -124,    44,   -33,    36,   -33, -1000, -1000,  -246, -1000, -1000,
 -1000, -1000, -1000,   483,    71, -1000,    51,    76, -1000,   -44,
 -1000, -1000, -1000,  -268, -1000,  -209,     5,     5, -1000,    45,
   127,   -33,    35,    54,     5,     5, -1000, -1000, -1000,  -209,
 -1000, -1000,    21, -1000,   -33,   335,    62,    76, -1000,   127,
    69,   443, -1000,    38, -1000 };
int yypgo[]={

     0,   804,   100,   151,   146,   145,    65,   602,   837,    41,
    63,   143,    38,   127,    44,   786,   124,   104,   123,   120,
   117 };
int yyr1[]={

     0,     4,     4,     3,     2,     2,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,    19,     1,    20,    20,    20,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,    18,    18,     5,     6,
     6,     5,     5,     5,    13,    13,    12,    12,    12,    12,
    12,    11,    11,    11,    11,    11,    14,    14,    10,    10,
     7,     7,     7,     7,     7,     8,     8,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     8,    17,    17,
    17,    17,     9,     9,    15,    15,    15,    15,    15,    16,
    16 };
int yyr2[]={

     0,     2,     2,     3,     2,     7,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     7,     7,     9,     7,
     7,     9,     9,     1,    11,     0,     3,     7,     9,     9,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
    11,     7,     7,     3,     3,     3,     3,     3,     3,     2,
     3,     3,     3,     9,     3,     3,     3,     7,     7,     7,
     9,     2,     5,     3,     2,     5,     3,     5,     3,     5,
     2,     7,     5,     3,     5,     3,     5,     7,     5,     7,
     2,     7,    13,    17,    19,     3,     3,     3,     3,     5,
     7,     5,     7,     7,     9,     5,     7,     5,     5,     5,
     5,     5,     3,     5,     3,    11,     5,     5,     2,     3,
     3,     3,     3,     7,     3,     3,     3,     3,     3,     2,
     2 };
int yychk[]={

 -1000,    -4,    -2,    -3,    -1,    -7,    42,    38,    45,    33,
   126,   296,   297,   270,   123,    40,   257,   264,   259,   265,
   258,   260,    -5,   281,   282,   283,   261,   285,   -13,    -8,
   -18,    -6,   272,   -16,   263,   278,   276,   277,   271,   266,
   267,   268,   269,   275,   273,   279,   280,   299,   262,    44,
   296,   297,   298,    46,    91,    40,    64,    42,    47,    37,
    43,    45,   293,   294,   289,   290,   291,   292,    60,    62,
    38,    94,   124,   288,   287,    63,    61,   284,    40,    -1,
    -8,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -7,
    -8,    -7,    -2,   272,   -12,    42,    38,   -11,    40,   -14,
   -10,    91,   272,   -15,   262,   299,   263,   264,   265,   278,
   276,   278,   276,   277,   -17,   263,   278,   -15,   -15,   -15,
   -15,   -17,   276,   277,   -15,    -8,    -8,    -1,   -15,    -6,
    42,   263,   -15,    -6,    42,    -2,   -19,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -8,
   272,    -7,   125,   272,    41,    41,   -15,   126,    42,   -12,
   -12,   -14,   -10,    40,   -12,    41,    -9,    -7,    93,   257,
   -15,   278,   278,   276,   278,    60,    -1,    -1,    93,   -20,
    -1,    58,   272,    41,    -1,    -1,   -15,    41,    41,    44,
    93,   278,    -7,    41,    44,    -1,    42,    -7,    62,    -1,
    41,    40,    41,    -9,    41 };
int yydef[]={

     0,    -2,     1,     2,     4,     3,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    64,    65,    90,    74,
     0,    71,     0,    73,    95,    96,    97,    98,   112,     0,
     0,     0,     0,   114,     0,     0,     0,    -2,   129,     0,
    13,    14,     0,     0,     0,    23,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     6,
     0,     7,     8,     9,    10,    11,    12,    15,     0,     0,
    74,     0,     0,     0,    75,    76,    78,    80,     0,    83,
    85,     0,     0,    72,   124,   125,   126,   127,   128,    99,
   101,   105,   120,   121,   111,   118,   119,   107,   108,   109,
   110,   113,   120,   121,     0,   116,   117,     5,    16,    17,
     0,    -2,    19,    20,     0,     0,    25,    31,    32,    33,
    34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    49,     0,    51,    52,     0,
     0,     0,     0,     0,     0,    30,    69,     0,    91,    77,
    79,    82,    84,     0,     0,    88,     0,   122,    86,     0,
    -2,   102,   100,   103,   106,     0,    18,    21,    22,     0,
    26,     0,     0,    63,    28,    29,    70,    81,    89,     0,
    87,   104,     0,    24,     0,    50,     0,   123,   115,    27,
    92,     0,    93,     0,    94 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"INT",	257,
	"CHAR",	258,
	"UINT",	259,
	"FLOAT",	260,
	"STRING",	261,
	"NAME",	262,
	"TYPENAME",	263,
	"NAME_OR_INT",	264,
	"NAME_OR_UINT",	265,
	"STRUCT",	266,
	"CLASS",	267,
	"UNION",	268,
	"ENUM",	269,
	"SIZEOF",	270,
	"UNSIGNED",	271,
	"COLONCOLON",	272,
	"TEMPLATE",	273,
	"ERROR",	274,
	"SIGNED_KEYWORD",	275,
	"LONG",	276,
	"SHORT",	277,
	"INT_KEYWORD",	278,
	"CONST_KEYWORD",	279,
	"VOLATILE_KEYWORD",	280,
	"LAST",	281,
	"REGNAME",	282,
	"VARIABLE",	283,
	"ASSIGN_MODIFY",	284,
	"THIS",	285,
	",",	44,
	"ABOVE_COMMA",	286,
	"=",	61,
	"?",	63,
	"OROR",	287,
	"ANDAND",	288,
	"|",	124,
	"^",	94,
	"&",	38,
	"EQUAL",	289,
	"NOTEQUAL",	290,
	"<",	60,
	">",	62,
	"LEQ",	291,
	"GEQ",	292,
	"LSH",	293,
	"RSH",	294,
	"@",	64,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"UNARY",	295,
	"INCREMENT",	296,
	"DECREMENT",	297,
	"ARROW",	298,
	".",	46,
	"[",	91,
	"(",	40,
	"BLOCKNAME",	299,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : exp1",
	"start : type_exp",
	"type_exp : type",
	"exp1 : exp",
	"exp1 : exp1 ',' exp",
	"exp : '*' exp",
	"exp : '&' exp",
	"exp : '-' exp",
	"exp : '!' exp",
	"exp : '~' exp",
	"exp : INCREMENT exp",
	"exp : DECREMENT exp",
	"exp : exp INCREMENT",
	"exp : exp DECREMENT",
	"exp : SIZEOF exp",
	"exp : exp ARROW name",
	"exp : exp ARROW qualified_name",
	"exp : exp ARROW '*' exp",
	"exp : exp '.' name",
	"exp : exp '.' qualified_name",
	"exp : exp '.' '*' exp",
	"exp : exp '[' exp1 ']'",
	"exp : exp '('",
	"exp : exp '(' arglist ')'",
	"arglist : /* empty */",
	"arglist : exp",
	"arglist : arglist ',' exp",
	"exp : '{' type '}' exp",
	"exp : '(' type ')' exp",
	"exp : '(' exp1 ')'",
	"exp : exp '@' exp",
	"exp : exp '*' exp",
	"exp : exp '/' exp",
	"exp : exp '%' exp",
	"exp : exp '+' exp",
	"exp : exp '-' exp",
	"exp : exp LSH exp",
	"exp : exp RSH exp",
	"exp : exp EQUAL exp",
	"exp : exp NOTEQUAL exp",
	"exp : exp LEQ exp",
	"exp : exp GEQ exp",
	"exp : exp '<' exp",
	"exp : exp '>' exp",
	"exp : exp '&' exp",
	"exp : exp '^' exp",
	"exp : exp '|' exp",
	"exp : exp ANDAND exp",
	"exp : exp OROR exp",
	"exp : exp '?' exp ':' exp",
	"exp : exp '=' exp",
	"exp : exp ASSIGN_MODIFY exp",
	"exp : INT",
	"exp : NAME_OR_INT",
	"exp : UINT",
	"exp : NAME_OR_UINT",
	"exp : CHAR",
	"exp : FLOAT",
	"exp : variable",
	"exp : LAST",
	"exp : REGNAME",
	"exp : VARIABLE",
	"exp : SIZEOF '(' type ')'",
	"exp : STRING",
	"exp : THIS",
	"block : BLOCKNAME",
	"block : block COLONCOLON name",
	"variable : block COLONCOLON name",
	"qualified_name : typebase COLONCOLON name",
	"qualified_name : typebase COLONCOLON '~' name",
	"variable : qualified_name",
	"variable : COLONCOLON name",
	"variable : name_not_typename",
	"ptype : typebase",
	"ptype : typebase abs_decl",
	"abs_decl : '*'",
	"abs_decl : '*' abs_decl",
	"abs_decl : '&'",
	"abs_decl : '&' abs_decl",
	"abs_decl : direct_abs_decl",
	"direct_abs_decl : '(' abs_decl ')'",
	"direct_abs_decl : direct_abs_decl array_mod",
	"direct_abs_decl : array_mod",
	"direct_abs_decl : direct_abs_decl func_mod",
	"direct_abs_decl : func_mod",
	"array_mod : '[' ']'",
	"array_mod : '[' INT ']'",
	"func_mod : '(' ')'",
	"func_mod : '(' nonempty_typelist ')'",
	"type : ptype",
	"type : typebase COLONCOLON '*'",
	"type : type '(' typebase COLONCOLON '*' ')'",
	"type : type '(' typebase COLONCOLON '*' ')' '(' ')'",
	"type : type '(' typebase COLONCOLON '*' ')' '(' nonempty_typelist ')'",
	"typebase : TYPENAME",
	"typebase : INT_KEYWORD",
	"typebase : LONG",
	"typebase : SHORT",
	"typebase : LONG INT_KEYWORD",
	"typebase : UNSIGNED LONG INT_KEYWORD",
	"typebase : LONG LONG",
	"typebase : LONG LONG INT_KEYWORD",
	"typebase : UNSIGNED LONG LONG",
	"typebase : UNSIGNED LONG LONG INT_KEYWORD",
	"typebase : SHORT INT_KEYWORD",
	"typebase : UNSIGNED SHORT INT_KEYWORD",
	"typebase : STRUCT name",
	"typebase : CLASS name",
	"typebase : UNION name",
	"typebase : ENUM name",
	"typebase : UNSIGNED typename",
	"typebase : UNSIGNED",
	"typebase : SIGNED_KEYWORD typename",
	"typebase : SIGNED_KEYWORD",
	"typebase : TEMPLATE name '<' type '>'",
	"typebase : CONST_KEYWORD typebase",
	"typebase : VOLATILE_KEYWORD typebase",
	"typename : TYPENAME",
	"typename : INT_KEYWORD",
	"typename : LONG",
	"typename : SHORT",
	"nonempty_typelist : type",
	"nonempty_typelist : nonempty_typelist ',' type",
	"name : NAME",
	"name : BLOCKNAME",
	"name : TYPENAME",
	"name : NAME_OR_INT",
	"name : NAME_OR_UINT",
	"name_not_typename : NAME",
	"name_not_typename : BLOCKNAME",
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
# line 216 "./c-exp.y"
{ write_exp_elt_opcode(OP_TYPE);
			  write_exp_elt_type(yypvt[-0].tval);
			  write_exp_elt_opcode(OP_TYPE);} break;
case 5:
# line 224 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_COMMA); } break;
case 6:
# line 229 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_IND); } break;
case 7:
# line 232 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_ADDR); } break;
case 8:
# line 235 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_NEG); } break;
case 9:
# line 239 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_ZEROP); } break;
case 10:
# line 243 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_LOGNOT); } break;
case 11:
# line 247 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_PREINCREMENT); } break;
case 12:
# line 251 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_PREDECREMENT); } break;
case 13:
# line 255 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_POSTINCREMENT); } break;
case 14:
# line 259 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_POSTDECREMENT); } break;
case 15:
# line 263 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_SIZEOF); } break;
case 16:
# line 267 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_PTR);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (STRUCTOP_PTR); } break;
case 17:
# line 273 "./c-exp.y"
{ /* exp->type::name becomes exp->*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MPTR); } break;
case 18:
# line 280 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_MPTR); } break;
case 19:
# line 284 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (STRUCTOP_STRUCT); } break;
case 20:
# line 290 "./c-exp.y"
{ /* exp.type::name becomes exp.*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MEMBER); } break;
case 21:
# line 298 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_MEMBER); } break;
case 22:
# line 302 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_SUBSCRIPT); } break;
case 23:
# line 308 "./c-exp.y"
{ start_arglist (); } break;
case 24:
# line 310 "./c-exp.y"
{ write_exp_elt_opcode (OP_FUNCALL);
			  write_exp_elt_longcst ((LONGEST) end_arglist ());
			  write_exp_elt_opcode (OP_FUNCALL); } break;
case 26:
# line 319 "./c-exp.y"
{ arglist_len = 1; } break;
case 27:
# line 323 "./c-exp.y"
{ arglist_len++; } break;
case 28:
# line 327 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_MEMVAL);
			  write_exp_elt_type (yypvt[-2].tval);
			  write_exp_elt_opcode (UNOP_MEMVAL); } break;
case 29:
# line 333 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type (yypvt[-2].tval);
			  write_exp_elt_opcode (UNOP_CAST); } break;
case 30:
# line 339 "./c-exp.y"
{ } break;
case 31:
# line 345 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_REPEAT); } break;
case 32:
# line 349 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_MUL); } break;
case 33:
# line 353 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_DIV); } break;
case 34:
# line 357 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_REM); } break;
case 35:
# line 361 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ADD); } break;
case 36:
# line 365 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_SUB); } break;
case 37:
# line 369 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LSH); } break;
case 38:
# line 373 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_RSH); } break;
case 39:
# line 377 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_EQUAL); } break;
case 40:
# line 381 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_NOTEQUAL); } break;
case 41:
# line 385 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LEQ); } break;
case 42:
# line 389 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_GEQ); } break;
case 43:
# line 393 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LESS); } break;
case 44:
# line 397 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_GTR); } break;
case 45:
# line 401 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LOGAND); } break;
case 46:
# line 405 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LOGXOR); } break;
case 47:
# line 409 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LOGIOR); } break;
case 48:
# line 413 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_AND); } break;
case 49:
# line 417 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_OR); } break;
case 50:
# line 421 "./c-exp.y"
{ write_exp_elt_opcode (TERNOP_COND); } break;
case 51:
# line 425 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ASSIGN); } break;
case 52:
# line 429 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode (yypvt[-1].opcode);
			  write_exp_elt_opcode (BINOP_ASSIGN_MODIFY); } break;
case 53:
# line 435 "./c-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  if (yypvt[-0].lval == (int) yypvt[-0].lval || yypvt[-0].lval == (unsigned int) yypvt[-0].lval)
			    write_exp_elt_type (builtin_type_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_LONGEST);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LONG); } break;
case 54:
# line 445 "./c-exp.y"
{ YYSTYPE val;
			  parse_number (yypvt[-0].ssym.stoken.ptr, yypvt[-0].ssym.stoken.length, 0, &val);
			  write_exp_elt_opcode (OP_LONG);
			  if (val.lval == (int) val.lval ||
			      val.lval == (unsigned int) val.lval)
			    write_exp_elt_type (builtin_type_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_LONGEST);
			  write_exp_elt_longcst (val.lval);
			  write_exp_elt_opcode (OP_LONG); } break;
case 55:
# line 458 "./c-exp.y"
{
			  write_exp_elt_opcode (OP_LONG);
			  if (yypvt[-0].ulval == (unsigned int) yypvt[-0].ulval)
			    write_exp_elt_type (builtin_type_unsigned_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_UNSIGNED_LONGEST);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].ulval);
			  write_exp_elt_opcode (OP_LONG);
			} break;
case 56:
# line 470 "./c-exp.y"
{ YYSTYPE val;
			  parse_number (yypvt[-0].ssym.stoken.ptr, yypvt[-0].ssym.stoken.length, 0, &val);
			  write_exp_elt_opcode (OP_LONG);
			  if (val.ulval == (unsigned int) val.ulval)
			    write_exp_elt_type (builtin_type_unsigned_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_UNSIGNED_LONGEST);
			  write_exp_elt_longcst ((LONGEST)val.ulval);
			  write_exp_elt_opcode (OP_LONG);
			} break;
case 57:
# line 483 "./c-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_char);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LONG); } break;
case 58:
# line 490 "./c-exp.y"
{ write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_double);
			  write_exp_elt_dblcst (yypvt[-0].dval);
			  write_exp_elt_opcode (OP_DOUBLE); } break;
case 60:
# line 500 "./c-exp.y"
{ write_exp_elt_opcode (OP_LAST);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LAST); } break;
case 61:
# line 506 "./c-exp.y"
{ write_exp_elt_opcode (OP_REGISTER);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_REGISTER); } break;
case 62:
# line 512 "./c-exp.y"
{ write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern (yypvt[-0].ivar);
			  write_exp_elt_opcode (OP_INTERNALVAR); } break;
case 63:
# line 518 "./c-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH (yypvt[-1].tval));
			  write_exp_elt_opcode (OP_LONG); } break;
case 64:
# line 525 "./c-exp.y"
{ write_exp_elt_opcode (OP_STRING);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (OP_STRING); } break;
case 65:
# line 532 "./c-exp.y"
{ write_exp_elt_opcode (OP_THIS);
			  write_exp_elt_opcode (OP_THIS); } break;
case 66:
# line 539 "./c-exp.y"
{
			  if (yypvt[-0].ssym.sym != 0)
			      yyval.bval = SYMBOL_BLOCK_VALUE (yypvt[-0].ssym.sym);
			  else
			    {
			      struct symtab *tem =
				  lookup_symtab (copy_name (yypvt[-0].ssym.stoken));
			      if (tem)
				yyval.bval = BLOCKVECTOR_BLOCK
					 (BLOCKVECTOR (tem), STATIC_BLOCK);
			      else
				error ("No file or function \"%s\".",
				       copy_name (yypvt[-0].ssym.stoken));
			    }
			} break;
case 67:
# line 557 "./c-exp.y"
{ struct symbol *tem
			    = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					     VAR_NAMESPACE, 0, NULL);
			  if (!tem || SYMBOL_CLASS (tem) != LOC_BLOCK)
			    error ("No function \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));
			  yyval.bval = SYMBOL_BLOCK_VALUE (tem); } break;
case 68:
# line 567 "./c-exp.y"
{ struct symbol *sym;
			  sym = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					       VAR_NAMESPACE, 0, NULL);
			  if (sym == 0)
			    error ("No symbol \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));

			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_sym (sym);
			  write_exp_elt_opcode (OP_VAR_VALUE); } break;
case 69:
# line 580 "./c-exp.y"
{
			  struct type *type = yypvt[-2].tval;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (OP_SCOPE);
			} break;
case 70:
# line 593 "./c-exp.y"
{
			  struct type *type = yypvt[-3].tval;
			  struct stoken tmp_token;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  if (strcmp (type_name_no_tag (type), yypvt[-0].sval.ptr))
			    error ("invalid destructor `%s::~%s'",
				   type_name_no_tag (type), yypvt[-0].sval.ptr);

			  tmp_token.ptr = (char*) alloca (yypvt[-0].sval.length + 2);
			  tmp_token.length = yypvt[-0].sval.length + 1;
			  tmp_token.ptr[0] = '~';
			  memcpy (tmp_token.ptr+1, yypvt[-0].sval.ptr, yypvt[-0].sval.length);
			  tmp_token.ptr[tmp_token.length] = 0;
			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string (tmp_token);
			  write_exp_elt_opcode (OP_SCOPE);
			} break;
case 72:
# line 619 "./c-exp.y"
{
			  char *name = copy_name (yypvt[-0].sval);
			  struct symbol *sym;
			  struct minimal_symbol *msymbol;

			  sym =
			    lookup_symbol (name, 0, VAR_NAMESPACE, 0, NULL);
			  if (sym)
			    {
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      write_exp_elt_sym (sym);
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      break;
			    }

			  msymbol = lookup_minimal_symbol (name,
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
			  else
			    if (!have_full_symbols () && !have_partial_symbols ())
			      error ("No symbol table is loaded.  Use the \"file\" command.");
			    else
			      error ("No symbol \"%s\" in current context.", name);
			} break;
case 73:
# line 661 "./c-exp.y"
{ struct symbol *sym = yypvt[-0].ssym.sym;

			  if (sym)
			    {
			      switch (SYMBOL_CLASS (sym))
				{
				case LOC_REGISTER:
				case LOC_ARG:
				case LOC_REF_ARG:
				case LOC_REGPARM:
				case LOC_LOCAL:
				case LOC_LOCAL_ARG:
				  if (innermost_block == 0 ||
				      contained_in (block_found, 
						    innermost_block))
				    innermost_block = block_found;
				case LOC_UNDEF:
				case LOC_CONST:
				case LOC_STATIC:
				case LOC_TYPEDEF:
				case LOC_LABEL:
				case LOC_BLOCK:
				case LOC_CONST_BYTES:

				  /* In this case the expression can
				     be evaluated regardless of what
				     frame we are in, so there is no
				     need to check for the
				     innermost_block.  These cases are
				     listed so that gcc -Wall will
				     report types that may not have
				     been considered.  */

				  break;
				}
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      write_exp_elt_sym (sym);
			      write_exp_elt_opcode (OP_VAR_VALUE);
			    }
			  else if (yypvt[-0].ssym.is_a_field_of_this)
			    {
			      /* C++: it hangs off of `this'.  Must
			         not inadvertently convert from a method call
				 to data ref.  */
			      if (innermost_block == 0 || 
				  contained_in (block_found, innermost_block))
				innermost_block = block_found;
			      write_exp_elt_opcode (OP_THIS);
			      write_exp_elt_opcode (OP_THIS);
			      write_exp_elt_opcode (STRUCTOP_PTR);
			      write_exp_string (yypvt[-0].ssym.stoken);
			      write_exp_elt_opcode (STRUCTOP_PTR);
			    }
			  else
			    {
			      struct minimal_symbol *msymbol;
			      register char *arg = copy_name (yypvt[-0].ssym.stoken);

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
				error ("No symbol table is loaded.  Use the \"file\" command.");
			      else
				error ("No symbol \"%s\" in current context.",
				       copy_name (yypvt[-0].ssym.stoken));
			    }
			} break;
case 75:
# line 749 "./c-exp.y"
{
		  /* This is where the interesting stuff happens.  */
		  int done = 0;
		  int array_size;
		  struct type *follow_type = yypvt[-1].tval;
		  
		  while (!done)
		    switch (pop_type ())
		      {
		      case tp_end:
			done = 1;
			break;
		      case tp_pointer:
			follow_type = lookup_pointer_type (follow_type);
			break;
		      case tp_reference:
			follow_type = lookup_reference_type (follow_type);
			break;
		      case tp_array:
			array_size = pop_type_int ();
			if (array_size != -1)
			  follow_type = create_array_type (follow_type,
							   array_size);
			else
			  follow_type = lookup_pointer_type (follow_type);
			break;
		      case tp_function:
			follow_type = lookup_function_type (follow_type);
			break;
		      }
		  yyval.tval = follow_type;
		} break;
case 76:
# line 784 "./c-exp.y"
{ push_type (tp_pointer); yyval.voidval = 0; } break;
case 77:
# line 786 "./c-exp.y"
{ push_type (tp_pointer); yyval.voidval = yypvt[-0].voidval; } break;
case 78:
# line 788 "./c-exp.y"
{ push_type (tp_reference); yyval.voidval = 0; } break;
case 79:
# line 790 "./c-exp.y"
{ push_type (tp_reference); yyval.voidval = yypvt[-0].voidval; } break;
case 81:
# line 795 "./c-exp.y"
{ yyval.voidval = yypvt[-1].voidval; } break;
case 82:
# line 797 "./c-exp.y"
{
			  push_type_int (yypvt[-0].lval);
			  push_type (tp_array);
			} break;
case 83:
# line 802 "./c-exp.y"
{
			  push_type_int (yypvt[-0].lval);
			  push_type (tp_array);
			  yyval.voidval = 0;
			} break;
case 84:
# line 808 "./c-exp.y"
{ push_type (tp_function); } break;
case 85:
# line 810 "./c-exp.y"
{ push_type (tp_function); } break;
case 86:
# line 814 "./c-exp.y"
{ yyval.lval = -1; } break;
case 87:
# line 816 "./c-exp.y"
{ yyval.lval = yypvt[-1].lval; } break;
case 88:
# line 820 "./c-exp.y"
{ yyval.voidval = 0; } break;
case 89:
# line 822 "./c-exp.y"
{ free ((PTR)yypvt[-1].tvec); yyval.voidval = 0; } break;
case 91:
# line 827 "./c-exp.y"
{ yyval.tval = lookup_member_type (builtin_type_int, yypvt[-2].tval); } break;
case 92:
# line 829 "./c-exp.y"
{ yyval.tval = lookup_member_type (yypvt[-5].tval, yypvt[-3].tval); } break;
case 93:
# line 831 "./c-exp.y"
{ yyval.tval = lookup_member_type
			    (lookup_function_type (yypvt[-7].tval), yypvt[-5].tval); } break;
case 94:
# line 834 "./c-exp.y"
{ yyval.tval = lookup_member_type
			    (lookup_function_type (yypvt[-8].tval), yypvt[-6].tval);
			  free ((PTR)yypvt[-1].tvec); } break;
case 95:
# line 841 "./c-exp.y"
{ yyval.tval = yypvt[-0].tsym.type; } break;
case 96:
# line 843 "./c-exp.y"
{ yyval.tval = builtin_type_int; } break;
case 97:
# line 845 "./c-exp.y"
{ yyval.tval = builtin_type_long; } break;
case 98:
# line 847 "./c-exp.y"
{ yyval.tval = builtin_type_short; } break;
case 99:
# line 849 "./c-exp.y"
{ yyval.tval = builtin_type_long; } break;
case 100:
# line 851 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long; } break;
case 101:
# line 853 "./c-exp.y"
{ yyval.tval = builtin_type_long_long; } break;
case 102:
# line 855 "./c-exp.y"
{ yyval.tval = builtin_type_long_long; } break;
case 103:
# line 857 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long_long; } break;
case 104:
# line 859 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long_long; } break;
case 105:
# line 861 "./c-exp.y"
{ yyval.tval = builtin_type_short; } break;
case 106:
# line 863 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_short; } break;
case 107:
# line 865 "./c-exp.y"
{ yyval.tval = lookup_struct (copy_name (yypvt[-0].sval),
					      expression_context_block); } break;
case 108:
# line 868 "./c-exp.y"
{ yyval.tval = lookup_struct (copy_name (yypvt[-0].sval),
					      expression_context_block); } break;
case 109:
# line 871 "./c-exp.y"
{ yyval.tval = lookup_union (copy_name (yypvt[-0].sval),
					     expression_context_block); } break;
case 110:
# line 874 "./c-exp.y"
{ yyval.tval = lookup_enum (copy_name (yypvt[-0].sval),
					    expression_context_block); } break;
case 111:
# line 877 "./c-exp.y"
{ yyval.tval = lookup_unsigned_typename (TYPE_NAME(yypvt[-0].tsym.type)); } break;
case 112:
# line 879 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_int; } break;
case 113:
# line 881 "./c-exp.y"
{ yyval.tval = lookup_signed_typename (TYPE_NAME(yypvt[-0].tsym.type)); } break;
case 114:
# line 883 "./c-exp.y"
{ yyval.tval = builtin_type_int; } break;
case 115:
# line 885 "./c-exp.y"
{ yyval.tval = lookup_template_type(copy_name(yypvt[-3].sval), yypvt[-1].tval,
						    expression_context_block);
			} break;
case 116:
# line 889 "./c-exp.y"
{ yyval.tval = yypvt[-0].tval; } break;
case 117:
# line 890 "./c-exp.y"
{ yyval.tval = yypvt[-0].tval; } break;
case 119:
# line 895 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "int";
		  yyval.tsym.stoken.length = 3;
		  yyval.tsym.type = builtin_type_int;
		} break;
case 120:
# line 901 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "long";
		  yyval.tsym.stoken.length = 4;
		  yyval.tsym.type = builtin_type_long;
		} break;
case 121:
# line 907 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "short";
		  yyval.tsym.stoken.length = 5;
		  yyval.tsym.type = builtin_type_short;
		} break;
case 122:
# line 916 "./c-exp.y"
{ yyval.tvec = (struct type **) xmalloc (sizeof (struct type *) * 2);
		  yyval.ivec[0] = 1;	/* Number of types in vector */
		  yyval.tvec[1] = yypvt[-0].tval;
		} break;
case 123:
# line 921 "./c-exp.y"
{ int len = sizeof (struct type *) * (++(yypvt[-2].ivec[0]) + 1);
		  yyval.tvec = (struct type **) xrealloc ((char *) yypvt[-2].tvec, len);
		  yyval.tvec[yyval.ivec[0]] = yypvt[-0].tval;
		} break;
case 124:
# line 927 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
case 125:
# line 928 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
case 126:
# line 929 "./c-exp.y"
{ yyval.sval = yypvt[-0].tsym.stoken; } break;
case 127:
# line 930 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
case 128:
# line 931 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
