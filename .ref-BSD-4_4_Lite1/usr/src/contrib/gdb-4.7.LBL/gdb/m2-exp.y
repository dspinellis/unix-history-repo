/* YACC grammar for Modula-2 expressions, for GDB.
   Copyright (C) 1986, 1989, 1990, 1991 Free Software Foundation, Inc.
   Generated from expread.y (now c-exp.y) and contributed by the Department
   of Computer Science at the State University of New York at Buffalo, 1991.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Parse a Modula-2 expression from text in a string,
   and return the result as a  struct expression  pointer.
   That structure contains arithmetic operations in reverse polish,
   with constants represented by operations that are followed by special data.
   See expression.h for the details of the format.
   What is important here is that it can be built up sequentially
   during the process of parsing; the lower levels of the tree always
   come first in the result.  */
   
%{
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
%}

/* Although the yacc "value" of an expression is not used,
   since the result is stored in the structure being created,
   other node types do have values.  */

%union
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
  }

%type <voidval> exp type_exp start set
%type <voidval> variable
%type <tval> type
%type <bval> block 
%type <sym> fblock 

%token <lval> INT HEX ERROR
%token <ulval> UINT M2_TRUE M2_FALSE CHAR
%token <dval> FLOAT

/* Both NAME and TYPENAME tokens represent symbols in the input,
   and both convey their data as strings.
   But a TYPENAME is a string that happens to be defined as a typedef
   or builtin type name (such as int or char)
   and a NAME is any other symbol.

   Contexts where this distinction is not important can use the
   nonterminal "name", which matches either NAME or TYPENAME.  */

%token <sval> STRING
%token <sval> NAME BLOCKNAME IDENT VARNAME
%token <sval> TYPENAME

%token SIZE CAP ORD HIGH ABS MIN_FUNC MAX_FUNC FLOAT_FUNC VAL CHR ODD TRUNC
%token INC DEC INCL EXCL

/* The GDB scope operator */
%token COLONCOLON

%token <lval> LAST REGNAME

%token <ivar> INTERNAL_VAR

/* M2 tokens */
%left ','
%left ABOVE_COMMA
%nonassoc ASSIGN
%left '<' '>' LEQ GEQ '=' NOTEQUAL '#' IN
%left OROR
%left ANDAND '&'
%left '@'
%left '+' '-'
%left '*' '/' DIV MOD
%right UNARY
%right '^' DOT '[' '('
%right NOT '~'
%left COLONCOLON QID
/* This is not an actual token ; it is used for precedence. 
%right QID
*/

%{
/* Ensure that if the generated parser contains any calls to malloc/realloc,
   that they get mapped to xmalloc/xrealloc.  We have to do this here
   rather than earlier in the file because this is the first point after
   the place where the SVR4 yacc includes <malloc.h>, and if we do it
   before that, then the remapped declarations in <malloc.h> will collide
   with the ones in "defs.h". */

#define malloc	xmalloc
#define realloc	xrealloc
%}

%%

start   :	exp
	|	type_exp
	;

type_exp:	type
		{ write_exp_elt_opcode(OP_TYPE);
		  write_exp_elt_type($1);
		  write_exp_elt_opcode(OP_TYPE);
		}
	;

/* Expressions */

exp     :       exp '^'   %prec UNARY
                        { write_exp_elt_opcode (UNOP_IND); }

exp	:	'-'
			{ number_sign = -1; }
		exp    %prec UNARY
			{ number_sign = 1;
			  write_exp_elt_opcode (UNOP_NEG); }
	;

exp	:	'+' exp    %prec UNARY
		{ write_exp_elt_opcode(UNOP_PLUS); }
	;

exp	:	not_exp exp %prec UNARY
			{ write_exp_elt_opcode (UNOP_ZEROP); }
	;

not_exp	:	NOT
	|	'~'
	;

exp	:	CAP '(' exp ')'
			{ write_exp_elt_opcode (UNOP_CAP); }
	;

exp	:	ORD '(' exp ')'
			{ write_exp_elt_opcode (UNOP_ORD); }
	;

exp	:	ABS '(' exp ')'
			{ write_exp_elt_opcode (UNOP_ABS); }
	;

exp	: 	HIGH '(' exp ')'
			{ write_exp_elt_opcode (UNOP_HIGH); }
	;

exp 	:	MIN_FUNC '(' type ')'
			{ write_exp_elt_opcode (UNOP_MIN);
			  write_exp_elt_type ($3);
			  write_exp_elt_opcode (UNOP_MIN); }
	;

exp	: 	MAX_FUNC '(' type ')'
			{ write_exp_elt_opcode (UNOP_MAX);
			  write_exp_elt_type ($3);
			  write_exp_elt_opcode (UNOP_MIN); }
	;

exp	:	FLOAT_FUNC '(' exp ')'
			{ write_exp_elt_opcode (UNOP_FLOAT); }
	;

exp	:	VAL '(' type ',' exp ')'
			{ write_exp_elt_opcode (BINOP_VAL);
			  write_exp_elt_type ($3);
			  write_exp_elt_opcode (BINOP_VAL); }
	;

exp	:	CHR '(' exp ')'
			{ write_exp_elt_opcode (UNOP_CHR); }
	;

exp	:	ODD '(' exp ')'
			{ write_exp_elt_opcode (UNOP_ODD); }
	;

exp	:	TRUNC '(' exp ')'
			{ write_exp_elt_opcode (UNOP_TRUNC); }
	;

exp	:	SIZE exp       %prec UNARY
			{ write_exp_elt_opcode (UNOP_SIZEOF); }
	;


exp	:	INC '(' exp ')'
			{ write_exp_elt_opcode(UNOP_PREINCREMENT); }
	;

exp	:	INC '(' exp ',' exp ')'
			{ write_exp_elt_opcode(BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode(BINOP_ADD);
			  write_exp_elt_opcode(BINOP_ASSIGN_MODIFY); }
	;

exp	:	DEC '(' exp ')'
			{ write_exp_elt_opcode(UNOP_PREDECREMENT);}
	;

exp	:	DEC '(' exp ',' exp ')'
			{ write_exp_elt_opcode(BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode(BINOP_SUB);
			  write_exp_elt_opcode(BINOP_ASSIGN_MODIFY); }
	;

exp	:	exp DOT NAME
			{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string ($3);
			  write_exp_elt_opcode (STRUCTOP_STRUCT); }
	;

exp	:	set
	;

exp	:	exp IN set
			{ error("Sets are not implemented.");}
	;

exp	:	INCL '(' exp ',' exp ')'
			{ error("Sets are not implemented.");}
	;

exp	:	EXCL '(' exp ',' exp ')'
			{ error("Sets are not implemented.");}

set	:	'{' arglist '}'
			{ error("Sets are not implemented.");}
	|	type '{' arglist '}'
			{ error("Sets are not implemented.");}
	;


/* Modula-2 array subscript notation [a,b,c...] */
exp     :       exp '['
                        /* This function just saves the number of arguments
			   that follow in the list.  It is *not* specific to
			   function types */
                        { start_arglist(); }
                non_empty_arglist ']'  %prec DOT
                        { write_exp_elt_opcode (BINOP_MULTI_SUBSCRIPT);
			  write_exp_elt_longcst ((LONGEST) end_arglist());
			  write_exp_elt_opcode (BINOP_MULTI_SUBSCRIPT); }
        ;

exp	:	exp '('
			/* This is to save the value of arglist_len
			   being accumulated by an outer function call.  */
			{ start_arglist (); }
		arglist ')'	%prec DOT
			{ write_exp_elt_opcode (OP_FUNCALL);
			  write_exp_elt_longcst ((LONGEST) end_arglist ());
			  write_exp_elt_opcode (OP_FUNCALL); }
	;

arglist	:
	;

arglist	:	exp
			{ arglist_len = 1; }
	;

arglist	:	arglist ',' exp   %prec ABOVE_COMMA
			{ arglist_len++; }
	;

non_empty_arglist
        :       exp
                        { arglist_len = 1; }
	;

non_empty_arglist
        :       non_empty_arglist ',' exp %prec ABOVE_COMMA
     	       	    	{ arglist_len++; }
     	;

/* GDB construct */
exp	:	'{' type '}' exp  %prec UNARY
			{ write_exp_elt_opcode (UNOP_MEMVAL);
			  write_exp_elt_type ($2);
			  write_exp_elt_opcode (UNOP_MEMVAL); }
	;

exp     :       type '(' exp ')' %prec UNARY
                        { write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type ($1);
			  write_exp_elt_opcode (UNOP_CAST); }
	;

exp	:	'(' exp ')'
			{ }
	;

/* Binary operators in order of decreasing precedence.  Note that some
   of these operators are overloaded!  (ie. sets) */

/* GDB construct */
exp	:	exp '@' exp
			{ write_exp_elt_opcode (BINOP_REPEAT); }
	;

exp	:	exp '*' exp
			{ write_exp_elt_opcode (BINOP_MUL); }
	;

exp	:	exp '/' exp
			{ write_exp_elt_opcode (BINOP_DIV); }
	;

exp     :       exp DIV exp
                        { write_exp_elt_opcode (BINOP_INTDIV); }
        ;

exp	:	exp MOD exp
			{ write_exp_elt_opcode (BINOP_REM); }
	;

exp	:	exp '+' exp
			{ write_exp_elt_opcode (BINOP_ADD); }
	;

exp	:	exp '-' exp
			{ write_exp_elt_opcode (BINOP_SUB); }
	;

exp	:	exp '=' exp
			{ write_exp_elt_opcode (BINOP_EQUAL); }
	;

exp	:	exp NOTEQUAL exp
			{ write_exp_elt_opcode (BINOP_NOTEQUAL); }
        |       exp '#' exp
                        { write_exp_elt_opcode (BINOP_NOTEQUAL); }
	;

exp	:	exp LEQ exp
			{ write_exp_elt_opcode (BINOP_LEQ); }
	;

exp	:	exp GEQ exp
			{ write_exp_elt_opcode (BINOP_GEQ); }
	;

exp	:	exp '<' exp
			{ write_exp_elt_opcode (BINOP_LESS); }
	;

exp	:	exp '>' exp
			{ write_exp_elt_opcode (BINOP_GTR); }
	;

exp	:	exp ANDAND exp
			{ write_exp_elt_opcode (BINOP_AND); }
	;

exp	:	exp '&'	exp
			{ write_exp_elt_opcode (BINOP_AND); }
	;

exp	:	exp OROR exp
			{ write_exp_elt_opcode (BINOP_OR); }
	;

exp	:	exp ASSIGN exp
			{ write_exp_elt_opcode (BINOP_ASSIGN); }
	;


/* Constants */

exp	:	M2_TRUE
			{ write_exp_elt_opcode (OP_BOOL);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_BOOL); }
	;

exp	:	M2_FALSE
			{ write_exp_elt_opcode (OP_BOOL);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_BOOL); }
	;

exp	:	INT
			{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_int);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	UINT
			{
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_card);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG);
			}
	;

exp	:	CHAR
			{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_m2_char);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG); }
	;


exp	:	FLOAT
			{ write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_m2_real);
			  write_exp_elt_dblcst ($1);
			  write_exp_elt_opcode (OP_DOUBLE); }
	;

exp	:	variable
	;

/* The GDB internal variable $$, et al. */
exp	:	LAST
			{ write_exp_elt_opcode (OP_LAST);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LAST); }
	;

exp	:	REGNAME
			{ write_exp_elt_opcode (OP_REGISTER);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_REGISTER); }
	;

exp	:	SIZE '(' type ')'	%prec UNARY
			{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH ($3));
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	STRING
			{ write_exp_elt_opcode (OP_M2_STRING);
			  write_exp_string ($1);
			  write_exp_elt_opcode (OP_M2_STRING); }
	;

/* This will be used for extensions later.  Like adding modules. */
block	:	fblock	
			{ $$ = SYMBOL_BLOCK_VALUE($1); }
	;

fblock	:	BLOCKNAME
			{ struct symbol *sym
			    = lookup_symbol (copy_name ($1), expression_context_block,
					     VAR_NAMESPACE, 0, NULL);
			  $$ = sym;}
	;
			     

/* GDB scope operator */
fblock	:	block COLONCOLON BLOCKNAME
			{ struct symbol *tem
			    = lookup_symbol (copy_name ($3), $1,
					     VAR_NAMESPACE, 0, NULL);
			  if (!tem || SYMBOL_CLASS (tem) != LOC_BLOCK)
			    error ("No function \"%s\" in specified context.",
				   copy_name ($3));
			  $$ = tem;
			}
	;

/* Useful for assigning to PROCEDURE variables */
variable:	fblock
			{ write_exp_elt_opcode(OP_VAR_VALUE);
			  write_exp_elt_sym ($1);
			  write_exp_elt_opcode (OP_VAR_VALUE); }
	;

/* GDB internal ($foo) variable */
variable:	INTERNAL_VAR
			{ write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern ($1);
			  write_exp_elt_opcode (OP_INTERNALVAR); }
	;

/* GDB scope operator */
variable:	block COLONCOLON NAME
			{ struct symbol *sym;
			  sym = lookup_symbol (copy_name ($3), $1,
					       VAR_NAMESPACE, 0, NULL);
			  if (sym == 0)
			    error ("No symbol \"%s\" in specified context.",
				   copy_name ($3));

			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_sym (sym);
			  write_exp_elt_opcode (OP_VAR_VALUE); }
	;

/* Base case for variables. */
variable:	NAME
			{ struct symbol *sym;
			  int is_a_field_of_this;

 			  sym = lookup_symbol (copy_name ($1),
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
			      register char *arg = copy_name ($1);

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
				       copy_name ($1));
			    }
			}
	;

type
	:	TYPENAME
			{ $$ = lookup_typename (copy_name ($1),
						expression_context_block, 0); }

	;

%%

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
