/* YACC parser for C expressions, for GDB.
   Copyright (C) 1986, 1989, 1990, 1991 Free Software Foundation, Inc.

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

/* Parse a C expression from text in a string,
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
    struct ttype tsym;
    struct symtoken ssym;
    int voidval;
    struct block *bval;
    enum exp_opcode opcode;
    struct internalvar *ivar;

    struct type **tvec;
    int *ivec;
  }

%{
/* YYSTYPE gets defined by %union */
static int
parse_number PARAMS ((char *, int, int, YYSTYPE *));
%}

%type <voidval> exp exp1 type_exp start variable qualified_name
%type <tval> type typebase
%type <tvec> nonempty_typelist
/* %type <bval> block */

/* Fancy type parsing.  */
%type <voidval> func_mod direct_abs_decl abs_decl
%type <tval> ptype
%type <lval> array_mod

%token <lval> INT CHAR
%token <ulval> UINT
%token <dval> FLOAT

/* Both NAME and TYPENAME tokens represent symbols in the input,
   and both convey their data as strings.
   But a TYPENAME is a string that happens to be defined as a typedef
   or builtin type name (such as int or char)
   and a NAME is any other symbol.
   Contexts where this distinction is not important can use the
   nonterminal "name", which matches either NAME or TYPENAME.  */

%token <sval> STRING
%token <ssym> NAME /* BLOCKNAME defined below to give it higher precedence. */
%token <tsym> TYPENAME
%type <sval> name
%type <ssym> name_not_typename
%type <tsym> typename

/* A NAME_OR_INT is a symbol which is not known in the symbol table,
   but which would parse as a valid number in the current input radix.
   E.g. "c" when input_radix==16.  Depending on the parse, it will be
   turned into a name or into a number.  NAME_OR_UINT ditto.  */

%token <ssym> NAME_OR_INT NAME_OR_UINT

%token STRUCT CLASS UNION ENUM SIZEOF UNSIGNED COLONCOLON
%token TEMPLATE
%token ERROR

/* Special type cases, put in to allow the parser to distinguish different
   legal basetypes.  */
%token SIGNED_KEYWORD LONG SHORT INT_KEYWORD CONST_KEYWORD VOLATILE_KEYWORD

%token <lval> LAST REGNAME

%token <ivar> VARIABLE

%token <opcode> ASSIGN_MODIFY

/* C++ */
%token THIS

%left ','
%left ABOVE_COMMA
%right '=' ASSIGN_MODIFY
%right '?'
%left OROR
%left ANDAND
%left '|'
%left '^'
%left '&'
%left EQUAL NOTEQUAL
%left '<' '>' LEQ GEQ
%left LSH RSH
%left '@'
%left '+' '-'
%left '*' '/' '%'
%right UNARY INCREMENT DECREMENT
%right ARROW '.' '[' '('
%token <ssym> BLOCKNAME 
%type <bval> block
%left COLONCOLON

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

start   :	exp1
	|	type_exp
	;

type_exp:	type
			{ write_exp_elt_opcode(OP_TYPE);
			  write_exp_elt_type($1);
			  write_exp_elt_opcode(OP_TYPE);}
	;

/* Expressions, including the comma operator.  */
exp1	:	exp
	|	exp1 ',' exp
			{ write_exp_elt_opcode (BINOP_COMMA); }
	;

/* Expressions, not including the comma operator.  */
exp	:	'*' exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_IND); }

exp	:	'&' exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_ADDR); }

exp	:	'-' exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_NEG); }
	;

exp	:	'!' exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_ZEROP); }
	;

exp	:	'~' exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_LOGNOT); }
	;

exp	:	INCREMENT exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_PREINCREMENT); }
	;

exp	:	DECREMENT exp    %prec UNARY
			{ write_exp_elt_opcode (UNOP_PREDECREMENT); }
	;

exp	:	exp INCREMENT    %prec UNARY
			{ write_exp_elt_opcode (UNOP_POSTINCREMENT); }
	;

exp	:	exp DECREMENT    %prec UNARY
			{ write_exp_elt_opcode (UNOP_POSTDECREMENT); }
	;

exp	:	SIZEOF exp       %prec UNARY
			{ write_exp_elt_opcode (UNOP_SIZEOF); }
	;

exp	:	exp ARROW name
			{ write_exp_elt_opcode (STRUCTOP_PTR);
			  write_exp_string ($3);
			  write_exp_elt_opcode (STRUCTOP_PTR); }
	;

exp	:	exp ARROW qualified_name
			{ /* exp->type::name becomes exp->*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MPTR); }
	;
exp	:	exp ARROW '*' exp
			{ write_exp_elt_opcode (STRUCTOP_MPTR); }
	;

exp	:	exp '.' name
			{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string ($3);
			  write_exp_elt_opcode (STRUCTOP_STRUCT); }
	;

exp	:	exp '.' qualified_name
			{ /* exp.type::name becomes exp.*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MEMBER); }
	;

exp	:	exp '.' '*' exp
			{ write_exp_elt_opcode (STRUCTOP_MEMBER); }
	;

exp	:	exp '[' exp1 ']'
			{ write_exp_elt_opcode (BINOP_SUBSCRIPT); }
	;

exp	:	exp '(' 
			/* This is to save the value of arglist_len
			   being accumulated by an outer function call.  */
			{ start_arglist (); }
		arglist ')'	%prec ARROW
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

exp	:	'{' type '}' exp  %prec UNARY
			{ write_exp_elt_opcode (UNOP_MEMVAL);
			  write_exp_elt_type ($2);
			  write_exp_elt_opcode (UNOP_MEMVAL); }
	;

exp	:	'(' type ')' exp  %prec UNARY
			{ write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type ($2);
			  write_exp_elt_opcode (UNOP_CAST); }
	;

exp	:	'(' exp1 ')'
			{ }
	;

/* Binary operators in order of decreasing precedence.  */

exp	:	exp '@' exp
			{ write_exp_elt_opcode (BINOP_REPEAT); }
	;

exp	:	exp '*' exp
			{ write_exp_elt_opcode (BINOP_MUL); }
	;

exp	:	exp '/' exp
			{ write_exp_elt_opcode (BINOP_DIV); }
	;

exp	:	exp '%' exp
			{ write_exp_elt_opcode (BINOP_REM); }
	;

exp	:	exp '+' exp
			{ write_exp_elt_opcode (BINOP_ADD); }
	;

exp	:	exp '-' exp
			{ write_exp_elt_opcode (BINOP_SUB); }
	;

exp	:	exp LSH exp
			{ write_exp_elt_opcode (BINOP_LSH); }
	;

exp	:	exp RSH exp
			{ write_exp_elt_opcode (BINOP_RSH); }
	;

exp	:	exp EQUAL exp
			{ write_exp_elt_opcode (BINOP_EQUAL); }
	;

exp	:	exp NOTEQUAL exp
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

exp	:	exp '&' exp
			{ write_exp_elt_opcode (BINOP_LOGAND); }
	;

exp	:	exp '^' exp
			{ write_exp_elt_opcode (BINOP_LOGXOR); }
	;

exp	:	exp '|' exp
			{ write_exp_elt_opcode (BINOP_LOGIOR); }
	;

exp	:	exp ANDAND exp
			{ write_exp_elt_opcode (BINOP_AND); }
	;

exp	:	exp OROR exp
			{ write_exp_elt_opcode (BINOP_OR); }
	;

exp	:	exp '?' exp ':' exp	%prec '?'
			{ write_exp_elt_opcode (TERNOP_COND); }
	;
			  
exp	:	exp '=' exp
			{ write_exp_elt_opcode (BINOP_ASSIGN); }
	;

exp	:	exp ASSIGN_MODIFY exp
			{ write_exp_elt_opcode (BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode ($2);
			  write_exp_elt_opcode (BINOP_ASSIGN_MODIFY); }
	;

exp	:	INT
			{ write_exp_elt_opcode (OP_LONG);
			  if ($1 == (int) $1 || $1 == (unsigned int) $1)
			    write_exp_elt_type (builtin_type_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_LONGEST);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	NAME_OR_INT
			{ YYSTYPE val;
			  parse_number ($1.stoken.ptr, $1.stoken.length, 0, &val);
			  write_exp_elt_opcode (OP_LONG);
			  if (val.lval == (int) val.lval ||
			      val.lval == (unsigned int) val.lval)
			    write_exp_elt_type (builtin_type_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_LONGEST);
			  write_exp_elt_longcst (val.lval);
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	UINT
			{
			  write_exp_elt_opcode (OP_LONG);
			  if ($1 == (unsigned int) $1)
			    write_exp_elt_type (builtin_type_unsigned_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_UNSIGNED_LONGEST);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG);
			}
	;

exp	:	NAME_OR_UINT
			{ YYSTYPE val;
			  parse_number ($1.stoken.ptr, $1.stoken.length, 0, &val);
			  write_exp_elt_opcode (OP_LONG);
			  if (val.ulval == (unsigned int) val.ulval)
			    write_exp_elt_type (builtin_type_unsigned_int);
			  else
			    write_exp_elt_type (BUILTIN_TYPE_UNSIGNED_LONGEST);
			  write_exp_elt_longcst ((LONGEST)val.ulval);
			  write_exp_elt_opcode (OP_LONG);
			}
	;

exp	:	CHAR
			{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_char);
			  write_exp_elt_longcst ((LONGEST) $1);
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	FLOAT
			{ write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_double);
			  write_exp_elt_dblcst ($1);
			  write_exp_elt_opcode (OP_DOUBLE); }
	;

exp	:	variable
	;

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

exp	:	VARIABLE
			{ write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern ($1);
			  write_exp_elt_opcode (OP_INTERNALVAR); }
	;

exp	:	SIZEOF '(' type ')'	%prec UNARY
			{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH ($3));
			  write_exp_elt_opcode (OP_LONG); }
	;

exp	:	STRING
			{ write_exp_elt_opcode (OP_STRING);
			  write_exp_string ($1);
			  write_exp_elt_opcode (OP_STRING); }
	;

/* C++.  */
exp	:	THIS
			{ write_exp_elt_opcode (OP_THIS);
			  write_exp_elt_opcode (OP_THIS); }
	;

/* end of C++.  */

block	:	BLOCKNAME
			{
			  if ($1.sym != 0)
			      $$ = SYMBOL_BLOCK_VALUE ($1.sym);
			  else
			    {
			      struct symtab *tem =
				  lookup_symtab (copy_name ($1.stoken));
			      if (tem)
				$$ = BLOCKVECTOR_BLOCK
					 (BLOCKVECTOR (tem), STATIC_BLOCK);
			      else
				error ("No file or function \"%s\".",
				       copy_name ($1.stoken));
			    }
			}
	;

block	:	block COLONCOLON name
			{ struct symbol *tem
			    = lookup_symbol (copy_name ($3), $1,
					     VAR_NAMESPACE, 0, NULL);
			  if (!tem || SYMBOL_CLASS (tem) != LOC_BLOCK)
			    error ("No function \"%s\" in specified context.",
				   copy_name ($3));
			  $$ = SYMBOL_BLOCK_VALUE (tem); }
	;

variable:	block COLONCOLON name
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

qualified_name:	typebase COLONCOLON name
			{
			  struct type *type = $1;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string ($3);
			  write_exp_elt_opcode (OP_SCOPE);
			}
	|	typebase COLONCOLON '~' name
			{
			  struct type *type = $1;
			  struct stoken tmp_token;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  if (strcmp (type_name_no_tag (type), $4.ptr))
			    error ("invalid destructor `%s::~%s'",
				   type_name_no_tag (type), $4.ptr);

			  tmp_token.ptr = (char*) alloca ($4.length + 2);
			  tmp_token.length = $4.length + 1;
			  tmp_token.ptr[0] = '~';
			  memcpy (tmp_token.ptr+1, $4.ptr, $4.length);
			  tmp_token.ptr[tmp_token.length] = 0;
			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string (tmp_token);
			  write_exp_elt_opcode (OP_SCOPE);
			}
	;

variable:	qualified_name
	|	COLONCOLON name
			{
			  char *name = copy_name ($2);
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
			}
	;

variable:	name_not_typename
			{ struct symbol *sym = $1.sym;

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
			  else if ($1.is_a_field_of_this)
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
			      write_exp_string ($1.stoken);
			      write_exp_elt_opcode (STRUCTOP_PTR);
			    }
			  else
			    {
			      struct minimal_symbol *msymbol;
			      register char *arg = copy_name ($1.stoken);

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
				       copy_name ($1.stoken));
			    }
			}
	;


ptype	:	typebase
	|	typebase abs_decl
		{
		  /* This is where the interesting stuff happens.  */
		  int done = 0;
		  int array_size;
		  struct type *follow_type = $1;
		  
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
		  $$ = follow_type;
		}
	;

abs_decl:	'*'
			{ push_type (tp_pointer); $$ = 0; }
	|	'*' abs_decl
			{ push_type (tp_pointer); $$ = $2; }
	|	'&'
			{ push_type (tp_reference); $$ = 0; }
	|	'&' abs_decl
			{ push_type (tp_reference); $$ = $2; }
	|	direct_abs_decl
	;

direct_abs_decl: '(' abs_decl ')'
			{ $$ = $2; }
	|	direct_abs_decl array_mod
			{
			  push_type_int ($2);
			  push_type (tp_array);
			}
	|	array_mod
			{
			  push_type_int ($1);
			  push_type (tp_array);
			  $$ = 0;
			}
	| 	direct_abs_decl func_mod
			{ push_type (tp_function); }
	|	func_mod
			{ push_type (tp_function); }
	;

array_mod:	'[' ']'
			{ $$ = -1; }
	|	'[' INT ']'
			{ $$ = $2; }
	;

func_mod:	'(' ')'
			{ $$ = 0; }
	|	'(' nonempty_typelist ')'
			{ free ((PTR)$2); $$ = 0; }
	;

type	:	ptype
	|	typebase COLONCOLON '*'
			{ $$ = lookup_member_type (builtin_type_int, $1); }
	|	type '(' typebase COLONCOLON '*' ')'
			{ $$ = lookup_member_type ($1, $3); }
	|	type '(' typebase COLONCOLON '*' ')' '(' ')'
			{ $$ = lookup_member_type
			    (lookup_function_type ($1), $3); }
	|	type '(' typebase COLONCOLON '*' ')' '(' nonempty_typelist ')'
			{ $$ = lookup_member_type
			    (lookup_function_type ($1), $3);
			  free ((PTR)$8); }
	;

typebase  /* Implements (approximately): (type-qualifier)* type-specifier */
	:	TYPENAME
			{ $$ = $1.type; }
	|	INT_KEYWORD
			{ $$ = builtin_type_int; }
	|	LONG
			{ $$ = builtin_type_long; }
	|	SHORT
			{ $$ = builtin_type_short; }
	|	LONG INT_KEYWORD
			{ $$ = builtin_type_long; }
	|	UNSIGNED LONG INT_KEYWORD
			{ $$ = builtin_type_unsigned_long; }
	|	LONG LONG
			{ $$ = builtin_type_long_long; }
	|	LONG LONG INT_KEYWORD
			{ $$ = builtin_type_long_long; }
	|	UNSIGNED LONG LONG
			{ $$ = builtin_type_unsigned_long_long; }
	|	UNSIGNED LONG LONG INT_KEYWORD
			{ $$ = builtin_type_unsigned_long_long; }
	|	SHORT INT_KEYWORD
			{ $$ = builtin_type_short; }
	|	UNSIGNED SHORT INT_KEYWORD
			{ $$ = builtin_type_unsigned_short; }
	|	STRUCT name
			{ $$ = lookup_struct (copy_name ($2),
					      expression_context_block); }
	|	CLASS name
			{ $$ = lookup_struct (copy_name ($2),
					      expression_context_block); }
	|	UNION name
			{ $$ = lookup_union (copy_name ($2),
					     expression_context_block); }
	|	ENUM name
			{ $$ = lookup_enum (copy_name ($2),
					    expression_context_block); }
	|	UNSIGNED typename
			{ $$ = lookup_unsigned_typename (TYPE_NAME($2.type)); }
	|	UNSIGNED
			{ $$ = builtin_type_unsigned_int; }
	|	SIGNED_KEYWORD typename
			{ $$ = lookup_signed_typename (TYPE_NAME($2.type)); }
	|	SIGNED_KEYWORD
			{ $$ = builtin_type_int; }
	|	TEMPLATE name '<' type '>'
			{ $$ = lookup_template_type(copy_name($2), $4,
						    expression_context_block);
			}
	/* "const" and "volatile" are curently ignored. */
	|	CONST_KEYWORD typebase { $$ = $2; }
	|	VOLATILE_KEYWORD typebase { $$ = $2; }
	;

typename:	TYPENAME
	|	INT_KEYWORD
		{
		  $$.stoken.ptr = "int";
		  $$.stoken.length = 3;
		  $$.type = builtin_type_int;
		}
	|	LONG
		{
		  $$.stoken.ptr = "long";
		  $$.stoken.length = 4;
		  $$.type = builtin_type_long;
		}
	|	SHORT
		{
		  $$.stoken.ptr = "short";
		  $$.stoken.length = 5;
		  $$.type = builtin_type_short;
		}
	;

nonempty_typelist
	:	type
		{ $$ = (struct type **) xmalloc (sizeof (struct type *) * 2);
		  $<ivec>$[0] = 1;	/* Number of types in vector */
		  $$[1] = $1;
		}
	|	nonempty_typelist ',' type
		{ int len = sizeof (struct type *) * (++($<ivec>1[0]) + 1);
		  $$ = (struct type **) xrealloc ((char *) $1, len);
		  $$[$<ivec>$[0]] = $3;
		}
	;

name	:	NAME { $$ = $1.stoken; }
	|	BLOCKNAME { $$ = $1.stoken; }
	|	TYPENAME { $$ = $1.stoken; }
	|	NAME_OR_INT  { $$ = $1.stoken; }
	|	NAME_OR_UINT  { $$ = $1.stoken; }
	;

name_not_typename :	NAME
	|	BLOCKNAME
/* These would be useful if name_not_typename was useful, but it is just
   a fake for "variable", so these cause reduce/reduce conflicts because
   the parser can't tell whether NAME_OR_INT is a name_not_typename (=variable,
   =exp) or just an exp.  If name_not_typename was ever used in an lvalue
   context where only a name could occur, this might be useful.
  	|	NAME_OR_INT
  	|	NAME_OR_UINT
 */
	;

%%

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
