/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 */

/* YACC parser for C++ syntax.
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Also note: this version contains experimental exception
   handling features.  They could break, change, disappear,
   or otherwise exhibit volatile behavior.  Don't depend on
   me (Michael Tiemann) to protect you from any negative impact
   this may have on your professional, personal, or spiritual life.  */

%{
#ifndef lint
static char sccsid[] = "@(#)cplus-parse.y	6.3 (Berkeley) 5/8/91";
#endif /* not lint */

#include "config.h"
#include "tree.h"
#include "input.h"
#include "cplus-parse.h"
#include "cplus-tree.h"
#include "assert.h"

/* C++ extensions */
extern tree ridpointers[];	/* need this up here */
extern tree void_list_node;

/* Bison compatibility */
#define	YYEMPTY		-1
#define	YYLEX		yylex()

#include <stdio.h>
#include <errno.h>

#ifndef errno
extern int errno;
#endif

extern int end_of_file;

void yyerror ();

/* Contains error message to give if user tries to declare
   a variable where one does not belong.  */
static char *stmt_decl_msg = 0;

#ifndef YYDEBUG
/* Cause the `yydebug' variable to be defined.  */
int yydebug;
#endif

/* Cons up an empty parameter list.  */
#ifdef __GNU__
__inline
#endif
static tree
empty_parms ()
{
  tree parms;

  if (strict_prototype)
    parms = void_list_node;
  else
    parms = NULL_TREE;
  return parms;
}

void yyhook ();
%}

%start program

%union {long itype; tree ttype; enum tree_code code; }

/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC

/* Reserved words that qualify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPE_QUAL

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM TYPEOF ALIGNOF
%token ATTRIBUTE

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token DELETE NEW OVERLOAD PRIVATE PUBLIC PROTECTED THIS OPERATOR
%token DYNAMIC POINTSAT_LEFT_RIGHT LEFT_RIGHT
%token <itype> SCOPE

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%left EMPTY			/* used to resolve s/r with epsilon */

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

%left IDENTIFIER TYPENAME TYPENAME_COLON SCSPEC TYPESPEC TYPE_QUAL ENUM AGGR

%left '{' ','

%right <code> ASSIGN '='
%right <code> '?' ':' RANGE
%left <code> OROR
%left <code> ANDAND
%left <code> '|'
%left <code> '^'
%left <code> '&'
%left <code> MIN_MAX
%left <code> EQCOMPARE
%left <code> ARITHCOMPARE
%left <code> LSHIFT RSHIFT
%left <code> '+' '-'
%left <code> '*' '/' '%'
%right <code> UNARY PLUSPLUS MINUSMINUS
%left HYPERUNARY
%left <ttype> PAREN_STAR_PAREN PAREN_X_SCOPE_STAR_PAREN PAREN_X_SCOPE_REF_PAREN LEFT_RIGHT
%left <code> POINTSAT '.' '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE RAISE RAISES RERAISE TRY EXCEPT CATCH
%right DYNAMIC

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist /* exprlist */
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> typed_declspecs reserved_declspecs
%type <ttype> typed_typespecs reserved_typespecquals
%type <ttype> declmods typespec typespecqual_reserved
%type <ttype> SCSPEC TYPESPEC TYPE_QUAL nonempty_type_quals maybe_type_qual
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attribute_list attrib

%type <ttype> compstmt except_stmts

%type <ttype> declarator notype_declarator after_type_declarator

%type <ttype> structsp opt.component_decl_list component_decl_list component_decl components component_declarator
%type <ttype> enumlist enumerator
%type <ttype> typename absdcl absdcl1 type_quals
%type <ttype> xexpr see_typename parmlist parms parm bad_parm

/* C++ extensions */
%token <ttype> TYPENAME_COLON TYPENAME_SCOPE TYPENAME_ELLIPSIS
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%type <ttype> fn.def2 dummy_decl x_typespec return_id
%type <ttype> class_head opt.init base_class_list base_class_visibility_list
%type <ttype> after_type_declarator_no_typename
%type <ttype> maybe_raises raise_identifier raise_identifiers
%type <ttype> component_declarator0 scoped_identifier
%type <ttype> forhead.1 identifier_or_opname operator_name
%type <ttype> new delete object primary_no_id aggr nonmomentary_expr
%type <itype> LC forhead.2 initdcl0 notype_initdcl0 wrapper member_init_list
%type <itype> .scope try

%{
/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
tree lastiddecl;

tree make_pointer_declarator (), make_reference_declarator ();

tree combine_strings ();
void reinit_parse_for_function ();
void reinit_parse_for_method ();

/* List of types and structure classes of the current declaration.  */
tree current_declspecs;

int undeclared_variable_notice;	/* 1 if we explained undeclared var errors.  */

int yylex ();
extern FILE *finput;
%}

%%
program: .program /* empty */
	| .program extdefs
		{ finish_file (); }
	;

.program: /* empty */
		{
		  if (flag_cadillac)
		    cadillac_start ();
		}

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	  {$<ttype>$ = NULL_TREE; } extdef
	| extdefs {$<ttype>$ = NULL_TREE; } extdef
	;

extdef:
	  fndef
		{ if (pending_inlines) do_pending_inlines (); }
	| datadef
		{ if (pending_inlines) do_pending_inlines (); }
	| overloaddef
	| ASM '(' string ')' ';'
		{ if (pedantic)
		    warning ("ANSI C forbids use of `asm' keyword");
		  if (TREE_CHAIN ($3)) $3 = combine_strings ($3);
		  assemble_asm ($3); }
	| extern_lang_string '{' extdefs '}'
		{ pop_lang_context (); }
	| extern_lang_string '{' '}'
		{ pop_lang_context (); }
	| extern_lang_string fndef
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	| extern_lang_string datadef
		{ if (pending_inlines) do_pending_inlines ();
		  pop_lang_context (); }
	;

extern_lang_string:
	  EXTERN_LANG_STRING
		{ push_lang_context ($1); }
	;

overloaddef:
	  OVERLOAD ov_identifiers ';'

ov_identifiers: IDENTIFIER
		{ declare_overloaded ($1); }
	| ov_identifiers ',' IDENTIFIER
		{ declare_overloaded ($3); }
	;
	  
dummy_decl: /* empty */
		{ $$ = NULL_TREE; }
	;

datadef:
	  dummy_decl notype_initdecls ';'
		{ if (pedantic)
		    error ("ANSI C forbids data definition lacking type or storage class");
  		  else if (! flag_traditional)
  		    warning ("data definition lacks type or storage class"); }
	| declmods notype_initdecls ';'
		{}
	/* Normal case to make fast: "int i;".  */
	| declmods declarator ';'
		{ tree d;
		  d = start_decl ($2, $1, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE);
		}
	| typed_declspecs initdecls ';'
		{
		  end_exception_decls ();
		  note_got_semicolon ($1);
		}
	/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
		{ tree d;
		  d = start_decl ($2, $1, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE);
		  end_exception_decls ();
		  note_got_semicolon ($1);
		}
        | declmods ';'
	  { error ("empty declaration"); }
	| typed_declspecs ';'
	  {
	    shadow_tag ($1);
	    note_got_semicolon ($1);
	  }
	| error ';'
	| error '}'
	| ';'
	;

fndef:
	  fn.def1 base_init compstmt_or_error
		{
		  finish_function (lineno, 1);
		  /* finish_function performs these three statements:

		     expand_end_bindings (getdecls (), 1, 0);
		     poplevel (1, 1, 0);

		     expand_end_bindings (0, 0, 0);
		     poplevel (0, 0, 1);
		     */
		}
	| fn.def1 return_init base_init compstmt_or_error
		{
		  finish_function (lineno, 1);
		  /* finish_function performs these three statements:

		     expand_end_bindings (getdecls (), 1, 0);
		     poplevel (1, 1, 0);

		     expand_end_bindings (0, 0, 0);
		     poplevel (0, 0, 1);
		     */
		}
	| fn.def1 nodecls compstmt_or_error
		{ finish_function (lineno, 0); }
	| fn.def1 return_init ';' nodecls compstmt_or_error
		{ finish_function (lineno, 0); }
	| fn.def1 return_init nodecls compstmt_or_error
		{ finish_function (lineno, 0); }
	| typed_declspecs declarator error
		{}
	| declmods notype_declarator error
		{}
	| dummy_decl notype_declarator error
		{}
	;

fn.def1:
	  typed_declspecs declarator maybe_raises
		{ if (! start_function ($1, $2, $3, 0))
		    YYERROR;
		  reinit_parse_for_function (); }
	| declmods notype_declarator maybe_raises
		{ if (! start_function ($1, $2, $3, 0))
		    YYERROR;
		  reinit_parse_for_function (); }
	| dummy_decl notype_declarator maybe_raises
		{ if (! start_function (NULL_TREE, $2, $3, 0))
		    YYERROR;
		  reinit_parse_for_function (); }
	| dummy_decl TYPENAME '(' parmlist ')' type_quals maybe_raises
		{ if (! start_function (NULL_TREE, build_parse_node (CALL_EXPR, $2, $4, $6), $7, 0))
		    YYERROR;
		  reinit_parse_for_function (); }
	| dummy_decl TYPENAME LEFT_RIGHT type_quals maybe_raises
		{ if (! start_function (NULL_TREE, build_parse_node (CALL_EXPR, $2, empty_parms (), $4), $5, 0))
		    YYERROR;
		  reinit_parse_for_function (); }
	| PRE_PARSED_FUNCTION_DECL
		{ start_function (NULL_TREE, $1, NULL_TREE, 1);
		  reinit_parse_for_function (); }
	;

/* more C++ complexity */
fn.def2:
	  typed_declspecs '(' parmlist ')' type_quals maybe_raises
		{
		  tree decl = build_parse_node (CALL_EXPR, TREE_VALUE ($1), $3, $5);
		  $$ = start_method (TREE_CHAIN ($1), decl, $6);
		  if (! $$)
		    YYERROR;
		  if (yychar == -1)
		    yychar = yylex();
		  reinit_parse_for_method (yychar, $$); }
	| typed_declspecs LEFT_RIGHT type_quals maybe_raises
		{
		  tree decl = build_parse_node (CALL_EXPR, TREE_VALUE ($1), empty_parms (), $3);
		  $$ = start_method (TREE_CHAIN ($1), decl, $4);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| typed_declspecs declarator maybe_raises
		{ $$ = start_method ($1, $2, $3);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| declmods '(' parmlist ')' type_quals maybe_raises
		{
		  tree decl = build_parse_node (CALL_EXPR, TREE_VALUE ($1), $3, $5);
		  $$ = start_method (TREE_CHAIN ($1), decl, $6);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| declmods LEFT_RIGHT type_quals maybe_raises
		{
		  tree decl = build_parse_node (CALL_EXPR, TREE_VALUE ($1), empty_parms (), $3);
		  $$ = start_method (TREE_CHAIN ($1), decl, $4);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| declmods declarator maybe_raises
		{ $$ = start_method ($1, $2, $3);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	| dummy_decl notype_declarator maybe_raises
		{ $$ = start_method (NULL_TREE, $2, $3);
		  if (! $$)
		    YYERROR;
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  reinit_parse_for_method (yychar, $$); }
	;

return_id: RETURN IDENTIFIER
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  $$ = $2;
		}
	;

return_init: return_id opt.init
		{
		  extern tree value_identifier;
		  tree result;

		  result = DECL_RESULT (current_function_decl);
		  if (DECL_NAME (result) == value_identifier)
		    DECL_NAME (result) = $1;
		  else
		    error ("return identifier `%s' already in place",
			   IDENTIFIER_POINTER (DECL_NAME (result)));
		  store_return_init ($2);
		}
	| return_id '(' nonnull_exprlist ')'
		{
		  extern tree value_identifier;
		  tree result;

		  result = DECL_RESULT (current_function_decl);
		  if (DECL_NAME (result) == value_identifier)
		    DECL_NAME (result) = $1;
		  else
		    error ("return identifier `%s' already in place",
			   IDENTIFIER_POINTER (DECL_NAME (result)));
		  store_return_init ($3);
		}
	| return_id LEFT_RIGHT
		{
		  extern tree value_identifier;
		  tree result;

		  result = DECL_RESULT (current_function_decl);
		  if (DECL_NAME (result) == value_identifier)
		    DECL_NAME (result) = $1;
		  else
		    error ("return identifier `%s' already in place",
			   IDENTIFIER_POINTER (DECL_NAME (result)));
		  store_return_init (NULL_TREE);
		}
	;

base_init:
	  ':' .set_base_init member_init_list
		{
		  if ($3 == 0)
		    error ("no base initializers given following ':'");
		  setup_vtbl_ptr ();
		}
	;

.set_base_init:
	/* empty */
		{
		  int preserve = (current_class_type
				  && TYPE_USES_VIRTUAL_BASECLASSES (current_class_type));
		  preserve = 0;	/* "in charge" arg means we no longer
				   need this hack.  */
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  else if (preserve)
		    preserve_data ();

		  /* Flag that we are processing base and member initializers.  */
		  current_vtable_decl = error_mark_node;

		  if (DECL_CONSTRUCTOR_P (current_function_decl))
		    {
		      /* Make a contour for the initializer list.  */
		      pushlevel (0);
		      clear_last_expr ();
		      expand_start_bindings (0);
		    }
		  else if (current_class_type == NULL_TREE)
		    error ("base initializers not allowed for non-member functions");
		  else if (! DECL_CONSTRUCTOR_P (current_function_decl))
		    error ("only constructors take base initializers");
		}
	;

member_init_list:
	  /* empty */
		{ $$ = 0; }
	| member_init
		{ $$ = 1; }
	| member_init_list ',' member_init
	| member_init_list error
	;



member_init: '(' nonnull_exprlist ')'
		{
		  if (current_class_name && pedantic)
		    warning ("old style base class initialization; use `%s (...)'",
			     IDENTIFIER_POINTER (current_class_name));
		  expand_member_init (C_C_D, NULL_TREE, $2);
		}
	| LEFT_RIGHT
		{
		  if (current_class_name && pedantic)
		    warning ("old style base class initialization; use `%s (...)'",
			     IDENTIFIER_POINTER (current_class_name));
		  expand_member_init (C_C_D, NULL_TREE, void_type_node);
		}
	| identifier '(' nonnull_exprlist ')'
		{
		  expand_member_init (C_C_D, $1, $3);
		}
	| identifier LEFT_RIGHT
		{ expand_member_init (C_C_D, $1, void_type_node); }
	| scoped_identifier identifier '(' nonnull_exprlist ')'
		{
		  tree base, basetype;
		  tree scopes = $1;

		  if (TREE_CODE (scopes) == SCOPE_REF)
		    /* just a pain to do this right now.  */
		    abort ();

		  if (current_class_type == NULL_TREE
		      || ! is_aggr_typedef (scopes, 1))
		    break;
		  basetype = get_base_type (TREE_TYPE (TREE_TYPE (scopes)),
					    current_class_type, 1);
		  if (basetype == error_mark_node)
		    break;
		  if (basetype == 0)
		    {
		      error_not_base_type (TREE_TYPE (TREE_TYPE (scopes)), current_class_type);
		      break;
		    }

		  base = convert_pointer_to (basetype, current_class_decl);
		  expand_member_init (build_indirect_ref (base), $2, $4);
		}
	| scoped_identifier identifier LEFT_RIGHT
		{
		  tree basetype, base;
		  tree scopes = $1;
		  if (TREE_CODE (scopes) == SCOPE_REF)
		    /* just a pain to do this right now.  */
		    abort ();

		  if (current_class_type == NULL_TREE
		      || ! is_aggr_typedef (scopes, 1))
		    break;
		  basetype = get_base_type (TREE_TYPE (TREE_TYPE (scopes)),
					    current_class_type, 1);
		  if (basetype == error_mark_node)
		    break;
		  if (basetype == 0)
		    {
		      error_not_base_type (TREE_TYPE (TREE_TYPE (scopes)), current_class_type);
		      break;
		    }

		  base = convert_pointer_to (basetype, current_class_decl);
		  expand_member_init (build_indirect_ref (base), $2, void_type_node);
		}
	;

identifier:
	  IDENTIFIER
	| TYPENAME
	;

identifier_or_opname:
	  IDENTIFIER
	| TYPENAME
	| '~' identifier
		{ $$ = build_parse_node (BIT_NOT_EXPR, $2); }
	| operator_name
		{ $$ = hack_operator ($1);
		  if ($$ == error_mark_node)
		    $$ = get_identifier ("<missing operator>"); }
	| wrapper IDENTIFIER
		{ $$ = hack_wrapper ($1, NULL_TREE, $2); }
	| wrapper TYPENAME
		{ $$ = hack_wrapper ($1, NULL_TREE, $2); }
	| wrapper operator_name
		{ $$ = hack_wrapper ($1, NULL_TREE, $2); }
	| wrapper scoped_identifier IDENTIFIER
		{ $$ = hack_wrapper ($1, $2, $3); }
	| wrapper scoped_identifier operator_name
		{ $$ = hack_wrapper ($1, $2, $3); }
	;

wrapper:  LEFT_RIGHT
		{ $$ = 0; }
	| '~' LEFT_RIGHT
		{ $$ = 1; }
	| LEFT_RIGHT '?'
		{ $$ = 2; }
	;

unop:     '-'
		{ $$ = NEGATE_EXPR; }
	| '+'
		{ $$ = CONVERT_EXPR; }
	| PLUSPLUS
		{ $$ = PREINCREMENT_EXPR; }
	| MINUSMINUS
		{ $$ = PREDECREMENT_EXPR; }
	| '!'
		{ $$ = TRUTH_NOT_EXPR; }
	;

expr:	  nonnull_exprlist
		{ $$ = build_x_compound_expr ($1); }
	/* Ugly, but faster.  */
	| expr_no_commas
		{
		  if (TREE_CODE ($1) == CALL_EXPR
		      && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE ($1)))
		    $$ = cleanup_after_call ($1);
		}
	;

/* Now obsolete.
exprlist:
	  / * empty * /
		{ $$ = NULL_TREE; }
	| nonnull_exprlist
	;
*/

nonnull_exprlist:
	  expr_no_commas
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| nonnull_exprlist ',' expr_no_commas
		{ chainon ($1, build_tree_list (NULL_TREE, $3)); }
	| nonnull_exprlist ',' error
		{ chainon ($1, build_tree_list (NULL_TREE, error_mark_node)); }
	;

unary_expr:
	  primary %prec UNARY
		{
		  if (TREE_CODE ($1) == TYPE_EXPR)
		    $$ = build_component_type_expr (C_C_D, $1, NULL_TREE, 1);
		  else
		    $$ = $1;
		}
	| '*' cast_expr   %prec UNARY
		{ $$ = build_x_indirect_ref ($2, "unary *"); }
	| '&' cast_expr   %prec UNARY
		{ $$ = build_x_unary_op (ADDR_EXPR, $2); }
	| '~' cast_expr   %prec UNARY
		{ $$ = build_x_unary_op (BIT_NOT_EXPR, $2); }
	| unop cast_expr  %prec UNARY
		{ $$ = build_x_unary_op ($1, $2);
		  if ($1 == NEGATE_EXPR && TREE_CODE ($2) == INTEGER_CST)
		    TREE_NEGATED_INT ($$) = 1;
		}
	| SIZEOF unary_expr  %prec UNARY
		{ if (TREE_CODE ($2) == COMPONENT_REF
		      && TREE_PACKED (TREE_OPERAND ($2, 1)))
		    error ("sizeof applied to a bit-field");
		  /* ANSI says arrays and functions are converted inside comma.
		     But we can't really convert them in build_compound_expr
		     because that would break commas in lvalues.
		     So do the conversion here if operand was a comma.  */
		  if (TREE_CODE ($2) == COMPOUND_EXPR
		      && (TREE_CODE (TREE_TYPE ($2)) == ARRAY_TYPE
			  || TREE_CODE (TREE_TYPE ($2)) == FUNCTION_TYPE))
		    $2 = default_conversion ($2);
		  $$ = c_sizeof (TREE_TYPE ($2)); }
	| SIZEOF '(' typename ')'  %prec HYPERUNARY
		{ $$ = c_sizeof (groktypename ($3)); }
	| ALIGNOF unary_expr  %prec UNARY
		{ if (TREE_CODE ($2) == COMPONENT_REF
		      && TREE_PACKED (TREE_OPERAND ($2, 1)))
		    error ("`__alignof' applied to a bit-field");
		  if (TREE_CODE ($2) == INDIRECT_REF)
		    {
		      tree t = TREE_OPERAND ($2, 0);
		      tree best = t;
		      int bestalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
		      while (TREE_CODE (t) == NOP_EXPR
			     && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == POINTER_TYPE)
			{
			  int thisalign;
			  t = TREE_OPERAND (t, 0);
			  thisalign = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (t)));
			  if (thisalign > bestalign)
			    best = t, bestalign = thisalign;
			}
		      $$ = c_alignof (TREE_TYPE (TREE_TYPE (best)));
		    }
		  else
		    {
		      /* ANSI says arrays and fns are converted inside comma.
			 But we can't convert them in build_compound_expr
			 because that would break commas in lvalues.
			 So do the conversion here if operand was a comma.  */
		      if (TREE_CODE ($2) == COMPOUND_EXPR
			  && (TREE_CODE (TREE_TYPE ($2)) == ARRAY_TYPE
			      || TREE_CODE (TREE_TYPE ($2)) == FUNCTION_TYPE))
			$2 = default_conversion ($2);
		      $$ = c_alignof (TREE_TYPE ($2));
		    }
		}
	| ALIGNOF '(' typename ')'  %prec HYPERUNARY
		{ $$ = c_alignof (groktypename ($3)); }

	| .scope new typename %prec '='
		{ $$ = build_new ($2, $3, NULL_TREE, $1); }
	| .scope new x_typespec '(' nonnull_exprlist ')'
		{ $$ = build_new ($2, $3, $5, $1); }
	| .scope new x_typespec LEFT_RIGHT
		{ $$ = build_new ($2, $3, NULL_TREE, $1); }
	| .scope new typename '=' init %prec '='
		{ $$ = build_new ($2, $3, $5, $1); }
	| .scope new '(' typename ')'
		{ $$ = build_new ($2, $4, NULL_TREE, $1); }
	/* Unswallow a ':' which is probably meant for ?: expression.  */
	| .scope new TYPENAME_COLON
		{ yyungetc (':', 1);
		  $$ = build_new ($2, $3, NULL_TREE, $1); }

	| delete cast_expr  %prec UNARY
		{ tree expr = stabilize_reference (convert_from_reference ($2));
		  tree type = TREE_TYPE (expr);

		  if (integer_zerop (expr))
		    $$ = build1 (NOP_EXPR, void_type_node, expr);
		  else if (TREE_CODE (type) != POINTER_TYPE)
		    {
		      error ("non-pointer type to `delete'");
		      $$ = error_mark_node;
		      break;
		    }
		  if (TYPE_HAS_DESTRUCTOR (TREE_TYPE (type)))
		    $$ = build_delete (type, expr, integer_three_node,
				       LOOKUP_NORMAL|LOOKUP_HAS_IN_CHARGE, $1);
		  else if (! TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (type)))
		    $$ = build_x_delete (type, expr, $1);
		  else
		    $$ = build_delete (type, expr, integer_three_node,
				       LOOKUP_NORMAL|LOOKUP_HAS_IN_CHARGE, 0);
		}
	| delete '[' expr ']' cast_expr  %prec UNARY
		{
		  tree maxindex = build_binary_op (MINUS_EXPR, $3, integer_one_node);
		  tree exp = stabilize_reference (convert_from_reference ($5));
		  tree elt_size = c_sizeof (TREE_TYPE (exp));

		  if (yychar == YYEMPTY)
		    yychar = YYLEX;

		  $$ = build_vec_delete (exp, maxindex, elt_size, NULL_TREE,
					 integer_one_node, integer_two_node);
		}
	;

cast_expr:
	  unary_expr
	| '(' typename ')' expr_no_commas  %prec UNARY
		{ tree type = groktypename ($2);
		  $$ = build_c_cast (type, $4); }
	| '(' typename ')' '{' initlist maybecomma '}'  %prec UNARY
		{ tree type = groktypename ($2);
		  tree init = build_nt (CONSTRUCTOR, NULL_TREE, nreverse ($5));
		  if (pedantic)
		    warning ("ANSI C forbids constructor-expressions");
		  /* Indicate that this was a GNU C constructor expression.  */
		  TREE_HAS_CONSTRUCTOR (init) = 1;
		  $$ = digest_init (type, init, 0);
		  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_SIZE (type) == 0)
		    {
		      int failure = complete_array_type (type, $$, 1);
		      if (failure)
			abort ();
		    }
		}
	;

expr_no_commas:
	  cast_expr
	| expr_no_commas '+' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '-' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '*' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '/' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '%' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas LSHIFT expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas RSHIFT expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas ARITHCOMPARE expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas EQCOMPARE expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas MIN_MAX expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '&' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '|' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas '^' expr_no_commas
		{ $$ = build_x_binary_op ($2, $1, $3); }
	| expr_no_commas ANDAND expr_no_commas
		{ $$ = build_x_binary_op (TRUTH_ANDIF_EXPR, $1, $3); }
	| expr_no_commas OROR expr_no_commas
		{ $$ = build_x_binary_op (TRUTH_ORIF_EXPR, $1, $3); }
	| expr_no_commas '?' xexpr ':' expr_no_commas
		{ $$ = build_x_conditional_expr ($1, $3, $5); }
	| expr_no_commas '=' expr_no_commas
		{ $$ = build_modify_expr ($1, NOP_EXPR, $3); }
	| expr_no_commas ASSIGN expr_no_commas
		{ register tree rval;
		  if (rval = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, $1, $3, $2))
		    $$ = rval;
		  else
		    $$ = build_modify_expr ($1, $2, $3); }

	/* Handle general members.  */
	| object '*' expr_no_commas   %prec UNARY
		{ $$ = build_m_component_ref ($1, build_x_indirect_ref ($3, "unary *")); }
	| object '&' expr_no_commas   %prec UNARY
		{ $$ = build_m_component_ref ($1, build_x_unary_op (ADDR_EXPR, $3)); }
	| object unop expr_no_commas  %prec UNARY
		{ $$ = build_m_component_ref ($1, build_x_unary_op ($2, $3)); }
	| object '(' typename ')' expr_no_commas  %prec UNARY
		{ tree type = groktypename ($3);
		  $$ = build_m_component_ref ($1, build_c_cast (type, $5)); }
	| object primary_no_id  %prec UNARY
		{ $$ = build_m_component_ref ($1, $2); }
	;

primary:
	IDENTIFIER
		{ $$ = do_identifier ($1); }
	| operator_name
		{
		  tree op = hack_operator ($1);
		  if (TREE_CODE (op) != IDENTIFIER_NODE)
		    $$ = op;
		  else
		    {
		      $$ = lookup_name (op);
		      if ($$ == NULL_TREE)
			{
			  error ("operator %s not defined", operator_name_string (op));
			  $$ = error_mark_node;
			}
		    }
		}
	| CONSTANT
	| string
		{ $$ = combine_strings ($1); }
	| '(' expr ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| '('
		{ if (current_function_decl == 0)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  keep_next_level ();
		  $<ttype>$ = expand_start_stmt_expr (); }
	  compstmt ')'
		{ tree rtl_exp;
		  if (pedantic)
		    warning ("ANSI C forbids braced-groups within expressions");
		  rtl_exp = expand_end_stmt_expr ($<ttype>2);
		  $$ = $3;
		  TREE_USED ($$) = 0;
		  /* Since the statements have side effects,
		     consider this volatile.  */
		  TREE_VOLATILE ($$) = 1;
		  TREE_TYPE ($$) = TREE_TYPE (rtl_exp);
		  STMT_BODY ($$) = rtl_exp; }
	| primary '(' nonnull_exprlist ')'
		{ $$ = build_x_function_call ($1, $3, current_class_decl); }
	| primary LEFT_RIGHT
		{ $$ = build_x_function_call ($1, NULL_TREE, current_class_decl); }
	| primary '[' expr ']'
		{
		do_array:
		  {
		    tree type = TREE_TYPE ($1);
		    if (type == error_mark_node || $3 == error_mark_node)
		      $$ = error_mark_node;
		    else if (type == NULL_TREE)
		      {
			/* Something has gone very wrong.  Assume we
			   are mistakenly reducing an expression
			   instead of a declaration.  */
			error ("parser may be lost: is there a '{' missing somewhere?");
			$$ = NULL_TREE;
		      }
		    else
		      {
			if (TREE_CODE (type) == OFFSET_TYPE)
			  type = TREE_TYPE (type);
			if (TREE_CODE (type) == REFERENCE_TYPE)
			  type = TREE_TYPE (type);

			if (TYPE_LANG_SPECIFIC (type) &&
			    TYPE_OVERLOADS_ARRAY_REF (type))
			  $$ = build_opfncall (ARRAY_REF, LOOKUP_NORMAL, $1, $3);
			else if (TREE_CODE (type) == POINTER_TYPE
				 || TREE_CODE (type) == ARRAY_TYPE)
			  $$ = build_array_ref ($1, $3);
			else
			  error("[] applied to non-pointer type");
		      }
		  }
		}
	| object identifier_or_opname  %prec UNARY
		{ $$ = build_component_ref ($1, $2, NULL_TREE, 1); }
	| object scoped_identifier identifier_or_opname %prec UNARY
		{
		  tree basetype = (TREE_CODE ($2) == SCOPE_REF) ? TREE_OPERAND ($2, 1) : $2;
		  if (is_aggr_typedef (basetype, 1))
		    {
		      basetype = TREE_TYPE (TREE_TYPE (basetype));

		      if ($1 == error_mark_node)
			$$ = error_mark_node;
		      else if (basetype_or_else (basetype, TREE_TYPE ($1)))
			$$ = build_component_ref (build_scoped_ref ($1, $2), $3, NULL_TREE, 1);
		      else
			$$ = error_mark_node;
		    }
		  else $$ = error_mark_node;
		}
	| primary PLUSPLUS
		{ $$ = build_x_unary_op (POSTINCREMENT_EXPR, $1); }
	| primary MINUSMINUS
		{ $$ = build_x_unary_op (POSTDECREMENT_EXPR, $1); }

	/* C++ extensions */
	| THIS
		{ if (current_class_decl)
		    {
#ifdef WARNING_ABOUT_CCD
		      TREE_USED (current_class_decl) = 1;
#endif
		      $$ = current_class_decl;
		    }
		  else if (current_function_decl
			   && DECL_STATIC_FUNCTION_P (current_function_decl))
		    {
		      error ("`this' is unavailable for static member functions");
		      $$ = error_mark_node;
		    }
		  else
		    {
		      if (current_function_decl)
			error ("invalid use of `this' in non-member function");
		      else
			error ("invalid use of `this' at top level");
		      $$ = error_mark_node;
		    }
		}
	| dummy_decl TYPE_QUAL '(' nonnull_exprlist ')'
		{
		  tree type;
		  tree id = $2;

		  /* This is a C cast in C++'s `functional' notation.  */
		  if ($4 == error_mark_node)
		    {
		      $$ = error_mark_node;
		      break;
		    }
#if 0
		  if ($4 == NULL_TREE)
		    {
		      error ("cannot cast null list to type `%s'",
		             IDENTIFIER_POINTER (TYPE_NAME ($2)));
		      $$ = error_mark_node;
		      break;
		    }
#endif
		  if (type == error_mark_node)
		    $$ = error_mark_node;
		  else
		    {
		      if (id == ridpointers[(int) RID_CONST])
		        type = build_type_variant (integer_type_node, 1, 0);
		      else if (id == ridpointers[(int) RID_VOLATILE])
		        type = build_type_variant (integer_type_node, 0, 1);
		      else if (id == ridpointers[(int) RID_FRIEND])
		        {
		          error ("cannot cast expression to `friend' type");
		          $$ = error_mark_node;
		          break;
		        }
		      else abort ();
		      $$ = build_c_cast (type, build_compound_expr ($4));
		    }
		}
	| x_typespec '(' nonnull_exprlist ')'
		{ $$ = build_functional_cast ($1, $3); }
	| x_typespec LEFT_RIGHT
		{ $$ = build_functional_cast ($1, NULL_TREE); }
	| SCOPE IDENTIFIER
		{
		do_scoped_identifier:
		  $$ = IDENTIFIER_GLOBAL_VALUE ($2);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  if (! $$)
		    {
		      if (yychar == '(' || yychar == LEFT_RIGHT)
			{
			  $$ = implicitly_declare ($2);
			}
		      else
			{
			  if (IDENTIFIER_GLOBAL_VALUE ($2) != error_mark_node)
			    error ("undeclared variable `%s' (first use here)",
				   IDENTIFIER_POINTER ($2));
			  $$ = error_mark_node;
			  /* Prevent repeated error messages.  */
			  IDENTIFIER_GLOBAL_VALUE ($2) = error_mark_node;
			}
		    }
		  else if (TREE_CODE ($$) == CONST_DECL)
		    $$ = DECL_INITIAL ($$);
		  if (! TREE_USED ($$))
		    {
		      if (TREE_EXTERNAL ($$))
			assemble_external ($$);
		      TREE_USED ($$) = 1;
		    }
		}
	| SCOPE operator_name
		{
		  $2 = hack_operator ($2);
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    goto do_scoped_identifier;
		do_scoped_operator:
		  $$ = $2;
		}
	| scoped_identifier identifier_or_opname  %prec HYPERUNARY
		{ $$ = build_offset_ref ($1, $2); }
	| scoped_identifier identifier_or_opname '(' nonnull_exprlist ')'
		{ $$ = build_member_call ($1, $2, $4); }
	| scoped_identifier identifier_or_opname LEFT_RIGHT
		{ $$ = build_member_call ($1, $2, NULL_TREE); }

	| object identifier_or_opname '(' nonnull_exprlist ')'
		{ $$ = build_method_call ($1, $2, $4, NULL_TREE,
					  (LOOKUP_NORMAL|LOOKUP_AGGR)); }
	| object identifier_or_opname LEFT_RIGHT
		{ $$ = build_method_call ($1, $2, NULL_TREE, NULL_TREE,
					  (LOOKUP_NORMAL|LOOKUP_AGGR)); }
	| object scoped_identifier identifier_or_opname '(' nonnull_exprlist ')'
		{ $$ = build_scoped_method_call ($1, $2, $3, $5); }
	| object scoped_identifier identifier_or_opname LEFT_RIGHT
		{ $$ = build_scoped_method_call ($1, $2, $3, NULL_TREE); }
	;

primary_no_id:
	  '(' expr ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| '('
		{ if (current_function_decl == 0)
		    {
		      error ("braced-group within expression allowed only inside a function");
		      YYERROR;
		    }
		  $<ttype>$ = expand_start_stmt_expr (); }
	  compstmt ')'
		{ if (pedantic)
		    warning ("ANSI C forbids braced-groups within expressions");
		  $$ = expand_end_stmt_expr ($<ttype>2); }
	| primary_no_id '(' nonnull_exprlist ')'
		{ $$ = build_x_function_call ($1, $3, current_class_decl); }
	| primary_no_id LEFT_RIGHT
		{ $$ = build_x_function_call ($1, NULL_TREE, current_class_decl); }
	| primary_no_id '[' expr ']'
		{ goto do_array; }
	| primary_no_id PLUSPLUS
		{ $$ = build_x_unary_op (POSTINCREMENT_EXPR, $1); }
	| primary_no_id MINUSMINUS
		{ $$ = build_x_unary_op (POSTDECREMENT_EXPR, $1); }
	| SCOPE IDENTIFIER
		{ goto do_scoped_identifier; }
	| SCOPE operator_name
		{ $2 = hack_operator ($2);
		  if (TREE_CODE ($2) == IDENTIFIER_NODE)
		    goto do_scoped_identifier;
		  goto do_scoped_operator;
		}
	;

new:	  NEW
		{ $$ = NULL_TREE; }
	| NEW '{' nonnull_exprlist '}'
		{ $$ = $3; }
	| NEW DYNAMIC  %prec EMPTY
		{ $$ = void_type_node; }
	| NEW DYNAMIC '(' string ')'
		{ $$ = combine_strings ($4); }
	;

.scope:
	/* empty  */
		{ $$ = 0; }
	| SCOPE
		{ $$ = 1; }
	;

delete:	  DELETE
		{ $$ = NULL_TREE; }
	| SCOPE delete
		{ if ($2)
		    error ("extra `::' before `delete' ignored");
		  $$ = error_mark_node;
		}
	;

/* Produces a STRING_CST with perhaps more STRING_CSTs chained onto it.  */
string:
	  STRING
	| string STRING
		{ $$ = chainon ($1, $2); }
	;

nodecls:
	  /* empty */
		{
		  if (! current_function_parms_stored)
		    store_parm_decls ();
		  setup_vtbl_ptr ();
		}
	;

object:	  primary '.'
		{
		  if ($1 == error_mark_node)
		    $$ = error_mark_node;
		  else
		    {
		      tree type = TREE_TYPE ($1);

		      if (IS_AGGR_TYPE (type)
			  || (TREE_CODE (type) == REFERENCE_TYPE
			      && IS_AGGR_TYPE (TREE_TYPE (type))))
			$$ = $1;
		      else
			{
			  error ("object in '.' expression is not of aggregate type");
			  $$ = error_mark_node;
			}
		    }
		}
	| primary POINTSAT
		{
		  $$ = build_x_arrow ($1);
		}
	;

decl:
	  typed_declspecs initdecls ';'
		{
		  resume_momentary ($2);
		  note_got_semicolon ($1);
		}
	/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
		{ tree d;
		  int yes = suspend_momentary ();
		  d = start_decl ($2, $1, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE);
		  resume_momentary (yes);
		  note_got_semicolon ($1);
		}
	| declmods notype_initdecls ';'
		{ resume_momentary ($2); }
	/* Normal case: make this fast.  */
	| declmods declarator ';'
		{ tree d;
		  int yes = suspend_momentary ();
		  d = start_decl ($2, $1, 0, NULL_TREE);
		  finish_decl (d, NULL_TREE, NULL_TREE);
		  resume_momentary (yes);
		}
	| typed_declspecs ';'
		{
		  shadow_tag ($1);
		  note_got_semicolon ($1);
		}
	| declmods ';'
		{ warning ("empty declaration"); }
	;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator
	| notype_declarator
	;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.  */

typed_declspecs:
	  x_typespec
		{ $$ = list_hash_lookup_or_cons ($1); }
	| declmods typespec
		{ $$ = hash_tree_chain ($2, $1); }
	| x_typespec reserved_declspecs
		{ $$ = hash_tree_chain ($1, $2); }
	| declmods typespec reserved_declspecs
		{ $$ = hash_tree_chain ($2, hash_chainon ($3, $1)); }
	;

reserved_declspecs:  /* empty
		{ $$ = NULL_TREE; } */
	  typespecqual_reserved
		{ $$ = build_decl_list (NULL_TREE, $1); }
	| SCSPEC
		{ $$ = build_decl_list (NULL_TREE, $1); }
	| reserved_declspecs typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	| reserved_declspecs SCSPEC
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.  */

declmods:
	  dummy_decl TYPE_QUAL
		{ $$ = IDENTIFIER_AS_LIST ($2); }
	| dummy_decl SCSPEC
		{ $$ = IDENTIFIER_AS_LIST ($2); }
	| declmods TYPE_QUAL
		{ $$ = hash_tree_chain ($2, $1); }
	| declmods SCSPEC
		{ $$ = hash_tree_chain ($2, $1); }
	;


/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  x_typespec  %prec EMPTY
		{ $$ = build_decl_list_1 ($1); }
	| nonempty_type_quals typespec
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	| x_typespec reserved_typespecquals
		{ $$ = decl_tree_cons (NULL_TREE, $1, $2); }
	| nonempty_type_quals typespec reserved_typespecquals
		{ $$ = decl_tree_cons (NULL_TREE, $2, hash_chainon ($3, $1)); }
	;

reserved_typespecquals:
	  typespecqual_reserved
		{ $$ = build_decl_list_1 ($1); }
	| reserved_typespecquals typespecqual_reserved
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec: TYPESPEC
	| structsp
	| TYPENAME
	| TYPEOF '(' expr ')'
		{ $$ = TREE_TYPE ($3);
		  if (pedantic)
		    warning ("ANSI C forbids `typeof'"); }
	| TYPEOF '(' typename ')'
		{ $$ = groktypename ($3);
		  if (pedantic)
		    warning ("ANSI C forbids `typeof'"); }
	;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved: TYPESPEC
	| TYPE_QUAL
	| structsp
	;

x_typespec:
	  dummy_decl TYPESPEC
		{ $$ = $2; }
	| dummy_decl structsp
		{ $$ = $2; }
	| dummy_decl TYPENAME
		{ $$ = $2; }
	| dummy_decl TYPEOF '(' expr ')'
		{ $$ = TREE_TYPE ($4);
		  if (pedantic)
		    warning ("ANSI C forbids `typeof'"); }
	| dummy_decl TYPEOF '(' typename ')'
		{ $$ = groktypename ($4);
		  if (pedantic)
		    warning ("ANSI C forbids `typeof'"); }
	;

initdecls:
	  initdcl0
	| initdecls ',' initdcl
	;

notype_initdecls:
	  notype_initdcl0
	| notype_initdecls ',' initdcl
	;

maybeasm:
	  /* empty */
		{ $$ = NULL_TREE; }
	| ASM '(' string ')'
		{ if (TREE_CHAIN ($3)) $3 = combine_strings ($3);
		  $$ = $3;
		  if (pedantic)
		    warning ("ANSI C forbids use of `asm' keyword");
		}
	;

initdcl0:
	  declarator maybe_raises maybeasm maybe_attribute '='
		{ current_declspecs = $<ttype>0;
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($1, current_declspecs, 1, $2); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3);
		  $$ = $<itype>5; }
	| declarator maybe_raises maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = $<ttype>0;
		  $$ = suspend_momentary ();
		  d = start_decl ($1, current_declspecs, 0, $2);
		  finish_decl (d, NULL_TREE, $3); }
	;

initdcl:
	  declarator maybe_raises maybeasm maybe_attribute '='
		{ $<ttype>$ = start_decl ($1, current_declspecs, 1, $2); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3); }
	| declarator maybe_raises maybeasm maybe_attribute
		{ tree d = start_decl ($1, current_declspecs, 0, $2);
		  finish_decl (d, NULL_TREE, $3); }
	;

notype_initdcl0:
	  notype_declarator maybe_raises maybeasm maybe_attribute '='
		{ current_declspecs = $<ttype>0;
		  $<itype>5 = suspend_momentary ();
		  $<ttype>$ = start_decl ($1, current_declspecs, 1, $2); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl ($<ttype>6, $7, $3);
		  $$ = $<itype>5; }
	| notype_declarator maybe_raises maybeasm maybe_attribute
		{ tree d;
		  current_declspecs = $<ttype>0;
		  $$ = suspend_momentary ();
		  d = start_decl ($1, current_declspecs, 0, $2);
		  finish_decl (d, NULL_TREE, $3); }
	;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile. */
maybe_attribute:
    /* empty */
	{ $$ = NULL_TREE; }
    | ATTRIBUTE '(' '(' attribute_list ')' ')'
        { $$ = $4; }
    ;

attribute_list
    : attrib
    | attribute_list ',' attrib
    ;

attrib
    : IDENTIFIER
	{ warning ("`%s' attribute directive ignored",
		   IDENTIFIER_POINTER ($1));
	  $$ = $1; }
    | IDENTIFIER '(' CONSTANT ')'
	{ /* if not "aligned(1)", then issue warning */
	  if (strcmp (IDENTIFIER_POINTER ($1), "aligned") != 0
	      || TREE_CODE ($3) != INTEGER_CST
	      || TREE_INT_CST_LOW ($3) != 1)
	    warning ("`%s' attribute directive ignored",
		     IDENTIFIER_POINTER ($1));
	  $$ = $1; }
    | IDENTIFIER '(' identifiers ')'
	{ warning ("`%s' attribute directive ignored",
		   IDENTIFIER_POINTER ($1));
	  $$ = $1; }
    ;

identifiers:
	  IDENTIFIER
		{ }
	| identifiers ',' IDENTIFIER
		{ }
	;

init:
	  expr_no_commas %prec '='
	| '{' '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, NULL_TREE);
		  TREE_HAS_CONSTRUCTOR ($$) = 1;
		  if (pedantic)
		    warning ("ANSI C forbids empty initializer braces"); }
	| '{' initlist '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, nreverse ($2));
		  TREE_HAS_CONSTRUCTOR ($$) = 1; }
	| '{' initlist ',' '}'
		{ $$ = build_nt (CONSTRUCTOR, NULL_TREE, nreverse ($2));
		  TREE_HAS_CONSTRUCTOR ($$) = 1; }
	| error
		{ $$ = NULL_TREE; }
	;

/* This chain is built in reverse order,
   and put in forward order where initlist is used.  */
initlist:
	  init
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| initlist ',' init
		{ $$ = tree_cons (NULL_TREE, $3, $1); }
	;

structsp:
	  ENUM identifier '{'
		{ $<itype>3 = suspend_momentary ();
		  $$ = start_enum ($2); }
	  enumlist maybecomma_warn '}'
		{ $$ = finish_enum ($<ttype>4, $5);
		  resume_momentary ($<itype>3);
		  check_for_missing_semicolon ($<ttype>4); }
	| ENUM identifier '{' '}'
		{ $$ = finish_enum (start_enum ($2), NULL_TREE);
		  check_for_missing_semicolon ($$); }
	| ENUM '{'
		{ $<itype>2 = suspend_momentary ();
		  $$ = start_enum (make_anon_name ()); }
	  enumlist maybecomma_warn '}'
		{ $$ = finish_enum ($<ttype>3, $4);
		  resume_momentary ($<itype>1);
		  check_for_missing_semicolon ($<ttype>3); }
	| ENUM '{' '}'
		{ $$ = finish_enum (start_enum (make_anon_name()), NULL_TREE);
		  check_for_missing_semicolon ($$); }
	| ENUM identifier
		{ $$ = xref_tag (enum_type_node, $2, NULL_TREE); }

	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head LC opt.component_decl_list '}'
		{
		  if (TREE_CODE ($1) == ENUMERAL_TYPE)
		    $$ = $1;
		  else if (CLASSTYPE_DECLARED_EXCEPTION ($1))
		    $$ = finish_exception ($1, $3);
		  else
		    $$ = finish_struct ($1, $3, 0, 0);
		    
		  if ($2 & 1)
		    resume_temporary_allocation ();
		  if ($2 & 2)
		    resume_momentary (1);
		  check_for_missing_semicolon ($$);
		}
	| class_head LC opt.component_decl_list '}' ';'
		{ if (TREE_CODE ($1) == ENUMERAL_TYPE)
		    $$ = $1;
		  else if (CLASSTYPE_DECLARED_EXCEPTION ($1))
		    warning ("empty exception declaration\n");
		  else
		    $$ = finish_struct ($1, $3, 1, 1);
		  if ($2 & 1)
		    resume_temporary_allocation ();
		  if ($2 & 2)
		    resume_momentary (1);
		  note_got_semicolon ($$);
		  yyungetc (';', 0); }
	| class_head  %prec EMPTY
		{ $$ = $1; }
	;

maybecomma:
	  /* empty */
	| ','
	;

maybecomma_warn:
	  /* empty */
	| ','
		{ if (pedantic) warning ("comma at end of enumerator list"); }
	;

aggr:	  AGGR
		{ $$ = $1; }
	| DYNAMIC AGGR
		{ $$ = build_tree_list (NULL_TREE, $2); }
	| DYNAMIC '(' string ')' AGGR
		{ $$ = build_tree_list ($3, $5); }
	| aggr SCSPEC
		{ error ("storage class specifier `%s' not allow after struct or class", IDENTIFIER_POINTER ($2));
		}
	| aggr TYPESPEC
		{ error ("type specifier `%s' not allow after struct or class", IDENTIFIER_POINTER ($2));
		}
	| aggr TYPE_QUAL
		{ error ("type qualifier `%s' not allow after struct or class", IDENTIFIER_POINTER ($2));
		}
	| aggr AGGR
		{ error ("no body nor ';' separates two class, struct or union declarations");
		}

class_head:
	  aggr  %prec EMPTY
		{ $$ = xref_tag ($1, make_anon_name (), NULL_TREE); }
	| aggr identifier  %prec EMPTY
		{ $$ = xref_tag ($1, $2, NULL_TREE); }
	| aggr IDENTIFIER ':' base_class_list  %prec EMPTY
		{ $$ = xref_tag ($1, $2, $4); }
	| aggr TYPENAME_COLON  %prec EMPTY
		{ yyungetc (':', 1);
		  $$ = xref_tag ($1, $2, NULL_TREE); }
	| aggr TYPENAME_COLON base_class_list  %prec EMPTY
		{ $$ = xref_tag ($1, $2, $3); }
	;

base_class_list:
	  identifier
		{ if (! is_aggr_typedef ($1, 1))
		    $$ = NULL_TREE;
		  else $$ = build_tree_list ((tree)visibility_default, $1); }
	| base_class_visibility_list identifier
		{ if (! is_aggr_typedef ($2, 1))
		    $$ = NULL_TREE;
		  else $$ = build_tree_list ($1, $2); }
	| base_class_list ',' identifier
		{ if (! is_aggr_typedef ($3, 1))
		    $$ = NULL_TREE;
		  else $$ = chainon ($1, build_tree_list ((tree)visibility_default, $3)); }
	| base_class_list ',' base_class_visibility_list identifier
		{ if (! is_aggr_typedef ($4, 1))
		    $$ = NULL_TREE;
		  else $$ = chainon ($1, build_tree_list ($3, $4)); }
	;

base_class_visibility_list:
	  PUBLIC
		{ $$ = (tree)visibility_public; }
	| PRIVATE
		{ $$ = (tree)visibility_private; }
	| SCSPEC
		{ if ($1 != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual visibility");
		  $$ = (tree)visibility_default_virtual; }
	| base_class_visibility_list PUBLIC
		{ if ($1 == (tree)visibility_private)
		    error ("base class cannot be public and private");
		  else if ($1 == (tree)visibility_default_virtual)
		    $$ = (tree)visibility_public_virtual; }
	| base_class_visibility_list PRIVATE
		{ if ($1 == (tree)visibility_public)
		    error ("base class cannot be private and public");
		  else if ($1 == (tree)visibility_default_virtual)
		    $$ = (tree)visibility_private_virtual; }
	| base_class_visibility_list SCSPEC
		{ if ($2 != ridpointers[(int)RID_VIRTUAL])
		    sorry ("non-virtual visibility");
		  if ($1 == (tree)visibility_public)
		    $$ = (tree)visibility_public_virtual;
		  else if ($1 == (tree)visibility_private)
		    $$ = (tree)visibility_private_virtual; }
	;

LC: '{'
		{ int temp = allocation_temporary_p ();
		  int momentary = suspend_momentary ();
		  if (temp)
		    end_temporary_allocation ();
		  $$ = (momentary << 1) | temp;
		  if (! IS_AGGR_TYPE ($<ttype>0))
		    {
		      $<ttype>0 = make_lang_type (RECORD_TYPE);
		      TYPE_NAME ($<ttype>0) = get_identifier ("erroneous type");
		    }
		  pushclass ($<ttype>0, 0); }

opt.component_decl_list:
	/* empty */
		{ $$ = NULL_TREE; }
	| component_decl_list
		{ $$ = build_tree_list ((tree)visibility_default, $1); }
	| opt.component_decl_list PUBLIC ':' component_decl_list
		{ $$ = chainon ($1, build_tree_list ((tree)visibility_public, $4)); }
	| opt.component_decl_list PRIVATE ':' component_decl_list
		{ $$ = chainon ($1, build_tree_list ((tree)visibility_private, $4)); }
	| opt.component_decl_list PROTECTED ':' component_decl_list
		{ $$ = chainon ($1, build_tree_list ((tree)visibility_protected, $4)); }
	| opt.component_decl_list PUBLIC ':'
	| opt.component_decl_list PRIVATE ':'
	| opt.component_decl_list PROTECTED ':'
	;

component_decl_list:
	  component_decl
		{ if ($1 == void_type_node) $$ = NULL_TREE; }
	| component_decl_list component_decl
		{ if ($2 != NULL_TREE && $2 != void_type_node)
		    $$ = chainon ($1, $2); }
	| component_decl_list ';'
		{ if (pedantic)
		    warning ("extra semicolon in struct or union specified"); }
	;

component_decl:
	  typed_declspecs components ';'
		{
		do_components:
		  if ($2 == void_type_node)
		    /* We just got some friends.
		       They have been recorded elsewhere.  */
		    $$ = NULL_TREE;
		  else if ($2 == NULL_TREE)
		    {
		      tree t = groktypename (build_decl_list ($1, NULL_TREE));
		      if (t == NULL_TREE)
			{
			  error ("error in component specification");
			  $$ = NULL_TREE;
			}
		      else if (TREE_CODE (t) == UNION_TYPE)
			{
			  /* handle anonymous unions */
			  if (CLASSTYPE_METHOD_VEC (t))
			    sorry ("methods in anonymous unions");
			  $$ = build_lang_field_decl (FIELD_DECL, NULL_TREE, t);
			  DECL_ANON_UNION_ELEM ($$) = 1;
			}
		      else if (TREE_CODE (t) == ENUMERAL_TYPE)
			$$ = grok_enum_decls (t, NULL_TREE);
		      else if (TREE_CODE (t) == RECORD_TYPE)
			{
			  if (TYPE_LANG_SPECIFIC (t)
			      && CLASSTYPE_DECLARED_EXCEPTION (t))
			    shadow_tag ($1);
			  $$ = NULL_TREE;
			}
		      else if (t != void_type_node)
			{
			  error ("empty component declaration");
			  $$ = NULL_TREE;
			}
		      else $$ = NULL_TREE;
		    }
		  else
		    {
		      tree t = TREE_TYPE ($2);
		      if (TREE_CODE (t) == ENUMERAL_TYPE && TREE_NONLOCAL (t))
			$$ = grok_enum_decls (t, $2);
		      else
			$$ = $2;
		    }
		  end_exception_decls ();
		}
	| typed_declspecs '(' parmlist ')' ';'
		{ $$ = groktypefield ($1, $3); }
	| typed_declspecs '(' parmlist ')' '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = groktypefield ($1, $3); }
	| typed_declspecs LEFT_RIGHT ';'
		{ $$ = groktypefield ($1, empty_parms ()); }
	| typed_declspecs LEFT_RIGHT '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = groktypefield ($1, empty_parms ()); }
	| declmods components ';'
		{ goto do_components; }
	/* Normal case: make this fast.  */
	| declmods declarator ';'
		{ $$ = grokfield ($2, $1, 0, 0, 0, 0); }
	| declmods components '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  goto do_components; }
	| declmods '(' parmlist ')' ';'
		{ $$ = groktypefield ($1, $3); }
	| declmods '(' parmlist ')' '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = groktypefield ($1, $3); }
	| declmods LEFT_RIGHT ';'
		{ $$ = groktypefield ($1, empty_parms ()); }
	| declmods LEFT_RIGHT '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = groktypefield ($1, empty_parms ()); }
	| ':' expr_no_commas ';'
		{ $$ = grokbitfield (NULL_TREE, NULL_TREE, $2); }
	| ':' expr_no_commas '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = grokbitfield (NULL_TREE, NULL_TREE, $2); }
	| error
		{ $$ = NULL_TREE; }

	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
		{ $$ = finish_method ($1); }
	| fn.def2 '{' /* nodecls compstmt */
		{ $$ = finish_method ($1); }
	| dummy_decl notype_declarator maybe_raises ';'
		{ $$ = grokfield ($2, NULL_TREE, $3, NULL_TREE, NULL_TREE); }
	| dummy_decl notype_declarator maybe_raises '}'
		{ error ("missing ';' before right brace");
		  yyungetc ('}', 0);
		  $$ = grokfield ($2, NULL_TREE, $3, NULL_TREE, NULL_TREE); }
	;

components:
	  /* empty: possibly anonymous */
		{ $$ = NULL_TREE; }
	| component_declarator0
	| components ',' component_declarator
		{
		  /* In this context, void_type_node encodes
		     friends.  They have been recorded elsewhere.  */
		  if ($1 == void_type_node)
		    $$ = $3;
		  else
		    $$ = chainon ($1, $3);
		}
	;

component_declarator0:
	  declarator maybe_raises maybeasm opt.init
		{ current_declspecs = $<ttype>0;
		  $$ = grokfield ($1, current_declspecs, $2, $4, $3); }
	| IDENTIFIER ':' expr_no_commas
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield ($1, current_declspecs, $3); }
	| TYPENAME_COLON expr_no_commas
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield ($1, current_declspecs, $2); }
	| ':' expr_no_commas
		{ current_declspecs = $<ttype>0;
		  $$ = grokbitfield (NULL_TREE, NULL_TREE, $2); }
	;

component_declarator:
	  declarator maybe_raises maybeasm opt.init
		{ $$ = grokfield ($1, current_declspecs, $2, $4, $3); }
	| IDENTIFIER ':' expr_no_commas
		{ $$ = grokbitfield ($1, current_declspecs, $3); }
	| TYPENAME_COLON expr_no_commas
		{ $$ = grokbitfield ($1, current_declspecs, $2); }
	| ':' expr_no_commas
		{ $$ = grokbitfield (NULL_TREE, NULL_TREE, $2); }
	;

/* We chain the enumerators in reverse order.
   Because of the way enums are built, the order is
   insignificant.  Take advantage of this fact.  */

enumlist:
	  enumerator
	| enumlist ',' enumerator
		{ TREE_CHAIN ($3) = $1; $$ = $3; }
	;

enumerator:
	  identifier
		{ $$ = build_enumerator ($1, NULL_TREE); }
	| identifier '=' expr_no_commas
		{ $$ = build_enumerator ($1, $3); }
	;

typename:
	  typed_typespecs absdcl
		{ $$ = build_decl_list ($1, $2); }
	| nonempty_type_quals absdcl
		{ $$ = build_decl_list ($1, $2); }
	;

absdcl:   /* an abstract declarator */
	/* empty */ %prec EMPTY
		{ $$ = NULL_TREE; }
	| absdcl1  %prec EMPTY
	;

nonempty_type_quals:
	  dummy_decl TYPE_QUAL
		{ $$ = IDENTIFIER_AS_LIST ($2); }
	| nonempty_type_quals TYPE_QUAL
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	;

type_quals:
	  /* empty */
		{ $$ = NULL_TREE; }
	| type_quals TYPE_QUAL
		{ $$ = decl_tree_cons (NULL_TREE, $2, $1); }
	;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are prefered.  */

/* An expression which will not live on the momentary obstack.  */
nonmomentary_expr:
	{ $<itype>$ = suspend_momentary (); } expr
	{ resume_momentary ($<itype>1); $$ = $2; }

/* A declarator that is allowed only after an explicit typespec.  */
/* may all be followed by prec '.' */
after_type_declarator:
	  after_type_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| after_type_declarator '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| after_type_declarator LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, empty_parms (), $3); }
	| after_type_declarator '(' error ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, NULL_TREE, NULL_TREE); }
	| after_type_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, $3); }
	| after_type_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, NULL_TREE); }
	| '(' dummy_decl after_type_declarator_no_typename ')'
		{ $$ = $3; }
	| '(' '*' type_quals after_type_declarator ')'
		{ $$ = make_pointer_declarator ($3, $4); }
	| PAREN_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_REF_PAREN
		{ $$ = $1;
		  see_typename (); }
	| '(' '&' type_quals after_type_declarator ')'
		{ $$ = make_reference_declarator ($3, $4); }
	| '*' type_quals after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' type_quals after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	| TYPENAME
	;

after_type_declarator_no_typename:
	  after_type_declarator_no_typename '(' nonnull_exprlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| after_type_declarator_no_typename '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| after_type_declarator_no_typename LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, empty_parms (), $3); }
	| after_type_declarator_no_typename '(' error ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, NULL_TREE, NULL_TREE); }
	| after_type_declarator_no_typename '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, $3); }
	| after_type_declarator_no_typename '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, NULL_TREE); }
	| '(' dummy_decl after_type_declarator_no_typename ')'
		{ $$ = $3; }
	| PAREN_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_REF_PAREN
		{ $$ = $1;
		  see_typename (); }
	| '*' type_quals after_type_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' type_quals after_type_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  notype_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| notype_declarator '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| notype_declarator LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, empty_parms (), $3); }
	| notype_declarator '(' error ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, NULL_TREE, NULL_TREE); }
	| '(' notype_declarator ')'
		{ $$ = $2; }
	| '*' type_quals notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '&' type_quals notype_declarator  %prec UNARY
		{ $$ = make_reference_declarator ($2, $3); }
	| notype_declarator '[' nonmomentary_expr ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, $3); }
	| notype_declarator '[' ']'
		{ $$ = build_parse_node (ARRAY_REF, $1, NULL_TREE); }
	| IDENTIFIER
		{ see_typename (); }

	/* C++ extensions.  */
	| operator_name
		{ see_typename (); }

	| '~' TYPENAME
		{ see_typename ();
		  $$ = build_parse_node (BIT_NOT_EXPR, $2); }
	| '~' IDENTIFIER
		{ see_typename ();
		  $$ = build_parse_node (BIT_NOT_EXPR, $2); }
	| LEFT_RIGHT identifier
		{
		  see_typename ();
		  $$ = build_parse_node (WRAPPER_EXPR, $2);
		}
	| LEFT_RIGHT '?' identifier
		{
		  see_typename ();
		  $$ = build_parse_node (WRAPPER_EXPR,
				 build_parse_node (COND_EXPR, $3, NULL_TREE, NULL_TREE));
		}
	| '~' LEFT_RIGHT identifier
		{ see_typename ();
		  $$ = build_parse_node (ANTI_WRAPPER_EXPR, $3); }
	| TYPENAME_SCOPE type_quals notype_declarator  %prec '('
		{ see_typename ();
		  $$ = build_parse_node (SCOPE_REF, $1, $3); }
	| TYPENAME_SCOPE TYPENAME  %prec '('
		{ $$ = build_parse_node (SCOPE_REF, $1, $2); }
	| TYPENAME_SCOPE see_typename TYPENAME '(' nonnull_exprlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (SCOPE_REF, $1,
				 build_parse_node (CALL_EXPR, $3, $5, $7)); }
	| TYPENAME_SCOPE see_typename TYPENAME '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (SCOPE_REF, $1,
				 build_parse_node (CALL_EXPR, $3, $5, $7)); }
	| TYPENAME_SCOPE see_typename TYPENAME LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (SCOPE_REF, $1,
				 build_parse_node (CALL_EXPR, $3, empty_parms (), $5)); }
	| TYPENAME_SCOPE see_typename TYPENAME '(' error ')' type_quals  %prec '.'
		{ $$ = build_parse_node (SCOPE_REF, $1, build_parse_node (CALL_EXPR, $3, NULL_TREE, NULL_TREE)); }
	| SCOPE see_typename notype_declarator
		{ $$ = build_parse_node (SCOPE_REF, NULL_TREE, $3); }
	;

scoped_identifier:
	  TYPENAME_SCOPE
	| IDENTIFIER SCOPE
	| scoped_identifier TYPENAME_SCOPE
		{ $$ = build_parse_node (SCOPE_REF, $1, $2); }
	;

absdcl1:  /* a nonempty abstract declarator */
	  '(' absdcl1 ')'
		{ see_typename ();
		  $$ = $2; }
	  /* `(typedef)1' is `int'.  */
	| '*' type_quals absdcl1  %prec EMPTY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '*' type_quals  %prec EMPTY
		{ $$ = make_pointer_declarator ($2, NULL_TREE); }
	| PAREN_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_STAR_PAREN
		{ $$ = $1;
		  see_typename (); }
	| PAREN_X_SCOPE_REF_PAREN
		{ $$ = $1;
		  see_typename (); }
	| '&' type_quals absdcl1 %prec EMPTY
		{ $$ = make_reference_declarator ($2, $3); }
	| '&' type_quals %prec EMPTY
		{ $$ = make_reference_declarator ($2, NULL_TREE); }
	| absdcl1 '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, $3, $5); }
	| absdcl1 LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, $1, empty_parms (), $3); }
	| absdcl1 '[' nonmomentary_expr ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $1, $3); }
	| absdcl1 '[' ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, $1, NULL_TREE); }
	| '(' parmlist ')' type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, NULL_TREE, $2, $4); }
	| LEFT_RIGHT type_quals  %prec '.'
		{ $$ = build_parse_node (CALL_EXPR, NULL_TREE, empty_parms (), $2); }
	| '[' nonmomentary_expr ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, NULL_TREE, $2); }
	| '[' ']'  %prec '.'
		{ $$ = build_parse_node (ARRAY_REF, NULL_TREE, NULL_TREE); }
	| TYPENAME_SCOPE type_quals absdcl1  %prec EMPTY
		{ $$ = build_parse_node (SCOPE_REF, $1, $3); }
	| IDENTIFIER SCOPE type_quals absdcl1  %prec EMPTY
		{ $$ = build_parse_node (SCOPE_REF, $1, $4); }
	| TYPENAME_SCOPE type_quals %prec EMPTY
		{ $$ = build_parse_node (SCOPE_REF, $1, 0); }
	| IDENTIFIER SCOPE type_quals %prec EMPTY
		{ $$ = build_parse_node (SCOPE_REF, $1, 0); }
	;

/* at least one statement, the first of which parses without error.  */
/* stmts is used only after decls, so an invalid first statement
   is actually regarded as an invalid decl and part of the decls.  */

stmts:
	  stmt
	| stmts stmt
	| stmts errstmt
	;

errstmt:  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

.pushlevel:  /* empty */
		{
		  pushlevel (0);
		  clear_last_expr ();
		  push_momentary ();
		  expand_start_bindings (0);
		  stmt_decl_msg = 0;
		}
	;

/* This is the body of a function definition.
   It causes syntax errors to ignore to the next openbrace.  */
compstmt_or_error:
	  compstmt
		{}
	| error compstmt
	;

compstmt: '{' '}'
		{ $$ = 0; }
	| '{' .pushlevel stmts '}'
		{ tree decls;

		  pop_implicit_try_blocks (NULL_TREE);
		  decls = getdecls ();
		  expand_end_bindings (decls, decls != 0, 1);
		  $$ = poplevel (decls != 0, 1, 0);
		  pop_momentary (); }
	| '{' .pushlevel error '}'
		{ pop_implicit_try_blocks (NULL_TREE);
		  expand_end_bindings (getdecls (), 0, 1);
		  $$ = poplevel (0, 0, 0);
		  pop_momentary (); }
	;

simple_if:
	  IF '(' expr ')'
		{ emit_line_note (input_filename, lineno);
		  expand_start_cond (truthvalue_conversion ($3), 0);
		  stmt_decl_msg = "if"; }
	  stmt
		{ stmt_decl_msg = 0; }
	;

stmt:
	  compstmt
		{ finish_stmt (); }
	| decl
		{ if (stmt_decl_msg)
		    error ("declaration after %s invalid", stmt_decl_msg);
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| expr ';'
		{ emit_line_note (input_filename, lineno);
		  /* Do default conversion if safe and possibly important,
		     in case within ({...}).  */
		  if ((TREE_CODE (TREE_TYPE ($1)) == ARRAY_TYPE
		       && lvalue_p ($1))
		      || TREE_CODE (TREE_TYPE ($1)) == FUNCTION_TYPE)
		    $1 = default_conversion ($1);
		  cplus_expand_expr_stmt ($1);
		  clear_momentary ();
		  finish_stmt (); }
	| simple_if ELSE
		{ expand_start_else ();
		  stmt_decl_msg = "else"; }
	  stmt
		{ expand_end_else ();
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| simple_if %prec IF
		{ expand_end_cond ();
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| WHILE
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop (1); }
	  '(' expr ')'
		{ expand_exit_loop_if_false (truthvalue_conversion ($4));
		  stmt_decl_msg = "while"; }
	  stmt
		{ 
		  expand_end_loop ();
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| DO
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop_continue_elsewhere (1);
		  stmt_decl_msg = "do"; }
	  stmt WHILE
		{ stmt_decl_msg = 0;
		  expand_loop_continue_here (); }
	  '(' expr ')' ';'
		{ emit_line_note (input_filename, lineno);
		  expand_exit_loop_if_false (truthvalue_conversion ($7));
		  expand_end_loop ();
		  clear_momentary ();
		  finish_stmt (); }
	| forhead.1
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  if ($1) cplus_expand_expr_stmt ($1);
		  expand_start_loop_continue_elsewhere (1); }
	  xexpr ';'
		{ emit_line_note (input_filename, lineno);
		  if ($3) expand_exit_loop_if_false (truthvalue_conversion ($3)); }
	  xexpr ')'
		/* Don't let the tree nodes for $6 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
		{ push_momentary ();
		  stmt_decl_msg = "for"; }
	  stmt
		{ emit_line_note (input_filename, lineno);
		  expand_loop_continue_here ();
		  if ($6) cplus_expand_expr_stmt ($6);
		  pop_momentary ();
		  expand_end_loop ();
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| forhead.2
		{ emit_nop ();
		  emit_line_note (input_filename, lineno);
		  expand_start_loop_continue_elsewhere (1); }
	  xexpr ';'
		{ emit_line_note (input_filename, lineno);
		  if ($3) expand_exit_loop_if_false (truthvalue_conversion ($3)); }
	  xexpr ')'
		/* Don't let the tree nodes for $6 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
		{ push_momentary ();
		  stmt_decl_msg = "for";
		  $<itype>7 = lineno; }
	  stmt
		{ emit_line_note (input_filename, $<itype>7);
		  expand_loop_continue_here ();
		  if ($6) cplus_expand_expr_stmt ($6);
		  pop_momentary ();
		  expand_end_loop ();
		  pop_implicit_try_blocks (NULL_TREE);
		  if ($1)
		    {
		      register keep = $1 > 0;
		      if (keep) expand_end_bindings (0, keep, 1);
		      poplevel (keep, 1, 0);
		      pop_momentary ();
		    }
		  stmt_decl_msg = 0;
		  finish_stmt ();
		}
	| SWITCH '(' expr ')'
		{ emit_line_note (input_filename, lineno);
		  c_expand_start_case ($3);
		  /* Don't let the tree nodes for $3 be discarded by
		     clear_momentary during the parsing of the next stmt.  */
		  push_momentary ();
		  stmt_decl_msg = "switch"; }
	  stmt
		{ expand_end_case ($3);
		  pop_momentary ();
		  stmt_decl_msg = 0;
		  finish_stmt (); }
	| CASE expr ':'
		{ register tree value = $2;
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		  /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
		     Strip such NOP_EXPRs.  */
		  if (TREE_CODE (value) == NOP_EXPR
		      && TREE_TYPE (value) == TREE_TYPE (TREE_OPERAND (value, 0)))
		    value = TREE_OPERAND (value, 0);

		  if (TREE_READONLY_DECL_P (value))
		    {
		      value = decl_constant_value (value);
		      /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
			 Strip such NOP_EXPRs.  */
		      if (TREE_CODE (value) == NOP_EXPR
			  && TREE_TYPE (value) == TREE_TYPE (TREE_OPERAND (value, 0)))
			value = TREE_OPERAND (value, 0);
		    }
		  value = fold (value);

		  if (TREE_CODE (value) != INTEGER_CST
		      && value != error_mark_node)
		    {
		      error ("case label does not reduce to an integer constant");
		      value = error_mark_node;
		    }
		  else
		    /* Promote char or short to int.  */
		    value = default_conversion (value);
		  if (value != error_mark_node)
		    {
		      int success = pushcase (value, label);
		      if (success == 1)
			error ("case label not within a switch statement");
		      else if (success == 2)
			error ("duplicate case value");
		      else if (success == 3)
			warning ("case value out of range");
		    }
		  define_case_label (label);
		}
	  stmt
	| CASE expr RANGE expr ':'
		{ register tree value1 = $2;
		  register tree value2 = $4;
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		  if (pedantic)
		    {
		      error ("ANSI C does not allow range expressions in switch statement");
		      value1 = error_mark_node;
		      value2 = error_mark_node;
		      break;
		    }
		  /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
		     Strip such NOP_EXPRs.  */
		  if (TREE_CODE (value1) == NOP_EXPR
		      && TREE_TYPE (value1) == TREE_TYPE (TREE_OPERAND (value1, 0)))
		    value1 = TREE_OPERAND (value1, 0);

		  if (TREE_READONLY_DECL_P (value1))
		    {
		      value1 = decl_constant_value (value1);
		      /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
			 Strip such NOP_EXPRs.  */
		      if (TREE_CODE (value1) == NOP_EXPR
			  && TREE_TYPE (value1) == TREE_TYPE (TREE_OPERAND (value1, 0)))
			value1 = TREE_OPERAND (value1, 0);
		    }
		  value1 = fold (value1);

		  /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
		     Strip such NOP_EXPRs.  */
		  if (TREE_CODE (value2) == NOP_EXPR
		      && TREE_TYPE (value2) == TREE_TYPE (TREE_OPERAND (value2, 0)))
		    value2 = TREE_OPERAND (value2, 0);

		  if (TREE_READONLY_DECL_P (value2))
		    {
		      value2 = decl_constant_value (value2);
		      /* build_c_cast puts on a NOP_EXPR to make a non-lvalue.
			 Strip such NOP_EXPRs.  */
		      if (TREE_CODE (value2) == NOP_EXPR
			  && TREE_TYPE (value2) == TREE_TYPE (TREE_OPERAND (value2, 0)))
			value2 = TREE_OPERAND (value2, 0);
		    }
		  value2 = fold (value2);


		  if (TREE_CODE (value1) != INTEGER_CST
		      && value1 != error_mark_node)
		    {
		      error ("case label does not reduce to an integer constant");
		      value1 = error_mark_node;
		    }
		  if (TREE_CODE (value2) != INTEGER_CST
		      && value2 != error_mark_node)
		    {
		      error ("case label does not reduce to an integer constant");
		      value2 = error_mark_node;
		    }
		  if (value1 != error_mark_node
		      && value2 != error_mark_node)
		    {
		      int success = pushcase_range (value1, value2, label);
		      if (success == 1)
			error ("case label not within a switch statement");
		      else if (success == 2)
			error ("duplicate (or overlapping) case value");
		      else if (success == 3)
			warning ("case value out of range");
		      else if (success == 4)
			warning ("empty range specified");
		    }
		  define_case_label (label);
		}
	  stmt
	| DEFAULT ':'
		{
		  register tree label
		    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		  int success = pushcase (NULL_TREE, label);
		  if (success == 1)
		    error ("default label not within a switch statement");
		  else if (success == 2)
		    error ("multiple default labels in one switch");
		}
	  stmt
	| BREAK ';'
		{ emit_line_note (input_filename, lineno);
		  if ( ! expand_exit_something ())
		    error ("break statement not within loop or switch"); }
	| CONTINUE ';'
		{ emit_line_note (input_filename, lineno);
		  if (! expand_continue_loop ())
		    error ("continue statement not within a loop"); }
	| RETURN ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return (NULL_TREE); }
	| RETURN expr ';'
		{ emit_line_note (input_filename, lineno);
		  c_expand_return ($2);
		  finish_stmt ();
		}
	| ASM maybe_type_qual '(' string ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  expand_asm ($4);
		  finish_stmt ();
		}
	/* This is the case with just output operands.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, NULL_TREE, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with input operands as well.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ':' asm_operands ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, NULL_TREE,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	/* This is the case with clobbered registers as well.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ':'
  	  asm_operands ':' asm_clobbers ')' ';'
		{ if (TREE_CHAIN ($4)) $4 = combine_strings ($4);
		  emit_line_note (input_filename, lineno);
		  c_expand_asm_operands ($4, $6, $8, $10,
					 $2 == ridpointers[(int)RID_VOLATILE],
					 input_filename, lineno);
		  finish_stmt ();
		}
	| GOTO identifier ';'
		{ tree decl;
		  emit_line_note (input_filename, lineno);
		  decl = lookup_label ($2);
		  TREE_USED (decl) = 1;
		  expand_goto (decl); }
	| IDENTIFIER ':'
		{ tree label = define_label (input_filename, lineno, $1);
		  emit_nop ();
		  if (label)
		    expand_label (label); }
	  stmt
		{ finish_stmt (); }
	| TYPENAME_COLON
		{ tree label = define_label (input_filename, lineno, $1);
		  if (label)
		    expand_label (label); }
	  stmt
		{ finish_stmt (); }
	| ';'
		{ finish_stmt (); }

	/* Exception handling extentions.  */
	| RAISE raise_identifier '(' nonnull_exprlist ')' ';'
		{ cplus_expand_raise ($2, $4, NULL_TREE);
		  finish_stmt (); }
	| RAISE raise_identifier LEFT_RIGHT ';'
		{ cplus_expand_raise ($2, NULL_TREE, NULL_TREE);
		  finish_stmt (); }
	| RAISE identifier ';'
		{ cplus_expand_reraise ($2);
		  finish_stmt (); }
	| try EXCEPT identifier '{'
		{
		  tree decl = cplus_expand_end_try ($1);
		  $<ttype>2 = current_exception_type;
		  $<ttype>4 = current_exception_decl;
		  $<ttype>$ = current_exception_object;
		  cplus_expand_start_except ($3, decl);
		  pushlevel (0);
		  clear_last_expr ();
		  push_momentary ();
		  expand_start_bindings (0);
		  stmt_decl_msg = 0;
		}
	  except_stmts '}'
		{
		  tree decls = getdecls ();
		  tree block;
		  /* If there is a default exception to handle,
		     handle it here.  */
		  if ($6)
		    {
		      tree decl = build_decl (CPLUS_CATCH_DECL, NULL_TREE, 0);

		      pushlevel (1);
		      expand_start_bindings (0);
		      expand_expr ($6, 0, 0, 0);
		      expand_end_bindings (0, 1, 0);
		      block = poplevel (1, 0, 0);

		      /* This is a catch block.  */
		      TREE_LANG_FLAG_2 (block) = 1;
		      STMT_VARS (block) = decl;
		    }

		  expand_end_bindings (decls, 1, 1);
		  block = poplevel (1, 1, 0);
		  TREE_LANG_FLAG_3 (block) = 1;
		  pop_momentary ();
		  current_exception_type = $<ttype>2;
		  current_exception_decl = $<ttype>4;
		  current_exception_object = $<ttype>5;
		  cplus_expand_end_except ($6);
		}
	| try RERAISE raise_identifiers /* ';' checked for at bottom.  */
		{ tree name = get_identifier ("(compiler error)");
		  tree orig_ex_type = current_exception_type;
		  tree orig_ex_decl = current_exception_decl;
		  tree orig_ex_obj = current_exception_object;
		  tree decl = cplus_expand_end_try ($1), decls;
		  tree block;

		  /* Start hidden EXCEPT.  */
		  cplus_expand_start_except (name, decl);
		  pushlevel (0);
		  clear_last_expr ();
		  push_momentary ();
		  expand_start_bindings (0);
		  stmt_decl_msg = 0;

		  /* This sets up the reraise.  */
		  cplus_expand_reraise ($3);

		  decls = getdecls ();
		  expand_end_bindings (decls, 1, 1);
		  block = poplevel (1, 1, 0);
		  TREE_LANG_FLAG_3 (block) = 1;
		  pop_momentary ();
		  current_exception_type = orig_ex_type;
		  current_exception_decl = orig_ex_decl;
		  current_exception_object = orig_ex_obj;
		  /* This will reraise for us.  */
		  cplus_expand_end_except (error_mark_node);
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  if (yychar != ';')
		    error ("missing ';' after reraise statement");
		}
	| try  %prec EMPTY
		{ yyerror ("`except' missing after `try' statement");
		  /* Terminate the binding contour started by special
		     code in `.pushlevel'.  Automagically pops off
		     the conditional we started for `try' stmt.  */
		  cplus_expand_end_try ($1);
		  expand_end_bindings (0, 0, 1);
		  poplevel (0, 0, 0);
		  pop_momentary ();
		  YYERROR; }
	;

try:	  TRY '{' '}'
		{ $$ = 0; }
	| try_head stmts '}'
		{
		  $$ = 1;
		  pop_implicit_try_blocks (NULL_TREE);
		}
	| try_head error '}'
		{
		  $$ = 0;
		  pop_implicit_try_blocks (NULL_TREE);
		}
	;

try_head: TRY '{' { cplus_expand_start_try (0); } .pushlevel

except_stmts:
	  /* empty */
		{ $$ = NULL_TREE; }
	| except_stmts raise_identifier
		{
		  tree type = lookup_exception_type (current_class_type, current_class_name, $2);
		  if (type == NULL_TREE)
		    {
		      error ("`%s' is not an exception type",
			     IDENTIFIER_POINTER (TREE_VALUE ($2)));
		      current_exception_type = NULL_TREE;
		      TREE_TYPE (current_exception_object) = error_mark_node;
		    }
		  else
		    {
		      current_exception_type = type;
		      /* In-place union.  */
		      TREE_TYPE (current_exception_object) = type;
		    }
		  $2 = cplus_expand_start_catch ($2);
		  pushlevel (1);
		  expand_start_bindings (0);
		}
	  compstmt
		{
		  expand_end_bindings (0, 1, 0);
		  $4 = poplevel (1, 0, 0);

		  cplus_expand_end_catch (0);

		  /* Mark this as a catch block.  */
		  TREE_LANG_FLAG_2 ($4) = 1;
		  if ($2 != error_mark_node)
		    {
		      tree decl = build_decl (CPLUS_CATCH_DECL, DECL_NAME ($2), 0);
		      DECL_RTL (decl) = DECL_RTL ($2);
		      TREE_CHAIN (decl) = STMT_VARS ($4);
		      STMT_VARS ($4) = decl;
		    }
		}
	| except_stmts DEFAULT
		{
		  if ($1)
		    error ("duplicate default in exception handler");
		  current_exception_type = NULL_TREE;
		  /* Takes it right out of scope.  */
		  TREE_TYPE (current_exception_object) = error_mark_node;

		  if (! expand_catch_default ())
		    compiler_error ("default catch botch");

		  /* The default exception is handled as the
		     last in the chain of exceptions handled.  */
		  do_pending_stack_adjust ();
		  start_sequence ();
		  $1 = make_node (RTL_EXPR);
		  TREE_TYPE ($1) = void_type_node;
		}
	  compstmt
		{
		  do_pending_stack_adjust ();
		  if (! expand_catch (NULL_TREE))
		    compiler_error ("except nesting botch");
		  if (! expand_end_catch ())
		    compiler_error ("except nesting botch");
		  RTL_EXPR_SEQUENCE ($1) = (struct rtx_def *)get_insns ();
		  if ($4)
		    {
		      /* Mark this block as the default catch block.  */
		      TREE_LANG_FLAG_1 ($4) = 1;
		      TREE_LANG_FLAG_2 ($4) = 1;
		    }
		  end_sequence ();
		}
	;

forhead.1:
	  FOR '(' ';'
		{ $$ = NULL_TREE; }
	| FOR '(' expr ';'
		{ $$ = $3; }
	| FOR '(' '{' '}'
		{ $$ = NULL_TREE; }
	;

forhead.2:
	  FOR '(' decl
		{ $$ = 0; }
	| FOR '(' error ';'
		{ $$ = 0; }
	| FOR '(' '{' .pushlevel stmts '}'
		{ $$ = 1; }
	| FOR '(' '{' .pushlevel error '}'
		{ $$ = -1; }
	;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_type_qual:
	/* empty */
		{ if (pedantic)
		    warning ("ANSI C forbids use of `asm' keyword");
		  emit_line_note (input_filename, lineno); }
	| TYPE_QUAL
		{ if (pedantic)
		    warning ("ANSI C forbids use of `asm' keyword");
		  emit_line_note (input_filename, lineno); }
	;

xexpr:
	/* empty */
		{ $$ = NULL_TREE; }
	| expr
	| error
		{ $$ = NULL_TREE; }
	;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
asm_operands: /* empty */
		{ $$ = NULL_TREE; }
	| nonnull_asm_operands
	;

nonnull_asm_operands:
	  asm_operand
	| nonnull_asm_operands ',' asm_operand
		{ $$ = chainon ($1, $3); }
	;

asm_operand:
	  STRING '(' expr ')'
		{ $$ = build_tree_list ($1, $3); }
	;

asm_clobbers:
	  STRING
		{ $$ = tree_cons (NULL_TREE, $1, NULL_TREE); }
	| asm_clobbers ',' STRING
		{ $$ = tree_cons (NULL_TREE, $3, $1); }
	;

/* This is what appears inside the parens in a function declarator.
   Its value is represented in the format that grokdeclarator expects.

   In C++, declaring a function with no parameters
   means that that function takes *no* parameters.  */
parmlist:  /* empty */
		{
		  if (strict_prototype)
		    $$ = void_list_node;
		  else
		    $$ = NULL_TREE;
		}
	| parms
  		{
		  $$ = chainon ($1, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		}
	| parms ',' ELLIPSIS
		{
		  $$ = $1;
		  TREE_PARMLIST ($$) = 1;
		}
	/* C++ allows an ellipsis without a separating ',' */
	| parms ELLIPSIS
		{
		  $$ = $1;
		  TREE_PARMLIST ($$) = 1;
		}
	| ELLIPSIS
		{
		  $$ = NULL_TREE;
		}
	| TYPENAME_ELLIPSIS
		{
		  $$ = $1;
		  TREE_PARMLIST ($$) = 1;
		}
	| parms TYPENAME_ELLIPSIS
		{
		  $$ = $1;
		  TREE_PARMLIST ($$) = 1;
		}
	| parms ':'
		{
		  /* This helps us recover from really nasty
		     parse errors, for example, a missing right
		     parenthesis.  */
		  yyerror ("possibly missing ')'");
		  $$ = chainon ($1, void_list_node);
		  TREE_PARMLIST ($$) = 1;
		  yyungetc (':', 0);
		  yychar = ')';
		}
	;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  parm opt.init
		{ $$ = build_tree_list ($2, $1); }
	| parms ',' parm opt.init
		{ $$ = chainon ($1, build_tree_list ($4, $3)); }
	| parms ',' bad_parm opt.init
		{ $$ = chainon ($1, build_tree_list ($4, $3)); }
	;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  */
parm:
	  typed_declspecs dont_see_typename notype_declarator
		{ $$ = build_tree_list ($1, $3);
		  see_typename (); }
	| typed_declspecs dont_see_typename absdcl
		{ $$ = build_tree_list ($1, $3);
		  see_typename (); }
	| declmods dont_see_typename notype_declarator
		{ $$ = build_tree_list ($1, $3);
		  see_typename (); }
	| declmods dont_see_typename absdcl
		{ $$ = build_tree_list ($1, $3);
		  see_typename (); }
	;

see_typename: type_quals
	{ see_typename (); }
	;

dont_see_typename: /* empty */
	{ dont_see_typename (); }
	;

bad_parm:
	  dummy_decl notype_declarator
		{
		  warning ("type specifier omitted for parameter");
		  $$ = build_tree_list (TREE_PURPOSE (TREE_VALUE ($<ttype>-1)), $2);
		}
	| dummy_decl absdcl
		{
		  warning ("type specifier omitted for parameter");
		  $$ = build_tree_list (TREE_PURPOSE (TREE_VALUE ($<ttype>-1)), $2);
		}
	;

	/* C++ extension: allow for initialization */
opt.init:
	  /* empty */
		{ $$ = NULL_TREE; }
	| '=' init
		{ $$ = $2; }
	;

maybe_raises:
	  /* empty */
		{ $$ = NULL_TREE; }
	| RAISES raise_identifiers  %prec EMPTY
		{ $$ = $2; }
	;

raise_identifier:
	  ALL
		{ $$ = void_list_node; }
	| IDENTIFIER
		{ $$ = build_decl_list (NULL_TREE, $1); }
	| TYPENAME
		{ $$ = build_decl_list (NULL_TREE, $1); }
	| SCOPE IDENTIFIER
		{ $$ = build_decl_list (void_type_node, $2); }
	| SCOPE TYPENAME
		{ $$ = build_decl_list (void_type_node, $2); }
	| scoped_identifier IDENTIFIER
		{ $$ = build_decl_list ($1, $2); }
	| scoped_identifier TYPENAME
		{ $$ = build_decl_list ($1, $2); }

raise_identifiers:
	  raise_identifier
	| raise_identifiers ',' raise_identifier
		{
  		  TREE_CHAIN ($3) = $1;
		  $$ = $3;
		}
	;

operator_name:
	  OPERATOR '*'
		{ $$ = build_opid (0, MULT_EXPR); }
	| OPERATOR '/'
		{ $$ = build_opid (0, TRUNC_DIV_EXPR); }
	| OPERATOR '%'
		{ $$ = build_opid (0, TRUNC_MOD_EXPR); }
	| OPERATOR '+'
		{ $$ = build_opid (0, PLUS_EXPR); }
	| OPERATOR '-'
		{ $$ = build_opid (0, MINUS_EXPR); }
	| OPERATOR '&'
		{ $$ = build_opid (0, BIT_AND_EXPR); }
	| OPERATOR '|'
		{ $$ = build_opid (0, BIT_IOR_EXPR); }
	| OPERATOR '^'
		{ $$ = build_opid (0, BIT_XOR_EXPR); }
	| OPERATOR '~'
		{ $$ = build_opid (0, BIT_NOT_EXPR); }
	| OPERATOR ','
		{ $$ = build_opid (0, COMPOUND_EXPR); }
	| OPERATOR ARITHCOMPARE
		{ $$ = build_opid (0, $2); }
	| OPERATOR EQCOMPARE
		{ $$ = build_opid (0, $2); }
	| OPERATOR ASSIGN
		{ $$ = build_opid (MODIFY_EXPR, $2); }
	| OPERATOR '='
		{
		  $$ = build_opid (MODIFY_EXPR, NOP_EXPR);
		  if (current_class_type)
		    {
		      TYPE_HAS_ASSIGNMENT (current_class_type) = 1;
		      TYPE_GETS_ASSIGNMENT (current_class_type) = 1;
		    }
		}
	| OPERATOR LSHIFT
		{ $$ = build_opid (0, $2); }
	| OPERATOR RSHIFT
		{ $$ = build_opid (0, $2); }
	| OPERATOR PLUSPLUS
		{ $$ = build_opid (0, POSTINCREMENT_EXPR); }
	| OPERATOR MINUSMINUS
		{ $$ = build_opid (0, PREDECREMENT_EXPR); }
	| OPERATOR ANDAND
		{ $$ = build_opid (0, TRUTH_ANDIF_EXPR); }
	| OPERATOR OROR
		{ $$ = build_opid (0, TRUTH_ORIF_EXPR); }
	| OPERATOR '!'
		{ $$ = build_opid (0, TRUTH_NOT_EXPR); }
	| OPERATOR '?' ':'
		{ $$ = build_opid (0, COND_EXPR); }
	| OPERATOR MIN_MAX
		{ $$ = build_opid (0, $2); }
	| OPERATOR POINTSAT  %prec EMPTY
		{ $$ = build_opid (0, COMPONENT_REF); }
	| OPERATOR POINTSAT_LEFT_RIGHT type_quals  %prec '.'
		{
		  if (yychar == YYEMPTY)
		    yychar = YYLEX;
		  if (yychar == '(' || yychar == LEFT_RIGHT)
		    {
		      $$ = build_opid (0, METHOD_CALL_EXPR);
		      if (current_class_type)
			{
			  tree t = current_class_type;
			  while (t)
			    {
			      TYPE_OVERLOADS_METHOD_CALL_EXPR (t) = 1;
			      t = TYPE_NEXT_VARIANT (t);
			    }
			}
		    }
		  else
		    $$ = build_parse_node (CALL_EXPR, build_opid (0, COMPONENT_REF), void_list_node, $3);
		}
	| OPERATOR LEFT_RIGHT
		{ $$ = build_opid (0, CALL_EXPR);
		  if (current_class_type)
		    {
		      tree t = current_class_type;
		      while (t)
			{
			  TYPE_OVERLOADS_CALL_EXPR (t) = 1;
			  t = TYPE_NEXT_VARIANT (t);
			}
		    }
		}
	| OPERATOR '[' ']'
		{ $$ = build_opid (0, ARRAY_REF);
		  if (current_class_type)
		    {
		      tree t = current_class_type;
		      while (t)
			{
			  TYPE_OVERLOADS_ARRAY_REF (t) = 1;
			  t = TYPE_NEXT_VARIANT (t);
			}
		    }
		}
	| OPERATOR NEW
		{
		  $$ = build_opid (0, NEW_EXPR);
		  if (current_class_type)
		    {
		      tree t = current_class_type;
		      while (t)
			{
			  TREE_GETS_NEW (t) = 1;
			  t = TYPE_NEXT_VARIANT (t);
			}
		    }
		}
	| OPERATOR DELETE
		{
		  $$ = build_opid (0, DELETE_EXPR);
		  if (current_class_type)
		    {
		      tree t = current_class_type;
		      while (t)
			{
			  TREE_GETS_DELETE (t) = 1;
			  t = TYPE_NEXT_VARIANT (t);
			}
		    }
		}

	/* These should do `groktypename' and set up TREE_HAS_X_CONVERSION
	   here, rather than doing it in class.c .  */
	| OPERATOR typed_typespecs absdcl
		{
		  $$ = build1 (TYPE_EXPR, $2, $3);
		}
	| OPERATOR error
		{ $$ = build_opid (ERROR_MARK, ERROR_MARK); }
	;

%%

#if YYDEBUG != 0
db_yyerror (s, yyps, yychar)
     char *s;
     short *yyps;
     int yychar;
{
  FILE *yyout;
  char buf[1024];
  int st;

  yyerror (s);
  printf ("State is %d, input token number is %d.\n", *yyps, yychar);

#ifdef PARSE_OUTPUT
  if (*yyps < 1) fatal ("Cannot start from here");
  else if ((yyout = fopen (PARSE_OUTPUT, "r")) == NULL)
    error ("cannot open file %s", PARSE_OUTPUT);
  else
    {
      printf ("That is to say,\n\n");
      while (fgets(buf, sizeof (buf)-1, yyout))
	{
	  if (buf[0] != 's') continue;
	  st = atoi (buf+6);
	  if (st != *yyps) continue;
	  printf ("%s", buf);
	  while (fgets (buf, sizeof (buf)-1, yyout))
	    {
	      if (buf[0] == 's') break;
	      printf ("%s", buf);
	    }
	  break;
	}
      printf ("With the token %s\n", yytname[YYTRANSLATE (yychar)]);
      fclose (yyout);
    }
#endif
}
#endif

void
yyerror (string)
     char *string;
{
  extern FILE *finput;
  extern char *token_buffer;
  char buf[200];

  strcpy (buf, string);

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  if (end_of_file || feof (finput))
    strcat (buf, " at end of input");
  else if (token_buffer[0] == 0)
    strcat (buf, " at null character");
  else if (token_buffer[0] == '"')
    strcat (buf, " before string constant");
  else if (token_buffer[0] == '\'')
    strcat (buf, " before character constant");
  else
    strcat (buf, " before `%s'");

  error (buf, token_buffer);
}

#ifndef GATHER_STATISTICS
int *init_parse () { return 0; }
#else
static int *reduce_count;
static int *token_count;

#define REDUCE_LENGTH (sizeof (yyr2) / sizeof (yyr2[0]))
#define TOKEN_LENGTH (256 + YYNTBASE + 1)

int *
init_parse ()
{
  reduce_count = (int *)malloc (sizeof (int) * (REDUCE_LENGTH + 1));
  bzero (reduce_count, sizeof (int) * (REDUCE_LENGTH + 1));
  reduce_count += 1;
  token_count = (int *)malloc (sizeof (int) * (TOKEN_LENGTH + 1));
  bzero (token_count, sizeof (int) * (TOKEN_LENGTH + 1));
  token_count += 1;
  return token_count;
}

void
yyhook (yyn)
     int yyn;
{
  reduce_count[yyn] += 1;
}

static int reduce_cmp (p, q)
     int *p, *q;
{
  return reduce_count[*q] - reduce_count[*p];
}

static int token_cmp (p, q)
     int *p, *q;
{
  return token_count[*q] - token_count[*p];
}

void
print_parse_statistics ()
{
  int i;
  int maxlen = REDUCE_LENGTH;
  unsigned *sorted;
  
  if (reduce_count[-1] == 0)
    return;

  if (TOKEN_LENGTH > REDUCE_LENGTH)
    maxlen = TOKEN_LENGTH;
  sorted = (unsigned *) alloca (sizeof (int) * maxlen);

  for (i = 0; i < TOKEN_LENGTH; i++)
    sorted[i] = i;
  qsort (sorted, TOKEN_LENGTH, sizeof (int), token_cmp);
  for (i = 0; i < TOKEN_LENGTH; i++)
    {
      int index = sorted[i];
      if (token_count[index] == 0)
	break;
      if (token_count[index] < token_count[-1])
	break;
      fprintf (stderr, "token %d", index);
#if YYDEBUG
      fprintf (stderr, ", `%s'", yytname[YYTRANSLATE (index)]);
#endif
      fprintf (stderr, ", count = %d\n", token_count[index]);
    }
  fprintf (stderr, "\n");
  for (i = 0; i < REDUCE_LENGTH; i++)
    sorted[i] = i;
  qsort (sorted, REDUCE_LENGTH, sizeof (int), reduce_cmp);
  for (i = 0; i < REDUCE_LENGTH; i++)
    {
      int index = sorted[i];
      if (reduce_count[index] == 0)
	break;
      if (reduce_count[index] < reduce_count[-1])
	break;
      fprintf (stderr, "rule %d", index);
#if YYDEBUG
      fprintf (stderr, ", line %d", yyrline[index]);
#endif
      fprintf (stderr, ", count = %d\n", reduce_count[index]);
    }
  fprintf (stderr, "\n");
}
#endif
