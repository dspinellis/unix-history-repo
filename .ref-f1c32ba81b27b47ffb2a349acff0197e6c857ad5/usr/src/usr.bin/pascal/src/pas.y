/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pas.y	5.5 (Berkeley) %G%
 */

/*
 * Yacc grammar for UNIX Pascal
 *
 * This grammar is processed by the commands in the shell script
 * "gram" to yield parse tables and semantic routines in the file
 * "y.tab.c" and a header defining the lexical tokens in "yy.h".
 *
 * In order for the syntactic error recovery possible with this
 * grammar to work, the grammar must be processed by a yacc which
 * has been modified to fully enumerate possibilities in states
 * which involve the symbol "error".
 * The parser used for Pascal also uses a different encoding of
 * the test entries in the action table which speeds the parse.
 * A version of yacc which will work for Pascal is included on
 * the distribution table as "eyacc".
 *
 * The "gram" script also makes the following changes to the "y.tab.c"
 * file:
 *
 *	1) Causes yyval to be declared int *.
 *
 *	2) Loads the variable yypv into a register as yyYpv so that
 *	   the arguments $1, ... are available as yyYpv[1] etc.
 *	   This produces much smaller code in the semantic actions.
 *
 *	3) Deletes the unused array yysterm.
 *
 *	4) Moves the declarations up to the flag line containing
 *	   '##' to the file yy.h so that the routines which use
 *	   these "magic numbers" don't have to all be compiled at
 *	   the same time.
 *
 *	5) Creates the semantic restriction checking routine yyEactr
 *	   by processing action lines containing `@@'.
 *
 * This compiler uses a different version of the yacc parser, a
 * different yyerror which is called yerror, and requires more
 * lookahead sets than normally provided by yacc.
 *
 * Source for the yacc used with this grammar is included on
 * distribution tapes.
 */

/*
 * TERMINAL DECLARATIONS
 *
 * Some of the terminal declarations are out of the most natural
 * alphabetic order because the error recovery
 * will guess the first of equal cost non-terminals.
 * This makes, e.g. YTO preferable to YDOWNTO.
 */

%term
	YAND		YARRAY		YBEGIN		YCASE
	YCONST		YDIV		YDO		YDOTDOT
	YTO		YELSE		YEND		YFILE
	YFOR		YFORWARD	YPROCEDURE	YGOTO
	YID		YIF		YIN		YINT
	YLABEL		YMOD		YNOT		YNUMB
	YOF		YOR		YPACKED		YNIL
	YFUNCTION	YPROG		YRECORD		YREPEAT
	YSET		YSTRING		YTHEN		YDOWNTO
	YTYPE		YUNTIL		YVAR		YWHILE
	YWITH		YBINT		YOCT		YHEX
	YCASELAB	YILLCH		YEXTERN		YLAST

/*
 * PRECEDENCE DECLARATIONS
 *
 * Highest precedence is the unary logical NOT.
 * Next are the multiplying operators, signified by '*'.
 * Lower still are the binary adding operators, signified by '+'.
 * Finally, at lowest precedence and non-associative are the relationals.
 */

%binary	'<'	'='	'>'	YIN
%left	'+'	'-'	YOR	'|'
%left	UNARYSIGN
%left	'*'	'/'	YDIV	YMOD	YAND	'&'
%left	YNOT

%{
/*
 * GLOBALS FOR ACTIONS
 */

/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)pas.y 5.5 %G%"; */

/*
 * The following line marks the end of the yacc
 * Constant definitions which are removed from
 * y.tab.c and placed in the file y.tab.h.
 */
##
/* Copyright (c) 1979 Regents of the University of California */

static	char sccsid[] = "@(#)pas.y 5.5 %G%";

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"		/* must be included for yy.h */
#include "yy.h"
#include "tree.h"

#ifdef PI
#define	lineof(l)	l
#define	line2of(l)	l
#endif

%}

%%

/*
 * PRODUCTIONS
 */

goal:
	prog_hedr decls block '.'
		= funcend($1.nl_entry, $3.tr_entry, lineof($4.i_entry));
		|
	decls
		= segend();
		;
	

prog_hedr:
	YPROG YID '(' id_list ')' ';' 
		= $$.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof($1.i_entry), $2.tr_entry, fixlist($4.tr_entry), TR_NIL)));
		|
	YPROG YID ';'
		= $$.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof($1.i_entry),  $2.tr_entry, TR_NIL, TR_NIL)));
		|
	YPROG error
		= {
			yyPerror("Malformed program statement", PPROG);
			/*
			 * Should make a program statement
			 * with "input" and "output" here.
			 */
			$$.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof($1.i_entry), TR_NIL, TR_NIL, TR_NIL)));
		  }
		;
block:
	YBEGIN stat_list YEND
		= {
			$$.tr_entry = tree3(T_BSTL, lineof($1.i_entry), fixlist($2.tr_entry));
			if ($3.i_entry < 0)
				brerror($1.i_entry, "begin");
		  }
		;


/*
 * DECLARATION PART
 */
decls:
	decls decl
		= trfree();
		|
	decls error
		= {
			constend(), typeend(), varend(), trfree();
			yyPerror("Malformed declaration", PDECL);
		  }
		|
	/* lambda */
		= trfree();
		;

decl:
	labels
		|
	const_decl
		= constend();
		|
	type_decl
		= typeend();
		|
	var_decl
		= varend();
		|
	proc_decl
		;

/*
 * LABEL PART
 */

labels:
	YLABEL label_decl ';'
		= label(fixlist($2.tr_entry), lineof($1.i_entry));
		;
label_decl:
	YINT
		= $$.tr_entry = newlist($1.i_entry == NIL ? TR_NIL :
					(struct tnode *) *hash($1.cptr, 1));
		|
	label_decl ',' YINT
		= $$.tr_entry = addlist($1.tr_entry, $3.i_entry == NIL ?
				TR_NIL : (struct tnode *) *hash($3.cptr, 1));
		;

/*
 * CONST PART
 */

const_decl:
	YCONST YID '=' const ';'
		= constbeg($1.i_entry, lineof($1.i_entry)),
		  constant(lineof($3.i_entry), $2.cptr, $4.tr_entry);
		|
	const_decl YID '=' const ';'
		= constant(lineof($3.i_entry), $2.cptr, $4.tr_entry);
		|
	YCONST error
		= {
			constbeg($1.i_entry);
Cerror:
			yyPerror("Malformed const declaration", PDECL);
		  }
		|
	const_decl error
		= goto Cerror;
		;

/*
 * TYPE PART
 */

type_decl:
	YTYPE YID '=' type ';'
		= typebeg($1.i_entry, line2of($2.i_entry)), type(lineof($3.i_entry), $2.cptr, $4.tr_entry);
		|
	type_decl YID '=' type ';'
		= type(lineof($3.i_entry), $2.cptr, $4.tr_entry);
		|
	YTYPE error
		= {
			typebeg($1.i_entry, line2of($1.i_entry));
Terror:
			yyPerror("Malformed type declaration", PDECL);
		  }
		|
	type_decl error
		= goto Terror;
		;

/*
 * VAR PART
 */

var_decl:
	YVAR id_list ':' type ';'
		= varbeg($1.i_entry, line2of($3.i_entry)), var(lineof($3.i_entry), fixlist($2.tr_entry), $4.tr_entry);
		|
	var_decl id_list ':' type ';'
		= var(lineof($3.i_entry), fixlist($2.tr_entry), $4.tr_entry);
		|
	YVAR error 
		= {
			varbeg($1.i_entry, line2of($1.i_entry));
Verror:
			yyPerror("Malformed var declaration", PDECL);
		  }
		|
	var_decl error
		= goto Verror;
		;

/*
 * PROCEDURE AND FUNCTION DECLARATION PART
 */

proc_decl:
	phead YFORWARD ';'
		= funcfwd($1.nl_entry);
		|
	phead YEXTERN ';'
		= (void) funcext($1.nl_entry);
		|
	pheadres decls block ';'
		= funcend($1.nl_entry, $3.tr_entry, lineof($4.i_entry));
		|
	phead error
		;
pheadres:
	phead
		= (void) funcbody($1.nl_entry);
		;
phead:
	porf YID params ftype ';'
		= $$.nl_entry = funchdr(tree5($1.i_entry, lineof($5.i_entry),
				$2.tr_entry, $3.tr_entry, $4.tr_entry));
		;
porf:
	YPROCEDURE
		= $$.i_entry = T_PDEC;
		|
	YFUNCTION
		= $$.i_entry = T_FDEC;
		;
params:
	'(' param_list ')'
		= $$.tr_entry = fixlist($2.tr_entry);
		|
	/* lambda */
		= $$.tr_entry = TR_NIL;
		;

/*
 * PARAMETERS
 */

param:
	id_list ':' type
		= $$.tr_entry = tree3(T_PVAL, (int) fixlist($1.tr_entry), $3.tr_entry);
		|
	YVAR id_list ':' type
		= $$.tr_entry = tree3(T_PVAR, (int) fixlist($2.tr_entry), $4.tr_entry);
		|
	YFUNCTION id_list params ftype
		= $$.tr_entry = tree5(T_PFUNC, (int) fixlist($2.tr_entry),
				$4.tr_entry, $3.tr_entry, 
				(struct tnode *) lineof($1.i_entry));
		|
	YPROCEDURE id_list params ftype
		= $$.tr_entry = tree5(T_PPROC, (int) fixlist($2.tr_entry),
				$4.tr_entry, $3.tr_entry, 
				(struct tnode *) lineof($1.i_entry));
		;
ftype:
	':' type
		= $$ = $2;
		|
	/* lambda */
		= $$.tr_entry = TR_NIL;
		;
param_list:
	param
		= $$.tr_entry = newlist($1.tr_entry);
		|
	param_list ';' param
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

/*
 * CONSTANTS
 */

const:
	YSTRING
		= $$.tr_entry = tree2(T_CSTRNG, $1.i_entry);
		|
	number
		|
	'+' number
		= $$.tr_entry = tree2(T_PLUSC, $2.i_entry);
		|
	'-' number
		= $$.tr_entry = tree2(T_MINUSC, $2.i_entry);
		;
number:
	const_id
		= $$.tr_entry = tree2(T_ID, $1.i_entry);
		|
	YINT
		= $$.tr_entry = tree2(T_CINT, $1.i_entry);
		|
	YBINT
		= $$.tr_entry = tree2(T_CBINT, $1.i_entry);
		|
	YNUMB
		= $$.tr_entry = tree2(T_CFINT, $1.i_entry);
		;
const_list:
	const
		= $$.tr_entry = newlist($1.tr_entry);
		|
	const_list ',' const
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

/*
 * TYPES
 */

type:
	simple_type
		|
	'^' YID
		= $$.tr_entry = tree3(T_TYPTR, lineof($1.i_entry), tree2(T_ID,
								$2.i_entry));
		|
	struct_type
		|
	YPACKED struct_type
		= $$.tr_entry = tree3(T_TYPACK, lineof($1.i_entry), $2.tr_entry);
		;
simple_type:
	type_id
		|
	'(' id_list ')'
		= $$.tr_entry = tree3(T_TYSCAL, lineof($1.i_entry), fixlist($2.tr_entry));
		|
	const YDOTDOT const
		= $$.tr_entry = tree4(T_TYRANG, lineof($2.i_entry), $1.tr_entry,
				$3.tr_entry);
		;
struct_type:
	YARRAY '[' simple_type_list ']' YOF type
		= $$.tr_entry = tree4(T_TYARY, lineof($1.i_entry),
					fixlist($3.tr_entry), $6.tr_entry);
		|
	YFILE YOF type
		= $$.tr_entry = tree3(T_TYFILE, lineof($1.i_entry), $3.tr_entry);
		|
	YSET YOF simple_type
		= $$.tr_entry = tree3(T_TYSET, lineof($1.i_entry), $3.tr_entry);
		|
	YRECORD field_list YEND
		= {
			$$.tr_entry = setuptyrec( lineof( $1.i_entry ) , $2.tr_entry);
			if ($3.i_entry < 0)
				brerror($1.i_entry, "record");
		  }
		;
simple_type_list:
	simple_type
		= $$.tr_entry = newlist($1.tr_entry);
		|
	simple_type_list ',' simple_type
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

/*
 * RECORD TYPE
 */
field_list:
	fixed_part variant_part
		= $$.tr_entry = tree4(T_FLDLST, lineof(NIL), 
				fixlist($1.tr_entry), $2.tr_entry);
		;
fixed_part:
	field
		= $$.tr_entry = newlist($1.tr_entry);
		|
	fixed_part ';' field
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		|
	fixed_part error
		= yyPerror("Malformed record declaration", PDECL);
		;
field:
	/* lambda */
		= $$.tr_entry = TR_NIL;
		|
	id_list ':' type
		= $$.tr_entry = tree4(T_RFIELD, lineof($2.i_entry),
				fixlist($1.tr_entry), $3.tr_entry);
		;

variant_part:
	/* lambda */
		= $$.tr_entry = TR_NIL;
		|
	YCASE type_id YOF variant_list
		= $$.tr_entry = tree5(T_TYVARPT, lineof($1.i_entry), TR_NIL, 
				$2.tr_entry, fixlist($4.tr_entry));
		|
	YCASE YID ':' type_id YOF variant_list
		= $$.tr_entry = tree5(T_TYVARPT, lineof($1.i_entry),
				$2.tr_entry, $4.tr_entry,
					fixlist($6.tr_entry));
		;
variant_list:
	variant
		= $$.tr_entry = newlist($1.tr_entry);
		|
	variant_list ';' variant
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		|
	variant_list error
		= yyPerror("Malformed record declaration", PDECL);
		;
variant:
	/* lambda */
		= $$.tr_entry = TR_NIL;
		|
	const_list ':' '(' field_list ')'
		= $$.tr_entry = tree4(T_TYVARNT,lineof($2.i_entry), fixlist($1.tr_entry),
				$4.tr_entry);
		;

/*
 * STATEMENT LIST
 */

stat_list:
	stat
		= $$.tr_entry = newlist($1.tr_entry);
		|
	stat_lsth stat
		= {
			if ((p = $1.tr_entry) != TR_NIL && (q = p->list_node.list)->tag == T_IFX) {
				q->tag = T_IFEL;
				q->if_node.else_stmnt = $2.tr_entry;
			} else
				$$.tr_entry= addlist($1.tr_entry, $2.tr_entry);
		  }
		;

stat_lsth:
	stat_list ';'
		= if ((q = $1.tr_entry) != TR_NIL && (p = q->list_node.list) != TR_NIL && p->tag == T_IF) {
			if (yychar < 0)
				yychar = yylex();
			if (yyshifts >= 2 && yychar == YELSE) {
				recovered();
				copy((char *) (&Y), (char *) (&OY), sizeof Y);
				yerror("Deleted ';' before keyword else");
				yychar = yylex();
				p->tag = T_IFX;
			}
		  }
		;

/*
 * CASE STATEMENT LIST
 */

cstat_list:
	cstat
		= $$.tr_entry = newlist($1.tr_entry);
		|
	cstat_list ';' cstat
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		|
	error
		= {
			$$.tr_entry = TR_NIL;
Kerror:
			yyPerror("Malformed statement in case", PSTAT);
		  }
		|
	cstat_list error
		= goto Kerror;
		;

cstat:
	const_list ':' stat
		= $$.tr_entry = tree4(T_CSTAT, lineof($2.i_entry),
				fixlist($1.tr_entry), $3.tr_entry);
		|
	YCASELAB stat
		= $$.tr_entry = tree4(T_CSTAT, lineof($1.i_entry), TR_NIL,
					$2.tr_entry);
		|
	/* lambda */
		= $$.tr_entry = TR_NIL;
		;

/*
 * STATEMENT
 */

stat:
	/* lambda */
		= $$.tr_entry = TR_NIL;
		|
	YINT ':' stat
		= $$.tr_entry = tree4(T_LABEL, lineof($2.i_entry),
				$1.tr_entry == TR_NIL ? TR_NIL :
					    (struct tnode *) *hash($1.cptr, 1), $3.tr_entry);
		|
	proc_id
		= $$.tr_entry = tree4(T_PCALL, lineof(yyline), $1.tr_entry,
						TR_NIL);
		|
	proc_id '(' wexpr_list ')'
		= $$.tr_entry = tree4(T_PCALL, lineof($2.i_entry), $1.tr_entry,
					fixlist($3.tr_entry));
		|
	YID error
		= goto NSerror;
		|
	assign
		|
	YBEGIN stat_list YEND
		= {
			$$.tr_entry = tree3(T_BLOCK, lineof($1.i_entry),
						fixlist($2.tr_entry));
			if ($3.i_entry < 0)
				brerror($1.i_entry, "begin");
		  }
		|
	YCASE expr YOF cstat_list YEND
		= {
			$$.tr_entry = tree4(T_CASE, lineof($1.i_entry),
					$2.tr_entry, fixlist($4.tr_entry));
			if ($5.i_entry < 0)
				brerror($1.i_entry, "case");
		  }
		|
	YWITH var_list YDO stat
		= $$.tr_entry = tree4(T_WITH, lineof($1.i_entry),
				fixlist($2.tr_entry), $4.tr_entry);
		|
	YWHILE expr YDO stat
		= $$.tr_entry = tree4(T_WHILE, lineof($1.i_entry), $2.tr_entry,
					$4.tr_entry);
		|
	YREPEAT stat_list YUNTIL expr
		= $$.tr_entry = tree4(T_REPEAT, lineof($3.i_entry),
				fixlist($2.tr_entry), $4.tr_entry);
		|
	YFOR assign YTO expr YDO stat
		= $$.tr_entry = tree5(T_FORU, lineof($1.i_entry), $2.tr_entry,
				$4.tr_entry, $6.tr_entry);
		|
	YFOR assign YDOWNTO expr YDO stat
		= $$.tr_entry = tree5(T_FORD, lineof($1.i_entry), $2.tr_entry,
				$4.tr_entry, $6.tr_entry);
		|
	YGOTO YINT
		= $$.tr_entry = tree3(T_GOTO, lineof($1.i_entry),
				(struct tnode *) *hash($2.cptr, 1));
		|
	YIF expr YTHEN stat
		= $$.tr_entry = tree5(T_IF, lineof($1.i_entry), $2.tr_entry,
				$4.tr_entry, TR_NIL);
		|
	YIF expr YTHEN stat YELSE stat
		= $$.tr_entry = tree5(T_IFEL, lineof($1.i_entry), $2.tr_entry,
					$4.tr_entry, $6.tr_entry);
		|
	error
		= {
NSerror:
			$$.tr_entry = TR_NIL;
			yyPerror("Malformed statement", PSTAT);
		  }
		;
assign:
	variable ':' '=' expr
		= $$.tr_entry = tree4(T_ASGN, lineof($2.i_entry), $1.tr_entry,
				    $4.tr_entry);
		;

/*
 * EXPRESSION
 */

expr:
	error
		= {
NEerror:
			$$.tr_entry = TR_NIL;
			yyPerror("Missing/malformed expression", PEXPR);
		  }
		|
	expr relop expr			%prec '<'
		= $$.tr_entry = tree4($2.i_entry,
			$1.tr_entry->expr_node.const_tag == SAWCON ?
			$3.tr_entry->expr_node.const_tag :
			$1.tr_entry->expr_node.const_tag,
			$1.tr_entry, $3.tr_entry);
		|
	'+' expr			%prec UNARYSIGN
		= $$.tr_entry = tree3(T_PLUS, $2.tr_entry->expr_node.const_tag,
				$2.tr_entry);
		|
	'-' expr			%prec UNARYSIGN
		= $$.tr_entry = tree3(T_MINUS, $2.tr_entry->expr_node.const_tag,
				$2.tr_entry);
		|
	expr addop expr			%prec '+'
		= $$.tr_entry = tree4($2.i_entry,
			$1.tr_entry->expr_node.const_tag == SAWCON ?
			$3.tr_entry->expr_node.const_tag :
			$1.tr_entry->expr_node.const_tag, $1.tr_entry,
			$3.tr_entry);
		|
	expr divop expr			%prec '*'
		= $$.tr_entry = tree4($2.i_entry,
			$1.tr_entry->expr_node.const_tag == SAWCON ?
			$3.tr_entry->expr_node.const_tag :
			$1.tr_entry->expr_node.const_tag, $1.tr_entry,
			$3.tr_entry);
		|
	YNIL
		= $$.tr_entry = tree2(T_NIL, NOCON);
		|
	YSTRING
		= $$.tr_entry = tree3(T_STRNG, SAWCON, $1.tr_entry);
		|
	YINT
		= $$.tr_entry = tree3(T_INT, NOCON, $1.tr_entry);
		|
	YBINT
		= $$.tr_entry = tree3(T_BINT, NOCON, $1.tr_entry);
		|
	YNUMB
		= $$.tr_entry = tree3(T_FINT, NOCON, $1.tr_entry);
		|
	variable
		|
	YID error
		= goto NEerror;
		|
	func_id '(' wexpr_list ')'
		= $$.tr_entry = tree4(T_FCALL, NOCON, $1.tr_entry,
			fixlist($3.tr_entry));
		|
	'(' expr ')'
		= $$.tr_entry = $2.tr_entry;
		|
	negop expr			%prec YNOT
		= $$.tr_entry = tree3(T_NOT, NOCON, $2.tr_entry);
		|
	'[' element_list ']'
		= $$.tr_entry = tree3(T_CSET, SAWCON, fixlist($2.tr_entry));
		|
	'[' ']'
		= $$.tr_entry = tree3(T_CSET, SAWCON, TR_NIL);
		;

element_list:
	element
		= $$.tr_entry = newlist($1.tr_entry);
		|
	element_list ',' element
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;
element:
	expr
		|
	expr YDOTDOT expr
		= $$.tr_entry = tree3(T_RANG, $1.i_entry, $3.tr_entry);
		;

/*
 * QUALIFIED VARIABLES
 */

variable:
	YID
		= {
			@@ return (identis(var, VAR));
			$$.tr_entry = setupvar($1.cptr, TR_NIL);
		  }
		|
	qual_var
		= $1.tr_entry->var_node.qual = 
					fixlist($1.tr_entry->var_node.qual);
		;
qual_var:
	array_id '[' expr_list ']'
		= $$.tr_entry = setupvar($1.cptr, tree2(T_ARY, 
				(int) fixlist($3.tr_entry)));
		|
	qual_var '[' expr_list ']'
		= $1.tr_entry->var_node.qual =
				addlist($1.tr_entry->var_node.qual,
				tree2(T_ARY, (int) fixlist($3.tr_entry)));
		|
	record_id '.' field_id
		= $$.tr_entry = setupvar($1.cptr, setupfield($3.tr_entry,
							TR_NIL));
		|
	qual_var '.' field_id
		= $1.tr_entry->var_node.qual =
		    addlist($1.tr_entry->var_node.qual,
		    setupfield($3.tr_entry, TR_NIL));
		|
	ptr_id '^'
		= $$.tr_entry = setupvar($1.cptr, tree1(T_PTR));
		|
	qual_var '^'
		= $1.tr_entry->var_node.qual = 
			addlist($1.tr_entry->var_node.qual, tree1(T_PTR));
		;

/*
 * Expression with write widths
 */
wexpr:
	expr
		|
	expr ':' expr
		= $$.tr_entry = tree4(T_WEXP, $1.i_entry, $3.tr_entry, TR_NIL);
		|
	expr ':' expr ':' expr
		= $$.tr_entry = tree4(T_WEXP, $1.i_entry, $3.tr_entry,
						$5.tr_entry);
		|
	expr octhex
		= $$.tr_entry = tree4(T_WEXP, $1.i_entry, TR_NIL, $2.tr_entry);
		|
	expr ':' expr octhex
		= $$.tr_entry = tree4(T_WEXP, $1.i_entry, $3.tr_entry,
					$4.tr_entry);
		;
octhex:
	YOCT
		= $$.i_entry = OCT;
		|
	YHEX
		= $$.i_entry = HEX;
		;

expr_list:
	expr
		= $$.tr_entry = newlist($1.tr_entry);
		|
	expr_list ',' expr
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

wexpr_list:
	wexpr
		= $$.tr_entry = newlist($1.tr_entry);
		|
	wexpr_list ',' wexpr
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

/*
 * OPERATORS
 */

relop:
	'='	= $$.i_entry = T_EQ;
		|
	'<'	= $$.i_entry = T_LT;
		|
	'>'	= $$.i_entry = T_GT;
		|
	'<' '>'	= $$.i_entry = T_NE;
		|
	'<' '='	= $$.i_entry = T_LE;
		|
	'>' '='	= $$.i_entry = T_GE;
		|
	YIN	= $$.i_entry = T_IN;
		;
addop:
	'+'	= $$.i_entry = T_ADD;
		|
	'-'	= $$.i_entry = T_SUB;
		|
	YOR	= $$.i_entry = T_OR;
		|
	'|'	= $$.i_entry = T_OR;
		;
divop:
	'*'	= $$.i_entry = T_MULT;
		|
	'/'	= $$.i_entry = T_DIVD;
		|
	YDIV	= $$.i_entry = T_DIV;
		|
	YMOD	= $$.i_entry = T_MOD;
		|
	YAND	= $$.i_entry = T_AND;
		|
	'&'	= $$.i_entry = T_AND;
		;

negop:
	YNOT
		|
	'~'
		;

/*
 * LISTS
 */

var_list:
	variable
		= $$.tr_entry = newlist($1.tr_entry);
		|
	var_list ',' variable
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

id_list:
	YID
		= $$.tr_entry = newlist($1.tr_entry);
		|
	id_list ',' YID
		= $$.tr_entry = addlist($1.tr_entry, $3.tr_entry);
		;

/*
 * Identifier productions with semantic restrictions
 *
 * For these productions, the characters @@ signify
 * that the associated C statement is to provide
 * the semantic restriction for this reduction.
 * These lines are made into a procedure yyEactr, similar to
 * yyactr, which determines whether the corresponding reduction
 * is permitted, or whether an error is to be signaled.
 * A zero return from yyEactr is considered an error.
 * YyEactr is called with an argument "var" giving the string
 * name of the variable in question, essentially $1, although
 * $1 will not work because yyEactr is called from loccor in
 * the recovery routines.
 */

const_id:
	YID
		= @@ return (identis(var, CONST));
		;
type_id:
	YID
		= {
			@@ return (identis(var, TYPE));
			$$.tr_entry = tree3(T_TYID, lineof(yyline), $1.tr_entry);
		  }
		;
var_id:
	YID
		= @@ return (identis(var, VAR));
		;
array_id:
	YID
		= @@ return (identis(var, ARRAY));
		;
ptr_id:
	YID
		= @@ return (identis(var, PTRFILE));
		;
record_id:
	YID
		= @@ return (identis(var, RECORD));
		;
field_id:
	YID
		= @@ return (identis(var, FIELD));
		;
proc_id:
	YID
		= @@ return (identis(var, PROC));
		;
func_id:
	YID
		= @@ return (identis(var, FUNC));
		;
