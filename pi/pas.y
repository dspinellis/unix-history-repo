/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
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
 *	   by processing action lines containing `@'.
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
	YFOR		YFORWARD	YFUNCTION	YGOTO
	YID		YIF		YIN		YINT
	YLABEL		YMOD		YNOT		YNUMB
	YOF		YOR		YPACKED		YNIL
	YPROCEDURE	YPROG		YRECORD		YREPEAT
	YSET		YSTRING		YTHEN		YDOWNTO
	YTYPE		YUNTIL		YVAR		YWHILE
	YWITH		YBINT		YOCT		YHEX
	YASSERT		YCASELAB	YILLCH		YLAST

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

/*
 * The following line marks the end of the yacc
 * Constant definitions which are removed from
 * y.tab.c and placed in the file y.tab.h.
 */
##

#include "whoami"
#include "0.h"
#include "yy.h"
#include "tree.h"

#ifdef PI
#define	lineof(l)	l
#endif

%}

%%

/*
 * PRODUCTIONS
 */

goal:
	prog_hedr decls procs block '.'
		= funcend($1, $4, lineof($5));
		;

prog_hedr:
	YPROG YID '(' id_list ')' ';' 
		= $$ = funcbody(funchdr(tree5(T_PROG, lineof($1), $2, fixlist($4), NIL)));
		|
	YPROG error
		= {
			yyPerror("Malformed program statement", PPROG);
			/*
			 * Should make a program statement
			 * with "input" and "output" here.
			 */
			$$ = funcbody(funchdr(tree5(T_PROG, lineof($1), NIL, NIL, NIL)));
		  }
		;
block:
	YBEGIN stat_list YEND
		= {
			$$ = tree2(lineof($1), fixstlist($2));
			if ($3.pint < 0)
				brerror($1, "begin");
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
Derror:
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
		;

/*
 * LABEL PART
 */

labels:
	YLABEL label_decl ';'
		= label(fixlist($2), lineof($1));
		;
label_decl:
	YINT
		= $$ = newlist($1 == NIL ? NIL : *hash($1, 1));
		|
	label_decl ',' YINT
		= $$ = addlist($1, $3 == NIL ? NIL : *hash($3, 1));
		;

/*
 * CONST PART
 */

const_decl:
	YCONST YID '=' const ';'
		= constbeg($1), const(lineof($3), $2, $4);
		|
	const_decl YID '=' const ';'
		= const(lineof($3), $2, $4);
		|
	YCONST error
		= {
			constbeg();
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
		= typebeg($1), type(lineof($3), $2, $4);
		|
	type_decl YID '=' type ';'
		= type(lineof($3), $2, $4);
		|
	YTYPE error
		= {
			typebeg();
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
		= varbeg($1), var(lineof($3), fixlist($2), $4);
		|
	var_decl id_list ':' type ';'
		= var(lineof($3), fixlist($2), $4);
		|
	YVAR error 
		= {
			varbeg();
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

procs:
	/* lambda */
		|
	procs proc
		= trfree();
		;
proc:
	phead YFORWARD ';'
		= funcfwd($1);
		|
	pheadres decls procs block ';'
		= funcend($1, $4, lineof($5));
		;
pheadres:
	phead
		= funcbody($1);
		;
phead:
	porf YID params ftype ';'
		= $$ = funchdr(tree5($1, lineof($5), $2, $3, $4));
		;
porf:
	YPROCEDURE
		= $$ = T_PDEC;
		|
	YFUNCTION
		= $$ = T_FDEC;
		;
params:
	'(' param_list ')'
		= $$ = fixlist($2);
		|
	/* lambda */
		= $$ = NIL;
		;

/*
 * PARAMETERS
 */

param:
	id_list ':' type
		= $$ = tree3(T_PVAL, fixlist($1), $3);
		|
	YVAR id_list ':' type
		= $$ = tree3(T_PVAR, fixlist($2), $4);
		|
	YFUNCTION id_list ':' type
		= $$ = tree3(T_PFUNC, fixlist($2), $4);
		|
	YPROCEDURE id_list
		= $$ = tree2(T_PPROC, fixlist($2));
		;
ftype:
	':' type
		= $$ = $2;
		|
	/* lambda */
		= $$ = NIL;
		;
param_list:
	param
		= $$ = newlist($1);
		|
	param_list ';' param
		= $$ = addlist($1, $3);
		;

/*
 * CONSTANTS
 */

const:
	YSTRING
		= $$ = tree2(T_CSTRNG, $1);
		|
	number
		|
	'+' number
		= $$ = tree2(T_PLUSC, $2);
		|
	'-' number
		= $$ = tree2(T_MINUSC, $2);
		;
number:
	const_id
		= $$ = tree2(T_ID, $1);
		|
	YINT
		= $$ = tree2(T_CINT, $1);
		|
	YBINT
		= $$ = tree2(T_CBINT, $1);
		|
	YNUMB
		= $$ = tree2(T_CFINT, $1);
		;
const_list:
	const
		= $$ = newlist($1);
		|
	const_list ',' const
		= $$ = addlist($1, $3);
		;

/*
 * TYPES
 */

type:
	simple_type
		|
	'^' YID
		= $$ = tree3(T_TYPTR, lineof($1), tree2(T_ID, $2));
		|
	struct_type
		|
	YPACKED struct_type
		= $$ = tree3(T_TYPACK, lineof($1), $2);
		;
simple_type:
	type_id
		|
	'(' id_list ')'
		= $$ = tree3(T_TYSCAL, lineof($1), fixlist($2));
		|
	const YDOTDOT const
		= $$ = tree4(T_TYRANG, lineof($2), $1, $3);
		;
struct_type:
	YARRAY '[' simple_type_list ']' YOF type
		= $$ = tree4(T_TYARY, lineof($1), fixlist($3), $6);
		|
	YFILE YOF type
		= $$ = tree3(T_TYFILE, lineof($1), $3);
		|
	YSET YOF simple_type
		= $$ = tree3(T_TYSET, lineof($1), $3);
		|
	YRECORD field_list YEND
		= {
			$$ = tree3(T_TYREC, lineof($1), $2);
			if ($3.pint < 0)
				brerror($1, "record");
		  }
		;
simple_type_list:
	simple_type
		= $$ = newlist($1);
		|
	simple_type_list ',' simple_type
		= $$ = addlist($1, $3);
		;

/*
 * RECORD TYPE
 */
field_list:
	fixed_part variant_part
		= $$ = tree4(T_LISTPP, lineof(NIL), fixlist($1), $2);
		;
fixed_part:
	field
		= $$ = newlist($1);
		|
	fixed_part ';' field
		= $$ = addlist($1, $3);
		|
	fixed_part error
		= yyPerror("Malformed record declaration", PDECL);
		;
field:
	/* lambda */
		= $$ = NIL;
		|
	id_list ':' type
		= $$ = tree4(T_FIELD, lineof($2), fixlist($1), $3);
		;

variant_part:
	/* lambda */
		= $$ = NIL;
		|
	YCASE type_id YOF variant_list
		= $$ = tree5(T_TYVARPT, lineof($1), NIL, $2, fixlist($4));
		|
	YCASE YID ':' type_id YOF variant_list
		= $$ = tree5(T_TYVARPT, lineof($1), $2, $4, fixlist($6));
		;
variant_list:
	variant
		= $$ = newlist($1);
		|
	variant_list ';' variant
		= $$ = addlist($1, $3);
		|
	variant_list error
		= yyPerror("Malformed record declaration", PDECL);
		;
variant:
	/* lambda */
		= $$ = NIL;
		|
	const_list ':' '(' field_list ')'
		= $$ = tree5(T_TYVARNT, lineof($2), fixlist($1), $4);
		|
	const_list ':' '(' ')'
		= $$ = tree5(T_TYVARNT, lineof($2), fixlist($1), NIL);
		;

/*
 * STATEMENT LIST
 */

stat_list:
	stat
		= $$ = newlist($1);
		|
	stat_lsth stat
		= {
			if ((p = $1) != NIL && (q = p[1])[0] == T_IFX) {
				q[0] = T_IFEL;
				q[4] = $2;
			} else
				$$ = addlist($1, $2);
		  }
		;

stat_lsth:
	stat_list ';'
		= if ((q = $1) != NIL && (p = q[1]) != NIL && p[0] == T_IF) {
			if (yychar < 0)
				yychar = yylex();
			if (yyshifts >= 2 && yychar == YELSE) {
				recovered();
				copy(&Y, &OY, sizeof Y);
				yerror("Deleted ';' before keyword else");
				yychar = yylex();
				p[0] = T_IFX;
			}
		  }
		;

/*
 * CASE STATEMENT LIST
 */

cstat_list:
	cstat
		= $$ = newlist($1);
		|
	cstat_list ';' cstat
		= $$ = addlist($1, $3);
		|
	error
		= {
			$$ = NIL;
Kerror:
			yyPerror("Malformed statement in case", PSTAT);
		  }
		|
	cstat_list error
		= goto Kerror;
		;

cstat:
	const_list ':' stat
		= $$ = tree4(T_CSTAT, lineof($2), fixlist($1), $3);
		|
	YCASELAB stat
		= $$ = tree4(T_CSTAT, lineof($1), NIL, $2);
		|
	/* lambda */
		= $$ = NIL;
		;

/*
 * STATEMENT
 */

stat:
	/* lambda */
		= $$ = NIL;
		|
	YINT ':' stat
		= $$ = tree4(T_LABEL, lineof($2), $1 == NIL ? NIL : *hash($1, 1), $3);
		|
	proc_id
		= $$ = tree4(T_PCALL, lineof(yyline), $1, NIL);
		|
	proc_id '(' wexpr_list ')'
		= $$ = tree4(T_PCALL, lineof($2), $1, fixlist($3));
		|
	YID error
		= goto NSerror;
		|
	assign
		|
	YBEGIN stat_list YEND
		= {
			$$ = tree3(T_BLOCK, lineof($1), fixstlist($2));
			if ($3.pint < 0)
				brerror($1, "begin");
		  }
		|
	YCASE expr YOF cstat_list YEND
		= {
			$$ = tree4(T_CASE, lineof($1), $2, fixlist($4));
			if ($5.pint < 0)
				brerror($1, "case");
		  }
		|
	YWITH var_list YDO stat
		= $$ = tree4(T_WITH, lineof($1), fixlist($2), $4);
		|
	YWHILE expr YDO stat
		= $$ = tree4(T_WHILE, lineof($1), $2, $4);
		|
	YREPEAT stat_list YUNTIL expr
		= $$ = tree4(T_REPEAT, lineof($3), fixstlist($2), $4);
		|
	YFOR assign YTO expr YDO stat
		= $$ = tree5(T_FORU, lineof($1), $2, $4, $6);
		|
	YFOR assign YDOWNTO expr YDO stat
		= $$ = tree5(T_FORD, lineof($1), $2, $4, $6);
		|
	YGOTO YINT
		= $$ = tree3(T_GOTO, lineof($1), *hash($2, 1));
		|
	YIF expr YTHEN stat
		= $$ = tree5(T_IF, lineof($1), $2, $4, NIL);
		|
	YIF expr YTHEN stat YELSE stat
		= $$ = tree5(T_IFEL, lineof($1), $2, $4, $6);
		|
	YIF expr YTHEN stat YELSE
		= $$ = tree5(T_IFEL, lineof($1), $2, $4, NIL);
		|
	YASSERT expr
		= $$ = tree3(T_ASRT, lineof($1), $2);
		|
	error
		= {
NSerror:
			$$ = NIL;
Serror:
			yyPerror("Malformed statement", PSTAT);
		  }
		;
assign:
	variable ':' '=' expr
		= $$ = tree4(T_ASGN, lineof($2), $1, $4);
		;

/*
 * EXPRESSION
 */

expr:
	error
		= {
NEerror:
			$$ = NIL;
Eerror:
			yyPerror("Missing/malformed expression", PEXPR);
		  }
		|
	expr relop expr			%prec '<'
		= $$ = tree4($2, $1[1] == SAWCON ? $3[1] : $1[1], $1, $3);
		|
	'+' expr			%prec UNARYSIGN
		= $$ = tree3(T_PLUS, $2[1], $2);
		|
	'-' expr			%prec UNARYSIGN
		= $$ = tree3(T_MINUS, $2[1], $2);
		|
	expr addop expr			%prec '+'
		= $$ = tree4($2, $1[1] == SAWCON ? $3[1] : $1[1], $1, $3);
		|
	expr divop expr			%prec '*'
		= $$ = tree4($2, $1[1] == SAWCON ? $3[1] : $1[1], $1, $3);
		|
	YNIL
		= $$ = tree2(T_NIL, NOCON);
		|
	YSTRING
		= $$ = tree3(T_STRNG, SAWCON, $1);
		|
	YINT
		= $$ = tree3(T_INT, NOCON, $1);
		|
	YBINT
		= $$ = tree3(T_BINT, NOCON, $1);
		|
	YNUMB
		= $$ = tree3(T_FINT, NOCON, $1);
		|
	variable
		|
	YID error
		= goto NEerror;
		|
	func_id '(' wexpr_list ')'
		= $$ = tree3(T_FCALL, $1, fixlist($3));
		|
	'(' expr ')'
		= $$ = $2;
		|
	YNOT expr
		= $$ = tree3(T_NOT, NOCON, $2);
		|
	'[' element_list ']'
		= $$ = tree3(T_CSET, SAWCON, fixlist($2));
		|
	'[' ']'
		= $$ = tree3(T_CSET, SAWCON, NIL);
		;

element_list:
	element
		= $$ = newlist($1);
		|
	element_list ',' element
		= $$ = addlist($1, $3);
		;
element:
	expr
		|
	expr YDOTDOT expr
		= $$ = tree3(T_RANG, $1, $3);
		;

/*
 * QUALIFIED VARIABLES
 */

variable:
	YID
		= {
			@ return (identis(var, VAR));
			$$ = setupvar($1, NIL);
		  }
		|
	qual_var
		= $1[3] = fixlist($1[3]);
		;
qual_var:
	array_id '[' expr_list ']'
		= $$ = setupvar($1, tree2(T_ARY, fixlist($3)));
		|
	qual_var '[' expr_list ']'
		= $1[3] = addlist($1[3], tree2(T_ARY, fixlist($3)));
		|
	record_id '.' field_id
		= $$ = setupvar($1, tree3(T_FIELD, $3, NIL));
		|
	qual_var '.' field_id
		= $1[3] = addlist($1[3], tree3(T_FIELD, $3, NIL));
		|
	ptr_id '^'
		= $$ = setupvar($1, tree1(T_PTR));
		|
	qual_var '^'
		= $1[3] = addlist($1[3], tree1(T_PTR));
		;

/*
 * Expression with write widths
 */
wexpr:
	expr
		|
	expr ':' expr
		= $$ = tree4(T_WEXP, $1, $3, NIL);
		|
	expr ':' expr ':' expr
		= $$ = tree4(T_WEXP, $1, $3, $5);
		|
	expr octhex
		= $$ = tree4(T_WEXP, $1, NIL, $2);
		|
	expr ':' expr octhex
		= $$ = tree4(T_WEXP, $1, $3, $4);
		;
octhex:
	YOCT
		= $$ = OCT;
		|
	YHEX
		= $$ = HEX;
		;

expr_list:
	expr
		= $$ = newlist($1);
		|
	expr_list ',' expr
		= $$ = addlist($1, $3);
		;

wexpr_list:
	wexpr
		= $$ = newlist($1);
		|
	wexpr_list ',' wexpr
		= $$ = addlist($1, $3);
		;

/*
 * OPERATORS
 */

relop:
	'='	= $$ = T_EQ;
		|
	'<'	= $$ = T_LT;
		|
	'>'	= $$ = T_GT;
		|
	'<' '>'	= $$ = T_NE;
		|
	'<' '='	= $$ = T_LE;
		|
	'>' '='	= $$ = T_GE;
		|
	YIN	= $$ = T_IN;
		;
addop:
	'+'	= $$ = T_ADD;
		|
	'-'	= $$ = T_SUB;
		|
	YOR	= $$ = T_OR;
		|
	'|'	= $$ = T_OR;
		;
divop:
	'*'	= $$ = T_MULT;
		|
	'/'	= $$ = T_DIVD;
		|
	YDIV	= $$ = T_DIV;
		|
	YMOD	= $$ = T_MOD;
		|
	YAND	= $$ = T_AND;
		|
	'&'	= $$ = T_AND;
		;

/*
 * LISTS
 */

var_list:
	variable
		= $$ = newlist($1);
		|
	var_list ',' variable
		= $$ = addlist($1, $3);
		;

id_list:
	YID
		= $$ = newlist($1);
		|
	id_list ',' YID
		= $$ = addlist($1, $3);
		;

/*
 * Identifier productions with semantic restrictions
 *
 * For these productions, the character @ signifies
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
		= @ return (identis(var, CONST));
		;
type_id:
	YID
		= {
			@ return (identis(var, TYPE));
			$$ = tree3(T_TYID, lineof(yyline), $1);
		  }
		;
var_id:
	YID
		= @ return (identis(var, VAR));
		;
array_id:
	YID
		= @ return (identis(var, ARRAY));
		;
ptr_id:
	YID
		= @ return (identis(var, PTRFILE));
		;
record_id:
	YID
		= @ return (identis(var, RECORD));
		;
field_id:
	YID
		= @ return (identis(var, FIELD));
		;
proc_id:
	YID
		= @ return (identis(var, PROC));
		;
func_id:
	YID
		= @ return (identis(var, FUNC));
		;
