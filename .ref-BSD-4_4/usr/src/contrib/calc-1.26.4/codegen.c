/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Module to generate opcodes from the input tokens.
 */

#include "calc.h"
#include "token.h"
#include "symbol.h"
#include "label.h"
#include "opcodes.h"
#include "string.h"
#include "func.h"
#include "config.h"
#include "hist.h"


FUNC *curfunc;

static BOOL getfilename(), getid();
static void getshowcommand(), getfunction(), getbody(), getdeclarations();
static void getstatement(), getobjstatement(), getobjvars();
static void getmatstatement(), getsimplebody();
static void getcondition(), getmatargs(), getelement(), checksymbol();
static void getcallargs();
static int getexprlist(), getassignment(), getaltcond(), getorcond();
static int getandcond(), getrelation(), getsum(), getproduct();
static int getorexpr(), getandexpr(), getshiftexpr(), getterm();
static int getidexpr();

/*
 * Read all the commands from an input file.
 * These are either declarations, or else are commands to execute now.
 * In general, commands are terminated by newlines or semicolons.
 * Exceptions are function definitions and escaped newlines.
 * Commands are read and executed until the end of file.
 */
void
getcommands()
{
	char name[PATHSIZE+1];	/* program name */

	for (;;) {
		tokenmode(TM_NEWLINES);
		switch (gettoken()) {

		case T_DEFINE:
			getfunction();
			break;

		case T_EOF:
			return;

		case T_HELP:
			if (!getfilename(name, FALSE)) {
				strcpy(name, DEFAULTCALCHELP);
			}
			givehelp(name);
			break;

		case T_READ:
			if (!getfilename(name, TRUE))
				break;
			if (opensearchfile(name, calcpath, CALCEXT) < 0) {
				scanerror(T_NULL, "Cannot open \"%s\"\n", name);
				break;
			}
			getcommands();
			break;

		case T_WRITE:
			if (!getfilename(name, TRUE))
				break;
			if (writeglobals(name))
				scanerror(T_NULL, "Error writing \"%s\"\n", name);
			break;

		case T_SHOW:
			rescantoken();
			getshowcommand();
			break;

		case T_NEWLINE:
		case T_SEMICOLON:
			break;

		default:
			rescantoken();
			initstack();
			if (evaluate(FALSE))
				updateoldvalue(curfunc);
		}
	}
}


/*
 * Evaluate a line of statements.
 * This is done by treating the current line as a function body,
 * compiling it, and then executing it.  Returns TRUE if the line
 * successfully compiled and executed.  The last expression result
 * is saved in the f_savedvalue element of the current function.
 * The nestflag variable should be FALSE for the outermost evaluation
 * level, and TRUE for all other calls (such as the 'eval' function).
 * The function name begins with an asterisk to indicate specialness.
 */
BOOL
evaluate(nestflag)
	BOOL nestflag;		/* TRUE if this is a nested evaluation */
{
	char *funcname;
	BOOL gotstatement;

	funcname = (nestflag ? "**" : "*");
	beginfunc(funcname, nestflag);
	gotstatement = FALSE;
	for (;;) {
		switch (gettoken()) {
			case T_SEMICOLON:
				break;

			case T_EOF:
				rescantoken();
				goto done;

			case T_NEWLINE:
				goto done;

			case T_GLOBAL:
			case T_LOCAL:
				if (gotstatement) {
					scanerror(T_SEMICOLON, "Declarations must be used before code");
					return FALSE;
				}
				rescantoken();
				getdeclarations();
				break;

			default:
				rescantoken();
				getstatement(NULL, NULL, NULL, NULL);
				gotstatement = TRUE;
		}
	}

done:
	addop(OP_UNDEF);
	addop(OP_RETURN);
	checklabels();
	if (errorcount)
		return FALSE;
	calculate(curfunc, 0);
	return TRUE;
}


/*
 * Get a function declaration.
 * func = name '(' '' | name [ ',' name] ... ')' simplebody
 *	| name '(' '' | name [ ',' name] ... ')' body.
 */
static void
getfunction()
{
	char *name;		/* parameter name */
	int type;		/* type of token read */

	tokenmode(TM_DEFAULT);
	if (gettoken() != T_SYMBOL) {
		scanerror(T_NULL, "Function name expected");
		return;
	}
	beginfunc(tokenstring(), FALSE);
	if (gettoken() != T_LEFTPAREN) {
		scanerror(T_SEMICOLON, "Left parenthesis expected for function");
		return;
	}
	for (;;) {
		type = gettoken();
		if (type == T_RIGHTPAREN)
			break;
		if (type != T_SYMBOL) {
			scanerror(T_COMMA, "Bad function definition");
			return;
		}
		name = tokenstring();
		switch (symboltype(name)) {
			case SYM_UNDEFINED:
			case SYM_GLOBAL:
				(void) addparam(name);
				break;
			default:
				scanerror(T_NULL, "Parameter \"%s\" is already defined", name);
		}
		type = gettoken();
		if (type == T_RIGHTPAREN)
			break;
		if (type != T_COMMA) {
			scanerror(T_COMMA, "Bad function definition");
			return;
		}
	}
	switch (gettoken()) {
		case T_ASSIGN:
			rescantoken();
			getsimplebody();
			break;
		case T_LEFTBRACE:
			rescantoken();
			getbody(NULL, NULL, NULL, NULL, TRUE);
			break;
		default:
			scanerror(T_NULL,
				"Left brace or equals sign expected for function");
			return;
	}
	addop(OP_UNDEF);
	addop(OP_RETURN);
	endfunc();
}


/*
 * Get a simple assignment style body for a function declaration.
 * simplebody = '=' assignment '\n'.
 */
static void
getsimplebody()
{
	if (gettoken() != T_ASSIGN) {
		scanerror(T_SEMICOLON, "Missing equals for simple function body");
		return;
	}
	tokenmode(TM_NEWLINES);
	(void) getexprlist();
	addop(OP_RETURN);
	if (gettoken() != T_SEMICOLON)
		rescantoken();
	if (gettoken() != T_NEWLINE)
		scanerror(T_NULL, "Illegal function definition");
}


/*
 * Get the body of a function, or a subbody of a function.
 * body = '{' [ declarations ] ... [ statement ] ... '}'
 *	| [ declarations ] ... [statement ] ... '\n'
 */
static void
getbody(contlabel, breaklabel, nextcaselabel, defaultlabel, toplevel)
	LABEL *contlabel, *breaklabel, *nextcaselabel, *defaultlabel;
	BOOL toplevel;
{
	BOOL gotstatement;	/* TRUE if seen a real statement yet */

	if (gettoken() != T_LEFTBRACE) {
		scanerror(T_SEMICOLON, "Missing left brace for function body");
		return;
	}
	gotstatement = FALSE;
	for (;;) {
		switch (gettoken()) {
		case T_RIGHTBRACE:
			return;

		case T_GLOBAL:
		case T_LOCAL:
			if (!toplevel) {
				scanerror(T_SEMICOLON, "Declarations must be at the top of the function");
				return;
			}
			if (gotstatement) {
				scanerror(T_SEMICOLON, "Declarations must be used before code");
				return;
			}
			rescantoken();
			getdeclarations();
			break;

		default:
			rescantoken();
			getstatement(contlabel, breaklabel, nextcaselabel, defaultlabel);
			gotstatement = TRUE;
		}
	}
}


/*
 * Get a line of local or global variable declarations.
 * declarations = { LOCAL | GLOBAL } name [ ',' name ] ... ';'.
 */
static void
getdeclarations()
{
	int type;		/* type of declaration */
	char *name;		/* name of symbol seen */

	switch (gettoken()) {
		case T_LOCAL:
			type = SYM_LOCAL;
			break;
		case T_GLOBAL:
			type = SYM_GLOBAL;
			break;
		default:
			rescantoken();
			return;
	}
	for (;;) {
		if (gettoken() != T_SYMBOL) {
			scanerror(T_SEMICOLON, "Variable name expected for declaration statement");
			return;
		}
		name = tokenstring();
		switch (symboltype(name)) {
		case SYM_UNDEFINED:
		case SYM_GLOBAL:
			if (type == SYM_LOCAL)
				(void) addlocal(name);
			else
				(void) addglobal(name);
			break;
		case SYM_PARAM:
		case SYM_LOCAL:
			scanerror(T_NULL, "variable \"%s\" is already defined", name);
			break;
		}
		switch (gettoken()) {
			case T_COMMA:
				break;
			case T_NEWLINE:
			case T_SEMICOLON:
				return;
			default:
				scanerror(T_SEMICOLON, "Bad syntax in declaration statement");
				return;
		}
	}
}


/*
 * Get a statement.
 * statement = IF condition statement [ELSE statement]
 *	| FOR '(' [assignment] ';' [assignment] ';' [assignment] ')' statement
 *	| WHILE condition statement
 *	| DO statement WHILE condition ';'
 *	| SWITCH condition '{' [caseclause] ... '}'
 *	| CONTINUE ';'
 *	| BREAK ';'
 *	| RETURN assignment ';'
 *	| GOTO label ';'
 *	| MAT name '[' value [ ':' value ] [',' value [ ':' value ] ] ']' ';'
 *	| OBJ type '{' arg [ ',' arg ] ... '}' ] ';'
 *	| OBJ type name [ ',' name ] ';'
 *	| PRINT assignment [, assignment ] ... ';'
 *	| QUIT [ string ] ';'
 *	| SHOW item ';'
 *	| body
 *	| assignment ';'
 *	| label ':' statement
 *	| ';'.
 */
static void
getstatement(contlabel, breaklabel, nextcaselabel, defaultlabel)
	LABEL *contlabel;	/* label for continue statement */
	LABEL *breaklabel;	/* label for break statement */
	LABEL *nextcaselabel;	/* label for next case statement */
	LABEL *defaultlabel;	/* label for default case */
{
	LABEL label1, label2, label3, label4;	/* locations for jumps */
	int type;
	BOOL printeol;

	addopindex(OP_DEBUG, linenumber());
	switch (gettoken()) {
	case T_NEWLINE:
		rescantoken();
		return;

	case T_SEMICOLON:
		return;

	case T_RIGHTBRACE:
		scanerror(T_NULL, "Extraneous right brace");
		return;

	case T_CONTINUE:
		if (contlabel == NULL) {
			scanerror(T_SEMICOLON, "CONTINUE not within FOR, WHILE, or DO");
			return;
		}
		addoplabel(OP_JUMP, contlabel);
		break;

	case T_BREAK:
		if (breaklabel == NULL) {
			scanerror(T_SEMICOLON, "BREAK not within FOR, WHILE, or DO");
			return;
		}
		addoplabel(OP_JUMP, breaklabel);
		break;

	case T_GOTO:
		if (gettoken() != T_SYMBOL) {
			scanerror(T_SEMICOLON, "Missing label in goto");
			return;
		}
		addop(OP_JUMP);
		addlabel(tokenstring());
		break;

	case T_RETURN:
		switch (gettoken()) {
			case T_NEWLINE:
			case T_SEMICOLON:
				addop(OP_UNDEF);
				addop(OP_RETURN);
				return;
			default:
				rescantoken();
				(void) getexprlist();
				if (curfunc->f_name[0] == '*')
					addop(OP_SAVE);
				addop(OP_RETURN);
		}
		break;

	case T_LEFTBRACE:
		rescantoken();
		getbody(contlabel, breaklabel, nextcaselabel, defaultlabel, FALSE);
		return;

	case T_IF:
		clearlabel(&label1);
		clearlabel(&label2);
		getcondition();
		addoplabel(OP_JUMPEQ, &label1);
		getstatement(contlabel, breaklabel, (LABEL*)NULL, (LABEL*)NULL);
		if (gettoken() != T_ELSE) {
			setlabel(&label1);
			rescantoken();
			return;
		}
		addoplabel(OP_JUMP, &label2);
		setlabel(&label1);
		getstatement(contlabel, breaklabel, (LABEL*)NULL, (LABEL*)NULL);
		setlabel(&label2);
		return;

	case T_FOR:	/* for (a; b; c) x */
		clearlabel(&label1);
		clearlabel(&label2);
		clearlabel(&label3);
		clearlabel(&label4);
		contlabel = NULL;
		breaklabel = &label4;
		if (gettoken() != T_LEFTPAREN) {
			scanerror(T_SEMICOLON, "Left parenthesis expected");
			return;
		}
		if (gettoken() != T_SEMICOLON) {	/* have 'a' part */
			rescantoken();
			(void) getexprlist();
			addop(OP_POP);
			if (gettoken() != T_SEMICOLON) {
				scanerror(T_SEMICOLON, "Missing semicolon");
				return;
			}
		}
		if (gettoken() != T_SEMICOLON) {	/* have 'b' part */
			setlabel(&label1);
			contlabel = &label1;
			rescantoken();
			(void) getexprlist();
			addoplabel(OP_JUMPNE, &label3);
			addoplabel(OP_JUMP, breaklabel);
			if (gettoken() != T_SEMICOLON) {
				scanerror(T_SEMICOLON, "Missing semicolon");
				return;
			}
		}
		if (gettoken() != T_RIGHTPAREN) {	/* have 'c' part */
			if (label1.l_offset <= 0)
				addoplabel(OP_JUMP, &label3);
			setlabel(&label2);
			contlabel = &label2;
			rescantoken();
			(void) getexprlist();
			addop(OP_POP);
			if (label1.l_offset > 0)
				addoplabel(OP_JUMP, &label1);
			if (gettoken() != T_RIGHTPAREN) {
				scanerror(T_SEMICOLON, "Right parenthesis expected");
				return;
			}
		}
		setlabel(&label3);
		if (contlabel == NULL)
			contlabel = &label3;
		getstatement(contlabel, breaklabel, (LABEL*)NULL, (LABEL*)NULL);
		addoplabel(OP_JUMP, contlabel);
		setlabel(breaklabel);
		return;

	case T_WHILE:
		contlabel = &label1;
		breaklabel = &label2;
		clearlabel(contlabel);
		clearlabel(breaklabel);
		setlabel(contlabel);
		getcondition();
		addoplabel(OP_JUMPEQ, breaklabel);
		getstatement(contlabel, breaklabel, (LABEL*)NULL, (LABEL*)NULL);
		addoplabel(OP_JUMP, contlabel);
		setlabel(breaklabel);
		return;

	case T_DO:
		contlabel = &label1;
		breaklabel = &label2;
		clearlabel(contlabel);
		clearlabel(breaklabel);
		clearlabel(&label3);
		setlabel(&label3);
		getstatement(contlabel, breaklabel, (LABEL*)NULL, (LABEL*)NULL);
		if (gettoken() != T_WHILE) {
			scanerror(T_SEMICOLON, "WHILE keyword expected for DO statement");
			return;
		}
		setlabel(contlabel);
		getcondition();
		addoplabel(OP_JUMPNE, &label3);
		setlabel(breaklabel);
		return;

	case T_SWITCH:
		breaklabel = &label1;
		nextcaselabel = &label2;
		defaultlabel = &label3;
		clearlabel(breaklabel);
		clearlabel(nextcaselabel);
		clearlabel(defaultlabel);
		getcondition();
		if (gettoken() != T_LEFTBRACE) {
			scanerror(T_SEMICOLON, "Missing left brace for switch statement");
			return;
		}
		addoplabel(OP_JUMP, nextcaselabel);
		rescantoken();
		getstatement(contlabel, breaklabel, nextcaselabel, defaultlabel);
		addoplabel(OP_JUMP, breaklabel);
		setlabel(nextcaselabel);
		if (defaultlabel->l_offset > 0)
			addoplabel(OP_JUMP, defaultlabel);
		else
			addop(OP_POP);
		setlabel(breaklabel);
		return;

	case T_CASE:
		if (nextcaselabel == NULL) {
			scanerror(T_SEMICOLON, "CASE not within SWITCH statement");
			return;
		}
		clearlabel(&label1);
		addoplabel(OP_JUMP, &label1);
		setlabel(nextcaselabel);
		clearlabel(nextcaselabel);
		(void) getexprlist();
		if (gettoken() != T_COLON) {
			scanerror(T_SEMICOLON, "Colon expected after CASE expression");
			return;
		}
		addoplabel(OP_CASEJUMP, nextcaselabel);
		setlabel(&label1);
		getstatement(contlabel, breaklabel, nextcaselabel, defaultlabel);
		return;

	case T_DEFAULT:
		if (gettoken() != T_COLON) {
			scanerror(T_SEMICOLON, "Colon expected after DEFAULT keyword");
			return;
		}
		if (defaultlabel == NULL) {
			scanerror(T_SEMICOLON, "DEFAULT not within SWITCH statement");
			return;
		}
		if (defaultlabel->l_offset > 0) {
			scanerror(T_SEMICOLON, "Multiple DEFAULT clauses in SWITCH");
			return;
		}
		clearlabel(&label1);
		addoplabel(OP_JUMP, &label1);
		setlabel(defaultlabel);
		addop(OP_POP);
		setlabel(&label1);
		getstatement(contlabel, breaklabel, nextcaselabel, defaultlabel);
		return;

	case T_ELSE:
		scanerror(T_SEMICOLON, "ELSE without preceeding IF");
		return;

	case T_MAT:
		getmatstatement();
		break;

	case T_OBJ:
		getobjstatement();
		break;

	case T_PRINT:
		printeol = TRUE;
		for (;;) {
			switch (gettoken()) {
				case T_RIGHTBRACE:
				case T_NEWLINE:
					rescantoken();
					/*FALLTHRU*/
				case T_SEMICOLON:
					if (printeol)
						addop(OP_PRINTEOL);
					return;
				case T_COLON:
					printeol = FALSE;
					break;
				case T_COMMA:
					printeol = TRUE;
					addop(OP_PRINTSPACE);
					break;
				case T_STRING:
					printeol = TRUE;
					addopptr(OP_PRINTSTRING, tokenstring());
					break;
				default:
					printeol = TRUE;
					rescantoken();
					(void) getassignment();
					addopindex(OP_PRINT,
						(long) PRINT_NORMAL);
			}
		}
		break;

	case T_QUIT:
		switch (gettoken()) {
			case T_STRING:
				addopptr(OP_QUIT, tokenstring());
				break;
			default:
				addopptr(OP_QUIT, NULL);
				rescantoken();
		}
		break;

	case T_SYMBOL:
		if (nextchar() == ':') {	/****HACK HACK ****/
			definelabel(tokenstring());
			getstatement(contlabel, breaklabel, 
			    (LABEL*)NULL, (LABEL*)NULL);
			return;
		}
		reread();
		/* fall into default case */

	default:
		rescantoken();
		type = getexprlist();
		if (contlabel || breaklabel || (curfunc->f_name[0] != '*')) {
			addop(OP_POP);
			break;
		}
		addop(OP_SAVE);
		if (isassign(type) || (curfunc->f_name[1] != '\0')) {
			addop(OP_POP);
			break;
		}
		addop(OP_PRINTRESULT);
		break;
	}
	switch (gettoken()) {
		case T_RIGHTBRACE:
		case T_NEWLINE:
			rescantoken();
			break;
		case T_SEMICOLON:
			break;
		default:
			scanerror(T_SEMICOLON, "Semicolon expected");
			break;
	}
}


/*
 * Read in an object definition statement.
 * This is of the following form:
 *	OBJ type [ '{' id [ ',' id ] ... '}' ]  [ objlist ].
 * The OBJ keyword has already been read.
 */
static void
getobjstatement()
{
	char *name;			/* name of object type */
	int count;			/* number of elements */
	int index;			/* current index */
	int i;				/* loop counter */
	BOOL err;			/* error flag */
	int indices[MAXINDICES];	/* indices for elements */

	err = FALSE;
	if (gettoken() != T_SYMBOL) {
		scanerror(T_SEMICOLON, "Object type name missing");
		return;
	}
	name = addliteral(tokenstring());
	if (gettoken() != T_LEFTBRACE) {
		rescantoken();
		getobjvars(name);
		return;
	}
	/*
	 * Read in the definition of the elements of the object.
	 */
	count = 0;
	for (;;) {
		if (gettoken() != T_SYMBOL) {
			scanerror(T_SEMICOLON, "Missing element name in OBJ statement");
			return;
		}
		index = addelement(tokenstring());
		for (i = 0; i < count; i++) {
			if (indices[i] == index) {
				scanerror(T_NULL, "Duplicate element name \"%s\"", tokenstring());
				err = TRUE;
				break;
			}
		}
		indices[count++] = index;
		switch (gettoken()) {
			case T_RIGHTBRACE:
				if (!err)
					(void) defineobject(name, indices, count);
				switch (gettoken()) {
					case T_SEMICOLON:
					case T_NEWLINE:
						rescantoken();
						return;
				}
				rescantoken();
				getobjvars(name);
				return;
			case T_COMMA:
			case T_SEMICOLON:
			case T_NEWLINE:
				break;
			default:
				scanerror(T_SEMICOLON, "Bad object element definition");
				return;
		}
	}
}


/*
 * Routine to collect a set of variables for the specified object type
 * and initialize them as being that type of object.
 * Here
 *	objlist = name [ ',' name] ... ';'.
 */
static void
getobjvars(name)
	char *name;		/* object name */
{
	long index;		/* index for object */

	index = checkobject(name);
	if (index < 0) {
		scanerror(T_SEMICOLON, "Object %s has not been defined yet", name);
		return;
	}
	for (;;) {
		(void) getidexpr(TRUE, TRUE);
		addopindex(OP_OBJINIT, index);
		switch (gettoken()) {
			case T_COMMA:
				break;
			case T_SEMICOLON:
			case T_NEWLINE:
				rescantoken();
				return;
			default:
				scanerror(T_SEMICOLON, "Bad OBJ statement");
				return;
		}
	}
}


/*
 * Read a matrix definition statment for a one or more dimensional matrix.
 * The MAT keyword has already been read.
 */
static void
getmatstatement()
{
	int dim;		/* dimension of matrix */

	(void) getidexpr(FALSE, TRUE);
	if (gettoken() != T_LEFTBRACKET) {
		scanerror(T_SEMICOLON, "Missing left bracket for MAT");
		return;
	}
	dim = 1;
	for (;;) {
		(void) getassignment();
		switch (gettoken()) {
			case T_RIGHTBRACKET:
			case T_COMMA:
				rescantoken();
				addop(OP_ONE);
				addop(OP_SUB);
				addop(OP_ZERO);
				break;
			case T_COLON:
				(void) getassignment();
				break;
			default:
				rescantoken();
		}
		switch (gettoken()) {
			case T_RIGHTBRACKET:
				if (gettoken() != T_LEFTBRACKET) {
					rescantoken();
					addopindex(OP_MATINIT, (long) dim);
					return;
				}
				/* proceed into comma case */
				/*FALLTHRU*/
			case T_COMMA:
				if (++dim <= MAXDIM)
					break;
				scanerror(T_SEMICOLON, "Only %d dimensions allowed", MAXDIM);
				return;
			default:
				scanerror(T_SEMICOLON, "Illegal matrix definition");
				return;
		}
	}
}


/*
 * Get a condition.
 * condition = '(' assignment ')'.
 */
static void
getcondition()
{
	if (gettoken() != T_LEFTPAREN) {
		scanerror(T_SEMICOLON, "Missing left parenthesis for condition");
		return;
	}
	(void) getexprlist();
	if (gettoken() != T_RIGHTPAREN) {
		scanerror(T_SEMICOLON, "Missing right parenthesis for condition");
		return;
	}
}


/*
 * Get an expression list consisting of one or more expressions,
 * separated by commas.  The value of the list is that of the final expression.
 * This is the top level routine for parsing expressions.
 * Returns flags describing the type of assignment or expression found.
 * exprlist = assignment [ ',' assignment ] ...
 */
static int
getexprlist()
{
	int	type;

	type = getassignment();
	while (gettoken() == T_COMMA) {
		addop(OP_POP);
		(void) getassignment();
		type = EXPR_RVALUE;
	}
	rescantoken();
	return type;
}


/*
 * Get an assignment (or possibly just an expression).
 * Returns flags describing the type of assignment or expression found.
 * assignment = lvalue '=' assignment
 *	| lvalue '+=' assignment
 *	| lvalue '-=' assignment
 *	| lvalue '*=' assignment
 *	| lvalue '/=' assignment
 *	| lvalue '%=' assignment
 *	| lvalue '//=' assignment
 *	| lvalue '&=' assignment
 *	| lvalue '|=' assignment
 *	| lvalue '<<=' assignment
 *	| lvalue '>>=' assignment
 *	| lvalue '^=' assignment
 *	| lvalue '**=' assignment
 *	| orcond.
 */
static int
getassignment()
{
	int type;		/* type of expression */
	long op;		/* opcode to generate */

	type = getaltcond();
	switch (gettoken()) {
		case T_ASSIGN:		op = 0; break;
		case T_PLUSEQUALS:	op = OP_ADD; break;
		case T_MINUSEQUALS:	op = OP_SUB; break;
		case T_MULTEQUALS:	op = OP_MUL; break;
		case T_DIVEQUALS:	op = OP_DIV; break;
		case T_SLASHSLASHEQUALS: op = OP_QUO; break;
		case T_MODEQUALS:	op = OP_MOD; break;
		case T_ANDEQUALS:	op = OP_AND; break;
		case T_OREQUALS:	op = OP_OR; break;
		case T_LSHIFTEQUALS: 	op = OP_LEFTSHIFT; break;
		case T_RSHIFTEQUALS: 	op = OP_RIGHTSHIFT; break;
		case T_POWEREQUALS:	op = OP_POWER; break;

		case T_NUMBER:
		case T_IMAGINARY:
		case T_STRING:
		case T_SYMBOL:
		case T_OLDVALUE:
		case T_LEFTPAREN:
		case T_PLUSPLUS:
		case T_MINUSMINUS:
		case T_NOT:
			scanerror(T_NULL, "Missing operator");
			return type;

		default:
			rescantoken();
			return type;
	}
	if (isrvalue(type)) {
		scanerror(T_NULL, "Illegal assignment");
		(void) getassignment();
		return (EXPR_RVALUE | EXPR_ASSIGN);
	}
	if (op)
		addop(OP_DUPLICATE);
	(void) getassignment();
	if (op) {
		addop(op);
	}
	addop(OP_ASSIGN);
	return (EXPR_RVALUE | EXPR_ASSIGN);
}


/*
 * Get a possible conditional result expression (question mark).
 * Flags are returned indicating the type of expression found.
 * altcond = orcond [ '?' orcond ':' altcond ].
 */
static int
getaltcond()
{
	int type;		/* type of expression */
	LABEL donelab;		/* label for done */
	LABEL altlab;		/* label for alternate expression */

	type = getorcond();
	if (gettoken() != T_QUESTIONMARK) {
		rescantoken();
		return type;
	}
	clearlabel(&donelab);
	clearlabel(&altlab);
	addoplabel(OP_JUMPEQ, &altlab);
	(void) getorcond();
	if (gettoken() != T_COLON) {
		scanerror(T_SEMICOLON, "Missing colon for conditional expression");
		return EXPR_RVALUE;
	}
	addoplabel(OP_JUMP, &donelab);
	setlabel(&altlab);
	(void) getaltcond();
	setlabel(&donelab);
	return EXPR_RVALUE;
}


/*
 * Get a possible conditional or expression.
 * Flags are returned indicating the type of expression found.
 * orcond = andcond [ '||' andcond ] ...
 */
static int
getorcond()
{
	int type;		/* type of expression */
	LABEL donelab;		/* label for done */

	clearlabel(&donelab);
	type = getandcond();
	while (gettoken() == T_OROR) {
		addoplabel(OP_CONDORJUMP, &donelab);
		(void) getandcond();
		type = EXPR_RVALUE;
	}
	rescantoken();
	if (donelab.l_chain > 0)
		setlabel(&donelab);
	return type;
}


/*
 * Get a possible conditional and expression.
 * Flags are returned indicating the type of expression found.
 * andcond = relation [ '&&' relation ] ...
 */
static int
getandcond()
{
	int type;		/* type of expression */
	LABEL donelab;		/* label for done */

	clearlabel(&donelab);
	type = getrelation();
	while (gettoken() == T_ANDAND) {
		addoplabel(OP_CONDANDJUMP, &donelab);
		(void) getrelation();
		type = EXPR_RVALUE;
	}
	rescantoken();
	if (donelab.l_chain > 0)
		setlabel(&donelab);
	return type;
}


/*
 * Get a possible relation (equality or inequality), or just an expression.
 * Flags are returned indicating the type of relation found.
 * relation = sum '==' sum
 *	| sum '!=' sum
 *	| sum '<=' sum
 *	| sum '>=' sum
 *	| sum '<' sum
 *	| sum '>' sum
 *	| sum.
 */
static int
getrelation()
{
	int type;		/* type of expression */
	long op;		/* opcode to generate */

	type = getsum();
	switch (gettoken()) {
		case T_EQ: op = OP_EQ; break;
		case T_NE: op = OP_NE; break;
		case T_LT: op = OP_LT; break;
		case T_GT: op = OP_GT; break;
		case T_LE: op = OP_LE; break;
		case T_GE: op = OP_GE; break;
		default:
			rescantoken();
			return type;
	}
	(void) getsum();
	addop(op);
	return EXPR_RVALUE;
}


/*
 * Get an expression made up of sums of products.
 * Flags indicating the type of expression found are returned.
 * sum = product [ {'+' | '-'} product ] ...
 */
static int
getsum()
{
	int type;		/* type of expression found */
	long op;		/* opcode to generate */

	type = getproduct();
	for (;;) {
		switch (gettoken()) {
			case T_PLUS:	op = OP_ADD; break;
			case T_MINUS:	op = OP_SUB; break;
			default:
				rescantoken();
				return type;
		}
		(void) getproduct();
		addop(op);
		type = EXPR_RVALUE;
	}
}


/*
 * Get the product of arithmetic or expressions.
 * Flags indicating the type of expression found are returned.
 * product = orexpr [ {'*' | '/' | '//' | '%'} orexpr ] ...
 */
static int
getproduct()
{
	int type;		/* type of value found */
	long op;		/* opcode to generate */

	type = getorexpr();
	for (;;) {
		switch (gettoken()) {
			case T_MULT:	op = OP_MUL; break;
			case T_DIV:	op = OP_DIV; break;
			case T_MOD:	op = OP_MOD; break;
			case T_SLASHSLASH: op = OP_QUO; break;
			default:
				rescantoken();
				return type;
		}
		(void) getorexpr();
		addop(op);
		type = EXPR_RVALUE;
	}
}


/*
 * Get an expression made up of arithmetic or operators.
 * Flags indicating the type of expression found are returned.
 * orexpr = andexpr [ '|' andexpr ] ...
 */
static int
getorexpr()
{
	int type;		/* type of value found */

	type = getandexpr();
	while (gettoken() == T_OR) {
		(void) getandexpr();
		addop(OP_OR);
		type = EXPR_RVALUE;
	}
	rescantoken();
	return type;
}


/*
 * Get an expression made up of arithmetic and operators.
 * Flags indicating the type of expression found are returned.
 * andexpr = shiftexpr [ '&' shiftexpr ] ...
 */
static int
getandexpr()
{
	int type;		/* type of value found */

	type = getshiftexpr();
	while (gettoken() == T_AND) {
		(void) getshiftexpr();
		addop(OP_AND);
		type = EXPR_RVALUE;
	}
	rescantoken();
	return type;
}


/*
 * Get a shift or power expression.
 * Flags indicating the type of expression found are returned.
 * shift = term '^' shiftexpr
 *	 | term '<<' shiftexpr
 *	 | term '>>' shiftexpr
 *	 | term.
 */
static int
getshiftexpr()
{
	int type;		/* type of value found */
	long op;		/* opcode to generate */

	type = getterm();
	switch (gettoken()) {
		case T_POWER:		op = OP_POWER; break;
		case T_LEFTSHIFT:	op = OP_LEFTSHIFT; break;
		case T_RIGHTSHIFT: 	op = OP_RIGHTSHIFT; break;
		default:
			rescantoken();
			return type;
	}
	(void) getshiftexpr();
	addop(op);
	return EXPR_RVALUE;
}


/*
 * Get a single term.
 * Flags indicating the type of value found are returned.
 * term = lvalue
 *	| lvalue '[' assignment ']'
 *	| lvalue '++'
 *	| lvalue '--'
 *	| '++' lvalue
 *	| '--' lvalue
 *	| real_number
 *	| imaginary_number
 *	| '.'
 *	| string
 *	| '(' assignment ')'
 *	| function [ '(' [assignment  [',' assignment] ] ')' ]
 *	| '!' term
 *	| '+' term
 *	| '-' term.
 */
static int
getterm()
{
	int type;		/* type of term found */

	type = gettoken();
	switch (type) {
		case T_NUMBER:
			addopindex(OP_NUMBER, tokennumber());
			type = (EXPR_RVALUE | EXPR_CONST);
			break;

		case T_IMAGINARY:
			addopindex(OP_IMAGINARY, tokennumber());
			type = (EXPR_RVALUE | EXPR_CONST);
			break;

		case T_OLDVALUE:
			addop(OP_OLDVALUE);
			type = 0;
			break;

		case T_STRING:
			addopptr(OP_STRING, tokenstring());
			type = (EXPR_RVALUE | EXPR_CONST);
			break;

		case T_PLUSPLUS:
			if (isrvalue(getterm()))
				scanerror(T_NULL, "Bad ++ usage");
			addop(OP_PREINC);
			type = (EXPR_RVALUE | EXPR_ASSIGN);
			break;

		case T_MINUSMINUS:
			if (isrvalue(getterm()))
				scanerror(T_NULL, "Bad -- usage");
			addop(OP_PREDEC);
			type = (EXPR_RVALUE | EXPR_ASSIGN);
			break;

		case T_NOT:
			(void) getterm();
			addop(OP_NOT);
			type = EXPR_RVALUE;
			break;

		case T_MINUS:
			(void) getterm();
			addop(OP_NEGATE);
			type = EXPR_RVALUE;
			break;

		case T_PLUS:
			(void) getterm();
			type = EXPR_RVALUE;
			break;

		case T_LEFTPAREN:
			type = getexprlist();
			if (gettoken() != T_RIGHTPAREN)
				scanerror(T_SEMICOLON, "Missing right parenthesis");
			break;

		case T_SYMBOL:
			rescantoken();
			type = getidexpr(TRUE, FALSE);
			break;

		case T_LEFTBRACKET:
			scanerror(T_NULL, "Bad index usage");
			type = 0;
			break;

		case T_PERIOD:
			scanerror(T_NULL, "Bad element reference");
			type = 0;
			break;

		default:
			if (iskeyword(type)) {
				scanerror(T_NULL, "Expression contains reserved keyword");
				type = 0;
				break;
			}
			rescantoken();
			scanerror(T_NULL, "Missing expression");
			type = 0;
	}
	switch (gettoken()) {
		case T_PLUSPLUS:
			if (isrvalue(type))
				scanerror(T_NULL, "Bad ++ usage");
			addop(OP_POSTINC);
			return (EXPR_RVALUE | EXPR_ASSIGN);
		case T_MINUSMINUS:
			if (isrvalue(type))
				scanerror(T_NULL, "Bad -- usage");
			addop(OP_POSTDEC);
			return (EXPR_RVALUE | EXPR_ASSIGN);
		default:
			rescantoken();
			return type;
	}
}


/*
 * Read in an identifier expressions.
 * This is a symbol name followed by parenthesis, or by square brackets or
 * element refernces.  The symbol can be a global or a local variable name.
 * Returns the type of expression found.
 */
static int
getidexpr(okmat, autodef)
	BOOL okmat, autodef;
{
	int type;
	char name[SYMBOLSIZE+1];	/* symbol name */

	type = 0;
	if (!getid(name))
		return type;
	switch (gettoken()) {
		case T_LEFTPAREN:
			getcallargs(name);
			type = EXPR_RVALUE;
			break;
		case T_ASSIGN:
			autodef = TRUE;
			/* fall into default case */
		default:
			rescantoken();
			checksymbol(name, autodef);
	}
	/*
	 * Now collect as many element references and matrix index operations
	 * as there are following the id.
	 */
	for (;;) {
		switch (gettoken()) {
			case T_LEFTBRACKET:
				rescantoken();
				if (!okmat)
					return type;
				getmatargs();
				type = 0;
				break;
			case T_PERIOD:
				getelement();
				type = 0;
				break;
			case T_LEFTPAREN:
				scanerror(T_NULL, "Function calls not allowed as expressions");
			default:
				rescantoken();
				return type;
		}
	}
}


/*
 * Read in a filename for a read or write command.
 * Both quoted and unquoted filenames are handled here.
 * The name must be terminated by an end of line or semicolon.
 * Returns TRUE if the filename was successfully parsed.
 */
static BOOL
getfilename(name, msg_ok)
	char name[PATHSIZE+1];
	BOOL msg_ok;		/* TRUE => ok to print error messages */
{
	tokenmode(TM_NEWLINES | TM_ALLSYMS);
	switch (gettoken()) {
		case T_STRING:
		case T_SYMBOL:
			break;
		default:
			if (msg_ok)
				scanerror(T_SEMICOLON, "Filename expected");
			return FALSE;
	}
	strcpy(name, tokenstring());
	switch (gettoken()) {
		case T_SEMICOLON:
		case T_NEWLINE:
		case T_EOF:
			break;
		default:
			if (msg_ok)
				scanerror(T_SEMICOLON, 
				    "Missing semicolon after filename");
			return FALSE;
	}
	return TRUE;
}


/*
 * Read the show command and display useful information.
 */
static void
getshowcommand()
{
	char name[SYMBOLSIZE+1];

	if ((gettoken() != T_SHOW) || (gettoken() != T_SYMBOL)) {
		scanerror(T_SEMICOLON, "Bad syntax for SHOW command");
		return;
	}
	strcpy(name, tokenstring());
	switch (gettoken()) {
		case T_NEWLINE:
		case T_SEMICOLON:
			break;
		default:
			scanerror(T_SEMICOLON, "Bad syntax for SHOW command");
	}
	switch ((int) stringindex("builtins\0globals\0functions\0objfuncs\0memory\0", name)) {
		case 1:
			showbuiltins();
			break;
		case 2:
			showglobals();
			break;
		case 3:
			showfunctions();
			break;
		case 4:
			showobjfuncs();
			break;
		case 5:
			mem_stats("");
			break;
		default:
			scanerror(T_NULL, "Unknown SHOW parameter \"%s\"", name);
	}
}


/*
 * Read in a set of matrix index arguments, surrounded with square brackets.
 * This also handles double square brackets for 'fast indexing'.
 */
static void
getmatargs()
{
	int dim;

	if (gettoken() != T_LEFTBRACKET) {
		scanerror(T_NULL, "Matrix indexing expected");
		return;
	}
	/*
	 * Parse all levels of the array reference
	 * Look for the 'fast index' first.
	 */
	if (gettoken() == T_LEFTBRACKET) {
		(void) getassignment();
		if ((gettoken() != T_RIGHTBRACKET) ||
			(gettoken() != T_RIGHTBRACKET)) {
				scanerror(T_NULL, "Bad fast index usage");
				return;
		}
		addop(OP_FIADDR);
		return;
	}
	rescantoken();
	/*
	 * Normal indexing with the indexes separated by commas.
	 */
	dim = 1;
	for (;;) {
		(void) getassignment();
		switch (gettoken()) {
			case T_RIGHTBRACKET:
				if (gettoken() != T_LEFTBRACKET) {
					rescantoken();
					addopindex(OP_INDEXADDR, (long) dim);
					return;
				}
				/* proceed into comma case */
				/*FALLTHRU*/
			case T_COMMA:
				if (++dim > MAXDIM)
					scanerror(T_NULL, "Too many dimensions for array reference");
				break;
			default:
				rescantoken();
				scanerror(T_NULL, "Missing right bracket in array reference");
				return;
		}
	}
}


/*
 * Get an element of an object reference.
 * The leading period which introduces the element has already been read.
 */
static void
getelement()
{
	long index;
	char name[SYMBOLSIZE+1];

	if (!getid(name))
		return;
	index = findelement(name);
	if (index < 0) {
		scanerror(T_NULL, "Element \"%s\" is undefined", name);
		return;
	}
	addopindex(OP_ELEMADDR, index);
}


/*
 * Read in a single symbol name and copy its value into the given buffer.
 * Returns TRUE if a valid symbol id was found.
 */
static BOOL
getid(buf)
	char buf[SYMBOLSIZE+1];
{
	int type;

	type = gettoken();
	if (iskeyword(type)) {
		scanerror(T_NULL, "Reserved keyword used as symbol name");
		type = T_SYMBOL;
	}
	if (type != T_SYMBOL) {
		rescantoken();
		scanerror(T_NULL, "Symbol name expected");
		*buf = '\0';
		return FALSE;
	}
	strncpy(buf, tokenstring(), SYMBOLSIZE);
	buf[SYMBOLSIZE] = '\0';
	return TRUE;
}


/*
 * Check a symbol name to see if it is known and generate code to reference it.
 * The symbol can be either a parameter name, a local name, or a global name.
 * If autodef is true, we automatically define the name as a global symbol
 * if it is not yet known.
 */
static void
checksymbol(name, autodef)
	char *name;		/* symbol name to be checked */
	BOOL autodef;
{
	switch (symboltype(name)) {
		case SYM_LOCAL:
			addopindex(OP_LOCALADDR, (long) findlocal(name));
			return;
		case SYM_PARAM:
			addopindex(OP_PARAMADDR, (long) findparam(name));
			return;
		case SYM_GLOBAL:
			addopptr(OP_GLOBALADDR, (char *) findglobal(name));
			return;
	}
	/*
	 * The symbol is not yet defined.
	 * If we are at the top level and we are allowed to, then define it.
	 */
	if ((curfunc->f_name[0] != '*') || !autodef) {
		scanerror(T_NULL, "\"%s\" is undefined", name);
		return;
	}
	(void) addglobal(name);
	addopptr(OP_GLOBALADDR, (char *) findglobal(name));
}


/*
 * Get arguments for a function call.
 * The name and beginning parenthesis has already been seen.
 * callargs = [ [ '&' ] assignment  [',' [ '&' ] assignment] ] ')'.
 */
static void
getcallargs(name)
	char *name;		/* name of function */
{
	long index;		/* function index */
	long op;		/* opcode to add */
	int argcount;		/* number of arguments */
	BOOL addrflag;

	op = OP_CALL;
	index = getbuiltinfunc(name);
	if (index < 0) {
		op = OP_USERCALL;
		index = adduserfunc(name);
	}
	if (gettoken() == T_RIGHTPAREN) {
		if (op == OP_CALL)
			builtincheck(index, 0);
		addopfunction(op, index, 0);
		return;
	}
	rescantoken();
	argcount = 0;
	for (;;) {
		argcount++;
		addrflag = (gettoken() == T_AND);
		if (!addrflag)
			rescantoken();
		if (!islvalue(getassignment()) && addrflag)
			scanerror(T_NULL, "Taking address of non-variable");
		if (!addrflag && (op != OP_CALL))
			addop(OP_GETVALUE);
		switch (gettoken()) {
			case T_RIGHTPAREN:
				if (op == OP_CALL)
					builtincheck(index, argcount);
				addopfunction(op, index, argcount);
				return;
			case T_COMMA:
				break;
			default:
				scanerror(T_SEMICOLON, "Missing right parenthesis in function call");
				return;
		}
	}
}

/* END CODE */
