%{
/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)grammar.y	5.1 (Berkeley) %G%";
#endif not lint

/*
 * yacc grammar for debugger commands
 */

#include "defs.h"
#include "command.h"
#include "sym.h"
#include "symtab.h"
#include "tree.h"
#include "process.h"
#include "source.h"

%}

%term ALIAS ASSIGN CALL CHFILE
%term CONT DUMP EDIT
%term GRIPE HELP LIST NEXT
%term QUIT REMAKE PRINT
%term RUN SH SOURCE
%term STATUS STEP
%term STOP STOPI TRACE TRACEI
%term DELETE
%term WHATIS WHICH WHERE
%term XI XD

%term AT IN IF
%term FILENAME
%term INT REAL NAME STRING
%term DIV MOD
%term AND OR NOT

%binary '<' '=' '>' IN
%left '+' '-' OR '|'
%left UNARYSIGN
%left '*' '/' DIV MOD AND '&'
%left NOT

%union {
	SYM *y_sym;
	NODE *y_node;
	int y_int;
	OP y_op;
	long y_long;
	double y_real;
	char *y_string;
	BOOLEAN y_bool;
};

%type <y_int> trace TRACE TRACEI stop STOP STOPI
%type <y_long> INT
%type <y_real> REAL
%type <y_op> addop mulop relop
%type <y_string> STRING FILENAME SH opt_filename
%type <y_sym> NAME
%type <y_node> command rcommand what where opt_arglist opt_cond
%type <y_node> exp_list exp boolean_exp term constant
%type <y_node> line_list line_number address_list
%%
input:
	input command_nl
{
		prompt();
}
|	/* empty */
;
command_nl:
	command_line '\n'
|	'\n'
;

/*
 * There are two kinds of commands, those that can be redirected
 * and those that can't.
 */

command_line:
	command
{
		eval($1);
}
|	rcommand
{
		eval($1);
}
|	rcommand '>' FILENAME
{
		setout($3);
		eval($1);
		unsetout();
}
|	SH
{
		shell($1);
}
|	run args
{
		run();
}
;
run:
	RUN
{
		arginit();
}
;
args:
	arg args
|	/* empty */
;
arg:
	FILENAME
{
		newarg($1);
}
|	'<' FILENAME
{
		inarg($2);
}
|	'>' FILENAME
{
		outarg($2);
}
;
command:
	ASSIGN term exp
{
		$$ = build(O_ASSIGN, $2, $3);
}
|	CHFILE opt_filename
{
		$$ = build(O_CHFILE, $2);
}
|	CONT
{
		$$ = build(O_CONT);
}
|	LIST line_list
{
		$$ = build(O_LIST, $2);
}
|	LIST NAME
{
		$$ = build(O_LIST, build(O_NAME, $2));
}
|	NEXT
{
		$$ = build(O_NEXT);
}
|	PRINT exp_list
{
		$$ = build(O_PRINT, $2);
}
|	QUIT
{
		quit(0);
}
|	STEP
{
		$$ = build(O_STEP);
}
|	stop where opt_cond
{
		$$ = build($1, NIL, $2, $3);
}
|	stop what opt_cond
{
		$$ = build($1, $2, NIL, $3);
}
|	stop IF boolean_exp
{
		$$ = build($1, NIL, NIL, $3);
}
|	trace what where opt_cond
{
		$$ = build($1, $2, $3, $4);
}
|	trace where opt_cond
{
		$$ = build($1, NIL, $2, $3);
}
|	trace what opt_cond
{
		$$ = build($1, $2, NIL, $3);
}
|	trace opt_cond
{
		$$ = build($1, NIL, NIL, $2);
}
|	DELETE INT
{
		$$ = build(O_DELETE, $2);
}
|	WHATIS term
{
		$$ = build(O_WHATIS, $2);
}
|	WHICH NAME
{
		$$ = build(O_WHICH, $2);
}
|	WHERE
{
		$$ = build(O_WHERE);
}
|	XI address_list
{
		$$ = build(O_XI, $2);
}
|	XD address_list
{
		$$ = build(O_XD, $2);
}
;
rcommand:
	ALIAS FILENAME opt_filename
{
		$$ = build(O_ALIAS, $2, $3);
}
|	ALIAS
{
		$$ = build(O_ALIAS, NIL, NIL);
}
|	CALL term opt_arglist
{
		$$ = build(O_CALL, $2, $3);
}
|	EDIT opt_filename
{
		$$ = build(O_EDIT, $2);
}
|	DUMP
{
		$$ = build(O_DUMP);
}
|	GRIPE
{
		$$ = build(O_GRIPE);
}
|	HELP
{
		$$ = build(O_HELP);
}
|	REMAKE
{
		$$ = build(O_REMAKE);
}
|	SOURCE FILENAME
{
		$$ = build(O_SOURCE, $2);
}
|	STATUS
{
		$$ = build(O_STATUS);
}
;
trace:
	TRACE
{
		$$ = O_TRACE;
}
|	TRACEI
{
		$$ = O_TRACEI;
}
;
stop:
	STOP
{
		$$ = O_STOP;
}
|	STOPI
{
		$$ = O_STOPI;
}
;
what:
	exp
|	FILENAME line_number
{
		$$ = build(O_QLINE, $1, $2);
}
;
where:
	IN term
{
		$$ = $2;
}
|	AT line_number
{
		$$ = build(O_QLINE, cursource, $2);
}
|	AT FILENAME line_number
{
		$$ = build(O_QLINE, $2, $3);
}
;
opt_filename:
	/* empty */
{
		$$ = NIL;
}
|	FILENAME
;
opt_arglist:
	/* empty */
{
		$$ = NIL;
}
|	'(' exp_list ')'
{
		$$ = $2;
}
;
line_list:
	/* empty */
{
		NODE *first, *last;

		first = build(O_LCON, (long) 1);
		last = build(O_LCON, (long) lastlinenum);
		$$ = build(O_COMMA, first, last);
}
|	line_number
{
		$$ = build(O_COMMA, $1, $1);
}
|	line_number ',' line_number
{
		$$ = build(O_COMMA, $1, $3);
}
;
line_number:
	INT
{
		$$ = build(O_LCON, $1);
}
|	'$'
{
		$$ = build(O_LCON, (long) lastlinenum);
}
;
address_list:
	exp
{
		$$ = build(O_COMMA, $1, $1);
}
|	exp ',' exp
{
		$$ = build(O_COMMA, $1, $3);
}
;
opt_cond:
	/* empty */
{
		$$ = NIL;
}
|	IF boolean_exp
{
		$$ = $2;
}
;
exp_list:
	exp
{
		$$ = build(O_COMMA, $1, NIL);
}
|	exp ',' exp_list
{
		$$ = build(O_COMMA, $1, $3);
}
;
exp:
	term
{
		$$ = build(O_RVAL, $1);
}
|	term '(' exp_list ')'
{
		$$ = build(O_CALL, $1, $3);
}
|	constant
|	'+' exp %prec UNARYSIGN
{
		$$ = $2;
}
|	'-' exp %prec UNARYSIGN
{
		$$ = build(O_NEG, $2);
}
|	exp addop exp %prec '+'
{
		$$ = build($2, $1, $3);
}
|	exp mulop exp %prec '*'
{
		$$ = build($2, $1, $3);
}
|	exp relop exp %prec '<'
{
		$$ = build($2, $1, $3);
}
|	'(' exp ')'
{
		$$ = $2;
}
;
boolean_exp:
	exp
{
		chkboolean($$ = $1);
}
;
term:
	NAME
{
		$$ = build(O_NAME, $1);
}
|	AT
{
		SYM *s;

		s = st_lookup(symtab, "at");
		if (s == NIL) {
			error("\"at\" is not defined");
		}
		$$ = build(O_NAME, s);
}
|	term '[' exp_list ']'
{
		$$ = subscript($1, $3);
}
|	term '.' NAME
{
		$$ = dot($1, $3);
}
|	term '^'
{
		$$ = build(O_INDIR, $1);
}
;
constant:
	INT
{
		$$ = build(O_LCON, $1);
}
|	REAL
{
		$$ = build(O_FCON, $1);
}
|	STRING
{
		$$ = build(O_SCON, $1);
}
;
addop:
	'+'
{
		$$ = O_ADD;
}
|	'-'
{
		$$ = O_SUB;
}
|	OR
{
		$$ = O_OR;
}
|	'|'
{
		$$ = O_OR;
}
;
mulop:
	'*'
{
		$$ = O_MUL;
}
|	'/'
{
		$$ = O_DIVF;
}
|	DIV
{
		$$ = O_DIV;
}
|	MOD
{
		$$ = O_MOD;
}
|	AND
{
		$$ = O_AND;
}
|	'&'
{
		$$ = O_AND;
}
;
relop:
	'<'
{
		$$ = O_LT;
}
|	'<' '='
{
		$$ = O_LE;
}
|	'>'
{
		$$ = O_GT;
}
|	'>' '='
{
		$$ = O_GE;
}
|	'='
{
		$$ = O_EQ;
}
|	'<' '>'
{
		$$ = O_NE;
}
;
%%

/*
 * parser error handling
 */

yyerror(s)
char *s;
{
	if (strcmp(s, "syntax error") == 0) {
		error("bad command syntax");
	} else {
		error(s);
	}
}

/*
 * In recovering from an error we gobble input up to a newline.
 */

gobble()
{
	register int t;

	if (!nlflag) {
		while ((t = yylex()) != '\n' && t != 0);
	}
}
