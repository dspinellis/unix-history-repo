%{

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)@(#)commands.y 1.1 %G%";

/*
 * Yacc grammar for debugger commands.
 */

#include "defs.h"
#include "symbols.h"
#include "operators.h"
#include "tree.h"
#include "process.h"
#include "source.h"
#include "scanner.h"
#include "names.h"

private String curformat = "X";

%}

%term
    ALIAS AND ASSIGN AT CALL CATCH CONT DELETE DIV DUMP
    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD NEXT NEXTI NIL NOT OR
    PRINT PSYM QUIT RUN SH SKIP SOURCE STATUS STEP STEPI
    STOP STOPI TRACE TRACEI
    USE WHATIS WHEN WHERE WHEREIS WHICH

%term INT REAL NAME STRING
%term LFORMER RFORMER ABSTRACTION ARROW

%right INT
%binary REDIRECT
%binary '<' '=' '>' '!' IN
%left '+' '-' OR
%left UNARYSIGN
%left '*' '/' DIV MOD AND
%left NOT '(' '[' '.' '^' ARROW

%union {
    Name y_name;
    Symbol y_sym;
    Node y_node;
    Integer y_int;
    Operator y_op;
    long y_long;
    double y_real;
    String y_string;
    Boolean y_bool;
    Cmdlist y_cmdlist;
};

%type <y_op>	    trace stop
%type <y_long>	    INT count
%type <y_real>	    REAL
%type <y_string>    STRING redirectout filename opt_filename mode
%type <y_name>	    ALIAS AND ASSIGN AT CALL CATCH CONT DELETE DIV DUMP
%type <y_name>	    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD
%type <y_name>	    NEXT NEXTI NIL NOT OR
%type <y_name>	    PRINT PSYM QUIT RUN SH SKIP SOURCE STATUS STEP STEPI
%type <y_name>	    STOP STOPI TRACE TRACEI
%type <y_name>	    USE WHATIS WHEN WHERE WHEREIS WHICH
%type <y_name>	    name NAME keyword
%type <y_node>      symbol
%type <y_node>	    command rcommand cmd step what where examine
%type <y_node>	    event opt_arglist opt_cond
%type <y_node>	    exp_list exp term boolean_exp constant address
%type <y_node>	    alias_command list_command line_number
%type <y_cmdlist>   actions

%%

input:
    input command_nl
|
    /* empty */
;
command_nl:
    command_line '\n'
|
    command_line ';'
|
    '\n'
;

command_line:
    command
{
	if ($1 != nil) {
	    eval($1);
	}
}
|
    rcommand redirectout
{
	if ($1 != nil) {
	    if ($2 != nil) {
		setout($2);
		eval($1);
		unsetout();
	    } else {
		eval($1);
	    }
	}
}
;
redirectout:
    '>' shellmode NAME
{
	$$ = ident($3);
}
|
    /* empty */
{
	$$ = nil;
}
;

/*
 * Non-redirectable commands.
 */
command:
    alias_command
{
	$$ = $1;
}
|
    ASSIGN term '=' exp
{
	$$ = build(O_ASSIGN, $2, $4);
}
|
    CATCH INT
{
	$$ = build(O_CATCH, $2);
}
|
    CONT
{
	$$ = build(O_CONT);
}
|
    DELETE INT
{
	$$ = build(O_DELETE, $2);
}
|
    EDIT shellmode opt_filename
{
	$$ = build(O_EDIT, $3);
}
|
    FILE shellmode opt_filename
{
	$$ = build(O_CHFILE, $3);
}
|
    FUNC
{
	$$ = build(O_FUNC, nil);
}
|
    FUNC symbol
{
	$$ = build(O_FUNC, $2);
}
|
    GRIPE
{
	$$ = build(O_GRIPE);
}
|
    HELP
{
	$$ = build(O_HELP);
}
|
    IGNORE INT
{
	$$ = build(O_IGNORE, $2);
}
|
    list_command
{
	$$ = $1;
}
|
    PSYM term
{
	$$ = build(O_PSYM, $2);
}
|
    QUIT
{
	if (not popinput()) {
	    quit(0);
	} else {
	    $$ = nil;
	}
}
|
    runcommand
{
	run();
	/* NOTREACHED */
}
|
    SH
{
	shellline();
	$$ = nil;
}
|
    SOURCE shellmode filename
{
	$$ = build(O_SOURCE, $3);
}
|
    step
{
	$$ = $1;
}
|
    stop where opt_cond
{
	$$ = build($1, nil, $2, $3);
}
|
    stop what opt_cond
{
	$$ = build($1, $2, nil, $3);
}
|
    stop IF boolean_exp
{
	$$ = build($1, nil, nil, $3);
}
|
    trace what where opt_cond
{
	$$ = build($1, $2, $3, $4);
}
|
    trace where opt_cond
{
	$$ = build($1, nil, $2, $3);
}
|
    trace what opt_cond
{
	$$ = build($1, $2, nil, $3);
}
|
    trace opt_cond
{
	$$ = build($1, nil, nil, $2);
}
|
    WHATIS term
{
	$$ = build(O_WHATIS, $2);
}
|
    WHEN event '{' actions '}'
{
	$$ = build(O_ADDEVENT, $2, $4);
}
|
    WHEREIS symbol
{
	$$ = build(O_WHEREIS, $2);
}
|
    WHICH symbol
{
	$$ = build(O_WHICH, $2);
}
|
    USE shellmode sourcepath
{
	$$ = nil;
	if (list_size(sourcepath) == 0) {
	    list_append(list_item("."), nil, sourcepath);
	}
}
;
runcommand:
    run shellmode arglist
;
run:
    RUN
{
	fflush(stdout);
	arginit();
}
;
arglist:
    arglist arg
|
    /* empty */
;
arg:
    NAME
{
	newarg(ident($1));
}
|
    '<' NAME
{
	inarg(ident($2));
}
|
    '>' NAME
{
	outarg(ident($2));
}
;
step:
    STEP
{
	$$ = build(O_STEP, true, false);
}
|
    STEPI
{
	$$ = build(O_STEP, false, false);
}
|
    NEXT
{
	$$ = build(O_STEP, true, true);
}
|
    NEXTI
{
	$$ = build(O_STEP, false, true);
}
;
shellmode:
    /* empty */
{
	beginshellmode();
}
;
sourcepath:
    sourcepath NAME
{
	list_append(list_item(ident($2)), nil, sourcepath);
}
|
    /* empty */
{
	String dir;

	foreach (String, dir, sourcepath)
	    list_delete(list_curitem(sourcepath), sourcepath);
	endfor
}
;
event:
    where
|
    exp
;
actions:
    actions cmd ';'
{
	$$ = $1;
	cmdlist_append($2, $$);
}
|
    cmd ';'
{
	$$ = list_alloc();
	cmdlist_append($1, $$);
}
;
cmd:
    command
|
    rcommand
;

/*
 * Redirectable commands.
 */
rcommand:
    PRINT exp_list
{
	$$ = build(O_PRINT, $2);
}
|
    WHERE
{
	$$ = build(O_WHERE);
}
|
    examine
{
	$$ = $1;
}
|
    CALL term
{
	$$ = $2;
}
|
    DUMP
{
	$$ = build(O_DUMP);
}
|
    STATUS
{
	$$ = build(O_STATUS);
}
;
alias_command:
    ALIAS name name
{
	$$ = build(O_ALIAS, build(O_NAME, $2), build(O_NAME, $3));
}
|
    ALIAS name
{
	$$ = build(O_ALIAS, build(O_NAME, $2), nil);
}
|
    ALIAS
{
	$$ = build(O_ALIAS, nil, nil);
}
;
trace:
    TRACE
{
	$$ = O_TRACE;
}
|
    TRACEI
{
	$$ = O_TRACEI;
}
;
stop:
    STOP
{
	$$ = O_STOP;
}
|
    STOPI
{
	$$ = O_STOPI;
}
;
what:
    exp
{
	$$ = $1;
}
|
    STRING ':' line_number
{
	$$ = build(O_QLINE, build(O_SCON, $1), $3);
}
;
where:
    IN term
{
	$$ = $2;
}
|
    AT line_number
{
	$$ = build(O_QLINE, build(O_SCON, cursource), $2);
}
|
    AT STRING ':' line_number
{
	$$ = build(O_QLINE, build(O_SCON, $2), $4);
}
;
filename:
    NAME
{
	$$ = ident($1);
}
;
opt_filename:
    /* empty */
{
	$$ = nil;
}
|
    filename
{
	$$ = $1;
}
;
opt_arglist:
    /* empty */
{
	$$ = nil;
}
|
    '(' exp_list ')'
{
	$$ = $2;
}
;
list_command:
    LIST
{
	$$ = build(O_LIST,
	    build(O_LCON, (long) cursrcline),
	    build(O_LCON, (long) cursrcline + 9)
	);
}
|
    LIST line_number
{
	$$ = build(O_LIST, $2, $2);
}
|
    LIST line_number ',' line_number
{
	$$ = build(O_LIST, $2, $4);
}
|
    LIST symbol
{
	$$ = build(O_LIST, $2);
}
;
line_number:
    INT
{
	$$ = build(O_LCON, $1);
}
|
    '$'
{
	$$ = build(O_LCON, (long) LASTLINE);
}
;
examine:
    address '/' count mode
{
	$$ = build(O_EXAMINE, $4, $1, nil, $3);
}
|
    address ',' address '/' mode
{
	$$ = build(O_EXAMINE, $5, $1, $3, 0);
}
|
    '/' count mode
{
	$$ = build(O_EXAMINE, $3, build(O_LCON, (long) prtaddr), nil, $2);
}
;
address:
    INT
{
	$$ = build(O_LCON, $1);
}
|
    '&' term
{
	$$ = amper($2);
}
|
    address '+' address
{
	$$ = build(O_ADD, $1, $3);
}
|
    address '-' address
{
	$$ = build(O_SUB, $1, $3);
}
|
    address '*' address
{
	$$ = build(O_MUL, $1, $3);
}
|
    '*' address %prec UNARYSIGN
{
	$$ = build(O_INDIR, $2);
}
|
    '(' exp ')'
{
	$$ = $2;
}
;
count:
    /* empty */
{
	$$ = 1;
}
|
    INT
{
	$$ = $1;
}
;
mode:
    name
{
	$$ = ident($1);
	curformat = $$;
}
|
    /* empty */
{
	$$ = curformat;
}
;
opt_cond:
    /* empty */
{
	$$ = nil;
}
|
    IF boolean_exp
{
	$$ = $2;
}
;
exp_list:
    exp
{
	$$ = build(O_COMMA, $1, nil);
}
|
    exp ',' exp_list
{
	$$ = build(O_COMMA, $1, $3);
}
;
exp:
    term
{
	$$ = build(O_RVAL, $1);
}
|
    constant
{
	$$ = $1;
}
|
    '+' exp %prec UNARYSIGN
{
	$$ = $2;
}
|
    '-' exp %prec UNARYSIGN
{
	$$ = build(O_NEG, $2);
}
|
    '&' exp %prec UNARYSIGN
{
	$$ = amper($2);
}
|
    exp '+' exp
{
	$$ = build(O_ADD, $1, $3);
}
|
    exp '-' exp
{
	$$ = build(O_SUB, $1, $3);
}
|
    exp '*' exp
{
	$$ = build(O_MUL, $1, $3);
}
|
    exp '/' exp
{
	$$ = build(O_DIVF, $1, $3);
}
|
    exp DIV exp
{
	$$ = build(O_DIV, $1, $3);
}
|
    exp MOD exp
{
	$$ = build(O_MOD, $1, $3);
}
|
    exp AND exp
{
	$$ = build(O_AND, $1, $3);
}
|
    exp OR exp
{
	$$ = build(O_OR, $1, $3);
}
|
    exp '<' exp
{
	$$ = build(O_LT, $1, $3);
}
|
    exp '<' '=' exp
{
	$$ = build(O_LE, $1, $4);
}
|
    exp '>' exp
{
	$$ = build(O_GT, $1, $3);
}
|
    exp '>' '=' exp
{
	$$ = build(O_GE, $1, $4);
}
|
    exp '=' exp
{
	$$ = build(O_EQ, $1, $3);
}
|
    exp '=' '=' exp
{
	$$ = build(O_EQ, $1, $4);
}
|
    exp '<' '>' exp
{
	$$ = build(O_NE, $1, $4);
}
|
    exp '!' '=' exp
{
	$$ = build(O_NE, $1, $4);
}
|
    '(' exp ')'
{
	$$ = $2;
}
;
term:
    symbol
{
	$$ = $1;
}
|
    term '[' exp_list ']'
{
	$$ = subscript($1, $3);
}
|
    term '.' name
{
	$$ = dot($1, $3);
}
|
    term ARROW name
{
	$$ = dot($1, $3);
}
|
    '*' term %prec UNARYSIGN
{
	$$ = build(O_INDIR, $2);
}
|
    '*' '(' exp ')' %prec UNARYSIGN
{
	$$ = build(O_INDIR, $3);
}
|
    term '^' %prec UNARYSIGN
{
	$$ = build(O_INDIR, $1);
}
|
    '#' term %prec UNARYSIGN
{
	$$ = concrete($2);
}
|
    term '(' exp_list ')'
{
	$$ = build(O_CALL, $1, $3);
}
;
boolean_exp:
    exp
{
	chkboolean($1);
	$$ = $1;
}
;
constant:
    INT
{
	$$ = build(O_LCON, $1);
}
|
    REAL
{
	$$ = build(O_FCON, $1);
}
|
    STRING
{
	$$ = build(O_SCON, $1);
}
;
symbol:
    name
{
	$$ = build(O_SYM, which($1));
}
;
name:
    NAME
{
	$$ = $1;
}
|
    keyword
{
	$$ = $1;
}
keyword:
    ALIAS | AND | ASSIGN | AT | CALL | CATCH | CONT | DELETE | DIV | DUMP |
    EDIT | FILE | FUNC | GRIPE | HELP | IGNORE | IN | LIST | MOD |
    NEXT | NEXTI | NIL | NOT | OR | PRINT | PSYM | QUIT | RUN |
    SH | SKIP | SOURCE | STATUS | STEP | STEPI |
    STOP | STOPI | TRACE | TRACEI |
    USE | WHATIS | WHEN | WHERE | WHICH
;
