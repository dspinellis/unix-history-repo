%{
/* Copyright (c) 1982 Regents of the University of California */

static	char sccsid[] = "@(#)commands.y	1.12 (Berkeley) %G%";

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
#include "lists.h"
#include <signal.h>

private String curformat = "X";

%}

%term
    ALIAS AND ASSIGN AT CALL CATCH CONT DEBUG DELETE DIV DOWN DUMP
    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD NEXT NEXTI NIL NOT OR
    PRINT PSYM QUIT RERUN RETURN RUN SH SKIP SOURCE STATUS STEP STEPI
    STOP STOPI TRACE TRACEI UP
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
%left '\\'

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
    List y_list;
};

%type <y_op>	    trace stop
%type <y_long>	    INT count
%type <y_real>	    REAL
%type <y_string>    STRING redirectout filename opt_filename mode
%type <y_name>	    ALIAS AND ASSIGN AT CALL CATCH CONT
%type <y_name>	    DEBUG DELETE DIV DOWN DUMP
%type <y_name>	    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD
%type <y_name>	    NEXT NEXTI NIL NOT OR
%type <y_name>	    PRINT PSYM QUIT RERUN RETURN RUN SH SKIP SOURCE STATUS
%type <y_name>	    STEP STEPI STOP STOPI TRACE TRACEI
%type <y_name>	    UP USE WHATIS WHEN WHERE WHEREIS WHICH
%type <y_name>	    name NAME keyword
%type <y_node>      opt_qual_symbol symbol
%type <y_node>	    command rcommand cmd step what where examine
%type <y_node>	    event opt_exp_list opt_cond
%type <y_node>	    exp_list exp term boolean_exp constant address
%type <y_node>	    int_list alias_command list_command line_number
%type <y_node>	    something search_command pattern
%type <y_node>	    signal_list signal
%type <y_cmdlist>   actions
%type <y_list>      sourcepath

%%

input:
    input command_nl
{
	endshellmode();
	startaliasing();
}
|
    /* empty */
;

command_nl:
    command_line '\n'
{
	if (istty()) {
		printf("(%s) ", cmdname);
		fflush(stdout);
	}
}
|
    command_line ';'
|
    '\n'
{
	if (istty()) {
		printf("(%s) ", cmdname);
		fflush(stdout);
	}
}
;

command_line:
    command
{
	if ($1 != nil) {
            if(debug_flag[2]) {dumptree(stderr,$1); fflush (stderr);}
	    eval($1);
	}
	startaliasing();
}
|
    rcommand redirectout
{
	if ($1 != nil) {
	    if ($2 != nil) {
		setout($2);
                if(debug_flag[2]) {dumptree(stderr,$1); fflush (stderr);}
		eval($1);
		unsetout();
	    } else {
                if(debug_flag[2]) {dumptree(stderr,$1); fflush (stderr);}
		eval($1);
	    }
	}
	startaliasing();
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
    ASSIGN stopaliasing term '=' exp
{
	$$ = build(O_ASSIGN, $3, $5);
}
|
    CATCH
{
	$$ = build(O_CATCH, nil);
}
|
    CATCH stopaliasing signal_list
{
	$$ = build(O_CATCH, $3);
}
|
    CONT
{
	$$ = build(O_CONT, (long) DEFSIG);
}
|
    CONT INT
{
	$$ = build(O_CONT, $2);
}
|
    DELETE int_list
{
	$$ = build(O_DELETE, $2);
}
|
    DOWN
{
	$$ = build(O_DOWN, build(O_LCON, (long) 1));
}
|
    DOWN INT
{
	$$ = build(O_DOWN, build(O_LCON, (long) $2));
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
    FUNC stopaliasing symbol
{
	$$ = build(O_FUNC, $3);
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
    IGNORE
{
	$$ = build(O_IGNORE, nil);
}
|
    IGNORE stopaliasing signal_list
{
	$$ = build(O_IGNORE, $3);
}
|
    list_command
{
	$$ = $1;
}
|
    PSYM stopaliasing term
{
	$$ = build(O_PSYM, $3);
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
    RETURN
{
	$$ = build(O_RETURN, nil);
}
|
    RETURN stopaliasing opt_qual_symbol
{
	$$ = build(O_RETURN, $3);
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
    UP
{
	$$ = build(O_UP, build(O_LCON, (long) 1));
}
|
    UP INT
{
	$$ = build(O_UP, build(O_LCON, (long) $2));
}
|
    USE shellmode sourcepath
{
	String dir;

	$$ = nil;
	if (list_size($3) == 0) {
	    foreach (String, dir, sourcepath)
		printf("%s ", dir);
	    endfor
	    printf("\n");
	} else {
	    foreach (String, dir, sourcepath)
		list_delete(list_curitem(sourcepath), sourcepath);
	    endfor
	    sourcepath = $3;
	}
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
    search_command
{
	$$ = $1;
}
;


search_command:
    '/' pattern
{
	$$ = build(O_SEARCH, build(O_LCON, 1), $2);
}
|
    '?' pattern
{
	$$ = build(O_SEARCH, build(O_LCON, 0), $2);
}
;

pattern:
    STRING
{
	$$ = build(O_SCON, $1);
}
;

int_list:
    INT
{
	$$ = build(O_COMMA, build(O_LCON, $1), nil);
}
|
    INT int_list
{
	$$ = build(O_COMMA, build(O_LCON, $1), $2);
}
;

signal_list:
     signal
{
	$$ = build(O_COMMA, $1, nil);
}
|
    signal signal_list
{
	$$ = build(O_COMMA, $1, $2);
}
;

signal:
    INT
{
	if ($1 < 1 || $1 > NSIG)
		error("Invalid signal %d.", $1);
	$$ = build(O_LCON, $1);
}
|
    NAME
{
	$$ = build(O_LCON, signalname(ident($1)));
}
;

runcommand:
    run arglist
|
    run
;

run:
    RUN shellmode
{
	arginit();
	fflush(stdout);
}
|
    RERUN shellmode
{
	fflush(stdout);
}
;
arglist:
    arglist arg
|
    arg
;
arg:
     NAME
{
	newarg(ident($1));
}
|
     STRING
{
	newarg($1);
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
    STEP stopaliasing
{
	$$ = build(O_STEP, true, false);
}
|
    STEPI stopaliasing
{
	$$ = build(O_STEP, false, false);
}
|
    NEXT stopaliasing
{
	$$ = build(O_STEP, true, true);
}
|
    NEXTI stopaliasing
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
	$$ = $1;
	list_append(list_item(ident($2)), nil, $$);
}
|
    /* empty */
{
	$$ = list_alloc();
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
    PRINT stopaliasing exp_list
{
	$$ = build(O_PRINT, $3);
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
    CALL stopaliasing term '(' opt_exp_list ')'
{
	$$ = build(O_CALL, $3, $5);
}
|
    DEBUG INT
{
 	$$ = build(O_DEBUG, $2);
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
    alias name something
{
	$$ = build(O_ALIAS, build(O_NAME, $2), $3);
}
|
    alias name
{
	$$ = build(O_ALIAS, build(O_NAME, $2), nil);
}
|
    alias
{
	$$ = build(O_ALIAS, nil, nil);
}
;

alias:
     ALIAS stopaliasing
;

stopaliasing:
    /* empty */
{
	stopaliasing();
}
;

trace:
    TRACE stopaliasing
{
	$$ = O_TRACE;
}
|
    TRACEI stopaliasing
{
	$$ = O_TRACEI;
}
;
stop:
    STOP stopaliasing
{
	$$ = O_STOP;
}
|
    STOPI stopaliasing
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
opt_exp_list:
    exp_list
{
	$$ = $1;
}
|
    /* empty */
{
	$$ = nil;
}
;
list_command:
    list
{
	$$ = build(O_LIST,
	    build(O_LCON, (long) cursrcline),
	    build(O_LCON, (long) cursrcline + 9)
	);
}
|
    list line_number
{
	$$ = build(O_LIST, $2, $2);
}
|
    list line_number ',' line_number
{
	$$ = build(O_LIST, $2, $4);
}
|
    list symbol
{
	$$ = build(O_LIST, $2, nil);
}
;

list:
    LIST stopaliasing
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
|
    address '=' mode
{
	$$ = build(O_EXAMINE, $3, $1, nil, 0);
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
    exp '\\' opt_qual_symbol
{
	$$ = build(O_TYPERENAME, $1, $3);
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
    '#' '(' exp ')' %prec UNARYSIGN
{
	$$ = concrete($3);
}
|
    term '(' opt_exp_list ')'
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
opt_qual_symbol:
    symbol
{
	$$ = $1;
}
|
    opt_qual_symbol '.' name
{
	$$ = dot($1, $3);
}
;
symbol:
    name
{
	$$ = build(O_SYM, which($1));
}
|
    '.' name
{
	$$ = dot(build(O_SYM, program), $2);
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
    ALIAS | AND | ASSIGN | AT | CALL | CATCH | CONT | DEBUG | DELETE | DIV | 
    DOWN | DUMP | EDIT | FILE | FUNC | GRIPE | HELP | IGNORE | IN | LIST |
    MOD | NEXT | NEXTI | NIL | NOT | OR | PRINT | PSYM | QUIT |
    RERUN | RETURN | RUN | SH | SKIP | SOURCE | STATUS | STEP | STEPI |
    STOP | STOPI | TRACE | TRACEI | UP |
    USE | WHATIS | WHEN | WHERE | WHEREIS | WHICH
;

something:
    NAME
{
	$$ = build(O_NAME, $1);
}
|
    keyword
{
	$$ = build(O_NAME, $1);
}
|
    STRING
{
	$$ = build(O_SCON, $1);
}
;
