%{

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)commands.y	5.3 (Berkeley) %G%
 */

static char rcsid[] = "$Header: commands.y,v 1.5 84/12/26 10:38:41 linton Exp $";

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
#include "keywords.h"
#include "names.h"
#include "lists.h"

private String curformat = "X";

%}

%term
    ALIAS AND ASSIGN AT CALL CATCH CONT DEBUG DELETE DIV DOWN DUMP
    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD NEXT NEXTI NIL NOT OR
    PRINT PSYM QUIT RERUN RETURN RUN SET SH SKIP SOURCE STATUS STEP STEPI
    STOP STOPI TRACE TRACEI UNALIAS UNSET UP USE
    WHATIS WHEN WHERE WHEREIS WHICH

%term INT CHAR REAL NAME STRING
%term ARROW

%right INT
%binary REDIRECT
%binary '<' '=' '>' '!' IN
%left '+' '-' OR
%left UNARYSIGN
%left '*' '/' DIV MOD AND
%left '\\'
%left NOT '(' '[' '.' '^' ARROW

%union {
    Name y_name;
    Symbol y_sym;
    Node y_node;
    Integer y_int;
    Operator y_op;
    long y_long;
    char y_char;
    double y_real;
    String y_string;
    Boolean y_bool;
    Cmdlist y_cmdlist;
    List y_list;
};

%type <y_op>	    trace stop
%type <y_long>	    INT count signal
%type <y_char>	    CHAR
%type <y_real>	    REAL
%type <y_string>    STRING redirectout filename opt_filename mode
%type <y_name>	    ALIAS AND ASSIGN AT CALL CATCH CONT
%type <y_name>	    DEBUG DELETE DIV DOWN DUMP
%type <y_name>	    EDIT FILE FUNC GRIPE HELP IF IGNORE IN LIST MOD
%type <y_name>	    NEXT NEXTI NIL NOT OR
%type <y_name>	    PRINT PSYM QUIT RERUN RETURN RUN SET SH SKIP SOURCE STATUS
%type <y_name>	    STEP STEPI STOP STOPI TRACE TRACEI
%type <y_name>	    UNALIAS UNSET UP USE WHATIS WHEN WHERE WHEREIS WHICH
%type <y_name>	    name NAME keyword
%type <y_node>      opt_qual_symbol symbol
%type <y_node>	    command rcommand cmd step what where examine
%type <y_node>	    event opt_exp_list opt_cond
%type <y_node>	    exp_list exp term boolean_exp constant address
%type <y_node>	    integer_list alias_command list_command line_number
%type <y_cmdlist>   actions
%type <y_list>      sourcepath name_list

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
{
	chkalias = true;
}
|
    '\n'
;

command_line:
    command
{
	if ($1 != nil) {
	    topeval($1);
	}
}
|
    rcommand redirectout
{
	if ($1 != nil) {
	    if ($2 != nil) {
		setout($2);
		topeval($1);
		unsetout();
	    } else {
		topeval($1);
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
    ASSIGN exp '=' exp
{
	$$ = build(O_ASSIGN, unrval($2), $4);
}
|
    CATCH signal
{
	$$ = build(O_CATCH, $2);
}
|
    CATCH
{
	$$ = build(O_CATCH, 0);
}
|
    CONT
{
	$$ = build(O_CONT, (long) DEFSIG);
}
|
    CONT signal
{
	$$ = build(O_CONT, $2);
}
|
    DELETE integer_list
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
    FUNC opt_qual_symbol
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
    IGNORE signal
{
	$$ = build(O_IGNORE, $2);
}
|
    IGNORE
{
	$$ = build(O_IGNORE, 0);
}
|
    list_command
{
	$$ = $1;
}
|
    PSYM exp
{
	$$ = build(O_PSYM, unrval($2));
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
    RETURN opt_qual_symbol
{
	$$ = build(O_RETURN, $2);
}
|
    runcommand
{
	run();
	/* NOTREACHED */
}
|
    SET name '=' exp
{
	$$ = build(O_SET, build(O_NAME, $2), $4);
}
|
    SET name
{
	$$ = build(O_SET, build(O_NAME, $2), nil);
}
|
    SET
{
	$$ = build(O_SET, nil, nil);
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
    UNALIAS name
{
	$$ = build(O_UNALIAS, build(O_NAME, $2));
}
|
    UNSET name
{
	$$ = build(O_UNSET, build(O_NAME, $2));
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
    WHATIS opt_qual_symbol
{
	$$ = build(O_WHATIS, $2);
}
|
    WHEN event '{' actions '}'
{
	$$ = build(O_ADDEVENT, $2, $4);
}
|
    WHEREIS name
{
	$$ = build(O_WHEREIS, build(O_SYM, lookup($2)));
}
|
    WHICH symbol
{
	$$ = build(O_WHICH, $2);
}
|
    '/'
{
	$$ = build(O_SEARCH,
	    build(O_LCON, (long) '/'),
	    build(O_SCON, strdup(scanner_linebuf))
	);
	gobble();
	insertinput("\n");
}
|
    '?'
{
	$$ = build(O_SEARCH,
	    build(O_LCON, (long) '?'),
	    build(O_SCON, strdup(scanner_linebuf))
	);
	gobble();
	insertinput("\n");
}
;
signal:
    INT
{
	$$ = $1;
}
|
    name
{
	$$ = siglookup(ident($1));
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
    CALL term '(' opt_exp_list ')'
{
	$$ = build(O_CALLPROC, $2, $4);
}
|
    DEBUG INT
{
 	$$ = build(O_DEBUG, $2);
}
|
    DEBUG '-' INT
{
	$$ = build(O_DEBUG, -$3);
}
|
    DUMP opt_qual_symbol
{
	$$ = build(O_DUMP, $2);
}
|
    DUMP '.'
{
	$$ = build(O_DUMP, nil);
}
|
    DUMP
{
	$$ = build(O_DUMP, build(O_SYM, curfunc));
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
    ALIAS name STRING
{
	$$ = build(O_ALIAS, build(O_NAME, $2), build(O_SCON, $3));
}
|
    ALIAS name '(' name_list ')' STRING
{
	$$ = build(O_ALIAS,
	    build(O_COMMA, build(O_NAME, $2), (Node) $4),
	    build(O_SCON, $6)
	);
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
name_list:
    name_list ',' name
{
	$$ = $1;
	list_append(list_item($3), nil, $$);
}
|
    name
{
	$$ = list_alloc();
	list_append(list_item($1), nil, $$);
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
    IN exp
{
	$$ = unrval($2);
}
|
    AT line_number
{
	$$ = build(O_QLINE, build(O_SCON, strdup(cursource)), $2);
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
    LIST opt_qual_symbol
{
	$$ = build(O_LIST, $2);
}
;
integer_list:
    INT
{
	$$ = build(O_LCON, $1);
}
|
    INT integer_list
{
	$$ = build(O_COMMA, build(O_LCON, $1), $2);
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
    '.'
{
	$$ = build(O_LCON, (long) prtaddr);
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
    '-' address %prec UNARYSIGN
{
	$$ = build(O_NEG, $2);
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
    term '.' name
{
	$$ = unrval(dot($1, $3));
}
|
    term ARROW name
{
	$$ = unrval(dot($1, $3));
}
|
    term '[' exp_list ']'
{
	$$ = unrval(subscript($1, $3));
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
    symbol
{
	$$ = build(O_RVAL, $1);
}
|
    exp '[' exp_list ']'
{
	$$ = subscript(unrval($1), $3);
}
|
    exp '.' name
{
	$$ = dot($1, $3);
}
|
    exp ARROW name
{
	$$ = dot($1, $3);
}
|
    '*' exp %prec UNARYSIGN
{
	$$ = build(O_INDIR, $2);
}
|
    exp '^' %prec UNARYSIGN
{
	$$ = build(O_INDIR, $1);
}
|
    exp '\\' opt_qual_symbol
{
	$$ = build(O_TYPERENAME, $1, $3);
}
|
    exp '\\' '&' opt_qual_symbol %prec '\\'
{
	$$ = renameptr($1, $4);
}
|
    exp '(' opt_exp_list ')'
{
	$$ = build(O_CALL, unrval($1), $3);
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
    CHAR
{
	$$ = build(O_CCON, $1);
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
	$$ = findvar($1);
	if ($$ == nil) {
	    $$ = build(O_SYM, which($1));
	}
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
    RERUN | RETURN | RUN | SET | SH | SKIP | SOURCE | STATUS | STEP | STEPI |
    STOP | STOPI | TRACE | TRACEI | UNALIAS | UNSET | UP | USE |
    WHATIS | WHEN | WHERE | WHEREIS | WHICH
;
