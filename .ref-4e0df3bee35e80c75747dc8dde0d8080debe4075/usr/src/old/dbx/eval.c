/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)eval.c 1.4 %G%";

/*
 * Tree evaluation.
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
#include "eval.h"
#include "events.h"
#include "symbols.h"
#include "scanner.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "process.h"
#include "machine.h"
#include <signal.h>

#ifndef public

#include "machine.h"

#define STACKSIZE 2000

typedef Char Stack;

#define push(type, value) { \
    ((type *) (sp += sizeof(type)))[-1] = (value); \
}

#define pop(type) ( \
    (*((type *) (sp -= sizeof(type)))) \
)

#define alignstack() { \
    sp = (Stack *) (( ((int) sp) + sizeof(int) - 1)&~(sizeof(int) - 1)); \
}

#endif

public Stack stack[STACKSIZE];
public Stack *sp = &stack[0];

#define chksp() \
{ \
    if (sp < &stack[0]) { \
	panic("stack underflow"); \
    } \
}

#define poparg(n, r, fr) { \
    eval(p->value.arg[n]); \
    if (isreal(p->op)) { \
	fr = pop(double); \
    } else if (isint(p->op)) { \
	r = popsmall(p->value.arg[n]->nodetype); \
    } \
}

#define Boolrep char	/* underlying representation type for booleans */

/*
 * Evaluate a parse tree leaving the value on the top of the stack.
 */

public eval(p)
register Node p;
{
    long r0, r1;
    double fr0, fr1;
    Address addr;
    long i, n;
    int len;
    Symbol s, f;
    Node n1, n2;
    Boolean b;
    File file;

    checkref(p);
    switch (degree(p->op)) {
	case BINARY:
	    poparg(1, r1, fr1);
	    poparg(0, r0, fr0);
	    break;

	case UNARY:
	    poparg(0, r0, fr0);
	    break;

	default:
	    /* do nothing */;
    }
    switch (p->op) {
	case O_SYM:
	    s = p->value.sym;
	    if (s == retaddrsym) {
		push(long, return_addr());
	    } else {
		if (isvariable(s)) {
		    if (s != program and not isactive(container(s))) {
			error("\"%s\" is not active", symname(s));
		    }
		    push(long, address(s, nil));
		} else if (isblock(s)) {
		    push(Symbol, s);
		} else {
		    error("can't evaluate a %s", classname(s));
		}
	    }
	    break;

	case O_LCON:
	    r0 = p->value.lcon;
	    pushsmall(p->nodetype, r0);
	    break;

	case O_FCON:
	    push(double, p->value.fcon);
	    break;

	case O_SCON:
	    len = size(p->nodetype);
	    mov(p->value.scon, sp, len);
	    sp += len;
	    break;

	case O_INDEX:
	    n = pop(long);
	    i = evalindex(p->value.arg[0]->nodetype,
		popsmall(p->value.arg[1]->nodetype));
	    push(long, n + i*size(p->nodetype));
	    break;

	case O_DOT:
	    s = p->value.arg[1]->value.sym;
	    n = lval(p->value.arg[0]);
	    push(long, n + (s->symvalue.field.offset div 8));
	    break;

	/*
	 * Get the value of the expression addressed by the top of the stack.
	 * Push the result back on the stack.
	 */

	case O_INDIR:
	case O_RVAL:
	    addr = pop(long);
	    if (addr == 0) {
		error("reference through nil pointer");
	    }
	    if (p->op == O_INDIR) {
		len = sizeof(long);
	    } else {
		len = size(p->nodetype);
	    }
	    rpush(addr, len);
	    break;

	/*
	 * Effectively, we want to pop n bytes off for the evaluated subtree
	 * and push len bytes on for the new type of the same tree.
	 */
	case O_TYPERENAME:
	    n = size(p->value.arg[0]->nodetype);
	    len = size(p->nodetype);
	    sp = sp - n + len;
	    break;

	case O_COMMA:
	    break;

	case O_ITOF:
	    push(double, (double) r0);
	    break;

	case O_ADD:
	    push(long, r0+r1);
	    break;

	case O_ADDF:
	    push(double, fr0+fr1);
	    break;

	case O_SUB:
	    push(long, r0-r1);
	    break;

	case O_SUBF:
	    push(double, fr0-fr1);
	    break;

	case O_NEG:
	    push(long, -r0);
	    break;

	case O_NEGF:
	    push(double, -fr0);
	    break;

	case O_MUL:
	    push(long, r0*r1);
	    break;

	case O_MULF:
	    push(double, fr0*fr1);
	    break;

	case O_DIVF:
	    if (fr1 == 0) {
		error("error: division by 0");
	    }
	    push(double, fr0 / fr1);
	    break;

	case O_DIV:
	    if (r1 == 0) {
		error("error: div by 0");
	    }
	    push(long, r0 div r1);
	    break;

	case O_MOD:
	    if (r1 == 0) {
		error("error: mod by 0");
	    }
	    push(long, r0 mod r1);
	    break;

	case O_LT:
	    push(Boolrep, r0 < r1);
	    break;

	case O_LTF:
	    push(Boolrep, fr0 < fr1);
	    break;

	case O_LE:
	    push(Boolrep, r0 <= r1);
	    break;

	case O_LEF:
	    push(Boolrep, fr0 <= fr1);
	    break;

	case O_GT:
	    push(Boolrep, r0 > r1);
	    break;

	case O_GTF:
	    push(Boolrep, fr0 > fr1);
	    break;

	case O_EQ:
	    push(Boolrep, r0 == r1);
	    break;

	case O_EQF:
	    push(Boolrep, fr0 == fr1);
	    break;

	case O_NE:
	    push(Boolrep, r0 != r1);
	    break;

	case O_NEF:
	    push(Boolrep, fr0 != fr1);
	    break;

	case O_AND:
	    push(Boolrep, r0 and r1);
	    break;

	case O_OR:
	    push(Boolrep, r0 or r1);
	    break;

	case O_ASSIGN:
	    assign(p->value.arg[0], p->value.arg[1]);
	    break;

	case O_CHFILE:
	    if (p->value.scon == nil) {
		printf("%s\n", cursource);
	    } else {
		file = opensource(p->value.scon);
		if (file == nil) {
		    error("can't read \"%s\"", p->value.scon);
		} else {
		    fclose(file);
		    setsource(p->value.scon);
		}
	    }
	    break;

	case O_CONT:
	    cont();
	    printnews();
	    break;

	case O_LIST:
	    if (p->value.arg[0]->op == O_SYM) {
		f = p->value.arg[0]->value.sym;
		addr = firstline(f);
		if (addr == NOADDR) {
		    error("no source lines for \"%s\"", symname(f));
		}
		setsource(srcfilename(addr));
		r0 = srcline(addr) - 5;
		r1 = r0 + 10;
		if (r0 < 1) {
		    r0 = 1;
		}
	    } else {
		eval(p->value.arg[0]);
		r0 = pop(long);
		eval(p->value.arg[1]);
		r1 = pop(long);
	    }
	    printlines((Lineno) r0, (Lineno) r1);
	    break;

	case O_FUNC:
	    if (p->value.arg[0] == nil) {
		printname(stdout, curfunc);
		putchar('\n');
	    } else {
		curfunc = p->value.arg[0]->value.sym;
		addr = codeloc(curfunc);
		if (addr != NOADDR) {
		    setsource(srcfilename(addr));
		    cursrcline = srcline(addr) - 5;
		    if (cursrcline < 1) {
			cursrcline = 1;
		    }
		}
	    }
	    break;

	case O_EXAMINE:
	    eval(p->value.examine.beginaddr);
	    r0 = pop(long);
	    if (p->value.examine.endaddr == nil) {
		n = p->value.examine.count;
		if (n == 0) {
		    printvalue(r0, p->value.examine.mode);
		} else if (streq(p->value.examine.mode, "i")) {
		    printninst(n, (Address) r0);
		} else {
		    printndata(n, (Address) r0, p->value.examine.mode);
		}
	    } else {
		eval(p->value.examine.endaddr);
		r1 = pop(long);
		if (streq(p->value.examine.mode, "i")) {
		    printinst((Address)r0, (Address)r1);
		} else {
		    printdata((Address)r0, (Address)r1, p->value.examine.mode);
		}
	    }
	    break;

	case O_PRINT:
	    for (n1 = p->value.arg[0]; n1 != nil; n1 = n1->value.arg[1]) {
		eval(n1->value.arg[0]);
		printval(n1->value.arg[0]->nodetype);
		putchar(' ');
	    }
	    putchar('\n');
	    break;

	case O_PSYM:
	    if (p->value.arg[0]->op == O_SYM) {
		psym(p->value.arg[0]->value.sym);
	    } else {
		psym(p->value.arg[0]->nodetype);
	    }
	    break;

	case O_QLINE:
	    eval(p->value.arg[1]);
	    break;

	case O_STEP:
	    b = inst_tracing;
	    inst_tracing = (Boolean) (not p->value.step.source);
	    if (p->value.step.skipcalls) {
		next();
	    } else {
		stepc();
	    }
	    inst_tracing = b;
	    printnews();
	    break;

	case O_WHATIS:
	    if (p->value.arg[0]->op == O_SYM) {
		printdecl(p->value.arg[0]->value.sym);
	    } else {
		printdecl(p->value.arg[0]->nodetype);
	    }
	    break;

	case O_WHERE:
	    wherecmd();
	    break;

	case O_WHEREIS:
	    printwhereis(stdout, p->value.arg[0]->value.sym);
	    break;

	case O_WHICH:
	    printwhich(stdout, p->value.arg[0]->value.sym);
	    putchar('\n');
	    break;

	case O_ALIAS:
	    n1 = p->value.arg[0];
	    n2 = p->value.arg[1];
	    if (n1 == nil) {
		print_alias(nil);
	    } else if (n2 == nil) {
		print_alias(n1->value.name);
	    } else {
		enter_alias(n1->value.name, n2->value.name);
	    }
	    break;

	case O_CALL:
	    callproc(p->value.arg[0], p->value.arg[1]);
	    break;

	case O_CATCH:
	    psigtrace(process, p->value.lcon, true);
	    break;

	case O_EDIT:
	    edit(p->value.scon);
	    break;

	case O_DUMP:
	    dump();
	    break;

	case O_GRIPE:
	    gripe();
	    break;

	case O_HELP:
	    help();
	    break;

	case O_IGNORE:
	    psigtrace(process, p->value.lcon, false);
	    break;

	case O_RUN:
	    run();
	    break;

	case O_SOURCE:
	    setinput(p->value.scon);
	    break;

	case O_STATUS:
	    status();
	    break;

	case O_TRACE:
	case O_TRACEI:
	    trace(p);
	    if (isstdin()) {
		status();
	    }
	    break;

	case O_STOP:
	case O_STOPI:
	    stop(p);
	    if (isstdin()) {
		status();
	    }
	    break;

	case O_ADDEVENT:
	    addevent(p->value.event.cond, p->value.event.actions);
	    break;

	case O_DELETE:
	    delevent((unsigned int) p->value.lcon);
	    break;

	case O_ENDX:
	    endprogram();
	    break;

	case O_IF:
	    if (cond(p->value.event.cond)) {
		evalcmdlist(p->value.event.actions);
	    }
	    break;

	case O_ONCE:
	    event_once(p->value.event.cond, p->value.event.actions);
	    break;

	case O_PRINTCALL:
	    printcall(p->value.sym, whatblock(return_addr()));
	    break;

	case O_PRINTIFCHANGED:
	    printifchanged(p->value.arg[0]);
	    break;

	case O_PRINTRTN:
	    printrtn(p->value.sym);
	    break;

	case O_PRINTSRCPOS:
	    getsrcpos();
	    if (p->value.arg[0] == nil) {
		printsrcpos();
		putchar('\n');
		printlines(curline, curline);
	    } else if (p->value.arg[0]->op == O_QLINE) {
		if (p->value.arg[0]->value.arg[1]->value.lcon == 0) {
		    printf("tracei: ");
		    printinst(pc, pc);
		} else {
		    printf("trace:  ");
		    printlines(curline, curline);
		}
	    } else {
		printsrcpos();
		printf(": ");
		eval(p->value.arg[0]);
		prtree(stdout, p->value.arg[0]);
		printf(" = ");
		printval(p->value.arg[0]->nodetype);
		putchar('\n');
	    }
	    break;

	case O_PROCRTN:
	    procreturn(p->value.sym);
	    break;

	case O_STOPIFCHANGED:
	    stopifchanged(p->value.arg[0]);
	    break;

	case O_STOPX:
	    isstopped = true;
	    break;

	case O_TRACEON:
	    traceon(p->value.trace.inst, p->value.trace.event,
		p->value.trace.actions);
	    break;

	case O_TRACEOFF:
	    traceoff(p->value.lcon);
	    break;

	default:
	    panic("eval: bad op %d", p->op);
    }
}

/*
 * Evaluate a list of commands.
 */

public evalcmdlist(cl)
Cmdlist cl;
{
    Command c;

    foreach (Command, c, cl)
	evalcmd(c);
    endfor
}

/*
 * Push "len" bytes onto the expression stack from address "addr"
 * in the process.  If there isn't room on the stack, print an error message.
 */

public rpush(addr, len)
Address addr;
int len;
{
    if (not canpush(len)) {
	error("expression too large to evaluate");
    } else {
	chksp();
	dread(sp, addr, len);
	sp += len;
    }
}

/*
 * Check if the stack has n bytes available.
 */

public Boolean canpush(n)
Integer n;
{
    return (Boolean) (sp + n < &stack[STACKSIZE]);
}

/*
 * Push a small scalar of the given type onto the stack.
 */

public pushsmall(t, v)
Symbol t;
long v;
{
    register Integer s;

    s = size(t);
    switch (s) {
	case sizeof(char):
	    push(char, v);
	    break;

	case sizeof(short):
	    push(short, v);
	    break;

	case sizeof(long):
	    push(long, v);
	    break;

	default:
	    panic("bad size %d in popsmall", s);
    }
}

/*
 * Pop an item of the given type which is assumed to be no larger
 * than a long and return it expanded into a long.
 */

public long popsmall(t)
Symbol t;
{
    long r;

    switch (size(t)) {
	case sizeof(char):
	    r = (long) pop(char);
	    break;

	case sizeof(short):
	    r = (long) pop(short);
	    break;

	case sizeof(long):
	    r = pop(long);
	    break;

	default:
	    panic("popsmall: size is %d", size(t));
    }
    return r;
}

/*
 * Evaluate a conditional expression.
 */

public Boolean cond(p)
Node p;
{
    register Boolean b;

    if (p == nil) {
	b = true;
    } else {
	eval(p);
	b = pop(Boolean);
    }
    return b;
}

/*
 * Return the address corresponding to a given tree.
 */

public Address lval(p)
Node p;
{
    if (p->op == O_RVAL) {
	eval(p->value.arg[0]);
    } else {
	eval(p);
    }
    return (Address) (pop(long));
}

/*
 * Process a trace command, translating into the appropriate events
 * and associated actions.
 */

public trace(p)
Node p;
{
    Node exp, place, cond;
    Node left;

    exp = p->value.arg[0];
    place = p->value.arg[1];
    cond = p->value.arg[2];
    if (exp == nil) {
	traceall(p->op, place, cond);
    } else if (exp->op == O_QLINE) {
	traceinst(p->op, exp, cond);
    } else if (place != nil and place->op == O_QLINE) {
	traceat(p->op, exp, place, cond);
    } else {
	left = exp;
	if (left->op == O_RVAL or left->op == O_CALL) {
	    left = left->value.arg[0];
	}
	if (left->op == O_SYM and isblock(left->value.sym)) {
	    traceproc(p->op, left->value.sym, place, cond);
	} else {
	    tracedata(p->op, exp, place, cond);
	}
    }
}

/*
 * Set a breakpoint that will turn on tracing.
 */

private traceall(op, place, cond)
Operator op;
Node place;
Node cond;
{
    Symbol s;
    Node event;
    Command action;

    if (place == nil) {
	s = program;
    } else {
	s = place->value.sym;
    }
    event = build(O_EQ, build(O_SYM, procsym), build(O_SYM, s));
    action = build(O_PRINTSRCPOS,
	build(O_QLINE, nil, build(O_LCON, (op == O_TRACE) ? 1 : 0)));
    if (cond != nil) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    action = build(O_TRACEON, (op == O_TRACEI), buildcmdlist(action));
    action->value.trace.event = addevent(event, buildcmdlist(action));
}

/*
 * Set up the appropriate breakpoint for tracing an instruction.
 */

private traceinst(op, exp, cond)
Operator op;
Node exp;
Node cond;
{
    Node event;
    Command action;

    if (op == O_TRACEI) {
	event = build(O_EQ, build(O_SYM, pcsym), exp);
    } else {
	event = build(O_EQ, build(O_SYM, linesym), exp);
    }
    action = build(O_PRINTSRCPOS, exp);
    if (cond) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    addevent(event, buildcmdlist(action));
}

/*
 * Set a breakpoint to print an expression at a given line or address.
 */

private traceat(op, exp, place, cond)
Operator op;
Node exp;
Node place;
Node cond;
{
    Node event;
    Command action;

    if (op == O_TRACEI) {
	event = build(O_EQ, build(O_SYM, pcsym), place);
    } else {
	event = build(O_EQ, build(O_SYM, linesym), place);
    }
    action = build(O_PRINTSRCPOS, exp);
    if (cond != nil) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    addevent(event, buildcmdlist(action));
}

/*
 * Construct event for tracing a procedure.
 *
 * What we want here is
 *
 * 	when $proc = p do
 *	    if <condition> then
 *	        printcall;
 *	        once $pc = $retaddr do
 *	            printrtn;
 *	        end;
 *	    end if;
 *	end;
 *
 * Note that "once" is like "when" except that the event
 * deletes itself as part of its associated action.
 */

private traceproc(op, p, place, cond)
Operator op;
Symbol p;
Node place;
Node cond;
{
    Node event;
    Command action;
    Cmdlist actionlist;

    action = build(O_PRINTCALL, p);
    actionlist = list_alloc();
    cmdlist_append(action, actionlist);
    event = build(O_EQ, build(O_SYM, pcsym), build(O_SYM, retaddrsym));
    action = build(O_ONCE, event, buildcmdlist(build(O_PRINTRTN, p)));
    cmdlist_append(action, actionlist);
    if (cond != nil) {
	actionlist = buildcmdlist(build(O_IF, cond, actionlist));
    }
    event = build(O_EQ, build(O_SYM, procsym), build(O_SYM, p));
    addevent(event, actionlist);
}

/*
 * Set up breakpoint for tracing data.
 */

private tracedata(op, exp, place, cond)
Operator op;
Node exp;
Node place;
Node cond;
{
    Symbol p;
    Node event;
    Command action;

    p = (place == nil) ? tcontainer(exp) : place->value.sym;
    if (p == nil) {
	p = program;
    }
    action = build(O_PRINTIFCHANGED, exp);
    if (cond != nil) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    action = build(O_TRACEON, (op == O_TRACEI), buildcmdlist(action));
    event = build(O_EQ, build(O_SYM, procsym), build(O_SYM, p));
    action->value.trace.event = addevent(event, buildcmdlist(action));
}

/*
 * Setting and unsetting of stops.
 */

public stop(p)
Node p;
{
    Node exp, place, cond;
    Symbol s;
    Command action;

    exp = p->value.arg[0];
    place = p->value.arg[1];
    cond = p->value.arg[2];
    if (exp != nil) {
	stopvar(p->op, exp, place, cond);
    } else if (cond != nil) {
	s = (place == nil) ? program : place->value.sym;
	action = build(O_IF, cond, buildcmdlist(build(O_STOPX)));
	action = build(O_TRACEON, (p->op == O_STOPI), buildcmdlist(action));
	cond = build(O_EQ, build(O_SYM, procsym), build(O_SYM, s));
	action->value.trace.event = addevent(cond, buildcmdlist(action));
    } else if (place->op == O_SYM) {
	s = place->value.sym;
	cond = build(O_EQ, build(O_SYM, procsym), build(O_SYM, s));
	addevent(cond, buildcmdlist(build(O_STOPX)));
    } else {
	stopinst(p->op, place, cond);
    }
}

private stopinst(op, place, cond)
Operator op;
Node place;
Node cond;
{
    Node event;

    if (op == O_STOP) {
	event = build(O_EQ, build(O_SYM, linesym), place);
    } else {
	event = build(O_EQ, build(O_SYM, pcsym), place);
    }
    addevent(event, buildcmdlist(build(O_STOPX)));
}

/*
 * Implement stopping on assignment to a variable by adding it to
 * the variable list.
 */

private stopvar(op, exp, place, cond)
Operator op;
Node exp;
Node place;
Node cond;
{
    Symbol p;
    Node event;
    Command action;

    p = (place == nil) ? tcontainer(exp) : place->value.sym;
    if (p == nil) {
	p = program;
    }
    action = build(O_IF, cond, buildcmdlist(build(O_STOPIFCHANGED, exp)));
    action = build(O_TRACEON, (op == O_STOPI), buildcmdlist(action));
    event = build(O_EQ, build(O_SYM, procsym), build(O_SYM, p));
    action->value.trace.event = addevent(event, buildcmdlist(action));
}

/*
 * Assign the value of an expression to a variable (or term).
 */

public assign(var, exp)
Node var;
Node exp;
{
    Address addr;
    int varsize;
    char cvalue;
    short svalue;
    long lvalue;

    if (not compatible(var->nodetype, exp->nodetype)) {
	error("incompatible types");
    }
    addr = lval(var);
    eval(exp);
    varsize = size(var->nodetype);
    if (varsize < sizeof(long)) {
	lvalue = pop(long);
	switch (varsize) {
	    case sizeof(char):
		cvalue = lvalue;
		dwrite(&cvalue, addr, varsize);
		break;

	    case sizeof(short):
		svalue = lvalue;
		dwrite(&svalue, addr, varsize);
		break;

	    default:
		panic("bad size %d", varsize);
	}
    } else {
	sp -= varsize;
	dwrite(sp, addr, varsize);
    }
}

#define DEF_EDITOR  "vi"

/*
 * Invoke an editor on the given file.  Which editor to use might change
 * installation to installation.  For now, we use "vi".  In any event,
 * the environment variable "EDITOR" overrides any default.
 */

public edit(filename)
String filename;
{
    extern String getenv();
    String ed, src;
    File f;
    Symbol s;
    Address addr;
    char buff[10];

    ed = getenv("EDITOR");
    if (ed == nil) {
	ed = DEF_EDITOR;
    }
    if (filename == nil) {
	call(ed, stdin, stdout, cursource, nil);
    } else {
	f = fopen(filename, "r");
	if (f == nil) {
	    s = which(identname(filename, true));
	    if (not isblock(s)) {
		error("can't read \"%s\"", filename);
	    }
	    addr = firstline(s);
	    if (addr == NOADDR) {
		error("no source for \"%s\"", filename);
	    }
	    src = srcfilename(addr);
	    sprintf(buff, "+%d", srcline(addr));
	    call(ed, stdin, stdout, buff, src, nil);
	} else {
	    fclose(f);
	    call(ed, stdin, stdout, filename, nil);
	}
    }
}

/*
 * Send some nasty mail to the current support person.
 */

public gripe()
{
    typedef Operation();
    Operation *old;

    char *maintainer = "linton@ucbarpa";

    puts("Type control-D to end your message.  Be sure to include");
    puts("your name and the name of the file you are debugging.");
    putchar('\n');
    old = signal(SIGINT, SIG_DFL);
    call("Mail", stdin, stdout, maintainer, nil);
    signal(SIGINT, old);
    puts("Thank you.");
}

/*
 * Give the user some help.
 */

public help()
{
    puts("run                    - begin execution of the program");
    puts("cont                   - continue execution");
    puts("step                   - single step one line");
    puts("next                   - step to next line (skip over calls)");
    puts("trace <line#>          - trace execution of the line");
    puts("trace <proc>           - trace calls to the procedure");
    puts("trace <var>            - trace changes to the variable");
    puts("trace <exp> at <line#> - print <exp> when <line> is reached");
    puts("stop at <line>         - suspend execution at the line");
    puts("stop in <proc>         - suspend execution when <proc> is called");
    puts("status                 - print trace/stop's in effect");
    puts("delete <number>        - remove trace or stop of given number");
    puts("call <proc>            - call the procedure");
    puts("where                  - print currently active procedures");
    puts("print <exp>            - print the value of the expression");
    puts("whatis <name>          - print the declaration of the name");
    puts("list <line>, <line>    - list source lines");
    puts("edit <proc>            - edit file containing <proc>");
    puts("gripe                  - send mail to the person in charge of dbx");
    puts("quit                   - exit dbx");
}

/*
 * Divert output to the given file name.
 * Cannot redirect to an existing file.
 */

private int so_fd;
private Boolean notstdout;

public setout(filename)
String filename;
{
    File f;

    f = fopen(filename, "r");
    if (f != nil) {
	fclose(f);
	error("%s: file already exists", filename);
    } else {
	so_fd = dup(1);
	close(1);
	if (creat(filename, 0666) == nil) {
	    unsetout();
	    error("can't create %s", filename);
	}
	notstdout = true;
    }
}

/*
 * Revert output to standard output.
 */

public unsetout()
{
    fflush(stdout);
    close(1);
    if (dup(so_fd) != 1) {
	panic("standard out dup failed");
    }
    close(so_fd);
    notstdout = false;
}

/*
 * Determine is standard output is currently being redirected
 * to a file (as far as we know).
 */

public Boolean isredirected()
{
    return notstdout;
}
