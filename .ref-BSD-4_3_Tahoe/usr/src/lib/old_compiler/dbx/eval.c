/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)eval.c	5.5 (Berkeley) 1/12/88";
#endif not lint

static char rcsid[] = "$Header: eval.c,v 1.2 87/03/25 19:48:33 donn Exp $";

/*
 * Tree evaluation.
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
#include "debug.h"
#include "eval.h"
#include "events.h"
#include "symbols.h"
#include "scanner.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"
#include <signal.h>

#ifndef public

#include "machine.h"

#define STACKSIZE 20000

typedef Char Stack;

#define push(type, value) { \
    ((type *) (sp += sizeof(type)))[-1] = (value); \
}

#define pop(type) ( \
    (*((type *) (sp -= sizeof(type)))) \
)

#define popn(n, dest) { \
    sp -= n; \
    bcopy(sp, dest, n); \
}

#define alignstack() { \
    sp = (Stack *) (( ((int) sp) + sizeof(int) - 1)&~(sizeof(int) - 1)); \
}

#endif

public Stack stack[STACKSIZE];
public Stack *sp = &stack[0];
public Boolean useInstLoc = false;

#define chksp() \
{ \
    if (sp < &stack[0]) { \
	panic("stack underflow"); \
    } \
}

#define poparg(n, r, fr) { \
    eval(p->value.arg[n]); \
    if (isreal(p->op)) { \
	if (size(p->value.arg[n]->nodetype) == sizeof(float)) { \
	    fr = pop(float); \
	} else { \
	    fr = pop(double); \
	} \
    } else if (isint(p->op)) { \
	r = popsmall(p->value.arg[n]->nodetype); \
    } \
}

#define Boolrep char	/* underlying representation type for booleans */

/*
 * Command-level evaluation.
 */

public Node topnode;

public topeval (p)
Node p;
{
    if (traceeval) {
	fprintf(stderr, "topeval(");
	prtree(stderr, p);
	fprintf(stderr, ")\n");
	fflush(stderr);
    }
    topnode = p;
    eval(p);
}

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
    Symbol s;
    Node n1, n2;
    boolean b;
    File file;
    String str;

    checkref(p);
    if (traceeval) {
	fprintf(stderr, "begin eval %s\n", opname(p->op));
    }
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
	    } else if (isvariable(s)) {
		if (s != program and not isactive(container(s))) {
		    error("\"%s\" is not active", symname(s));
		}
		if (isvarparam(s) and not isopenarray(s)) {
		    rpush(address(s, nil), sizeof(Address));
		} else {
		    push(Address, address(s, nil));
		}
	    } else if (isblock(s)) {
		push(Symbol, s);
	    } else if (isconst(s)) {
		eval(constval(s));
	    } else {
		error("can't evaluate a %s", classname(s));
	    }
	    break;

	case O_LCON:
	case O_CCON:
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
	    s = p->value.arg[0]->nodetype;
	    p->value.arg[0]->nodetype = t_addr;
	    eval(p->value.arg[0]);
	    p->value.arg[0]->nodetype = s;
	    n = pop(Address);
	    eval(p->value.arg[1]);
	    evalindex(s, n, popsmall(p->value.arg[1]->nodetype));
	    break;

	case O_DOT:
	    s = p->value.arg[1]->value.sym;
	    eval(p->value.arg[0]);
	    n = pop(long);
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
	    len = size(p->nodetype);
	    rpush(addr, len);
	    break;

	case O_TYPERENAME:
	    loophole(size(p->value.arg[0]->nodetype), size(p->nodetype));
	    break;

	case O_COMMA:
	    eval(p->value.arg[0]);
	    if (p->value.arg[1] != nil) {
		eval(p->value.arg[1]);
	    }
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
	    cont(p->value.lcon);
	    printnews();
	    break;

	case O_LIST:
	    list(p);
	    break;

	case O_FUNC:
	    func(p->value.arg[0]);
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
	    useInstLoc = (Boolean) (not p->value.step.source);
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
	    if (p->value.arg[0]->op == O_SYM) {
		printwhereis(stdout, p->value.arg[0]->value.sym);
	    } else {
		printwhereis(stdout, p->value.arg[0]->nodetype);
	    }
	    break;

	case O_WHICH:
	    if (p->value.arg[0]->op == O_SYM) {
		printwhich(stdout, p->value.arg[0]->value.sym);
	    } else {
		printwhich(stdout, p->value.arg[0]->nodetype);
	    }
	    putchar('\n');
	    break;

	case O_ALIAS:
	    n1 = p->value.arg[0];
	    n2 = p->value.arg[1];
	    if (n2 == nil) {
		if (n1 == nil) {
		    alias(nil, nil, nil);
		} else {
		    alias(n1->value.name, nil, nil);
		}
	    } else if (n2->op == O_NAME) {
		str = ident(n2->value.name);
		alias(n1->value.name, nil, strdup(str));
	    } else {
		if (n1->op == O_COMMA) {
		    alias(
			n1->value.arg[0]->value.name,
			(List) n1->value.arg[1],
			n2->value.scon
		    );
		} else {
		    alias(n1->value.name, nil, n2->value.scon);
		}
	    }
	    break;

	case O_UNALIAS:
	    unalias(p->value.arg[0]->value.name);
	    break;

	case O_CALLPROC:
	    callproc(p, false);
	    break;

	case O_CALL:
	    callproc(p, true);
	    break;

	case O_CATCH:
	    if (p->value.lcon == 0) {
		printsigscaught(process);
	    } else {
		psigtrace(process, p->value.lcon, true);
	    }
	    break;

	case O_EDIT:
	    edit(p->value.scon);
	    break;

        case O_DEBUG:
            debug(p);
	    break;

	case O_DOWN:
	    checkref(p->value.arg[0]);
	    assert(p->value.arg[0]->op == O_LCON);
	    down(p->value.arg[0]->value.lcon);
	    break;

	case O_DUMP:
	    if (p->value.arg[0] == nil) {
		dumpall();
	    } else {
		s = p->value.arg[0]->value.sym;
		if (s == curfunc) {
		    dump(nil);
		} else {
		    dump(s);
		}
	    }
	    break;

	case O_GRIPE:
	    gripe();
	    break;

	case O_HELP:
	    help();
	    break;

	case O_IGNORE:
	    if (p->value.lcon == 0) {
		printsigsignored(process);
	    } else {
		psigtrace(process, p->value.lcon, false);
	    }
	    break;

	case O_RETURN:
	    if (p->value.arg[0] == nil) {
		rtnfunc(nil);
	    } else {
		assert(p->value.arg[0]->op == O_SYM);
		rtnfunc(p->value.arg[0]->value.sym);
	    }
	    break;

	case O_RUN:
	    run();
	    break;

	case O_SET:
	    set(p->value.arg[0], p->value.arg[1]);
	    break;

	case O_SEARCH:
	    search(p->value.arg[0]->value.lcon, p->value.arg[1]->value.scon);
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
	    break;

	case O_STOP:
	case O_STOPI:
	    stop(p);
	    break;

	case O_UNSET:
	    undefvar(p->value.arg[0]->value.name);
	    break;

	case O_UP:
	    checkref(p->value.arg[0]);
	    assert(p->value.arg[0]->op == O_LCON);
	    up(p->value.arg[0]->value.lcon);
	    break;

	case O_ADDEVENT:
	    addevent(p->value.event.cond, p->value.event.actions);
	    break;

	case O_DELETE:
	    n1 = p->value.arg[0];
	    while (n1->op == O_COMMA) {
		n2 = n1->value.arg[0];
		assert(n2->op == O_LCON);
		if (not delevent((unsigned int) n2->value.lcon)) {
		    error("unknown event %ld", n2->value.lcon);
		}
		n1 = n1->value.arg[1];
	    }
	    assert(n1->op == O_LCON);
	    if (not delevent((unsigned int) n1->value.lcon)) {
		error("unknown event %ld", n1->value.lcon);
	    }
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
		    if (canReadSource()) {
			printf("trace:  ");
			printlines(curline, curline);
		    }
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
    if (traceeval) { 
	fprintf(stderr, "end eval %s\n", opname(p->op));
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
    register integer n;
    long r;

    n = size(t);
    if (n == sizeof(char)) {
	if (t->class == RANGE and t->symvalue.rangev.lower >= 0) {
	    r = (long) pop(unsigned char);
	} else {
	    r = (long) pop(char);
	}
    } else if (n == sizeof(short)) {
	if (t->class == RANGE and t->symvalue.rangev.lower >= 0) {
	    r = (long) pop(unsigned short);
	} else {
	    r = (long) pop(short);
	}
    } else if (n == sizeof(long)) {
	r = pop(long);
    } else {
	error("[internal error: size %d in popsmall]", n);
    }
    return r;
}

/*
 * Evaluate a conditional expression.
 */

public Boolean cond(p)
Node p;
{
    Boolean b;
    int i;

    if (p == nil) {
	b = true;
    } else {
	eval(p);
	i = pop(Boolrep);
	b = (Boolean) i;
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
    } else if (exp->op == O_QLINE or exp->op == O_LCON) {
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
    if (isstdin()) {
	printevent(action->value.trace.event);
    }
}

/*
 * Set up the appropriate breakpoint for tracing an instruction.
 */

private traceinst(op, exp, cond)
Operator op;
Node exp;
Node cond;
{
    Node event, wh;
    Command action;
    Event e;

    if (exp->op == O_LCON) {
	wh = build(O_QLINE, build(O_SCON, strdup(cursource)), exp);
    } else {
	wh = exp;
    }
    if (op == O_TRACEI) {
	event = build(O_EQ, build(O_SYM, pcsym), wh);
    } else {
	event = build(O_EQ, build(O_SYM, linesym), wh);
    }
    action = build(O_PRINTSRCPOS, wh);
    if (cond) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    e = addevent(event, buildcmdlist(action));
    if (isstdin()) {
	printevent(e);
    }
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
    Event e;

    if (op == O_TRACEI) {
	event = build(O_EQ, build(O_SYM, pcsym), place);
    } else {
	event = build(O_EQ, build(O_SYM, linesym), place);
    }
    action = build(O_PRINTSRCPOS, exp);
    if (cond != nil) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    e = addevent(event, buildcmdlist(action));
    if (isstdin()) {
	printevent(e);
    }
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
    Event e;

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
    e = addevent(event, actionlist);
    if (isstdin()) {
	printevent(e);
    }
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

    if (size(exp->nodetype) > MAXTRSIZE) {
	error("expression too large to trace (limit is %d bytes)", MAXTRSIZE);
    }
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
    if (isstdin()) {
	printevent(action->value.trace.event);
    }
}

/*
 * Setting and unsetting of stops.
 */

public stop(p)
Node p;
{
    Node exp, place, cond, t;
    Symbol s;
    Command action;
    Event e;

    exp = p->value.arg[0];
    place = p->value.arg[1];
    cond = p->value.arg[2];
    if (exp != nil) {
	stopvar(p->op, exp, place, cond);
    } else {
	action = build(O_STOPX);
	if (cond != nil) {
	    action = build(O_IF, cond, buildcmdlist(action));
	}
	if (place == nil or place->op == O_SYM) {
	    if (place == nil) {
		s = program;
	    } else {
		s = place->value.sym;
	    }
	    t = build(O_EQ, build(O_SYM, procsym), build(O_SYM, s));
	    if (cond != nil) {
		action = build(O_TRACEON, (p->op == O_STOPI),
		    buildcmdlist(action));
		e = addevent(t, buildcmdlist(action));
		action->value.trace.event = e;
	    } else {
		e = addevent(t, buildcmdlist(action));
	    }
	    if (isstdin()) {
		printevent(e);
	    }
	} else {
	    stopinst(p->op, place, cond, action);
	}
    }
}

private stopinst(op, place, cond, action)
Operator op;
Node place;
Node cond;
Command action;
{
    Node event;
    Event e;

    if (op == O_STOP) {
	event = build(O_EQ, build(O_SYM, linesym), place);
    } else {
	event = build(O_EQ, build(O_SYM, pcsym), place);
    }
    e = addevent(event, buildcmdlist(action));
    if (isstdin()) {
	printevent(e);
    }
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

    if (size(exp->nodetype) > MAXTRSIZE) {
	error("expression too large to trace (limit is %d bytes)", MAXTRSIZE);
    }
    if (place == nil) {
	if (exp->op == O_LCON) {
	    p = program;
	} else {
	    p = tcontainer(exp);
	    if (p == nil) {
		p = program;
	    }
	}
    } else {
	p = place->value.sym;
    }
    action = build(O_STOPIFCHANGED, exp);
    if (cond != nil) {
	action = build(O_IF, cond, buildcmdlist(action));
    }
    action = build(O_TRACEON, (op == O_STOPI), buildcmdlist(action));
    event = build(O_EQ, build(O_SYM, procsym), build(O_SYM, p));
    action->value.trace.event = addevent(event, buildcmdlist(action));
    if (isstdin()) {
	printevent(action->value.trace.event);
    }
}

/*
 * Assign the value of an expression to a variable (or term).
 */

public assign(var, exp)
Node var;
Node exp;
{
    Address addr;
    integer varsize, expsize;
    char cvalue;
    short svalue;
    long lvalue;
    float fvalue;

    if (var->op == O_SYM and regnum(var->value.sym) != -1) {
	eval(exp);
	setreg(regnum(var->value.sym), pop(Address));
    } else {
	addr = lval(var);
	varsize = size(var->nodetype);
	expsize = size(exp->nodetype);
	eval(exp);
	if (varsize == sizeof(float) and expsize == sizeof(double)) {
	    fvalue = (float) pop(double);
	    dwrite(&fvalue, addr, sizeof(fvalue));
	} else {
	    if (varsize < sizeof(long)) {
		lvalue = 0;
		popn(expsize, &lvalue);
		if (varsize == sizeof(char)) {
		    cvalue = lvalue;
		    dwrite(&cvalue, addr, sizeof(cvalue));
		} else if (varsize == sizeof(short)) {
		    svalue = lvalue;
		    dwrite(&svalue, addr, sizeof(svalue));
		} else {
		    error("[internal error: bad size %d in assign]", varsize);
		}
	    } else {
		if (expsize <= varsize) {
		    sp -= expsize;
		    dwrite(sp, addr, expsize);
		} else {
		    sp -= expsize;
		    dwrite(sp, addr, varsize);
		}
	    }
	}
    }
}

/*
 * Set a debugger variable.
 */

private set (var, exp)
Node var, exp;
{
    Symbol t;

    if (var == nil) {
	defvar(nil, nil);
    } else if (exp == nil) {
	defvar(var->value.name, nil);
    } else if (var->value.name == identname("$frame", true)) {
	t = exp->nodetype;
	if (not compatible(t, t_int) and not compatible(t, t_addr)) {
	    error("$frame must be an address");
	}
	eval(exp);
	getnewregs(pop(Address));
    } else {
	defvar(var->value.name, unrval(exp));
    }
}

/*
 * Execute a list command.
 */

private list (p)
Node p;
{
    Symbol f;
    Address addr;
    Lineno line, l1, l2;

    if (p->value.arg[0]->op == O_SYM) {
	f = p->value.arg[0]->value.sym;
	addr = firstline(f);
	if (addr == NOADDR) {
	    error("no source lines for \"%s\"", symname(f));
	}
	setsource(srcfilename(addr));
	line = srcline(addr);
	getsrcwindow(line, &l1, &l2);
    } else {
	eval(p->value.arg[0]);
	l1 = (Lineno) (pop(long));
	eval(p->value.arg[1]);
	l2 = (Lineno) (pop(long));
    }
    printlines(l1, l2);
}

/*
 * Execute a func command.
 */

private func (p)
Node p;
{
    Symbol s, f;
    Address addr;

    if (p == nil) {
	printname(stdout, curfunc);
	putchar('\n');
    } else {
	s = p->value.sym;
	if (isroutine(s)) {
	    setcurfunc(s);
	} else {
	    find(f, s->name) where isroutine(f) endfind(f);
	    if (f == nil) {
		error("%s is not a procedure or function", symname(s));
	    }
	    setcurfunc(f);
	}
	addr = codeloc(curfunc);
	if (addr != NOADDR) {
	    setsource(srcfilename(addr));
	    cursrcline = srcline(addr);
	}
    }
}

/*
 * Send a message to the current support person.
 */

public gripe()
{
    typedef Operation();
    Operation *old;
    int pid, status;
    extern int versionNumber;
    char subject[100];

#   ifdef MAINTAINER
	puts("Type control-D to end your message.  Be sure to include");
	puts("your name and the name of the file you are debugging.");
	putchar('\n');
	old = signal(SIGINT, SIG_DFL);
	sprintf(subject, "dbx (version 3.%d) gripe", versionNumber);
	pid = back("Mail", stdin, stdout, "-s", subject, MAINTAINER, nil);
	signal(SIGINT, SIG_IGN);
	pwait(pid, &status);
	signal(SIGINT, old);
	if (status == 0) {
	    puts("Thank you.");
	} else {
	    puts("\nMail not sent.");
	}
#   else
	puts("Sorry, no dbx maintainer available to gripe to.");
	puts("Try contacting your system manager.");
#   endif
}

/*
 * Give the user some help.
 */

public help()
{
    puts("run                    - begin execution of the program");
    puts("print <exp>            - print the value of the expression");
    puts("where                  - print currently active procedures");
    puts("stop at <line>         - suspend execution at the line");
    puts("stop in <proc>         - suspend execution when <proc> is called");
    puts("cont                   - continue execution");
    puts("step                   - single step one line");
    puts("next                   - step to next line (skip over calls)");
    puts("trace <line#>          - trace execution of the line");
    puts("trace <proc>           - trace calls to the procedure");
    puts("trace <var>            - trace changes to the variable");
    puts("trace <exp> at <line#> - print <exp> when <line> is reached");
    puts("status                 - print trace/stop's in effect");
    puts("delete <number>        - remove trace or stop of given number");
    puts("call <proc>            - call a procedure in program");
    puts("whatis <name>          - print the declaration of the name");
    puts("list <line>, <line>    - list source lines");
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
