/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)runtime.c 1.3 %G%";

/*
 * Runtime organization dependent routines, mostly dealing with
 * activation records.
 */

#include "defs.h"
#include "runtime.h"
#include "process.h"
#include "machine.h"
#include "events.h"
#include "mappings.h"
#include "symbols.h"
#include "tree.h"
#include "eval.h"
#include "operators.h"
#include "object.h"

#ifndef public
typedef struct Frame *Frame;

#include "machine.h"
#endif

#define NSAVEREG 12

struct Frame {
    Integer condition_handler;
    Integer mask;
    Address save_ap;		/* argument pointer */
    Address save_fp;		/* frame pointer */
    Address save_pc;		/* program counter */
    Word save_reg[NSAVEREG];	/* not necessarily there */
};

private Boolean walkingstack = false;

/*
 * Set a frame to the current activation record.
 */

private getcurframe(frp)
register Frame frp;
{
    register int i;

    checkref(frp);
    frp->mask = reg(NREG);
    frp->save_ap = reg(ARGP);
    frp->save_fp = reg(FRP);
    frp->save_pc = reg(PROGCTR);
    for (i = 0; i < NSAVEREG; i++) {
	frp->save_reg[i] = reg(i);
    }
}

/*
 * Return a pointer to the next activation record up the stack.
 * Return nil if there is none.
 * Writes over space pointed to by given argument.
 */

#define bis(b, n) ((b & (1 << (n))) != 0)

private Frame nextframe(frp)
Frame frp;
{
    register Frame newfrp;
    struct Frame frame;
    register Integer i, j, mask;

    newfrp = frp;
    dread(&frame, newfrp->save_fp, sizeof(struct Frame));
    if (frame.save_fp == nil) {
	newfrp = nil;
    } else {
	mask = ((frame.mask >> 16) & 0x0fff);
	j = 0;
	for (i = 0; i < NSAVEREG; i++) {
	    if (bis(mask, i)) {
		newfrp->save_reg[i] = frame.save_reg[j];
		++j;
	    }
	}
	newfrp->condition_handler = frame.condition_handler;
	newfrp->mask = mask;
	newfrp->save_ap = frame.save_ap;
	newfrp->save_fp = frame.save_fp;
	newfrp->save_pc = frame.save_pc;
    }
    return newfrp;
}

/*
 * Return the frame associated with the given function.
 * If the function is nil, return the most recently activated frame.
 *
 * Static allocation for the frame.
 */

public Frame findframe(f)
Symbol f;
{
    register Frame frp;
    static struct Frame frame;

    frp = &frame;
    getcurframe(frp);
    if (f != nil) {
	while (frp != nil and whatblock(frp->save_pc) != f) {
	    frp = nextframe(frp);
	}
    }
    return frp;
}

/*
 * Find the return address of the current procedure/function.
 */

public Address return_addr()
{
    Frame frp;
    Address addr;
    struct Frame frame;

    frp = &frame;
    getcurframe(frp);
    frp = nextframe(frp);
    if (frp == nil) {
	addr = 0;
    } else {
	addr = frp->save_pc;
    }
    return addr;
}

/*
 * Push the value associated with the current function.
 */

public pushretval(len, isindirect)
Integer len;
Boolean isindirect;
{
    Word r0;

    r0 = reg(0);
    if (isindirect) {
	rpush((Address) r0, len);
    } else {
	switch (len) {
	    case sizeof(char):
		push(char, r0);
		break;

	    case sizeof(short):
		push(short, r0);
		break;

	    default:
		if (len == sizeof(Word)) {
		    push(Word, r0);
		} else if (len == 2*sizeof(Word)) {
		    push(Word, r0);
		    push(Word, reg(1));
		} else {
		    panic("not indirect in pushretval?");
		}
		break;
	}
    }
}

/*
 * Return the base address for locals in the given frame.
 */

public Address locals_base(frp)
register Frame frp;
{
    return (frp == nil) ? reg(FRP) : frp->save_fp;
}

/*
 * Return the base address for arguments in the given frame.
 */

public Address args_base(frp)
register Frame frp;
{
    return (frp == nil) ? reg(ARGP) : frp->save_ap;
}

/*
 * Return saved register n from the given frame.
 */

public Word savereg(n, frp)
register Integer n;
register Frame frp;
{
    register Word w;

    if (frp == nil) {
	w = reg(n);
    } else {
	switch (n) {
	    case ARGP:
		w = frp->save_ap;
		break;

	    case FRP:
		w = frp->save_fp;
		break;

	    case STKP:
		w = reg(STKP);
		break;

	    case PROGCTR:
		w = frp->save_pc;
		break;

	    default:
		assert(n >= 0 and n < NSAVEREG);
		w = frp->save_reg[n];
		break;
	}
    }
    return w;
}

/*
 * Return the nth argument to the current procedure.
 */

public Word argn(n, frp)
Integer n;
Frame frp;
{
    Word w;

    dread(&w, args_base(frp) + (n * sizeof(Word)), sizeof(w));
    return w;
}

/*
 * Calculate the entry address for a procedure or function parameter,
 * given the address of the descriptor.
 */

public Address fparamaddr(a)
Address a;
{
    Address r;

    dread(&r, a, sizeof(r));
    return r;
}

/*
 * Print a list of currently active blocks starting with most recent.
 */

public wherecmd()
{
    walkstack(false);
}

/*
 * Dump the world to the given file.
 * Like "where", but variables are dumped also.
 */

public dump()
{
    walkstack(true);
}

/*
 * Walk the stack of active procedures printing information
 * about each active procedure.
 */

#define lastfunc(f)     (f == program)

private walkstack(dumpvariables)
Boolean dumpvariables;
{
    register Frame frp;
    register Symbol f;
    register Boolean save;
    register Lineno line;
    struct Frame frame;

    if (notstarted(process)) {
	error("program is not active");
    } else {
	save = walkingstack;
	walkingstack = true;
	frp = &frame;
	getcurframe(frp);
	f = whatblock(frp->save_pc);
	do {
	    printf("%s", symname(f));
	    printparams(f, frp);
	    line = srcline(frp->save_pc - 1);
	    if (line != 0) {
		printf(", line %d", line);
		printf(" in \"%s\"\n", srcfilename(frp->save_pc - 1));
	    } else {
		printf(" at 0x%x\n", frp->save_pc);
	    }
	    if (dumpvariables) {
		dumpvars(f, frp);
		putchar('\n');
	    }
	    frp = nextframe(frp);
	    if (frp != nil) {
		f = whatblock(frp->save_pc);
	    }
	} while (frp != nil and not lastfunc(f));
	if (dumpvariables) {
	    printf("in \"%s\":\n", symname(program));
	    dumpvars(program, nil);
	    putchar('\n');
	}
	walkingstack = save;
    }
}

/*
 * Find the entry point of a procedure or function.
 */

public findbeginning(f)
Symbol f;
{
    f->symvalue.funcv.beginaddr += 2;
}

/*
 * Return the address corresponding to the first line in a function.
 */

public Address firstline(f)
Symbol f;
{
    Address addr;

    addr = codeloc(f);
    while (linelookup(addr) == 0 and addr < objsize) {
	++addr;
    }
    if (addr == objsize) {
	addr = -1;
    }
    return addr;
}

/*
 * Catcher drops strike three ...
 */

public runtofirst()
{
    Address addr;

    addr = pc;
    while (linelookup(addr) == 0 and addr < objsize) {
	++addr;
    }
    if (addr < objsize) {
	stepto(addr);
    }
}

/*
 * Return the address corresponding to the end of the program.
 *
 * We look for the entry to "exit".
 */

public Address lastaddr()
{
    register Symbol s;

    s = lookup(identname("exit", true));
    if (s == nil) {
	panic("can't find exit");
    }
    return codeloc(s);
}

/*
 * Decide if the given function is currently active.
 *
 * We avoid calls to "findframe" during a stack trace for efficiency.
 * Presumably information evaluated while walking the stack is active.
 */

public Boolean isactive(f)
Symbol f;
{
    register Boolean b;

    if (isfinished(process)) {
	b = false;
    } else {
	if (walkingstack or f == program or
	  (ismodule(f) and isactive(container(f)))) {
	    b = true;
	} else {
	    b = (Boolean) (findframe(f) != nil);
	}
    }
    return b;
}

/*
 * Evaluate a call to a procedure.
 */

public callproc(procnode, arglist)
Node procnode;
Node arglist;
{
    Symbol proc;
    Integer argc;

    if (procnode->op != O_SYM) {
	beginerrmsg();
	fprintf(stderr, "can't call \"");
	prtree(stderr, procnode);
	fprintf(stderr, "\"");
	enderrmsg();
    }
    assert(procnode->op == O_SYM);
    proc = procnode->value.sym;
    if (not isblock(proc)) {
	error("\"%s\" is not a procedure or function", symname(proc));
    }
    pushenv();
    pc = codeloc(proc);
    argc = pushargs(proc, arglist);
    beginproc(proc, argc);
    isstopped = true;
    event_once(build(O_EQ, build(O_SYM, pcsym), build(O_SYM, retaddrsym)),
	buildcmdlist(build(O_PROCRTN, proc)));
    cont();
    /* NOTREACHED */
}

/*
 * Push the arguments on the process' stack.  We do this by first
 * evaluating them on the "eval" stack, then copying into the process'
 * space.
 */

private Integer pushargs(proc, arglist)
Symbol proc;
Node arglist;
{
    Stack *savesp;
    int argc, args_size;

    savesp = sp;
    argc = evalargs(proc, arglist);
    args_size = sp - savesp;
    setreg(STKP, reg(STKP) - args_size);
    dwrite(savesp, reg(STKP), args_size);
    sp = savesp;
    return argc;
}

/*
 * Evaluate arguments left-to-right.
 */

private Integer evalargs(proc, arglist)
Symbol proc;
Node arglist;
{
    Node p, exp;
    Symbol arg;
    Stack *savesp;
    Address addr;
    Integer count;

    savesp = sp;
    count = 0;
    arg = proc->chain;
    for (p = arglist; p != nil; p = p->value.arg[1]) {
	if (p->op != O_COMMA) {
	    panic("evalargs: arglist missing comma");
	}
	if (arg == nil) {
	    sp = savesp;
	    error("too many parameters to %s", symname(proc));
	}
	exp = p->value.arg[0];
	if (not compatible(arg->type, exp->nodetype)) {
	    sp = savesp;
	    error("expression for parameter %s is of wrong type", symname(arg));
	}
	if (arg->class == REF) {
	    if (exp->op != O_RVAL) {
		sp = savesp;
		error("variable expected for parameter \"%s\"", symname(arg));
	    }
	    addr = lval(exp->value.arg[0]);
	    push(Address, addr);
	} else {
	    eval(exp);
	}
	arg = arg->chain;
	++count;
    }
    if (arg != nil) {
	sp = savesp;
	error("not enough parameters to %s", symname(proc));
    }
    return count;
}

public procreturn(f)
Symbol f;
{
    flushoutput();
    putchar('\n');
    printname(stdout, f);
    printf(" returns successfully\n", symname(f));
    popenv();
    erecover();
}

/*
 * Push the current environment.
 */

private pushenv()
{
    push(Address, pc);
    push(Lineno, curline);
    push(String, cursource);
    push(Boolean, isstopped);
    push(Symbol, curfunc);
    push(Word, reg(PROGCTR));
    push(Word, reg(STKP));
}

/*
 * Pop back to the real world.
 */

public popenv()
{
    register String filename;

    setreg(STKP, pop(Word));
    setreg(PROGCTR, pop(Word));
    curfunc = pop(Symbol);
    isstopped = pop(Boolean);
    filename = pop(String);
    curline = pop(Lineno);
    pc = pop(Address);
    setsource(filename);
}

/*
 * Flush the debuggee's standard output.
 *
 * This is VERY dependent on the use of stdio.
 */

public flushoutput()
{
    register Symbol p, iob;
    register Stack *savesp;

    p = lookup(identname("fflush", true));
    while (p != nil and not isblock(p)) {
	p = p->next_sym;
    }
    if (p != nil) {
	iob = lookup(identname("_iob", true));
	if (iob != nil) {
	    pushenv();
	    pc = codeloc(p);
	    savesp = sp;
	    push(long, address(iob, nil) + sizeof(struct _iobuf));
	    setreg(STKP, reg(STKP) - sizeof(long));
	    dwrite(savesp, reg(STKP), sizeof(long));
	    sp = savesp;
	    beginproc(p, 1);
	    stepto(return_addr());
	    popenv();
	}
    }
}
