/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

static char sccsid[] = "@(#)runtime.c 5.3 %G%";

static char rcsid[] = "$Header: runtime.c,v 1.5 84/12/26 10:41:52 linton Exp $";

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
#include <sys/param.h>

#ifndef public
typedef struct Frame *Frame;

#include "machine.h"
#endif

#define NSAVEREG 12

struct Frame {
    integer condition_handler;
    integer mask;
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
Frame frp;
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
 * Get the saved registers from one frame to another
 * given mask specifying which registers were actually saved.
 */

#define bis(b, n) ((b & (1 << (n))) != 0)

private getsaveregs (newfrp, frp, mask)
Frame newfrp, frp;
integer mask;
{
    integer i, j;

    j = 0;
    for (i = 0; i < NSAVEREG; i++) {
	if (bis(mask, i)) {
	    newfrp->save_reg[i] = frp->save_reg[j];
	    ++j;
	}
    }
}

/*
 * Return a pointer to the next activation record up the stack.
 * Return nil if there is none.
 * Writes over space pointed to by given argument.
 */

private Frame nextframe(frp)
Frame frp;
{
    Frame newfrp;
    struct Frame frame;
    integer mask;
    Address prev_frame, callpc; 
    static integer ntramp = 0;

    newfrp = frp;
    prev_frame = frp->save_fp;

/*
 *  The check for interrupt generated frames is taken from adb with only
 *  partial understanding.  If you're in "sub" and on a sigxxx "sigsub"
 *  gets control, then the stack does NOT look like <main, sub, sigsub>.
 *
 *  As best I can make out it looks like:
 *
 *     <main, (machine check exception block + sub), sysframe, sigsub>.
 *
 *  When the signal occurs an exception block and a frame for the routine
 *  in which it occured are pushed on the user stack.  Then another frame
 *  is pushed corresponding to a call from the kernel to sigsub.
 *
 *  The addr in sub at which the exception occured is not in sub.save_pc
 *  but in the machine check exception block.  It is at the magic address
 *  fp + 84.
 *
 *  The current approach ignores the sys_frame (what adb reports as sigtramp)
 *  and takes the pc for sub from the exception block.  This allows the
 *  "where" command to report <main, sub, sigsub>, which seems reasonable.
 */

nextf:
    dread(&frame, prev_frame, sizeof(struct Frame));
    if (ntramp == 1) {
	dread(&callpc, prev_frame + 84, sizeof(callpc));
    } else {
	callpc = frame.save_pc;
    }
    if (frame.save_fp == nil or frame.save_pc == (Address) -1) {
	newfrp = nil;
    } else if (isstackaddr(callpc)) {
	ntramp++;
	prev_frame = frame.save_fp;
	goto nextf;
    } else {
	frame.save_pc = callpc;
        ntramp = 0;
	mask = ((frame.mask >> 16) & 0x0fff);
	getsaveregs(newfrp, &frame, mask);
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
    Frame frp;
    static struct Frame frame;
    Symbol p;
    Boolean done;

    frp = &frame;
    getcurframe(frp);
    if (f != nil) {
	if (f == curfunc and curframe != nil) {
	    *frp = *curframe;
	} else {
	    done = false;
	    p = whatblock(frp->save_pc);
	    do {
		if (p == f) {
		    done = true;
		} else if (p == program) {
		    done = true;
		    frp = nil;
		} else {
		    frp = nextfunc(frp, &p);
		    if (frp == nil) {
			done = true;
		    }
		}
	    } while (not done);
	}
    }
    return frp;
}

/*
 * Set the registers according to the given frame pointer.
 */

public getnewregs (addr)
Address addr;
{
    struct Frame frame;
    integer i, j, mask;

    dread(&frame, addr, sizeof(frame));
    setreg(ARGP, frame.save_ap);
    setreg(FRP, frame.save_fp);
    setreg(PROGCTR, frame.save_pc);
    mask = ((frame.mask >> 16) & 0x0fff);
    j = 0;
    for (i = 0; i < NSAVEREG; i++) {
	if (bis(mask, i)) {
	    setreg(i, frame.save_reg[j]);
	    ++j;
	}
    }
    pc = frame.save_pc;
    setcurfunc(whatblock(pc));
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
integer len;
boolean isindirect;
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
		    error("[internal error: bad size %d in pushretval]", len);
		}
		break;
	}
    }
}

/*
 * Return the base address for locals in the given frame.
 */

public Address locals_base(frp)
Frame frp;
{
    return (frp == nil) ? reg(FRP) : frp->save_fp;
}

/*
 * Return the base address for arguments in the given frame.
 */

public Address args_base(frp)
Frame frp;
{
    return (frp == nil) ? reg(ARGP) : frp->save_ap;
}

/*
 * Return saved register n from the given frame.
 */

public Word savereg(n, frp)
integer n;
Frame frp;
{
    Word w;

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
integer n;
Frame frp;
{
    Word w;

    dread(&w, args_base(frp) + (n * sizeof(Word)), sizeof(w));
    return w;
}

/*
 * Print a list of currently active blocks starting with most recent.
 */

public wherecmd()
{
    walkstack(false);
}

/*
 * Print the variables in the given frame or the current one if nil.
 */

public dump (func)
Symbol func;
{
    Symbol f;
    Frame frp;

    if (func == nil) {
	f = curfunc;
	if (curframe != nil) {
	    frp = curframe;
	} else {
	    frp = findframe(f);
	}
    } else {
	f = func;
	frp = findframe(f);
    }
    showaggrs = true;
    printcallinfo(f, frp);
    dumpvars(f, frp);
}

/*
 * Dump all values.
 */

public dumpall ()
{
    walkstack(true);
}

/*
 * Walk the stack of active procedures printing information
 * about each active procedure.
 */

private walkstack(dumpvariables)
Boolean dumpvariables;
{
    Frame frp;
    boolean save;
    struct Frame frame;

    if (notstarted(process) or isfinished(process)) {
	error("program is not active");
    } else {
	save = walkingstack;
	walkingstack = true;
	showaggrs = dumpvariables;
	frp = &frame;
	getcurframe(frp);
	f = whatblock(frp->save_pc);
	for (;;) {
	    printcallinfo(f, frp);
	    if (dumpvariables) {
		dumpvars(f, frp);
		putchar('\n');
	    }
	    if (isinline(f)) {
		f = container(f);
	    } else {
		frp = nextframe(frp);
		if (frp != nil) {
		    f = whatblock(frp->save_pc);
		}
	    }
	    if (frp == nil or f == program) {
		break;
	    }
	}
	if (dumpvariables) {
	    printf("in \"%s\":\n", symname(program));
	    dumpvars(program, nil);
	    putchar('\n');
	}
	walkingstack = save;
    }
}

/*
 * Print out the information about a call, i.e.,
 * routine name, parameter values, and source location.
 */

private printcallinfo (f, frp)
Symbol f;
Frame frp;
{
    Lineno line;
    Address savepc;

    savepc = frp->save_pc;
    if (frp->save_fp != reg(FRP)) {
	savepc -= 1;
    }
    printname(stdout, f);
    if (not isinline(f)) {
	printparams(f, frp);
    }
    line = srcline(savepc);
    if (line != 0) {
	printf(", line %d", line);
	printf(" in \"%s\"\n", srcfilename(savepc));
    } else {
	printf(" at 0x%x\n", savepc);
    }
}

/*
 * Find the entry point of a procedure or function.
 */

public findbeginning (f)
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
    Symbol s;

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
    Boolean b;

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

public callproc(exprnode, isfunc)
Node exprnode;
boolean isfunc;
{
    Node procnode, arglist;
    Symbol proc;
    integer argc;

    procnode = exprnode->value.arg[0];
    arglist = exprnode->value.arg[1];
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
    endproc.isfunc = isfunc;
    endproc.callnode = exprnode;
    endproc.cmdnode = topnode;
    pushenv();
    pc = codeloc(proc);
    argc = pushargs(proc, arglist);
    beginproc(proc, argc);
    /* NOTREACHED */
}

/*
 * Push the arguments on the process' stack.  We do this by first
 * evaluating them on the "eval" stack, then copying into the process'
 * space.
 */

private integer pushargs(proc, arglist)
Symbol proc;
Node arglist;
{
    Stack *savesp;
    int argc, args_size;

    savesp = sp;
    if (varIsSet("$unsafecall")) {
	argc = unsafe_evalargs(proc, arglist);
    } else {
	argc = evalargs(proc, arglist);
    }
    args_size = sp - savesp;
    setreg(STKP, reg(STKP) - args_size);
    dwrite(savesp, reg(STKP), args_size);
    sp = savesp;
    return argc;
}

/*
 * Evaluate arguments left-to-right.
 */

private integer evalargs(proc, arglist)
Symbol proc;
Node arglist;
{
    Node p, exp;
    Symbol arg;
    Stack *savesp;
    Address addr;
    integer count;

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
    integer retvalsize;
    Node tmp;
    char *copy;

    flushoutput();
    popenv();
    if (endproc.isfunc) {
	retvalsize = size(f->type);
	if (retvalsize > sizeof(long)) {
	    pushretval(retvalsize, true);
	    copy = newarr(char, retvalsize);
	    popn(retvalsize, copy);
	    tmp = build(O_SCON, copy);
	} else {
	    tmp = build(O_LCON, (long) (reg(0)));
	}
	tmp->nodetype = f->type;
	tfree(endproc.callnode);
	*(endproc.callnode) = *(tmp);
	dispose(tmp);
	eval(endproc.cmdnode);
    } else {
	putchar('\n');
	printname(stdout, f);
	printf("%s returns successfully\n", symname(f));
    }
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
    String filename;

    setreg(STKP, pop(Word));
    setreg(PROGCTR, pop(Word));
    endproc = pop(CallEnv);
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
    Symbol p, iob;
    Stack *savesp;

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
