/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)runtime.vax.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

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
#include <signal.h>

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

private Frame curframe = nil;
private struct Frame curframerec;
private Boolean walkingstack = false;

#define frameeq(f1, f2) ((f1)->save_fp == (f2)->save_fp)

#define inSignalHandler(addr) \
    (((addr) < 0x80000000) and ((addr) > 0x80000000 - ctob(UPAGES)))

typedef struct {
    Node callnode;
    Node cmdnode;
    boolean isfunc;
} CallEnv;

private CallEnv endproc;

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
    if (prev_frame + sizeof(struct Frame) <= USRSTACK) {
	dread(&frame, prev_frame, sizeof(struct Frame));
    } else if (USRSTACK - prev_frame > 2 * sizeof(Word)) {
	dread(&frame, prev_frame, USRSTACK - prev_frame);
    } else {
	frame.save_fp = nil;
    }
    if (ntramp == 1) {
	dread(&callpc, prev_frame + 92, sizeof(callpc));
    } else {
	callpc = frame.save_pc;
    }
    if (frame.save_fp == nil or frame.save_pc == (Address) -1) {
	newfrp = nil;
    } else {
	if (inSignalHandler(callpc)) {
	    ntramp++;
	    prev_frame = frame.save_fp;
	    goto nextf;
	}
	frame.save_pc = callpc;
        ntramp = 0;
	newfrp->save_fp = frame.save_fp;
	newfrp->save_pc = frame.save_pc;
	mask = ((frame.mask >> 16) & 0x0fff);
	getsaveregs(newfrp, &frame, mask);
	newfrp->condition_handler = frame.condition_handler;
	newfrp->mask = mask;
	newfrp->save_ap = frame.save_ap;
    }
    return newfrp;
}

/*
 * Get the current frame information in the given Frame and store the
 * associated function in the given value-result parameter.
 */

private getcurfunc (frp, fp)
Frame frp;
Symbol *fp;
{
    getcurframe(frp);
    *fp = whatblock(frp->save_pc);
}

/*
 * Return the frame associated with the next function up the call stack, or
 * nil if there is none.  The function is returned in a value-result parameter.
 * For "inline" functions the statically outer function and same frame
 * are returned.
 */

public Frame nextfunc (frp, fp)
Frame frp;
Symbol *fp;
{
    Symbol t;
    Frame nfrp;

    t = *fp;
    checkref(t);
    if (isinline(t)) {
	t = container(t);
	nfrp = frp;
    } else {
	nfrp = nextframe(frp);
	if (nfrp == nil) {
	    t = nil;
	} else {
	    t = whatblock(nfrp->save_pc);
	}
    }
    *fp = t;
    return nfrp;
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
    setreg(FRP, frame.save_fp);
    setreg(PROGCTR, frame.save_pc);
    setreg(ARGP, frame.save_ap);
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
    Address argaddr;
    Word w;

    argaddr = args_base(frp) + (n * sizeof(Word));
    dread(&w, argaddr, sizeof(w));
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
    Symbol f;
    struct Frame frame;

    if (notstarted(process) or isfinished(process)) {
	error("program is not active");
    } else {
	save = walkingstack;
	walkingstack = true;
	showaggrs = dumpvariables;
	frp = &frame;
	getcurfunc(frp, &f);
	for (;;) {
	    printcallinfo(f, frp);
	    if (dumpvariables) {
		dumpvars(f, frp);
		putchar('\n');
	    }
	    frp = nextfunc(frp, &f);
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
 * Set the current function to the given symbol.
 * We must adjust "curframe" so that subsequent operations are
 * not confused; for simplicity we simply clear it.
 */

public setcurfunc (f)
Symbol f;
{
    curfunc = f;
    curframe = nil;
}

/*
 * Return the frame for the current function.
 * The space for the frame is allocated statically.
 */

public Frame curfuncframe ()
{
    static struct Frame frame;
    Frame frp;

    if (curframe == nil) {
	frp = findframe(curfunc);
	curframe = &curframerec;
	*curframe = *frp;
    } else {
	frp = &frame;
	*frp = *curframe;
    }
    return frp;
}

/*
 * Set curfunc to be N up/down the stack from its current value.
 */

public up (n)
integer n;
{
    integer i;
    Symbol f;
    Frame frp;
    boolean done;

    if (not isactive(program)) {
	error("program is not active");
    } else if (curfunc == nil) {
	error("no current function");
    } else {
	i = 0;
	f = curfunc;
	frp = curfuncframe();
	done = false;
	do {
	    if (frp == nil) {
		done = true;
		error("not that many levels");
	    } else if (i >= n) {
		done = true;
		curfunc = f;
		curframe = &curframerec;
		*curframe = *frp;
		showaggrs = false;
		printcallinfo(curfunc, curframe);
	    } else if (f == program) {
		done = true;
		error("not that many levels");
	    } else {
		frp = nextfunc(frp, &f);
	    }
	    ++i;
	} while (not done);
    }
}

public down (n)
integer n;
{
    integer i, depth;
    Frame frp, curfrp;
    Symbol f;
    struct Frame frame;

    if (not isactive(program)) {
	error("program is not active");
    } else if (curfunc == nil) {
	error("no current function");
    } else {
	depth = 0;
	frp = &frame;
	getcurfunc(frp, &f);
	if (curframe == nil) {
	    curfrp = findframe(curfunc);
	    curframe = &curframerec;
	    *curframe = *curfrp;
	}
	while ((f != curfunc or !frameeq(frp, curframe)) and f != nil) {
	    frp = nextfunc(frp, &f);
	    ++depth;
	}
	if (f == nil or n > depth) {
	    error("not that many levels");
	} else {
	    depth -= n;
	    frp = &frame;
	    getcurfunc(frp, &f);
	    for (i = 0; i < depth; i++) {
		frp = nextfunc(frp, &f);
		assert(frp != nil);
	    }
	    curfunc = f;
	    *curframe = *frp;
	    showaggrs = false;
	    printcallinfo(curfunc, curframe);
	}
    }
}

/*
 * Find the entry point of a procedure or function.
 *
 * On the VAX we add the size of the register mask (FUNCOFFSET) or
 * the size of the Modula-2 internal entry sequence, on other machines
 * (68000's) we add the entry sequence size (FUNCOFFSET) unless
 * we're right at the beginning of the program.
 */

public findbeginning (f)
Symbol f;
{
    if (isinternal(f)) {
	f->symvalue.funcv.beginaddr += 18;	/* VAX only */
    } else {
	f->symvalue.funcv.beginaddr += FUNCOFFSET;
    }
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
    Address addr, endaddr;

    addr = pc;
    endaddr = objsize + CODESTART;
    while (linelookup(addr) == 0 and addr < endaddr) {
	++addr;
    }
    if (addr < endaddr) {
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

public Boolean isactive (f)
Symbol f;
{
    Boolean b;

    if (isfinished(process)) {
	b = false;
    } else {
	if (walkingstack or f == program or f == nil or
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
    setreg(FRP, 1);	/* have to ensure it's non-zero for return_addr() */
    beginproc(proc, argc);
    event_once(
	build(O_EQ, build(O_SYM, pcsym), build(O_SYM, retaddrsym)),
	buildcmdlist(build(O_PROCRTN, proc))
    );
    isstopped = false;
    if (not bpact()) {
	isstopped = true;
	cont(0);
    }
    /*
     * bpact() won't return true, it will call printstatus() and go back
     * to command input if a breakpoint is found.
     */
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
 * Check to see if an expression is correct for a given parameter.
 * If the given parameter is false, don't worry about type inconsistencies.
 *
 * Return whether or not it is ok.
 */

private boolean chkparam (actual, formal, chk)
Node actual;
Symbol formal;
boolean chk;
{
    boolean b;

    b = true;
    if (chk) {
	if (formal == nil) {
	    beginerrmsg();
	    fprintf(stderr, "too many parameters");
	    b = false;
	} else if (not compatible(formal->type, actual->nodetype)) {
	    beginerrmsg();
	    fprintf(stderr, "type mismatch for %s", symname(formal));
	    b = false;
	}
    }
    if (b and formal != nil and
	isvarparam(formal) and not isopenarray(formal->type) and
	not (
	    actual->op == O_RVAL or actual->nodetype == t_addr or
	    (
		actual->op == O_TYPERENAME and
		(
		    actual->value.arg[0]->op == O_RVAL or
		    actual->value.arg[0]->nodetype == t_addr
		)
	    )
	)
    ) {
	beginerrmsg();
	fprintf(stderr, "expected variable, found \"");
	prtree(stderr, actual);
	fprintf(stderr, "\"");
	b = false;
    }
    return b;
}

/*
 * Pass an expression to a particular parameter.
 *
 * Normally we pass either the address or value, but in some cases
 * (such as C strings) we want to copy the value onto the stack and
 * pass its address.
 *
 * Another special case raised by strings is the possibility that
 * the actual parameter will be larger than the formal, even with
 * appropriate type-checking.  This occurs because we assume during
 * evaluation that strings are null-terminated, whereas some languages,
 * notably Pascal, do not work under that assumption.
 */

private passparam (actual, formal)
Node actual;
Symbol formal;
{
    boolean b;
    Address addr;
    Stack *savesp;
    integer actsize, formsize;

    if (formal != nil and isvarparam(formal) and
	(not isopenarray(formal->type))
    ) {
	addr = lval(actual->value.arg[0]);
	push(Address, addr);
    } else if (passaddr(formal, actual->nodetype)) {
	savesp = sp;
	eval(actual);
	actsize = sp - savesp;
	setreg(STKP,
	    reg(STKP) - ((actsize + sizeof(Word) - 1) & ~(sizeof(Word) - 1))
	);
	dwrite(savesp, reg(STKP), actsize);
	sp = savesp;
	push(Address, reg(STKP));
	if (formal != nil and isopenarray(formal->type)) {
	    push(integer, actsize div size(formal->type->type));
	}
    } else if (formal != nil) {
	formsize = size(formal);
	savesp = sp;
	eval(actual);
	actsize = sp - savesp;
	if (actsize > formsize) {
	    sp -= (actsize - formsize);
	}
    } else {
	eval(actual);
    }
}

/*
 * Evaluate an argument list left-to-right.
 */

private integer evalargs(proc, arglist)
Symbol proc;
Node arglist;
{
    Node p, actual;
    Symbol formal;
    Stack *savesp;
    integer count;
    boolean chk;

    savesp = sp;
    count = 0;
    formal = proc->chain;
    chk = (boolean) (not nosource(proc));
    for (p = arglist; p != nil; p = p->value.arg[1]) {
	assert(p->op == O_COMMA);
	actual = p->value.arg[0];
	if (not chkparam(actual, formal, chk)) {
	    fprintf(stderr, " in call to %s", symname(proc));
	    sp = savesp;
	    enderrmsg();
	}
	passparam(actual, formal);
	if (formal != nil) {
	    formal = formal->chain;
	}
	++count;
    }
    if (chk) {
	if (formal != nil) {
	    sp = savesp;
	    error("not enough parameters to %s", symname(proc));
	}
    }
    return count;
}

/*
 * Evaluate an argument list without any type checking.
 * This is only useful for procedures with a varying number of
 * arguments that are compiled -g.
 */

private integer unsafe_evalargs (proc, arglist)
Symbol proc;
Node arglist;
{
    Node p;
    integer count;

    count = 0;
    for (p = arglist; p != nil; p = p->value.arg[1]) {
	assert(p->op == O_COMMA);
	eval(p->value.arg[0]);
	++count;
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
	printf(" returns successfully\n");
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
    push(Frame, curframe);
    push(struct Frame, curframerec);
    push(CallEnv, endproc);
    push(Word, reg(PROGCTR));
    push(Word, reg(STKP));
    push(Word, reg(FRP));
}

/*
 * Pop back to the real world.
 */

public popenv()
{
    String filename;

    setreg(FRP, pop(Word));
    setreg(STKP, pop(Word));
    setreg(PROGCTR, pop(Word));
    endproc = pop(CallEnv);
    curframerec = pop(struct Frame);
    curframe = pop(Frame);
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
	    pc = codeloc(p) - FUNCOFFSET;
	    savesp = sp;
	    push(long, address(iob, nil) + sizeof(*stdout));
	    setreg(STKP, reg(STKP) - sizeof(long));
	    dwrite(savesp, reg(STKP), sizeof(long));
	    sp = savesp;
	    beginproc(p, 1);
	    stepto(return_addr());
	    popenv();
	}
    }
}
