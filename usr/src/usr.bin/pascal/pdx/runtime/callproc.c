/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)callproc.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Evaluate a call to a procedure.
 *
 * This file is a botch as far as modularity is concerned.
 *
 * In fact, FIXME, it does not work on either the Vax or Tahoe
 * at this point (Sep 22, 1988).  It possibly doesn't work because
 * the ptrace interface never sets "pc" back into the interpreter's
 * program counter location.
 *
 * Due to portability changes
 * in px for ANSI C, it is now even further broken, since the operand
 * stack is no longer the system stack and since the interpreter's
 * "pc" that we see is never read by the interpreter.  We could fix
 * this, and increase the modularity, by:
 *
 *    * changing this whole module to build a string of bytecodes
 *	that would: push a series of constant parameters, then call a
 *	procedure, then take a breakpoint.
 *    * Having px allocate a place for us to do this, and pass us the
 *	address of this (otherwise unused) variable.
 *    * Creating an entry point into the px interpreter which would
 *	pick up the pc value from "*addrpc" and then enter the main loop.
 *	Currently we never pick up *addrpc for speed.
 *    * Fix the code below to use the new entry point rather than "loopaddr".
 *
 * But I suspect this code is dead enough that nobody will ever get
 * around to it.		-- gnu@toad.com, 22Sep88
 */

#include "defs.h"
#include "runtime.h"
#include "sym.h"
#include "tree.h"
#include "breakpoint.h"
#include "machine.h"
#include "process.h"
#include "source.h"
#include "frame.rep"
#include "sym/classes.h"
#include "sym/sym.rep"
#include "tree/tree.rep"
#include "process/process.rep"
#include "process/pxinfo.h"

LOCAL ADDRESS retaddr;
#ifdef tahoe
BOOLEAN didret;
#endif

/*
 * Controlling logic of procedure calling.
 * Calling a procedure before ever executing the program must
 * be special cased.
 */

callproc(procnode, arglist)
NODE *procnode;
NODE *arglist;
{
	register SYM *proc;
#ifdef tahoe
	register int tmpsp, tmptmp;
	extern BOOLEAN shouldrestart;

	if (shouldrestart) {
		initstart();
	}
#endif
	if (pc == 0) {
		curline = firstline(program);
		setbp(curline);
		resume();
		unsetbp(curline);
	}
	proc = procnode->nameval;
	if (!isblock(proc)) {
		error("\"%s\" is not a procedure or function", proc->symbol);
	}
#ifdef tahoe
	doret(process);
	tmpsp = process->sp;
#endif
	pushargs(proc, arglist);
#ifdef tahoe
	tmptmp = tmpsp;
	tmpsp = process->sp;
	process->sp = tmptmp;
#endif
	pushenv(proc->symvalue.funcv.codeloc);
#ifdef tahoe
	process->sp = tmpsp;
#endif
	pushframe(proc->blkno);
	execute(proc);
	/* NOTREACHED */
}

/*
 * Push the arguments on the process' stack.  We do this by first
 * evaluating them on the "eval" stack, then copying into the process'
 * space.
 */

LOCAL pushargs(proc, arglist)
SYM *proc;
NODE *arglist;
{
	STACK *savesp;
	int args_size;

	savesp = sp;
#ifdef tahoe
	/*
	 * evalargs hopefully keeps stack aligned, so we won't bother
	 * aligning it afterwards, neither will we align process->sp
	 * after subtracting args_size.
	 */
#endif
	evalargs(proc->symbol, proc->chain, arglist);
	args_size = sp - savesp;
	process->sp -= args_size;
	dwrite(savesp, process->sp, args_size);
	sp = savesp;
}

/*
 * Evaluate arguments right-to-left because the eval stack
 * grows up, px's stack grows down.
 */

LOCAL evalargs(procname, arg, explist)
char *procname;
SYM *arg;
NODE *explist;
{
	NODE *exp;
	STACK *savesp;
	ADDRESS addr;

	if (arg == NIL) {
		if (explist != NIL) {
			error("too many parameters to \"%s\"", procname);
		}
	} else if (explist == NIL) {
		error("not enough parameters to \"%s\"", procname);
	} else {
		if (explist->op != O_COMMA) {
			panic("evalargs: arglist missing comma");
		}
		savesp = sp;
		evalargs(procname, arg->chain, explist->right);
		exp = explist->left;
		if (!compatible(arg->type, exp->nodetype)) {
			sp = savesp;
			trerror("%t is not the same type as parameter \"%s\"",
				exp, arg->symbol);
		}
		if (arg->class == REF) {
			if (exp->op != O_RVAL) {
				sp = savesp;
				error("variable expected for parameter \"%s\"", arg->symbol);
			}
			addr = lval(exp->left);
			push(ADDRESS, addr);
		} else {
			eval(exp);
		}
	}
}

/*
 * Simulate a CALL instruction by pushing the appropriate
 * stack frame information.
 *
 * Massage register 10 or 11 appropriately since it contains the
 * stack frame pointer.
 */

LOCAL pushframe(b)
int b;
{
	ADDRESS *newdp;
	FRAME callframe;

	retaddr = program->symvalue.funcv.codeloc;

/*
 * This stuff is set by the callee, just here to take up space.
 */
	callframe.stackref = 0;
	callframe.file = 0;
	callframe.blockp = 0;
	callframe.save_loc = NIL;
	callframe.save_disp = NIL;

/*
 * This is the useful stuff.
 */
	callframe.save_dp = curdp();
	callframe.save_pc = retaddr + ENDOFF;
	callframe.save_lino = 0;
	newdp = DISPLAY + (2 * b);
	dwrite(&newdp, DP, sizeof(newdp));
	process->sp -= sizeof(callframe);
	dwrite(&callframe, process->sp, sizeof(callframe));
#ifdef tahoe
	process->reg[11] = process->sp;
#else
	process->reg[10] = process->sp;
#endif
}

/*
 * Execute the procedure.  This routine does NOT return because it
 * calls "cont", which doesn't return.  We set a CALLPROC breakpoint
 * at "retaddr", the address where the called routine will return.
 *
 * The action for a CALLPROC is to call "procreturn" where we restore
 * the environment.
 */

LOCAL execute(f)
SYM *f;
{
	isstopped = TRUE;
	addbp(retaddr, CALLPROC, f, NIL, NIL, 0);
	cont();
	/* NOTREACHED */
}

procreturn(f)
SYM *f;
{
	int len;

#ifdef tahoe
	doret(process);
#endif
	printf("%s returns ", f->symbol);
	if (f->class == FUNC) {
		len = size(f->type);
		dread(sp, process->sp, len);
#ifdef tahoe
		len = (len + 3) & ~3;
#endif
		sp += len;
		printval(f->type);
		putchar('\n');
	} else {
		printf("successfully\n");
	}
	popenv();
}

/*
 * Push the current environment.
 *
 * This involves both saving pdx and interpreter values.
 * LOOPADDR is the address of the main interpreter loop.
 */

LOCAL pushenv(newpc)
ADDRESS newpc;
{
#ifdef tahoe
	/* this should be done somewhere else, but... */
	INTFP = process->fp;
#endif
	push(ADDRESS, pc);
	push(LINENO, curline);
	push(char *, cursource);
	push(BOOLEAN, isstopped);
	push(SYM *, curfunc);
	push(WORD, process->pc);
	push(WORD, process->sp);
	process->pc = LOOPADDR;
	pc = newpc;
#ifdef tahoe
	process->reg[12] = pc + ENDOFF;
#else
	process->reg[11] = pc + ENDOFF;
#endif
}

/*
 * Pop back to the real world.
 */

popenv()
{
	register PROCESS *p;
	char *filename;

	p = process;
	p->sp = pop(WORD);
	p->pc = pop(WORD);
	curfunc = pop(SYM *);
	isstopped = pop(BOOLEAN);
	filename = pop(char *);
	curline = pop(LINENO);
	pc = pop(ADDRESS);
#ifdef tahoe
	p->reg[12] = pc + 1 + ENDOFF;
#endif
	if (filename != cursource) {
		skimsource(filename);
	}
}
