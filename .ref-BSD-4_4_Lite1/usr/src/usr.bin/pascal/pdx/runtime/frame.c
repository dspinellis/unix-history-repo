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
static char sccsid[] = "@(#)frame.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Activation record handling.
 *
 * The routines curframe and nextframe cheat by using a global copy
 * of the display.  This means there can't be multiple instances of
 * them active at the same time and nextframe won't work in arbitrary cases.
 *
 * This could be solved by putting the display copy into the FRAME structure,
 * but I didn't feel like doing this.  The idea is that they be used
 * in looping through all frames, if I had generators I would use them.
 */

#include "defs.h"
#include "runtime.h"
#include "machine.h"
#include "process.h"
#include "sym.h"
#include "object.h"
#include "mappings.h"
#include "process/pxinfo.h"
#include "frame.rep"
#include "process/process.rep"

/*
 * Return a pointer to the current activation record.
 * Return NIL if currently in the main program.
 * The storage is static.
 */

#define MAXDEPTH 7
#define dispblk(dp)		((dp - DISPLAY) / 2)

LOCAL ADDRESS *display[MAXDEPTH];
LOCAL int dispindex;

FRAME *curframe()
{
	static FRAME frame;
	FRAME *frp;
	ADDRESS *dp, *disp;
	int i;

	frp = &frame;
	dp = curdp();
	disp = contents(dp);
	if (dispblk(dp) <= MAINBLK) {
		return NIL;
	} else {
		getframe(frp, disp);
		for (i = 1; i < MAXDEPTH; i++) {
			display[i] = dispval(i);
		}
		dispindex = dispblk(dp);
		return frp;
	}
}

/*
 * Return a pointer to the next activation record up the stack.
 * Return NIL if there is none.
 * Writes over space pointed to by given argument.
 */

FRAME *nextframe(frp)
FRAME *frp;
{
	ADDRESS *fp;

	if (dispblk(frp->save_dp) <= MAINBLK) {
		return(NIL);
	} else {
		display[dispindex] = frp->save_disp;
		dispindex = dispblk(frp->save_dp);
		fp = display[dispindex];
		getframe(frp, fp);
		return(frp);
	}
}

/*
 * Return the frame associated with the given function.
 */

FRAME *findframe(f)
SYM *f;
{
	static FRAME frame;
	FRAME *frp, *prevfrp;

	frame.save_dp = curdp();
	frame.save_disp = contents(frame.save_dp);
	prevfrp = &frame;
	for (frp = curframe(); frp != NIL; frp = nextframe(frp)) {
		if (whatblock(entry(frp)) == f) {
			return frp;
		}
		*prevfrp = *frp;
	}
	if (f == program) {
		return prevfrp;
	} else {
		return NIL;
	}
}

/*
 * Get the activation record associated with the given display pointer.
 */

LOCAL getframe(frp, disp)
FRAME *frp;
ADDRESS *disp;
{
	if (disp == NIL) {
		panic("bad disp in getframe");
	}
	dread(frp, disp, sizeof(FRAME));
	frp->save_pc -= ENDOFF;
}

/*
 * Return the address of the display in the px process for the given block.
 */

ADDRESS *dispval(b)
int b;
{
	ADDRESS *r;

	dread(&r, (ADDRESS) (DISPLAY + 2*b), sizeof(r));
	return r;
}

/*
 * Return the current display pointer.
 */

ADDRESS *curdp()
{
	ADDRESS *r;

	dread(&r, (ADDRESS) DP, sizeof(r));
	return r;
}

/*
 * Return the contents of the given display pointer.
 */

ADDRESS *contents(dp)
ADDRESS *dp;
{
	ADDRESS *r;

	dread(&r, (ADDRESS) dp, sizeof(r));
	return r;
}

/*
 * Return the px stack address associated with a given frame pointer.
 * Actually, to confuse the issue we want the stack address of the
 * frame one up from the given one.
 */

ADDRESS stkaddr(frp, b)
FRAME *frp;
int b;
{
	return (ADDRESS) display[b];
}
