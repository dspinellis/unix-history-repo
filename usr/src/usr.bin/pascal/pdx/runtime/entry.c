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
static char sccsid[] = "@(#)entry.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * routines to deal with the entry addresses of blocks
 */

#include "defs.h"
#include "runtime.h"
#include "frame.rep"
#include "machine.h"
#include "process.h"
#include "sym.h"
#include "source.h"
#include "object.h"
#include "process/pxinfo.h"
#include "process/process.rep"

/*
 * Return the address of the beginning of the procedure/function
 * associated with the given frame.
 */

ADDRESS entry(frp)
register FRAME *frp;
{
	return(frp->blockp - 2 - ENDOFF);
}

/*
 * Find the entry address of the caller of the current block.
 * This is only called in connection with breakpoints.
 *
 * This routine assumes it is at the very beginning of the block.
 */

ADDRESS caller_addr()
{
	FRAME *frp;

	if ((frp = curframe()) == NIL) {
		panic("caller_addr(main program)");
	}
	frp = nextframe(frp);
	if (frp == NIL) {
		return(codeloc(program));
	} else {
		return(entry(frp));
	}
}

/*
 * Find the return address of the current procedure/function.
 *
 * There are two special cases:
 *
 *	we're right at the beginning of the main program
 *	we're right at the beginning of some procedure or function
 *
 * The first one is handled by returning the last instruction in
 * the object code.  In the second case, we get the return address
 * directly from the process' stack.
 */

ADDRESS return_addr()
{
	ADDRESS addr;
	FRAME *frp, frame;

	if (pc == codeloc(program)) {
		addr = lastaddr();
	} else {
		frp = curframe();
		if (frp == NIL) {
			dread(&frame, (ADDRESS) process->sp, sizeof(FRAME));
			addr = frame.save_pc - ENDOFF;
		} else {
			addr = frp->save_pc;
		}
	}
	return addr;
}

/*
 * Calculate the entry address for a procedure or function parameter,
 * given the address of the descriptor.
 */

ADDRESS fparamaddr(a)
ADDRESS a;
{
	ADDRESS r;

	dread(&r, a, sizeof(r));
	return (r - ENDOFF);
}
