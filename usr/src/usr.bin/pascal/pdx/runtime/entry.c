/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)entry.c	5.2 (Berkeley) %G%";
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
