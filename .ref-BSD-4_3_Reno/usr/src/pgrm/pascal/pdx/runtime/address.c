/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)address.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * Some machine and runtime dependent manipulation of a symbol.
 */

#include "defs.h"
#include "runtime.h"
#include "sym.h"
#include "machine.h"
#include "process.h"
#include "object.h"
#include "mappings.h"
#include "sym/classes.h"
#include "frame.rep"
#include "sym/sym.rep"

/*
 * Calculate the address of a symbol.
 * If frame is not NIL, then it is the frame associated with the
 * activation in which the symbol we're interested in is defined.
 */

ADDRESS address(s, frame)
register SYM *s;
FRAME *frame;
{
    SYM *f;
    FRAME *frp;
    ADDRESS r, *dp, *disp;
    short offs;

    f = s->func;
    if (s->class == FVAR) {
	offs = f->symvalue.offset;
    } else {
	offs = s->symvalue.offset;
    }
    if (f == program) {
	r = (ADDRESS) dispval(MAINBLK) + offs;
    } else if (f == curfunc && frame == NIL) {
	dp = curdp();
	disp = contents(dp);
	r = (ADDRESS) disp + offs;
    } else {
	if (frame == NIL) {
	    frp = findframe(s->func);
	    if (frp == NIL) {
		panic("address: findframe returned NIL");
	    }
	} else {
	    frp = frame;
	}
	r = stkaddr(frp, s->blkno) + offs;
    }
    return r;
}

/*
 * The next three routines assume the procedure entry code is
 *
 *      f:  tra4    A
 *          ...
 *      A:  beg
 *      B:  <code for first line>
 *
 * Pi gives f, we compute and store A with "findbeginning(f)",
 * (retrieved by "codeloc(f)"), B is computed by "firstline(f)".
 *
 * The procedure "runtofirst" assumes you're at A and want to step to B.
 * It should be changed to a nop if A is equal to B.
 */

/*
 * Find the beginning of a procedure or function.  This is a destructive
 * routine, it changes the value associated with the procedure symbol.
 * Should only be called once per symbol.
 */

findbeginning(f)
SYM *f;
{
    f->symvalue.funcv.codeloc = nextaddr(f->symvalue.funcv.codeloc, FALSE);
}

/*
 * Find the object code associated with the first line of a block.
 */

ADDRESS firstline(f)
SYM *f;
{
    ADDRESS addr;

    addr = codeloc(f);
    while (linelookup(addr) == 0) {
	if (isendofproc(addr)) {
	    return -1;
	}
	addr = nextaddr(addr, FALSE);
    }
    return addr;
}

/*
 * Catcher drops strike three ...
 */

runtofirst()
{
    stepto(firstline(curfunc));
}

/*
 * Calculate the address of the last line in the program.
 * This is assumed to be at the physical end.
 */

ADDRESS lastaddr()
{
    if (objsize == 0) {
	panic("lastaddr: objsize = 0!");
    }
    return(objsize - sizeof(short));
}
