/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lstForEach.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstForeach.c --
 *	Perform a given function on all elements of a list.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_ForEach --
 *	Apply the given function to each element of the given list. The
 *	function should return 0 if Lst_ForEach should continue and non-
 *	zero if it should abort.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Only those created by the passed-in function.
 *
 *-----------------------------------------------------------------------
 */
/*VARARGS2*/
void
Lst_ForEach (l, proc, d)
    Lst	    	  	l;
    register int	(*proc)();
    register ClientData	d;
{
    Lst_ForEachFrom(l, Lst_First(l), proc, d);
}

