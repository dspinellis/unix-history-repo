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
static char sccsid[] = "@(#)lstDatum.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstDatum.c --
 *	Return the datum associated with a list node.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Datum --
 *	Return the datum stored in the given node.
 *
 * Results:
 *	The datum or (ick!) NIL if the node is invalid.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
ClientData
Lst_Datum (ln)
    LstNode	ln;
{
    if (ln != NILLNODE) {
	return (((ListNode)ln)->datum);
    } else {
	return ((ClientData) NIL);
    }
}

