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
static char sccsid[] = "@(#)lstLast.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstLast.c --
 *	Return the last element of a list
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Last --
 *	Return the last node on the list l.
 *
 * Results:
 *	The requested node or NILLNODE if the list is empty.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Last (l)
    Lst	    l;
{
    if (!LstValid(l) || LstIsEmpty (l)) {
	return (NILLNODE);
    } else {
	return ((LstNode)((List)l)->lastPtr);
    }
}

