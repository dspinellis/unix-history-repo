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
static char sccsid[] = "@(#)lstIsEmpty.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstIsEmpty.c --
 *	A single function to decide if a list is empty
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_IsEmpty --
 *	Return TRUE if the given list is empty.
 *
 * Results:
 *	TRUE if the list is empty, FALSE otherwise.
 *
 * Side Effects:
 *	None.
 *
 *	A list is considered empty if its firstPtr == NilListNode (or if
 *	the list itself is NILLIST).
 *-----------------------------------------------------------------------
 */
Boolean
Lst_IsEmpty (l)
    Lst	l;
{
    return ( ! LstValid (l) || LstIsEmpty(l));
}

