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
static char sccsid[] = "@(#)lstAtFront.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstAtFront.c --
 *	Add a node at the front of the list
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_AtFront --
 *	Place a piece of data at the front of a list
 *
 * Results:
 *	SUCCESS or FAILURE
 *
 * Side Effects:
 *	A new ListNode is created and stuck at the front of the list.
 *	hence, firstPtr (and possible lastPtr) in the list are altered.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_AtFront (l, d)
    Lst		l;
    ClientData	d;
{
    register LstNode	front;
    
    front = Lst_First (l);
    return (Lst_Insert (l, front, d));
}
