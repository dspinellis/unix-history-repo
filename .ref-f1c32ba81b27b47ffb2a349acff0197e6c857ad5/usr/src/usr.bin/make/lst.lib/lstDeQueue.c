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
static char sccsid[] = "@(#)lstDeQueue.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstDeQueue.c --
 *	Remove the node and return its datum from the head of the list
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_DeQueue --
 *	Remove and return the datum at the head of the given list.
 *
 * Results:
 *	The datum in the node at the head or (ick) NIL if the list
 *	is empty.
 *
 * Side Effects:
 *	The head node is removed from the list.
 *
 *-----------------------------------------------------------------------
 */
ClientData
Lst_DeQueue (l)
    Lst	    	  l;
{
    ClientData	  rd;
    register ListNode	tln;
    
    tln = (ListNode) Lst_First (l);
    if (tln == NilListNode) {
	return ((ClientData) NIL);
    }
    
    rd = tln->datum;
    if (Lst_Remove (l, (LstNode)tln) == FAILURE) {
	return ((ClientData) NIL);
    } else {
	return (rd);
    }
}

