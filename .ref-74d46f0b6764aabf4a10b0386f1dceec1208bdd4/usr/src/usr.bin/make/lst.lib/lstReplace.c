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
static char sccsid[] = "@(#)lstReplace.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstReplace.c --
 *	Replace the datum in a node with a new datum
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Replace --
 *	Replace the datum in the given node with the new datum
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	The datum field fo the node is altered.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Replace (ln, d)
    register LstNode	ln;
    ClientData	  	d;
{
    if (ln == NILLNODE) {
	return (FAILURE);
    } else {
	((ListNode) ln)->datum = d;
	return (SUCCESS);
    }
}

