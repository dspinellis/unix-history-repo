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
static char sccsid[] = "@(#)lstFindFrom.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstFindFrom.c --
 *	Find a node on a list from a given starting point. Used by Lst_Find.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_FindFrom --
 *	Search for a node starting and ending with the given one on the
 *	given list using the passed datum and comparison function to
 *	determine when it has been found.
 *
 * Results:
 *	The found node or NILLNODE
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_FindFrom (l, ln, d, cProc)
    Lst		      	l;
    register LstNode    ln;
    register ClientData d;
    register int	(*cProc)();
{
    register ListNode	tln;
    Boolean		found = FALSE;
    
    if (!LstValid (l) || LstIsEmpty (l) || !LstNodeValid (ln, l)) {
	return (NILLNODE);
    }
    
    tln = (ListNode)ln;
    
    do {
	if ((*cProc) (tln->datum, d) == 0) {
	    found = TRUE;
	    break;
	} else {
	    tln = tln->nextPtr;
	}
    } while (tln != (ListNode)ln && tln != NilListNode);
    
    if (found) {
	return ((LstNode)tln);
    } else {
	return (NILLNODE);
    }
}

