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
static char sccsid[] = "@(#)lstMember.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * lstMember.c --
 *	See if a given datum is on a given list.
 */

#include    "lstInt.h"

LstNode
Lst_Member (l, d)
    Lst	    	  	l;
    ClientData	  	d;
{
    List    	  	list = (List) l;
    register ListNode	lNode;

    lNode = list->firstPtr;
    if (lNode == NilListNode) {
	return NILLNODE;
    }
    
    do {
	if (lNode->datum == d) {
	    return (LstNode)lNode;
	}
	lNode = lNode->nextPtr;
    } while (lNode != NilListNode && lNode != list->firstPtr);

    return NILLNODE;
}
