/*-
 * lstMember.c --
 *	See if a given datum is on a given list.
 *
 * Copyright (c) 1988 by the Regents of the University of California
 * Copyright (c) 1988 by Adam de Boor
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  The University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 *
 */
#ifndef lint
static char *rcsid =
"$Id: lstMember.c,v 1.5 88/11/17 20:53:32 adam Exp $ SPRITE (Berkeley)";
#endif lint

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
