/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstInsert.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * LstInsert.c --
 *	Insert a new datum before an old one
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Insert --
 *	Insert a new node with the given piece of data before the given
 *	node in the given list.
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	the firstPtr field will be changed if ln is the first node in the
 *	list.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Insert (l, ln, d)
    Lst	    	  	l;	/* list to manipulate */
    LstNode	  	ln;	/* node before which to insert d */
    ClientData	  	d;	/* datum to be inserted */
{
    register ListNode	nLNode;	/* new lnode for d */
    register ListNode	lNode = (ListNode)ln;
    register List 	list = (List)l;


    /*
     * check validity of arguments
     */
    if (LstValid (l) && (LstIsEmpty (l) && ln == NILLNODE))
	goto ok;
    
    if (!LstValid (l) || LstIsEmpty (l) || !LstNodeValid (ln, l)) {
	return (FAILURE);
    }
    
    ok:
    PAlloc (nLNode, ListNode);
    
    nLNode->datum = d;
    nLNode->useCount = nLNode->flags = 0;
    
    if (ln == NILLNODE) {
	if (list->isCirc) {
	    nLNode->prevPtr = nLNode->nextPtr = nLNode;
	} else {
	    nLNode->prevPtr = nLNode->nextPtr = NilListNode;
	}
	list->firstPtr = list->lastPtr = nLNode;
    } else {
	nLNode->prevPtr = lNode->prevPtr;
	nLNode->nextPtr = lNode;
	
	if (nLNode->prevPtr != NilListNode) {
	    nLNode->prevPtr->nextPtr = nLNode;
	}
	lNode->prevPtr = nLNode;
	
	if (lNode == list->firstPtr) {
	    list->firstPtr = nLNode;
	}
    }
    
    return (SUCCESS);
}
	
