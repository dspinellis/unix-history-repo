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
static char sccsid[] = "@(#)lstRemove.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstRemove.c --
 *	Remove an element from a list
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Remove --
 *	Remove the given node from the given list.
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	The list's firstPtr will be set to NilListNode if ln is the last
 *	node on the list. firsPtr and lastPtr will be altered if ln is
 *	either the first or last node, respectively, on the list.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Remove (l, ln)
    Lst	    	  	l;
    LstNode	  	ln;
{
    register List 	list = (List) l;
    register ListNode	lNode = (ListNode) ln;

    if (!LstValid (l) ||
	!LstNodeValid (ln, l)) {
	    return (FAILURE);
    }
    
    /*
     * unlink it from the list
     */
    if (lNode->nextPtr != NilListNode) {
	lNode->nextPtr->prevPtr = lNode->prevPtr;
    }
    if (lNode->prevPtr != NilListNode) {
	lNode->prevPtr->nextPtr = lNode->nextPtr;
    }
    
    /*
     * if either the firstPtr or lastPtr of the list point to this node,
     * adjust them accordingly
     */
    if (list->firstPtr == lNode) {
	list->firstPtr = lNode->nextPtr;
    }
    if (list->lastPtr == lNode) {
	list->lastPtr = lNode->prevPtr;
    }

    /*
     * Sequential access stuff. If the node we're removing is the current
     * node in the list, reset the current node to the previous one. If the
     * previous one was non-existent (prevPtr == NilListNode), we set the
     * end to be Unknown, since it is.
     */
    if (list->isOpen && (list->curPtr == lNode)) {
	list->curPtr = list->prevPtr;
	if (list->curPtr == NilListNode) {
	    list->atEnd = Unknown;
	}
    }

    /*
     * the only way firstPtr can still point to ln is if ln is the last
     * node on the list (the list is circular, so lNode->nextptr == lNode in
     * this case). The list is, therefore, empty and is marked as such
     */
    if (list->firstPtr == lNode) {
	list->firstPtr = NilListNode;
    }
    
    /*
     * note that the datum is unmolested. The caller must free it as
     * necessary and as expected.
     */
    if (lNode->useCount == 0) {
	free ((Address)ln);
    } else {
	lNode->flags |= LN_DELETED;
    }
    
    return (SUCCESS);
}

