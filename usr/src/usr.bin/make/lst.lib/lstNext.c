/*-
 * LstNext.c --
 *	Return the next node for a list.
 *	The sequential functions access the list in a slightly different way.
 *	CurPtr points to their idea of the current node in the list and they
 *	access the list based on it. Because the list is circular, Lst_Next
 *	and Lst_Prev will go around the list forever. Lst_IsAtEnd must be
 *	used to determine when to stop.
 *
 * Copyright (c) 1988 by University of California Regents
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  Neither the University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */
#ifndef lint
static char *rcsid =
"$Id: lstNext.c,v 1.8 88/11/17 20:53:40 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Next --
 *	Return the next node for the given list.
 *
 * Results:
 *	The next node or NILLNODE if the list has yet to be opened. Also
 *	if the list is non-circular and the end has been reached, NILLNODE
 *	is returned.
 *
 * Side Effects:
 *	the curPtr field is updated.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Next (l)
    Lst	    	  l;
{
    register ListNode	tln;
    register List 	list = (List)l;
    
    if ((LstValid (l) == FALSE) ||
	(list->isOpen == FALSE)) {
	    return (NILLNODE);
    }
    
    list->prevPtr = list->curPtr;
    
    if (list->curPtr == NilListNode) {
	if (list->atEnd == Unknown) {
	    /*
	     * If we're just starting out, atEnd will be Unknown.
	     * Then we want to start this thing off in the right
	     * direction -- at the start with atEnd being Middle.
	     */
	    list->curPtr = tln = list->firstPtr;
	    list->atEnd = Middle;
	} else {
	    tln = NilListNode;
	    list->atEnd = Tail;
	}
    } else {
	tln = list->curPtr->nextPtr;
	list->curPtr = tln;

	if (tln == list->firstPtr || tln == NilListNode) {
	    /*
	     * If back at the front, then we've hit the end...
	     */
	    list->atEnd = Tail;
	} else {
	    /*
	     * Reset to Middle if gone past first.
	     */
	    list->atEnd = Middle;
	}
    }
    
    return ((LstNode)tln);
}

