/*-
 * LstPrev.c --
 *	Get the node previous to the current one in the list and make it the
 *	current node.
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
"$Id: lstPrev.c,v 1.8 88/11/17 20:53:54 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Prev --
 *	Return the node previous to the current one for the given list.
 *
 * Results:
 *	The previous node or NILLNODE if the list hasn't been opened
 *	yet or the beginning was reached.
 *
 * Side Effects:
 *	the curPtr is changed to reflect reality.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Prev (l)
    Lst	    	  	l;
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
	    list->curPtr = tln = list->lastPtr;
	    list->atEnd = Middle;
	} else {
	    tln = NilListNode;
	    list->atEnd = Head;
	}
    } else {
	tln = list->curPtr->prevPtr;
	list->curPtr = tln;
	if (tln == list->lastPtr || tln == NilListNode) {
	    list->atEnd = Head;
	} else {
	    list->atEnd = Middle;
	}
    }
    
    return ((LstNode)tln);
}

