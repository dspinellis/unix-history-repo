/*-
 * lstForEachFrom.c --
 *	Perform a given function on all elements of a list starting from
 *	a given point.
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
"$Id: lstForEachFrom.c,v 1.8 89/07/13 13:55:55 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_ForEachFrom --
 *	Apply the given function to each element of the given list. The
 *	function should return 0 if traversal should continue and non-
 *	zero if it should abort. 
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Only those created by the passed-in function.
 *
 *-----------------------------------------------------------------------
 */
/*VARARGS2*/
void
Lst_ForEachFrom (l, ln, proc, d)
    Lst	    	    	l;
    LstNode    	  	ln;
    register int	(*proc)();
    register ClientData	d;
{
    register ListNode	tln = (ListNode)ln;
    register List 	list = (List)l;
    register ListNode	next;
    Boolean 	    	done;
    int     	    	result;
    
    if (!LstValid (list) || LstIsEmpty (list)) {
	return;
    }
    
    do {
	/*
	 * Take care of having the current element deleted out from under
	 * us.
	 */
	
	next = tln->nextPtr;
	
	tln->useCount++;
	result = (*proc) (tln->datum, d);
	tln->useCount--;

	/*
	 * We're done with the traversal if
	 *  - nothing's been added after the current node and
	 *  - the next node to examine is the first in the queue or
	 *    doesn't exist.
	 */
	done = (next == tln->nextPtr &&
		(next == NilListNode || next == list->firstPtr));
	
	next = tln->nextPtr;

	if (tln->flags & LN_DELETED) {
	    free((char *)tln);
	}
	tln = next;
    } while (!result && !LstIsEmpty(list) && !done);
    
}

