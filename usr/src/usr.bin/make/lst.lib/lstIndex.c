/*-
 * lstIndex.c --
 *	Function to return the index of a datum in a Lst.
 *
 * Copyright (c) 1988 by the Regents of the University of California
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
"$Id: lstIndex.c,v 1.2 88/11/17 20:52:54 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include    "lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Index --
 *	Return the index of a datum in a Lst. Indices start at 0.
 *
 * Results:
 *	Returns -1 if the datum isn't in the Lst, or the index of
 *	the datum if it is.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
int
Lst_Index(l, d)
    Lst	    	  	l;
    ClientData	  	d;
{
    List    	  	list = (List)l;
    register ListNode	lNode;
    register int  	index;

    lNode = list->firstPtr;

    if (lNode == NilListNode) {
	return(-1);
    }

    index = 0;

    do {
	if (lNode->datum == d) {
	    return(index);
	} else {
	    lNode = lNode->nextPtr;
	    index += 1;
	}
    } while (lNode != NilListNode && lNode != list->firstPtr);
    return(-1);
}
