/*-
 * listSetCirc.c --
 *	Change the library's notion of the circularity of a list.
 *
 * Copyright (c) 1988 by the Regents of the University of California
 *
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
"$Id: lstSetCirc.c,v 1.3 88/11/17 20:54:04 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*
 *------------------------------------------------------------
 * Lst_SetCirc --
 *	change the circularity of a list
 *
 * Results:
 *	none
 *
 * Side Effects:
 *	The circularity of the list is set appropriately. The head and
 *	tail of the list will be linked or unlinked as necessary
 *------------------------------------------------------------
 */
void
Lst_SetCirc (l, circ)
    Lst	    	  l;
    Boolean	  circ;
{
    register List list = (List) l;

    /*
     * if this isn't a change, do nothing.
     */
    if ((list->isCirc && circ) || (!list->isCirc && !circ)) {
	return;
    }
    list->isCirc = circ;
    
    if (LstIsEmpty (l)) {
	return;
    }
    
    if (circ) {
	list->firstPtr->prevPtr = list->lastPtr;
	list->lastPtr->nextPtr = list->firstPtr;
    } else {
	list->firstPtr->prevPtr = NilListNode;
	list->lastPtr->nextPtr = NilListNode;
    }
}
