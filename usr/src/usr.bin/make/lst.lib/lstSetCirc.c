/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstSetCirc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * listSetCirc.c --
 *	Change the library's notion of the circularity of a list.
 */

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
