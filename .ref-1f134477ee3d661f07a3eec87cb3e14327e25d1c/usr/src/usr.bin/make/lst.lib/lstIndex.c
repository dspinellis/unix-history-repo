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
static char sccsid[] = "@(#)lstIndex.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * lstIndex.c --
 *	Function to return the index of a datum in a Lst.
 */

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
