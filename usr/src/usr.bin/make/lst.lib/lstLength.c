/*-
 * lstLength.c --
 *	Find the length of a lst
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
"$Id: lstLength.c,v 1.2 88/11/17 20:53:27 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include    "lstInt.h"

int
Lst_Length(l)
    Lst	    l;	  /* List whose length is desired */
{
    register ListNode 	node;
    register List 	list = (List)l;
    register int  	len;

    if (!LstValid(l)) {
	return -1;
    }

    for (len = 0, node = list->firstPtr;
	 node != NilListNode;
	 len++, node = node->nextPtr) {
	if (node == list->firstPtr && len != 0) {
	    break;
	}
    }
    return len;
}
