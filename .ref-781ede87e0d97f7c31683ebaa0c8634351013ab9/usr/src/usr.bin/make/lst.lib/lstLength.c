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
static char sccsid[] = "@(#)lstLength.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * lstLength.c --
 *	Find the length of a lst
 */

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
