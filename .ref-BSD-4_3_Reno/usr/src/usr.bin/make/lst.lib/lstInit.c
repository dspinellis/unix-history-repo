/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstInit.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * init.c --
 *	Initialize a new linked list.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Init --
 *	Create and initialize a new list.
 *
 * Results:
 *	The created list.
 *
 * Side Effects:
 *	A list is created, what else?
 *
 *-----------------------------------------------------------------------
 */
Lst
Lst_Init(circ)
    Boolean		circ;	/* TRUE if the list should be made circular */
{
    register List	nList;
    
    PAlloc (nList, List);
    
    nList->firstPtr = NilListNode;
    nList->lastPtr = NilListNode;
    nList->isOpen = FALSE;
    nList->isCirc = circ;
    nList->atEnd = Unknown;
    
    return ((Lst)nList);
}

Malloc(nbytes)
{
#ifdef DEBUG
    printf("malloc: %d\n", nbytes);
#endif DEBUG
    return(malloc(nbytes));
}
