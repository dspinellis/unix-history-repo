/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lstInit.c	5.4 (Berkeley) %G%";
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
