/*-
 * init.c --
 *	Initialize a new linked list.
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
"$Id: lstInit.c,v 1.7 88/11/17 20:52:57 adam Exp $ SPRITE (Berkeley)";
#endif lint

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
