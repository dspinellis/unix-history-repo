/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
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
static char sccsid[] = "@(#)list.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/* list.c -
 *
 * This file contains procedures for manipulating lists.
 * Structures may be inserted into or deleted from lists, and
 * they may be moved from one place in a list to another.
 *
 * The header file contains macros to help in determining the destination
 * locations for List_Insert and List_Move.  See list.h for details.
 */

#include "sprite.h"
#include "list.h"


/*
 * ----------------------------------------------------------------------------
 *
 * List_Insert --
 *
 *	Insert the list element pointed to by itemPtr into a List after 
 *	destPtr.  Perform a primitive test for self-looping by returning
 *	failure if the list element is being inserted next to itself.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The list containing destPtr is modified to contain itemPtr.
 *
 * ----------------------------------------------------------------------------
 */
void
List_Insert(itemPtr, destPtr)
    register	List_Links *itemPtr;	/* structure to insert */
    register	List_Links *destPtr;	/* structure after which to insert it */
{
    if (itemPtr == (List_Links *) NIL || destPtr == (List_Links *) NIL
	    || !itemPtr || !destPtr || (itemPtr == destPtr)) {
	Punt("List_Insert: inserting this item would create a loop.\n");
	return;
    }
    itemPtr->nextPtr = destPtr->nextPtr;
    itemPtr->prevPtr = destPtr;
    destPtr->nextPtr->prevPtr = itemPtr;
    destPtr->nextPtr = itemPtr;
}


/*
 * ----------------------------------------------------------------------------
 *
 * List_Remove --
 *
 *	Remove a list element from the list in which it is contained.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The given structure is removed from its containing list.
 *
 * ----------------------------------------------------------------------------
 */
void
List_Remove(itemPtr)
    register	List_Links *itemPtr;	/* list element to remove */
{
    if (itemPtr == (List_Links *) NIL || itemPtr == itemPtr->nextPtr
	    || !itemPtr) {
	Punt("List_Remove: invalid item to remove.\n");
    }
    itemPtr->prevPtr->nextPtr = itemPtr->nextPtr;
    itemPtr->nextPtr->prevPtr = itemPtr->prevPtr;
}


/*
 * ----------------------------------------------------------------------------
 *
 * List_Move --
 *
 *	Move the list element referenced by itemPtr to follow destPtr.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	List ordering is modified.
 *
 * ----------------------------------------------------------------------------
 */
void
List_Move(itemPtr, destPtr)
    register List_Links *itemPtr; /* list element to be moved */
    register List_Links *destPtr; /* element after which it is to be placed */
{

    if (itemPtr == (List_Links *) NIL || destPtr == (List_Links *) NIL
	    || !itemPtr || !destPtr) {
	Punt("List_Move: One of the list items is NIL.\n");
    }
    /*
     * It is conceivable that someone will try to move a list element to
     * be after itself.
     */
    if (itemPtr != destPtr) {
	List_Remove(itemPtr);
	List_Insert(itemPtr, destPtr);
    }    
}


/*
 * ----------------------------------------------------------------------------
 *
 * List_Init --
 *
 *	Initialize a header pointer to point to an empty list.  The List_Links
 *	structure must already be allocated.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The header's pointers are modified to point to itself.
 *
 * ----------------------------------------------------------------------------
 */
void
List_Init(headerPtr)
    register List_Links *headerPtr;  /* Pointer to a List_Links structure 
					to be header */
{
    if (headerPtr == (List_Links *) NIL || !headerPtr) {
	Punt("List_Init: invalid header pointer.\n");
    }
    headerPtr->nextPtr = headerPtr;
    headerPtr->prevPtr = headerPtr;
}
