/*-
 * LstCur.c --
 *	Return the current node in the list.
 *	The sequential functions access the list in a slightly different way.
 *	CurPtr points to their idea of the current node in the list and they
 *	access the list based on it. Because the list is circular, Lst_Next
 *	and Lst_Prev will go around the list forever. Lst_IsAtEnd must be
 *	used to determine when to stop.
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
"$Id: lstCur.c,v 1.4 88/11/17 20:52:03 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Cur --
 *	Return the current node if the list is open for sequential
 *	access.
 *
 * Results:
 *	The current node or NILLNODE if the list isn't open..
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Cur (l)
    Lst	    l;
{
    register List list = (List)l;

    if ((LstValid(l) == FALSE) ||
	(list->isOpen == FALSE)) {
	    return (NILLNODE);
    } else {
	return ((LstNode)list->curPtr);
    }
}

