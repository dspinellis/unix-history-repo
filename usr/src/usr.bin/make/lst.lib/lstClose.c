/*-
 * LstClose.c --
 *	Close a list for sequential access.
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
"$Id: lstClose.c,v 1.6 88/11/17 20:51:55 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Close --
 *	Close a list which was opened for sequential access.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The list is closed.
 *
 *-----------------------------------------------------------------------
 */
void
Lst_Close (l)
    Lst	    l;	  	/* The list to close */
{
    register List 	list = (List) l;
    
    if (LstValid(l) == TRUE) {
	list->isOpen = FALSE;
	list->atEnd = Unknown;
    }
}

