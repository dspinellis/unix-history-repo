/*-
 * LstFirst.c --
 *	Return the first node of a list
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
"$Id: lstFirst.c,v 1.5 88/11/17 20:52:44 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_First --
 *	Return the first node on the given list.
 *
 * Results:
 *	The first node or NILLNODE if the list is empty.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_First (l)
    Lst	l;
{
    if (!LstValid (l) || LstIsEmpty (l)) {
	return (NILLNODE);
    } else {
	return ((LstNode)((List)l)->firstPtr);
    }
}

