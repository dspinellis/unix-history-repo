/*-
 * LstIsEmpty.c --
 *	A single function to decide if a list is empty
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
"$Id: lstIsEmpty.c,v 1.5 88/11/17 20:53:19 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_IsEmpty --
 *	Return TRUE if the given list is empty.
 *
 * Results:
 *	TRUE if the list is empty, FALSE otherwise.
 *
 * Side Effects:
 *	None.
 *
 *	A list is considered empty if its firstPtr == NilListNode (or if
 *	the list itself is NILLIST).
 *-----------------------------------------------------------------------
 */
Boolean
Lst_IsEmpty (l)
    Lst	l;
{
    return ( ! LstValid (l) || LstIsEmpty(l));
}

