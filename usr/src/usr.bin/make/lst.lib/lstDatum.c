/*-
 * LstDatum.c --
 *	Return the datum associated with a list node.
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
"$Id: lstDatum.c,v 1.4 88/11/17 20:52:07 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Datum --
 *	Return the datum stored in the given node.
 *
 * Results:
 *	The datum or (ick!) NIL if the node is invalid.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
ClientData
Lst_Datum (ln)
    LstNode	ln;
{
    if (ln != NILLNODE) {
	return (((ListNode)ln)->datum);
    } else {
	return ((ClientData) NIL);
    }
}

