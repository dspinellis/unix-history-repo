/*-
 * LstPred.c --
 *	Return the predecessor of a given list node
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
"$Id: lstPred.c,v 1.4 88/11/17 20:53:50 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Pred --
 *	Return the predecessor of the given node.
 *
 * Results:
 *	The node's predecessor, if any, or NILLNODE if it has none.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Pred (ln)
    LstNode	ln;
{
    if (ln == NILLNODE) {
	return (NILLNODE);
    } else {
	return ((LstNode)((ListNode) ln)->prevPtr);
    }
}

