/*-
 * LstSucc.c --
 *	return the successor to a given node
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
"$Id: lstSucc.c,v 1.4 88/11/17 20:54:07 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Succ --
 *	Return the sucessor to the given node on its list.
 *
 * Results:
 *	The successor of the node, if it exists (note that on a circular
 *	list, if the node is the only one in the list, it is its own
 *	successor).
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Succ (ln)
    LstNode	ln;
{
    if (ln == NILLNODE) {
	return (NILLNODE);
    } else {
	return ((LstNode) ((ListNode) ln)->nextPtr);
    }
}

