/*-
 * LstReplace.c --
 *	Replace the datum in a node with a new datum
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
"$Id: lstReplace.c,v 1.4 88/11/17 20:54:01 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Replace --
 *	Replace the datum in the given node with the new datum
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	The datum field fo the node is altered.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Replace (ln, d)
    register LstNode	ln;
    ClientData	  	d;
{
    if (ln == NILLNODE) {
	return (FAILURE);
    } else {
	((ListNode) ln)->datum = d;
	return (SUCCESS);
    }
}

