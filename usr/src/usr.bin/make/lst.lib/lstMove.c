/*-
 * LstMove.c --
 *	Move an existing node after or before one in the same or different
 *	list.
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
"$Id: lstMove.c,v 1.6 89/06/13 15:01:48 adam Exp $ SPRITE (Berkeley)";
#endif lint

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Move --
 *	Move a node after or before a destination node. The nodes do not
 *	need to be in the same list, of course.
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	The firstPtr and lastPtr fields of either or both of the involved
 *	lists may be altered to reflect reality.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Move (ls, lns, ld, lnd, before)
    Lst	    	    	ls; 	/* Source list */
    register LstNode  	lns;	/* source list node */
    Lst	    	    	ld; 	/* Destination list */
    register LstNode  	lnd;	/* destination list node */
    Boolean		before;	/* true if should use Lst_Insert */
{
    ClientData	d;
    ReturnStatus	rval;
    
    if (lns == NILLNODE || lnd == NILLNODE) {
	return (FAILURE);
    }
    
    d = ((ListNode)lns)->datum;
    
    if (Lst_Remove (ls, lns) == FAILURE) {
	return (FAILURE);
    }
    
    if (before == TRUE) {
	rval = Lst_Insert (ld, lnd, d);
    } else {
	rval = Lst_Append (ld, lnd, d);
    }
    
    return (rval);
}

