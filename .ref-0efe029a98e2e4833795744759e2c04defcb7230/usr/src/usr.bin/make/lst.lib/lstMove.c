/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstMove.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstMove.c --
 *	Move an existing node after or before one in the same or different
 *	list.
 */

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

