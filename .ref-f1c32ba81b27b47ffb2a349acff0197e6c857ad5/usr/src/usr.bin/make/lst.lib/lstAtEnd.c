/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lstAtEnd.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstAtEnd.c --
 *	Add a node at the end of the list
 */

#include	"lstInt.h"
	
/*-
 *-----------------------------------------------------------------------
 * Lst_AtEnd --
 *	Add a node to the end of the given list
 *
 * Results:
 *	SUCCESS if life is good.
 *
 * Side Effects:
 *	A new ListNode is created and added to the list.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_AtEnd (l, d)
    Lst		l;	/* List to which to add the datum */
    ClientData	d;	/* Datum to add */
{
    register LstNode	end;
    
    end = Lst_Last (l);
    return (Lst_Append (l, end, d));
}
