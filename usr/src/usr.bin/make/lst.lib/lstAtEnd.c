/*-
 * LstAtEnd.c --
 *	Add a node at the end of the list
 *
 * Copyright (c) 1988 by the Regents of the University of California
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  Neither the University of California nor
 * Adam de Boor makes any representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */
#ifndef lint
static char *rcsid =
"$Id: lstAtEnd.c,v 1.3 88/11/17 20:51:48 adam Exp $ SPRITE (Berkeley)";
#endif lint

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
