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
static char sccsid[] = "@(#)lstEnQueue.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstEnQueue.c--
 *	Treat the list as a queue and place a datum at its end
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_EnQueue --
 *	Add the datum to the tail of the given list.
 *
 * Results:
 *	SUCCESS or FAILURE as returned by Lst_Append.
 *
 * Side Effects:
 *	the lastPtr field is altered all the time and the firstPtr field
 *	will be altered if the list used to be empty.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_EnQueue (l, d)
    Lst	    	  l;
    ClientData	  d;
{
    if (LstValid (l) == FALSE) {
	return (FAILURE);
    }
    
    return (Lst_Append (l, Lst_Last(l), d));
}

