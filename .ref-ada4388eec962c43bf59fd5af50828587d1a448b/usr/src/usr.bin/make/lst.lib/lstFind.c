/*
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lstFind.c	8.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstFind.c --
 *	Find a node on a list.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Find --
 *	Find a node on the given list using the given comparison function
 *	and the given datum.
 *
 * Results:
 *	The found node or NILLNODE if none matches.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Find (l, d, cProc)
    Lst		l;
    ClientData	d;
    int		(*cProc) __P((ClientData, ClientData));
{
    return (Lst_FindFrom (l, Lst_First(l), d, cProc));
}

