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
static char sccsid[] = "@(#)lstOpen.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstOpen.c --
 *	Open a list for sequential access. The sequential functions access the
 *	list in a slightly different way. CurPtr points to their idea of the
 *	current node in the list and they access the list based on it.
 *	If the list is circular, Lst_Next and Lst_Prev will go around
 *	the list forever. Lst_IsAtEnd must be used to determine when to stop.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Open --
 *	Open a list for sequential access. A list can still be searched,
 *	etc., without confusing these functions.
 *
 * Results:
 *	SUCCESS or FAILURE.
 *
 * Side Effects:
 *	isOpen is set TRUE and curPtr is set to NilListNode so the
 *	other sequential functions no it was just opened and can choose
 *	the first element accessed based on this.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_Open (l)
	register Lst	l;
{
	if (LstValid (l) == FALSE) {
		return (FAILURE);
	}
	((List) l)->isOpen = TRUE;
	((List) l)->atEnd = LstIsEmpty (l) ? Head : Unknown;
	((List) l)->curPtr = NilListNode;

	return (SUCCESS);
}

