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
static char sccsid[] = "@(#)lstCur.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*-
 * LstCur.c --
 *	Return the current node in the list.
 *	The sequential functions access the list in a slightly different way.
 *	CurPtr points to their idea of the current node in the list and they
 *	access the list based on it. Because the list is circular, Lst_Next
 *	and Lst_Prev will go around the list forever. Lst_IsAtEnd must be
 *	used to determine when to stop.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_Cur --
 *	Return the current node if the list is open for sequential
 *	access.
 *
 * Results:
 *	The current node or NILLNODE if the list isn't open..
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
LstNode
Lst_Cur (l)
    Lst	    l;
{
    register List list = (List)l;

    if ((LstValid(l) == FALSE) ||
	(list->isOpen == FALSE)) {
	    return (NILLNODE);
    } else {
	return ((LstNode)list->curPtr);
    }
}

