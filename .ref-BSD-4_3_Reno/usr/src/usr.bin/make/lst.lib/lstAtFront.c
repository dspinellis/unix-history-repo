/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)lstAtFront.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * LstAtFront.c --
 *	Add a node at the front of the list
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_AtFront --
 *	Place a piece of data at the front of a list
 *
 * Results:
 *	SUCCESS or FAILURE
 *
 * Side Effects:
 *	A new ListNode is created and stuck at the front of the list.
 *	hence, firstPtr (and possible lastPtr) in the list are altered.
 *
 *-----------------------------------------------------------------------
 */
ReturnStatus
Lst_AtFront (l, d)
    Lst		l;
    ClientData	d;
{
    register LstNode	front;
    
    front = Lst_First (l);
    return (Lst_Insert (l, front, d));
}
