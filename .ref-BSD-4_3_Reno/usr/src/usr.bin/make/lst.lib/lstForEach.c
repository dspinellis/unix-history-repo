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
static char sccsid[] = "@(#)lstForEach.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * LstForeach.c --
 *	Perform a given function on all elements of a list.
 */

#include	"lstInt.h"

/*-
 *-----------------------------------------------------------------------
 * Lst_ForEach --
 *	Apply the given function to each element of the given list. The
 *	function should return 0 if Lst_ForEach should continue and non-
 *	zero if it should abort.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Only those created by the passed-in function.
 *
 *-----------------------------------------------------------------------
 */
/*VARARGS2*/
void
Lst_ForEach (l, proc, d)
    Lst	    	  	l;
    register int	(*proc)();
    register ClientData	d;
{
    Lst_ForEachFrom(l, Lst_First(l), proc, d);
}

