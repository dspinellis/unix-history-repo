/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)IN.c	1.3 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

bool
IN(element, lower, upper, setptr)

	long	element;	/* element to check */
	long	lower;		/* lowest element of set */
	long	upper;		/* upper - lower of set */
	char	setptr[];	/* pointer to set */
{
	register int	indx;

	if ((indx = element - lower) < 0 || indx > upper)
		return FALSE;
	if (setptr[indx >> LG2BITSBYTE] & (1 << (indx & MSKBITSBYTE)))
		return TRUE;
	return FALSE;
}
