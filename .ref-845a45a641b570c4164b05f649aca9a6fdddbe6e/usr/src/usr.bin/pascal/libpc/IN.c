/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)IN.c	1.3 (Berkeley) %G%";
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
