/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)IN.c 1.2 3/7/81";

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
