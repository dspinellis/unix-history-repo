/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)IN.c 1.1 %G%";

#include "h00vars.h"

IN(element, lower, upper, setptr)

	int	element;	/* element to check */
	int	lower;		/* lowest element of set */
	int	upper;		/* upper - lower of set */
	char	setptr[];	/* pointer to set */
{
	int	indx;

	if ((indx = element - lower) < 0 || indx > upper)
		return FALSE;
	if (setptr[indx / BITSPERBYTE] & (1 << (indx % BITSPERBYTE)))
		return TRUE;
	return FALSE;
}
