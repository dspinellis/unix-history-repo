/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)INCT.c 1.1 10/29/80";

#include "h00vars.h"

INCT(element, paircnt, singcnt, data)

	register int	element;	/* element to find */
	int		paircnt;	/* number of pairs to check */
	int		singcnt;	/* number of singles to check */
	int		data;		/* paircnt plus singcnt bounds */
{
	register int	*dataptr;
	register int	cnt;

	dataptr = &data;
	for (cnt = 0; cnt < paircnt; cnt++) {
		if (element > *dataptr++) {
			dataptr++;
			continue;
		}
		if (element >= *dataptr++) {
			return TRUE;
		}
	}
	for (cnt = 0; cnt < singcnt; cnt++) {
		if (element == *dataptr++) {
			return TRUE;
		}
	}
	return FALSE;
}
