/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)INCT.c 1.2 3/7/81";

#include "h00vars.h"

bool
INCT(element, paircnt, singcnt, data)

	register long	element;	/* element to find */
	long		paircnt;	/* number of pairs to check */
	long		singcnt;	/* number of singles to check */
	long		data;		/* paircnt plus singcnt bounds */
{
	register long	*dataptr = &data;
	register int	cnt;

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
