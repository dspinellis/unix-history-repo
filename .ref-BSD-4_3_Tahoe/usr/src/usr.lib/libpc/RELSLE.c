/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELSLE.c 1.2 3/7/81";

#include "h00vars.h"

bool
RELSLE(siz, str1, str2)

	long		siz;
	register char	*str1;
	register char	*str2;
{
	register int size = siz;

	while (*str1++ == *str2++ && --size)
		/* void */;
	if ((size == 0) || (*--str1 <= *--str2))
		return TRUE;
	return FALSE;
}
