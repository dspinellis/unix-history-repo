/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELEQ.c 1.1 10/29/80";

#include "h00vars.h"

RELEQ(size, str1, str2)

	register int	size;
	register char	*str1;
	register char	*str2;
{
	while (*str1++ == *str2++ && --size)
		/* void */;
	if (size == 0)
		return TRUE;
	return FALSE;
}
