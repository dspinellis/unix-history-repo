/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELNE.c 1.1 %G%";

#include "h00vars.h"

RELNE(size, str1, str2)

	register int	size;
	register char	*str1;
	register char	*str2;
{
	while (*str1++ == *str2++ && --size)
		/* void */;
	if (size == 0)
		return FALSE;
	return TRUE;
}
