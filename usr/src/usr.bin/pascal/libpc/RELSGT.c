/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELSGT.c 1.1 %G%";

#include "h00vars.h"

RELSGT(size, str1, str2)

	register int	size;
	register char	*str1;
	register char	*str2;
{
	while (*str1++ == *str2++ && --size)
		/* void */;
	if ((size == 0) || (*--str1 <= *--str2))
		return FALSE;
	return TRUE;
}
