/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELTLE.c 1.1 10/29/80";

#include "h00vars.h"

RELTLE(bytecnt, left, right)

	int		bytecnt;
	register long	*left;
	register long	*right;
{
	register int	longcnt;

	longcnt = bytecnt >> 2;
	do	{
		if ((*left++ & ~*right++) != 0) 
			return FALSE;
	} while (--longcnt);
	return TRUE;
}
