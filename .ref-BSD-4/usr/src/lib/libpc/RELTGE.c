/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELTGE.c 1.1 10/29/80";

#include "h00vars.h"

RELTGE(bytecnt, left, right)

	int		bytecnt;
	register long	*left;
	register long	*right;
{
	register int	longcnt;

	longcnt = bytecnt >> 2;
	do	{
		if ((*right++ & ~*left++) != 0) 
			return FALSE;
	} while (--longcnt);
	return TRUE;
}
