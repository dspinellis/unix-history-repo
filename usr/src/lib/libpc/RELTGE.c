/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELTGE.c 1.2 3/7/81";

#include "h00vars.h"

bool
RELTGE(bytecnt, left, right)

	long		bytecnt;
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
