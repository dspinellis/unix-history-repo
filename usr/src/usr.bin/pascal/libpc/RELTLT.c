/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELTLT.c 1.1 %G%";

#include "h00vars.h"

RELTLT(bytecnt, left, right)

	int		bytecnt;
	register long	*left;
	register long	*right;
{
	register int	longcnt;

	longcnt = bytecnt >> 2;
	do	{
		if ((*left & ~*right) != 0)
			return FALSE;
		if ((*right++ & ~*left++) != 0)
			goto leq;
	} while (--longcnt);
	return FALSE;
leq:
	while (--longcnt) {
		if ((*left++ & ~*right++) != 0) 
			return FALSE;
	}
	return TRUE;
}
