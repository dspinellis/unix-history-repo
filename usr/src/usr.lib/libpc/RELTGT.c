/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RELTGT.c 1.2 3/7/81";

#include "h00vars.h"

bool
RELTGT(bytecnt, left, right)

	long		bytecnt;
	register long	*left;
	register long	*right;
{
	register int	longcnt;

	longcnt = bytecnt >> 2;
	do	{
		if ((*right & ~*left) != 0)
			return FALSE;
		if ((*left++ & ~*right++) != 0)
			goto geq;
	} while (--longcnt);
	return FALSE;
geq:
	while (--longcnt) {
		if ((*right++ & ~*left++) != 0) 
			return FALSE;
	}
	return TRUE;
}
