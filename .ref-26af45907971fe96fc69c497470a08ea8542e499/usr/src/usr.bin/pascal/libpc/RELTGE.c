/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)RELTGE.c	1.3 (Berkeley) %G%";
#endif /* not lint */

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
