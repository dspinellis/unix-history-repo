/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwflush.c	3.12 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwflush()
{
	register row, col;

	if ((row = wwcursorrow) < 0)
		row = 0;
	else if (row >= wwnrow)
		row = wwnrow - 1;
	if ((col = wwcursorcol) < 0)
		col = 0;
	else if (col >= wwncol)
		col = wwncol - 1;
	xxmove(row, col);
	xxflush(1);
}
