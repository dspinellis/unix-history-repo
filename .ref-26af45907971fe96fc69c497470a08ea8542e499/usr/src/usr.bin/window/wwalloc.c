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
static char sccsid[] = "@(#)wwalloc.c	3.11 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

char **
wwalloc(row, col, nrow, ncol, size)
{
	register char *p, **pp;
	register int i;

	/* fast, call malloc only once */
	pp = (char **)
		malloc((unsigned) sizeof (char **) * nrow + size * nrow * ncol);
	if (pp == 0) {
		wwerrno = WWE_NOMEM;
		return 0;
	}
	p = (char *)&pp[nrow];
	col *= size;
	size /= sizeof (char);		/* paranoid */
	size *= ncol;
	for (i = 0; i < nrow; i++) {
		pp[i] = p - col;
		p += size;
	}
	return pp - row;
}

wwfree(p, row)
register char **p;
{
	free((char *)(p + row));
}
