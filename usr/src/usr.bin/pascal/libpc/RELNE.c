/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)RELNE.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

bool
RELNE(siz, str1, str2)

	long		siz;
	register char	*str1;
	register char	*str2;
{
	register int size = siz;

	while (*str1++ == *str2++ && --size)
		/* void */;
	if (size == 0)
		return FALSE;
	return TRUE;
}
