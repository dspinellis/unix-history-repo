/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)isatty_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * determine if stream is associated with a tty (async port)
 *
 * calling sequence:
 *	logical	isatty, val
 *	val = isatty (lunit)
 * where:
 *	val will be .TRUE. if lunit is associated with a 'tty'
 */

#include	"../libI77/fiodefs.h"

extern unit units[];	/* logical units table from iolib */

long isatty_(u)
long *u;
{
	int	i;
	unit	*lu;

	if (*u < 0 || *u >= MXUNIT)
		return(0);
	lu = &units[*u];
	if (!lu->ufd)
		return(0);
	return((long)(isatty(fileno(lu->ufd)) != 0));
}
