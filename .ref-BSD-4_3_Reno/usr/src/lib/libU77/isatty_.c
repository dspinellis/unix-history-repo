/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)isatty_.c	5.1	6/7/85
 */

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
