/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)fork_.c	5.1	%G%
 */

/*
 * fork a copy of this process
 *
 * calling sequence:
 *	integer fork
 *	ierror = fork()
 * where:
 *	ierror will be	- child pid if parent and successful
 *			- 0 if child
 *			- -errno if unsuccessful
 */

#include	"../libI77/fiodefs.h"

extern int errno;

long fork_()
{
	long i;

	for (i = 0; i < MXUNIT; i++)
		flush_(&i);
	i = (long)fork();
	if (i < 0)
		return((long)(-errno));
	return(i);
}
