/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fork_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

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
