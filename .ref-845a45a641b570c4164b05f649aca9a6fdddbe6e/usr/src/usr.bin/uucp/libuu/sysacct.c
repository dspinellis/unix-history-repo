/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sysacct.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

/*LINTLIBRARY*/

/*
 *	output accounting info
 */

/*ARGSUSED*/
sysacct(bytes, time)
time_t time;
long bytes;
{
	return;
}
