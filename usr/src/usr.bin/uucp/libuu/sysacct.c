/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sysacct.c	8.1 (Berkeley) %G%";
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
