/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)abort_.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

#if	pdp11
abort_()
{
	fprintf(stderr, "Fortran abort routine called\n");
	f_exit();
	_cleanup();
	abort();
}
#else	vax || tahoe
abort_(msg,len)
char *msg; int len;
{
	fprintf(stderr, "abort: ");
	if (nargs()) while (len-- > 0) fputc(*msg++, stderr);
	else fprintf(stderr, "called");
	fputc('\n', stderr);
	f77_abort();
}
#endif	vax || tahoe
