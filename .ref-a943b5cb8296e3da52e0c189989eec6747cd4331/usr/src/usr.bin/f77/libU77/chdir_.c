/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)chdir_.c	5.1	%G%
 */

/*
 * change default directory
 *
 * calling sequence:
 *	integer chdir
 *	ierror = chdir(dirname)
 * where:
 *	ierror will receive a returned status (0 == OK)
 *	dirname is the directory name
 */

#include "../libI77/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

long chdir_(dname, dnamlen)
char *dname;
long dnamlen;
{
	char buf[MAXPATHLEN];

	if (dnamlen >= sizeof buf)
		return((long)(errno=F_ERARG));
	g_char(dname, dnamlen, buf);
	if (chdir(buf) != 0)
		return((long)errno);
	return(0L);
}
