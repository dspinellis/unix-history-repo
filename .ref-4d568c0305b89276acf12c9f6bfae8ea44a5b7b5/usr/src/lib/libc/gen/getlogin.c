/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getlogin.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <pwd.h>
#include <utmp.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int	_logname_valid;		/* known to setlogin() */

char *
getlogin()
{
	static char logname[MAXLOGNAME + 1];

	if (_logname_valid == 0) {
		if (_getlogin(logname, sizeof(logname) - 1) < 0)
			return ((char *)NULL);
		_logname_valid = 1;
	}
	return (*logname ? logname : (char *)NULL);
}
