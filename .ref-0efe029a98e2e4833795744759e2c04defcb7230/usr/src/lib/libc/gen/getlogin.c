/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getlogin.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <stdio.h>
#include <pwd.h>
#include <utmp.h>

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

uid_t	geteuid();

char *
cuserid(s)
	char *s;
{
	register struct passwd *pwd;

	if ((pwd = getpwuid(geteuid())) == NULL) {
		if (s)
			*s = '\0';
		return (s);
	}
	if (s) {
		(void)strncpy(s, pwd->pw_name, L_cuserid);
		return (s);
	}
	return (pwd->pw_name);
}
