/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getlogin.c	5.8 (Berkeley) 6/24/90";
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
