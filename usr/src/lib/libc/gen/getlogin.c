/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getlogin.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <stdio.h>
#include <pwd.h>
#include <utmp.h>

static	char	logname[UT_NAMESIZE+1];

char *
getlogin()
{
	static int notcalled = 1;

	if (notcalled) {
		if (getlogname(logname, sizeof(logname) - 1) < 0)
			return((char *)NULL);
		notcalled = 0;
	}
	return(*logname ? logname : (char *)NULL);
}

uid_t	geteuid();

char *
cuserid(s)
	char	*s;
{
	register struct passwd *pwd;

	if ((pwd = getpwuid(geteuid())) == NULL) {
		if (s)
			*s = '\0';
		return(NULL);
	}
	if (s) {
		(void)strncpy(s, pwd->pw_name, L_cuserid);
		return(s);
	}
	return(pwd->pw_name);
}
