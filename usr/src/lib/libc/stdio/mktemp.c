/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)mktemp.c	5.3 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>

#define	YES	1
#define	NO	0

mkstemp(as)
	char	*as;
{
	int	fd;

	return (_gettemp(as, &fd) ? fd : -1);
}

char *
mktemp(as)
	char	*as;
{
	return(_gettemp(as, (int *)NULL) ? as : (char *)NULL);
}

static
_gettemp(as, doopen)
	char	*as;
	register int	*doopen;
{
	register char	*start, *trv;
	u_int	pid;
	char	savech;

	pid = getpid();

	/* extra X's get set to 0's */
	for (trv = as;*trv;++trv);
	while (*--trv == 'X') {
		*trv = (pid % 10) + '0';
		pid /= 10;
	}

	/*
	 * check for write permission on target directory; if you have
	 * six X's and you can't write the directory, this will run for
	 * a *very* long time.
	 */
	for (start = ++trv;trv > as && *trv != '/';--trv);
	if (*trv == '/') {
		savech = *++trv;
		*trv = '\0';
		if (access(as, W_OK))
			return(NO);
		*trv = savech;
	}
	else if (access(".", W_OK))
		return(NO);

	for (;;) {
		if (doopen
		    && (*doopen = open(as, O_CREAT|O_EXCL|O_RDWR, 0600)) != -1
		    || access(as, F_OK))
			return(YES);
		/* tricky little algorithm for backward compatibility */
		for (trv = start;;) {
			if (!*trv)
				return(NO);
			if (*trv == 'z')
				*trv++ = 'a';
			else {
				if (isdigit(*trv))
					*trv = 'a';
				else
					++*trv;
				break;
			}
		}
	}
	/*NOTREACHED*/
}
