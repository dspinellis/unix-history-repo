/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)mktemp.c	5.4 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>
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
	extern int	errno;
	register char	*start, *trv;
	struct stat	sbuf;
	u_int	pid;

	pid = getpid();

	/* extra X's get set to 0's */
	for (trv = as; *trv; ++trv);
	while (*--trv == 'X') {
		*trv = (pid % 10) + '0';
		pid /= 10;
	}

	/*
	 * check for write permission on target directory; if you have
	 * six X's and you can't write the directory, this will run for
	 * a *very* long time.
	 */
	for (start = ++trv; trv > as && *trv != '/'; --trv);
	if (*trv == '/') {
		*trv = '\0';
		if (stat(as, &sbuf) || !(sbuf.st_mode & S_IFDIR))
			return(NO);
		*trv = '/';
	}
	else if (stat(".", &sbuf) == -1)
		return(NO);

	for (;;) {
		if (doopen) {
		    if ((*doopen = open(as, O_CREAT|O_EXCL|O_RDWR, 0600)) >= 0)
			return(YES);
		    if (errno != EEXIST)
			return(NO);
		}
		else if (stat(as, &sbuf))
			return(errno == ENOENT ? YES : NO);

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
