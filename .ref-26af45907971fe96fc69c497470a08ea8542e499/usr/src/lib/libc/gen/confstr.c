/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)confstr.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/sysctl.h>

#include <errno.h>
#include <paths.h>
#include <stdlib.h>
#include <unistd.h>

size_t
confstr(name, buf, len)
	int name;
	char *buf;
	size_t len;
{
	size_t tlen;
	int mib[2], sverrno;
	char *p;

	switch (name) {
	case _CS_PATH:
		mib[0] = CTL_USER;
		mib[1] = USER_CS_PATH;
		if (sysctl(mib, 2, NULL, &tlen, NULL, 0) == -1)
			return (-1);
		if (len != 0 && buf != NULL) {
			if ((p = malloc(tlen)) == NULL)
				return (-1);
			if (sysctl(mib, 2, p, &tlen, NULL, 0) == -1) {
				sverrno = errno;
				free(p);
				errno = sverrno;
				return (-1);
			}
			/*
			 * POSIX 1003.2 requires partial return of
			 * the string -- that should be *real* useful.
			 */
			(void)strncpy(buf, p, len - 1);
			buf[len - 1] = '\0';
			free(p);
		}
		return (tlen + 1);
	default:
		errno = EINVAL;
		return (0);
	}
	/* NOTREACHED */
}
