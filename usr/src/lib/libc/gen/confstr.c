/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)confstr.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <errno.h>
#include <paths.h>
#include <unistd.h>

size_t
confstr(name, buf, len)
	int name;
	char *buf;
	size_t len;
{
	switch (name) {
	case _CS_PATH:
		if (len != 0 && buf != NULL) {
			(void)strncpy(buf, _PATH_STDPATH, len - 1);
			buf[len - 1] = '\0';
		}
		return (sizeof(_PATH_STDPATH));
	default:
		errno = EINVAL;
		return (0);
	}
	/* NOTREACHED */
}
