/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)login.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <utmp.h>
#include <stdio.h>

void
login(ut)
	struct utmp *ut;
{
	register int fd;
	int tty;

	tty = ttyslot();
	if (tty > 0 && (fd = open(_PATH_UTMP, O_WRONLY|O_CREAT, 0644)) >= 0) {
		(void)lseek(fd, (off_t)(tty * sizeof(struct utmp)), L_SET);
		(void)write(fd, ut, sizeof(struct utmp));
		(void)close(fd);
	}
	if ((fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) >= 0) {
		(void)write(fd, ut, sizeof(struct utmp));
		(void)close(fd);
	}
}
