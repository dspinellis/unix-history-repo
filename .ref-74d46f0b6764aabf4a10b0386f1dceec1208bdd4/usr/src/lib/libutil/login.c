/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)login.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <utmp.h>
#include <stdio.h>

void
login(ut)
	struct utmp *ut;
{
	register int fd;
	int tty;
	off_t lseek();

	tty = ttyslot();
	if (tty > 0 && (fd = open(_PATH_UTMP, O_WRONLY|O_CREAT, 0644)) >= 0) {
		(void)lseek(fd, (long)(tty * sizeof(struct utmp)), L_SET);
		(void)write(fd, (char *)ut, sizeof(struct utmp));
		(void)close(fd);
	}
	if ((fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) >= 0) {
		(void)write(fd, (char *)ut, sizeof(struct utmp));
		(void)close(fd);
	}
}
