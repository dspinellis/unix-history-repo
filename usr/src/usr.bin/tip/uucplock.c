/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)uucplock.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <errno.h>
#include "pathnames.h"

/* 
 * uucp style locking routines
 * return: 0 - success
 * 	  -1 - failure
 */

uu_lock(ttyname)
	char *ttyname;
{
	extern int errno;
	int fd, pid;
	char tbuf[sizeof(_PATH_LOCKDIRNAME) + MAXNAMLEN];
	off_t lseek();

	(void)sprintf(tbuf, _PATH_LOCKDIRNAME, ttyname);
	fd = open(tbuf, O_RDWR|O_CREAT|O_EXCL, 0660);
	if (fd < 0) {
		/*
		 * file is already locked
		 * check to see if the process holding the lock still exists
		 */
		fd = open(tbuf, O_RDWR, 0);
		if (fd < 0) {
			perror("lock open");
			return(-1);
		}
		if (read(fd, &pid, sizeof(pid)) != sizeof(pid)) {
			(void)close(fd);
			perror("lock read");
			return(-1);
		}

		if (kill(pid, 0) == 0 || errno != ESRCH) {
			(void)close(fd);	/* process is still running */
			return(-1);
		}
		/*
		 * The process that locked the file isn't running, so
		 * we'll lock it ourselves
		 */
		if (lseek(fd, 0L, L_SET) < 0) {
			(void)close(fd);
			perror("lock lseek");
			return(-1);
		}
		/* fall out and finish the locking process */
	}
	pid = getpid();
	if (write(fd, (char *)&pid, sizeof(pid)) != sizeof(pid)) {
		(void)close(fd);
		(void)unlink(tbuf);
		perror("lock write");
		return(-1);
	}
	(void)close(fd);
	return(0);
}

uu_unlock(ttyname)
	char *ttyname;
{
	char tbuf[sizeof(_PATH_LOCKDIRNAME) + MAXNAMLEN];

	(void)sprintf(tbuf, _PATH_LOCKDIRNAME, ttyname);
	return(unlink(tbuf));
}
