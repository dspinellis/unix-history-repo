/*-
 * Copyright (c) 1990, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pty.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

openpty(amaster, aslave, name, termp, winp)
	int *amaster, *aslave;
	char *name;
	struct termios *termp;
	struct winsize *winp;
{
	static char line[] = "/dev/ptyXX";
	register const char *cp1, *cp2;
	register int master, slave, ttygid;
	struct group *gr;

	if ((gr = getgrnam("tty")) != NULL)
		ttygid = gr->gr_gid;
	else
		ttygid = -1;

	for (cp1 = "pqrs"; *cp1; cp1++) {
		line[8] = *cp1;
		for (cp2 = "0123456789abcdef"; *cp2; cp2++) {
			line[5] = 'p';
			line[9] = *cp2;
			if ((master = open(line, O_RDWR, 0)) == -1) {
				if (errno == ENOENT)
					return (-1);	/* out of ptys */
			} else {
				line[5] = 't';
				(void) chown(line, getuid(), ttygid);
				(void) chmod(line, S_IRUSR|S_IWUSR|S_IWGRP);
				(void) revoke(line);
				if ((slave = open(line, O_RDWR, 0)) != -1) {
					*amaster = master;
					*aslave = slave;
					if (name)
						strcpy(name, line);
					if (termp)
						(void) tcsetattr(slave, 
							TCSAFLUSH, termp);
					if (winp)
						(void) ioctl(slave, TIOCSWINSZ, 
							(char *)winp);
					return (0);
				}
				(void) close(master);
			}
		}
	}
	errno = ENOENT;	/* out of ptys */
	return (-1);
}

forkpty(amaster, name, termp, winp)
	int *amaster;
	char *name;
	struct termios *termp;
	struct winsize *winp;
{
	int master, slave, pid;

	if (openpty(&master, &slave, name, termp, winp) == -1)
		return (-1);
	switch (pid = fork()) {
	case -1:
		return (-1);
	case 0:
		/* 
		 * child
		 */
		(void) close(master);
		login_tty(slave);
		return (0);
	}
	/*
	 * parent
	 */
	*amaster = master;
	(void) close(slave);
	return (pid);
}
