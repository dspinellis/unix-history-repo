/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pty.c	1.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/file.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <grp.h>
#include <errno.h>

openpty(amaster, aslave, name, termp, winp)
	int *amaster, *aslave;
	char *name;
	struct termios *termp;
	struct winsize *winp;
{
	register char *line = "/dev/ptyXX", *cp1, *cp2;
	register master, slave, ruid, ttygid;
	struct group *gr;
	extern errno;

	if ((gr = getgrnam("tty")) != NULL)
		ttygid = gr->gr_gid;
	else
		ttygid = -1;
	ruid = getuid();

	for (cp1 = "pqrs"; *cp1; cp1++) {
		line[8] = *cp1;
		for (cp2 = "0123456789abcdef"; *cp2; cp2++) {
			line[9] = *cp2;
			if ((master = open(line, O_RDWR, 0)) == -1) {
				if (errno != EIO)
					return (-1);	/* out of ptys */
			} else {
				line[5] = 't';
				(void) chown(line, ruid, ttygid);
				(void) chmod(line, 0620);
				(void) revoke(line);
				if ((slave = open(line, O_RDWR, 0)) != -1) {
					*amaster = master;
					*aslave = slave;
					if (name)
						strcpy(name, line);
					if (termp)
						(void) tcsetattr(slave, 
							TCSAFLUSH, &termp);
					if (winp)
						(void) ioctl(slave, TIOCSWINSZ, 
							(char *)&winp);
					return (0);
				}
				(void) close(master);
				line[5] = 'p';
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
