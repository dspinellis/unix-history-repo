/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pty.c	1.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/file.h>
#include <errno.h>

openpty(amaster, aslave, name, flag)
	int *amaster, *aslave;
	char *name;
	int flag;
{
	register char *line = "/dev/ptyXX", *cp1, *cp2;
	register master, slave;
	extern errno;

	for (cp1 = "pqrs"; *cp1; cp1++) {
		line[8] = *cp1;
		for (cp2 = "0123456789abcdef"; *cp2; cp2++) {
			line[9] = *cp2;
			if ((master = open(line, O_RDWR, 0)) == -1) {
				if (errno != EIO)
					return (-1);
			} else {
				line[5] = 't';
				if ((slave = open(line, O_RDWR, 0)) != -1) {
					*amaster = master;
					*aslave = slave;
					if (name)
						strcpy(name, line);
					return (0);
				}
				close(master);
				line[5] = 'p';
			}
		}
	}
	return (-1);
}
