/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sgtty.h>

#define	DEVDIR	"/dev/"

char *
ttyname(fd)
	int fd;
{
	register struct direct *dirp;
	register DIR *dp;
	struct stat sb1, sb2;
	struct sgttyb ttyb;
	static char buf[sizeof(DEVDIR) + MAXNAMLEN] = DEVDIR;
	char *rval, *strcpy();

	if (ioctl(fd, TIOCGETP, &ttyb) < 0)
		return(NULL);
	if (fstat(fd, &sb1) < 0 || (sb1.st_mode&S_IFMT) != S_IFCHR)
		return(NULL);
	if ((dp = opendir(DEVDIR)) == NULL)
		return(NULL);
	for (rval = NULL; dirp = readdir(dp);) {
		if (dirp->d_ino != sb1.st_ino)
			continue;
		(void)strcpy(buf + sizeof(DEVDIR) - 1, dirp->d_name);
		if (stat(buf, &sb2) < 0 || sb1.st_dev != sb2.st_dev ||
		    sb1.st_ino != sb1.st_ino)
			continue;
		closedir(dp);
		rval = buf;
		break;
	}
	closedir(dp);
	return(rval);
}
