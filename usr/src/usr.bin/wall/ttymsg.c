/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ttymsg.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/file.h>
#include <dirent.h>
#include <errno.h>
#include <paths.h>

/*
 * display the contents of a uio structure on a terminal.  Used by
 * wall(1) and syslogd(8).
 */
char *
ttymsg(iov, iovcnt, line, nonblock)
	struct iovec *iov;
	int iovcnt;
	char *line;
	int nonblock;
{
	extern int errno;
	static char device[MAXNAMLEN] = _PATH_DEV;
	static char errbuf[1024];
	register int cnt, fd, total, wret;
	char *strcpy(), *strerror();

	/*
	 * open will fail on slip lines or exclusive-use lines
	 * if not running as root; not an error.
	 */
	(void)strcpy(device + sizeof(_PATH_DEV) - 1, line);
	if ((fd = open(device, O_WRONLY|(nonblock ? O_NONBLOCK : 0), 0)) < 0)
		if (errno != EBUSY && errno != EPERM)
			goto bad;
		else
			return(NULL);

	for (cnt = total = 0; cnt < iovcnt; ++cnt)
		total += iov[cnt].iov_len;

	for (;;)
		if ((wret = writev(fd, iov, iovcnt)) < 0)
			if (errno == EWOULDBLOCK) {
				if (fork())
					goto bad;
				/* wait at most 5 minutes */
				(void)alarm((u_int)(60 * 5));
				(void)ttymsg(iov, iovcnt, line, 0);
				exit(0);
			} else {
				/*
				 * we get ENODEV on a slip line if we're
				 * running as root
				 */
				if (errno == ENODEV)
					break;
bad:				(void)sprintf(errbuf, "%s: %s\n", device,
				    strerror(errno));
				(void)close(fd);
				return(errbuf);
			}
		else if (wret) {
			if (wret == total)
				break;
			for (cnt = 0; wret >= iov->iov_len; ++cnt) {
				wret -= iov->iov_len;
				++iov;
				--iovcnt;
			}
			if (wret) {
				iov->iov_base += wret;
				iov->iov_len -= wret;
			}
		}
	(void)close(fd);
	return(NULL);
}
