/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ttymsg.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <dirent.h>
#include <errno.h>
#include <paths.h>
#include <stdio.h>

/*
 * display the contents of a uio structure on a terminal.  Used by
 * wall(1) and syslogd(8).  Forks and finishes in child if write
 * would block, waiting at most five minutes.
 * Returns pointer to error string on unexpected error;
 * string is not newline-terminated.  Various "normal" errors
 * are ignored (exclusive-use, lack of permission, etc.).
 */
char *
ttymsg(iov, iovcnt, line)
	struct iovec *iov;
	int iovcnt;
	char *line;
{
	extern int errno;
	static char device[MAXNAMLEN] = _PATH_DEV;
	static char errbuf[1024];
	register int cnt, fd, left, wret;
	char *strcpy(), *strerror();
	struct iovec localiov[6];
	int forked = 0;

	if (iovcnt > 6)
		return ("too many iov's (change code in wall/ttymsg.c)");
	/*
	 * open will fail on slip lines or exclusive-use lines
	 * if not running as root; not an error.
	 */
	(void) strcpy(device + sizeof(_PATH_DEV) - 1, line);
	if ((fd = open(device, O_WRONLY|O_NONBLOCK, 0)) < 0) {
		if (errno != EBUSY && errno != EACCES) {
			(void) sprintf(errbuf, "open %s: %s", device,
			    strerror(errno));
			return (errbuf);
		} else
			return (NULL);
	}

	for (cnt = left = 0; cnt < iovcnt; ++cnt)
		left += iov[cnt].iov_len;

	for (;;) {
		if ((wret = writev(fd, iov, iovcnt)) < 0) {
			if (errno == EWOULDBLOCK) {
				int off = 0;
				int cpid;

				if (forked) {
					(void) close(fd);
					/* return ("already forked"); */
					/* "can't happen" */
					exit(1);
				}
				cpid = fork();
				if (cpid < 0) {
					(void) sprintf(errbuf, "can't fork: %s",
						strerror(errno));
					(void) close(fd);
					return (errbuf);
				}
				if (cpid) {	/* parent */
					(void) close(fd);
					return (NULL);
				}
				forked++;
				/* wait at most 5 minutes */
				(void) signal(SIGALRM, SIG_DFL);
				(void) signal(SIGTERM, SIG_DFL); /* XXX */
				(void) sigsetmask(0);
				(void) alarm((u_int)(60 * 5));
				(void) fcntl(fd, FNDELAY, &off);
				continue;
			} else {
				/*
				 * we get ENODEV on a slip line if we're
				 * running as root, and EIO if the line
				 * just went away
				 */
				if (errno == ENODEV || errno == EIO)
					break;
				(void) sprintf(errbuf, "writing %s: %s",
				    device, strerror(errno));
				(void) close(fd);
				return (errbuf);
			}
		} 
		if (wret < left) {
			left -= wret;
			if (iov != localiov) {
				bcopy(iov, localiov, 
					iovcnt * sizeof (struct iovec));
				iov = localiov;
			}
			for (cnt = 0; wret >= iov->iov_len; ++cnt) {
				wret -= iov->iov_len;
				++iov;
				--iovcnt;
			}
			if (wret) {
				iov->iov_base += wret;
				iov->iov_len -= wret;
			}
		} else
			break;
	}
	if (forked)
		exit(0);
	(void) close(fd);
	return (NULL);
}
