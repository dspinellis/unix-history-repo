/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software written by Ken Arnold and
 * published in UNIX Review, Vol. 6, No. 8.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)popen.c	5.14 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <paths.h>

static pid_t *pids;

FILE *
popen(program, type)
	char *program, *type;
{
	FILE *iop;
	int pdes[2], fds, pid;
	char *malloc();

	if (*type != 'r' && *type != 'w' || type[1])
		return (NULL);

	if (pids == NULL) {
		if ((fds = getdtablesize()) <= 0)
			return (NULL);
		if ((pids = (pid_t *)malloc((u_int)(fds * sizeof(int)))) == NULL)
			return (NULL);
		bzero((char *)pids, fds * sizeof(pid_t));
	}
	if (pipe(pdes) < 0)
		return (NULL);
	switch (pid = vfork()) {
	case -1:			/* error */
		(void) close(pdes[0]);
		(void) close(pdes[1]);
		return (NULL);
		/* NOTREACHED */
	case 0:				/* child */
		if (*type == 'r') {
			if (pdes[1] != STDOUT_FILENO) {
				(void) dup2(pdes[1], STDOUT_FILENO);
				(void) close(pdes[1]);
			}
			(void) close(pdes[0]);
		} else {
			if (pdes[0] != STDIN_FILENO) {
				(void) dup2(pdes[0], STDIN_FILENO);
				(void) close(pdes[0]);
			}
			(void) close(pdes[1]);
		}
		execl(_PATH_BSHELL, "sh", "-c", program, NULL);
		_exit(127);
		/* NOTREACHED */
	}
	/* parent; assume fdopen can't fail...  */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void) close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void) close(pdes[0]);
	}
	pids[fileno(iop)] = pid;
	return (iop);
}

pclose(iop)
	FILE *iop;
{
	extern int errno;
	register int fdes;
	int omask;
	union wait pstat;
	pid_t pid, waitpid();

	/*
	 * pclose returns -1 if stream is not associated with a
	 * `popened' command, if already `pclosed', or waitpid
	 * returns an error.
	 */
	if (pids == NULL || pids[fdes = fileno(iop)] == 0)
		return (-1);
	(void) fclose(iop);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	do {
		pid = waitpid(pids[fdes], &pstat, 0);
	} while (pid == -1 && errno == EINTR);
	(void) sigsetmask(omask);
	pids[fdes] = 0;
	return (pid == -1 ? -1 : pstat.w_status);
}
