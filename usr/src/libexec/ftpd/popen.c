/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software written by Ken Arnold and
 * published in UNIX Review, Vol. 6, No. 8.
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
 *
 * static char sccsid[] = "@(#)popen.c	5.7 (Berkeley) 9/1/88";
 */

#ifndef lint
static char sccsid[] = "@(#)popen.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <stdio.h>

static uid_t *pids;
static int fds;

FILE *
popen(program, type)
	char *program, *type;
{
	FILE *iop;
	int pdes[2], pid;
	char *malloc();

	if (*type != 'r' && *type != 'w' || type[1])
		return(NULL);

	if (!pids) {
		if ((fds = getdtablesize()) <= 0)
			return(NULL);
		if (!(pids =
		    (uid_t *)malloc((u_int)(fds * sizeof(uid_t)))))
			return(NULL);
		bzero(pids, fds * sizeof(uid_t));
	}
	if (pipe(pdes) < 0)
		return(NULL);
	switch(pid = vfork()) {
	case -1:			/* error */
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		return(NULL);
		/* NOTREACHED */
	case 0:				/* child */
		if (*type == 'r') {
			if (pdes[1] != 1) {
				dup2(pdes[1], 1);
				(void)close(pdes[1]);
			}
			(void)close(pdes[0]);
		} else {
			if (pdes[0] != 0) {
				dup2(pdes[0], 0);
				(void)close(pdes[0]);
			}
			(void)close(pdes[1]);
		}
		execl("/bin/sh", "sh", "-c", program, NULL);
		_exit(127);
		/* NOTREACHED */
	}
	/* parent; assume fdopen can't fail...  */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void)close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void)close(pdes[0]);
	}
	pids[fileno(iop)] = pid;
	return(iop);
}

pclose(iop)
	FILE *iop;
{
	register int fdes;
	long omask;
	int stat_loc;
	pid_t waitpid();

	/*
	 * pclose returns -1 if stream is not associated with a
	 * `popened' command, or, if already `pclosed'.
	 */
	if (pids[fdes = fileno(iop)] == 0)
		return(-1);
	(void)fclose(iop);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	stat_loc = waitpid(pids[fdes], &stat_loc, 0) == -1 ?
	    -1 : WEXITSTATUS(stat_loc);
	(void)sigsetmask(omask);
	pids[fdes] = 0;
	return(stat_loc);
}
