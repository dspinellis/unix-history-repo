/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software written by Ken Arnold and
 * published in UNIX Review, Vol. 6, No. 8.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)popen.c	8.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/wait.h>
#include <sys/socket.h>

#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>

static struct pid {
	struct pid *next;
	FILE *fp;
	pid_t pid;
} *pidlist; 
	
FILE *
popen(command, type)
	const char *command, *type;
{
	struct pid *cur;
	FILE *iop;
	int pdes[2], pid, twoway;

	if (strchr(type, '+')) {
		twoway = 1;
		type = "r+";
		if (socketpair(AF_UNIX, SOCK_STREAM, 0, pdes) < 0)
			return (NULL);
	} else  {
		twoway = 0;
		if (*type != 'r' && *type != 'w' || type[1] ||
		    (pipe(pdes) < 0))
			return (NULL);
	}

	if ((cur = malloc(sizeof(struct pid))) == NULL)
		return (NULL);

	switch (pid = vfork()) {
	case -1:			/* Error. */
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		(void)free(cur);
		return (NULL);
		/* NOTREACHED */
	case 0:				/* Child. */
		if (*type == 'r') {
			if (pdes[1] != STDOUT_FILENO) {
				(void)dup2(pdes[1], STDOUT_FILENO);
				(void)close(pdes[1]);
				pdes[1] = STDOUT_FILENO;
			}
			(void) close(pdes[0]);
			if (twoway && (pdes[1] != STDIN_FILENO))
				(void)dup2(pdes[1], STDIN_FILENO);
		} else {
			if (pdes[0] != STDIN_FILENO) {
				(void)dup2(pdes[0], STDIN_FILENO);
				(void)close(pdes[0]);
			}
			(void)close(pdes[1]);
		}
		execl(_PATH_BSHELL, "sh", "-c", command, NULL);
		_exit(127);
		/* NOTREACHED */
	}

	/* Parent; assume fdopen can't fail. */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void)close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void)close(pdes[0]);
	}

	/* Link into list of file descriptors. */
	cur->fp = iop;
	cur->pid =  pid;
	cur->next = pidlist;
	pidlist = cur;

	return (iop);
}

/*
 * pclose --
 *	Pclose returns -1 if stream is not associated with a `popened' command,
 *	if already `pclosed', or waitpid returns an error.
 */
int
pclose(iop)
	FILE *iop;
{
	register struct pid *cur, *last;
	int omask;
	int pstat;
	pid_t pid;

	/* Find the appropriate file pointer. */
	for (last = NULL, cur = pidlist; cur; last = cur, cur = cur->next)
		if (cur->fp == iop)
			break;
	if (cur == NULL)
		return (-1);

	(void)fclose(iop);

	do {
		pid = waitpid(cur->pid, &pstat, 0);
	} while (pid == -1 && errno == EINTR);

	/* Remove the entry from the linked list. */
	if (last == NULL)
		pidlist = cur->next;
	else
		last->next = cur->next;
	free(cur);
		
	return (pid == -1 ? -1 : pstat);
}
