/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)system.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <paths.h>

system(command)
	const char *command;
{
	union wait pstat;
	pid_t pid;
	int omask;
	sig_t intsave, quitsave;

	if (!command)		/* just checking... */
		return(1);

	omask = sigblock(sigmask(SIGCHLD));
	switch(pid = vfork()) {
	case -1:			/* error */
		(void)sigsetmask(omask);
		pstat.w_status = 0;
		pstat.w_retcode = 127;
		return(pstat.w_status);
	case 0:				/* child */
		(void)sigsetmask(omask);
		execl(_PATH_BSHELL, "sh", "-c", command, (char *)NULL);
		_exit(127);
	}
	intsave = signal(SIGINT, SIG_IGN);
	quitsave = signal(SIGQUIT, SIG_IGN);
	pid = waitpid(pid, (int *)&pstat, 0);
	(void)sigsetmask(omask);
	(void)signal(SIGINT, intsave);
	(void)signal(SIGQUIT, quitsave);
	return(pid == -1 ? -1 : pstat.w_status);
}
