/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)system.c	5.9 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stddef.h>
#include <paths.h>

system(command)
	char *command;
{
	union wait pstat;
	pid_t pid, waitpid();
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
	pid = waitpid(pid, &pstat, 0);
	(void)sigsetmask(omask);
	(void)signal(SIGINT, intsave);
	(void)signal(SIGQUIT, quitsave);
	return(pid == -1 ? -1 : pstat.w_status);
}
