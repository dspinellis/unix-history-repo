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
static char sccsid[] = "@(#)system.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <stdio.h>

system(command)
	char *command;
{
	union wait pstat;
	pid_t pid, waitpid();
	int omask;

	switch(pid = vfork()) {
	case -1:			/* error */
		pstat.w_status = 0;
		pstat.w_retcode = 127;
		return(pstat.w_status);
	case 0:				/* child */
		execl("/bin/sh", "sh", "-c", command, (char *)NULL);
		_exit(127);
	}
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	pid = waitpid(pid, &pstat, 0);
	(void)sigsetmask(omask);
	return(pid == -1 ? -1 : pstat.w_status);
}
