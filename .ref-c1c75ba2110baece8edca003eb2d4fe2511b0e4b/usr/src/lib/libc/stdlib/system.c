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
static char sccsid[] = "@(#)system.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/wait.h>
#include <sys/signal.h>
#include <stdio.h>

system(command)
	char *command;
{
	int pid, wval, (*i)(), (*q)();
	union wait stat_loc;

	switch(pid = vfork()) {
	case -1:			/* error */
		stat_loc.w_status = 0;
		stat_loc.w_retcode = 127;
		return(stat_loc.w_status);
	case 0:				/* child */
		execl("/bin/sh", "sh", "-c", command, (char *)NULL);
		_exit(127);
	}
	i = signal(SIGINT, SIG_IGN);
	q = signal(SIGQUIT, SIG_IGN);
	while ((wval = wait(&stat_loc)) != pid && wval != -1);
	(void)signal(SIGINT, i);
	(void)signal(SIGQUIT, q);
	return(wval == -1 ? -1 : stat_loc.w_status);
}
