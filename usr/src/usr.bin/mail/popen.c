/*
 * Copyright (c) 1980 Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)popen.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include "rcv.h"
#include <stdio.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	popen_pid[20];

FILE *
Popen(cmd,mode)
char	*cmd;
char	*mode;
{
	int p[2];
	register int myside, hisside;
	int pid;
	char *argv[MAXARGC];

	if (pipe(p) < 0)
		return NULL;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	if ((pid = vfork()) == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
		dup2(hisside, tst(0, 1));
		close(hisside);
		(void) getrawlist(cmd, argv, MAXARGC);
		execvp(argv[0], argv);
		fprintf(stderr, "Cannot execute %s\n", argv[0]);
		_exit(1);
	}
	if (pid == -1)
		return NULL;
	popen_pid[myside] = pid;
	close(hisside);
	return fdopen(myside, mode);
}

Pclose(ptr)
FILE *ptr;
{
	register f, r;
	int omask;
	union wait status;
	extern int errno;

	f = fileno(ptr);
	fclose(ptr);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	while((r = wait(&status)) != popen_pid[f] && r != -1 && errno != EINTR)
		;
	if(r == -1)
		status.w_status = -1;
	sigsetmask(omask);
	return (status.w_status);
}
