/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsiod.c	5.2 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>

/* Global defs */
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif

/*
 * Nfsiod does asynchronous buffered I/O on behalf of the NFS client.
 * It does not have to be running for correct operation, but will improve
 * throughput. The one optional argument is the number of children to fork.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	int cnt;

	if (debug == 0) {
		if (fork())
			exit(0);
		{ int s;
		for (s = 0; s < 10; s++)
			(void) close(s);
		}
		(void) open("/", O_RDONLY);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		{ int tt = open("/dev/tty", O_RDWR);
		  if (tt > 0) {
			ioctl(tt, TIOCNOTTY, (char *)0);
			close(tt);
		  }
		}
		(void) setpgrp(0, 0);
		signal(SIGTSTP, SIG_IGN);
		signal(SIGTTIN, SIG_IGN);
		signal(SIGTTOU, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTERM, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	if (argc != 2 || (cnt = atoi(argv[1])) <= 0 || cnt > 20)
		cnt = 1;
	for (i = 1; i < cnt; i++)
		if (fork() == 0)
			break;
	async_daemon();		/* Never returns */
	exit(1);
}
