/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsiod.c	5.4 (Berkeley) 6/29/90";
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
		daemon(0, 0);
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
