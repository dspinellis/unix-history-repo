/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsiod.c	8.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/syslog.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/ucred.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>

/* Global defs */
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif
extern int errno;
void reapchild();

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

	if (argc != 2 || (cnt = atoi(argv[1])) <= 0 || cnt > 20)
		cnt = 1;
	if (debug == 0) {
		daemon(0, 0);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	signal(SIGCHLD, reapchild);
	openlog("nfsiod:", LOG_PID, LOG_DAEMON);
	for (i = 1; i < cnt; i++)
		if (fork() == 0)
			break;
	if (nfssvc(NFSSVC_BIOD, (char *)0) < 0)
		syslog(LOG_ERR, "nfssvc failed %m");
}

void
reapchild()
{

	while (wait3((int *) NULL, WNOHANG, (struct rusage *) NULL))
		;
}
