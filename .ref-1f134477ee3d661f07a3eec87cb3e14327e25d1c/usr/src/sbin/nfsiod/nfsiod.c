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
static char sccsid[] = "@(#)nfsiod.c	8.3 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/syslog.h>
#include <sys/ucred.h>
#include <sys/wait.h>

#include <nfs/nfsv2.h>
#include <nfs/nfs.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Global defs */
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif

void nonfs __P((int));
void reapchild __P((int));
void usage __P((void));

/*
 * Nfsiod does asynchronous buffered I/O on behalf of the NFS client.
 * It does not have to be running for correct operation, but will
 * improve throughput.
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, num_servers;

#define	MAXNFSDCNT      20
#define	DEFNFSDCNT       1
	num_servers = DEFNFSDCNT;
	while ((ch = getopt(argc, argv, "n:")) != EOF)
		switch (ch) {
		case 'n':
			num_servers = atoi(optarg);
			if (num_servers < 1 || num_servers > MAXNFSDCNT) {
				warnx("nfsiod count %d; reset to %d",
				    DEFNFSDCNT);
				num_servers = DEFNFSDCNT;
			}
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/*
	 * XXX
	 * Backward compatibility, trailing number is the count of daemons.
	 */
	if (argc > 1)
		usage();
	if (argc == 1) {
		num_servers = atoi(argv[0]);
		if (num_servers < 1 || num_servers > MAXNFSDCNT) {
			warnx("nfsiod count %d; reset to %d", DEFNFSDCNT);
			num_servers = DEFNFSDCNT;
		}
	}

	if (debug == 0) {
		daemon(0, 0);
		(void)signal(SIGHUP, SIG_IGN);
		(void)signal(SIGINT, SIG_IGN);
		(void)signal(SIGQUIT, SIG_IGN);
		(void)signal(SIGSYS, nonfs);
	}
	(void)signal(SIGCHLD, reapchild);

	openlog("nfsiod:", LOG_PID, LOG_DAEMON);

	while (num_servers--)
		switch (fork()) {
		case -1:
			syslog(LOG_ERR, "fork: %m");
			exit (1);
		case 0:
			if (nfssvc(NFSSVC_BIOD, NULL) < 0) {
				syslog(LOG_ERR, "nfssvc: %m");
				exit (1);
			}
			exit(0);
		}
	exit (0);
}

void
nonfs(signo)
	int signo;
{
	syslog(LOG_ERR, "missing system call: NFS not available.");
}

void
reapchild(signo)
	int signo;
{

	while (wait3(NULL, WNOHANG, NULL));
}

void
usage()
{
	(void)fprintf(stderr, "usage: nfsiod [-n num_servers]\n");
	exit(1);
}
