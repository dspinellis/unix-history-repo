/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ktrace.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <sys/uio.h>
#include <sys/ktrace.h>
#include <stdio.h>
#include "ktrace.h"

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	enum { NOTSET, CLEAR, CLEARALL } clear;
	int append, ch, fd, inherit, ops, pid, pidset, trpoints;
	char *tracefile;

	clear = NOTSET;
	append = ops = pidset = inherit = 0;
	trpoints = DEF_POINTS;
	tracefile = DEF_TRACEFILE;
	while ((ch = getopt(argc,argv,"aCcdf:g:ip:t:")) != EOF)
		switch((char)ch) {
		case 'a':
			append = 1;
			break;
		case 'C':
			clear = CLEARALL;
			pidset = 1;
			break;
		case 'c':
			clear = CLEAR;
			break;
		case 'd':
			ops |= KTRFLAG_DESCEND;
			break;
		case 'f':
			tracefile = optarg;
			break;
		case 'g':
			pid = -rpid(optarg);
			pidset = 1;
			break;
		case 'i':
			inherit = 1;
			break;
		case 'p':
			pid = rpid(optarg);
			pidset = 1;
			break;
		case 't':
			trpoints = getpoints(optarg);
			if (trpoints < 0) {
				(void)fprintf(stderr, 
				    "ktrace: unknown facility in %s\n", optarg);
				usage();
			}
			break;
		default:
			usage();
		}
	argv += optind;
	argc -= optind;
	
	if (pidset && *argv || !pidset && !*argv)
		usage();
			
	if (inherit)
		trpoints |= KTRFAC_INHERIT;

	if (clear != NOTSET) {
		if (clear == CLEARALL) {
			ops = KTROP_CLEAR | KTRFLAG_DESCEND;
			trpoints = ALL_POINTS;
			pid = 1;
		} else
			ops |= pid ? KTROP_CLEAR : KTROP_CLEARFILE;

		if (ktrace(tracefile, ops, trpoints, pid) < 0)
			error(tracefile);
		exit(0);
	}

	if ((fd = open(tracefile, O_CREAT | O_WRONLY | (append ? 0 : O_TRUNC),
	    DEFFILEMODE)) < 0)
		error(tracefile);
	(void)close(fd);

	if (*argv) { 
		if (ktrace(tracefile, ops, trpoints, getpid()) < 0)
			error();
		execvp(argv[0], &argv[0]);
		error(argv[0]);
		exit(1);
	}
	else if (ktrace(tracefile, ops, trpoints, pid) < 0)
		error(tracefile);
	exit(0);
}

rpid(p)
	char *p;
{
	static int first;

	if (first++) {
		(void)fprintf(stderr,
		    "ktrace: only one -g or -p flag is permitted.\n");
		usage();
	}
	if (!*p) {
		(void)fprintf(stderr, "ktrace: illegal process id.\n");
		usage();
	}
	return(atoi(p));
}

error(name)
	char *name;
{
	(void)fprintf(stderr, "ktrace: %s: %s.\n", name, strerror(errno));
	exit(1);
}

usage()
{
	(void)fprintf(stderr,
"usage:\tktrace [-aCcid] [-f trfile] [-g pgid] [-p pid] [-t [acgn]\n\tktrace [-aCcid] [-f trfile] [-t [acgn] command\n");
	exit(1);
}
