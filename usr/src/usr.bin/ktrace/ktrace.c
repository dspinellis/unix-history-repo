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
static char sccsid[] = "@(#)ktrace.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "ktrace.h"

#define USAGE \
 "usage: ktrace [-acid] [-f trfile] [-t trpoints] [-p pid] [-g pgid]\n\
	trops: c = syscalls, n = namei, g = generic-i/o, a = everything\n\
	ktrace -C (clear everthing)\n"
	

char	*tracefile = DEF_TRACEFILE;
int	append, clear, descend, inherit;

main(argc, argv)
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	int trpoints = ALL_POINTS;
	int ops = 0;
	int pid = 0;
	int ch;

	while ((ch = getopt(argc,argv,"Cacdp:g:if:t:")) != EOF)
		switch((char)ch) {
		case 'C':
			clear = 2;
			break;
		case 'c':
			clear = 1;
			break;
		case 'd':
			ops |= KTRFLAG_DESCEND;
			break;
		case 't':
			trpoints = getpoints(optarg);
			if (trpoints < 0) {
				fprintf(stderr, 
				    "ktrace: unknown facility in %s\n",
			 	     optarg);
				exit(1);
			}
			break;
		case 'p':
			pid = atoi(optarg);
			break;
		case 'g':
			pid = -atoi(optarg);
			break;
		case 'i':
			inherit++;
			break;
		case 'f':
			tracefile = optarg;
			break;
		case 'a':
			append++;
			break;
		default:
			fprintf(stderr,"usage: \n",*argv);
			exit(-1);
		}
	argv += optind, argc -= optind;
	
	if (inherit)
		trpoints |= KTRFAC_INHERIT;
	if (clear) {			/* untrace something */
		if (clear == 2) {	/* -C */
			ops = KTROP_CLEAR | KTRFLAG_DESCEND;
			pid = 1;
		} else {
			ops |= pid ? KTROP_CLEAR : KTROP_CLEARFILE;
		}
		if (ktrace(tracefile, ops, trpoints, pid) < 0) {
			perror("ktrace");
			exit(1);
		}
		exit(0);
	}

	if (pid == 0 && !*argv) {	/* nothing to trace */
		fprintf(stderr, USAGE);
		exit(1);
	}
			
	close(open(tracefile, O_WRONLY | O_CREAT, 0666));
	if (!append)
		close(open(tracefile, O_WRONLY | O_TRUNC));
	if (!*argv) {
		if (ktrace(tracefile, ops, trpoints, pid) < 0) {
			perror("ktrace");
			exit(1);
		}
	} else {
		pid = getpid();
		if (ktrace(tracefile, ops, trpoints, pid) < 0) {
			perror("ktrace");
			exit(1);
		}
		execvp(argv[0], &argv[0]);
		perror("ktrace: exec failed");
	}
	exit(0);
}
