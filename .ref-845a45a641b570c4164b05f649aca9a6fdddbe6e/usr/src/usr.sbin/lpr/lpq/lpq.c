/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lpq.c	5.11 (Berkeley) %G%";
#endif /* not lint */

/*
 * Spool Queue examination program
 *
 * lpq [-l] [-Pprinter] [user...] [job...]
 *
 * -l long output
 * -P used to identify printer as per lpr/lprm
 */

#include <sys/param.h>

#include <syslog.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "lp.h"
#include "lp.local.h"

int	 requ[MAXREQUESTS];	/* job number of spool entries */
int	 requests;		/* # of spool requests */
char	*user[MAXUSERS];	/* users to process */
int	 users;			/* # of users in user array */

void usage __P((void));

int
main(argc, argv)
	register int	argc;
	register char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	int	ch, lflag;		/* long output option */

	name = *argv;
	if (gethostname(host, sizeof(host))) {
		perror("lpq: gethostname");
		exit(1);
	}
	openlog("lpd", 0, LOG_LPR);

	lflag = 0;
	while ((ch = getopt(argc, argv, "lP:")) != EOF)
		switch((char)ch) {
		case 'l':			/* long output */
			++lflag;
			break;
		case 'P':		/* printer name */
			printer = optarg;
			break;
		case '?':
		default:
			usage();
		}

	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;

	for (argc -= optind, argv += optind; argc; --argc, ++argv)
		if (isdigit(argv[0][0])) {
			if (requests >= MAXREQUESTS)
				fatal("too many requests");
			requ[requests++] = atoi(*argv);
		}
		else {
			if (users >= MAXUSERS)
				fatal("too many users");
			user[users++] = *argv;
		}

	displayq(lflag);
	exit(0);
}

void
usage()
{
	puts("usage: lpq [-l] [-Pprinter] [user ...] [job ...]");
	exit(1);
}
