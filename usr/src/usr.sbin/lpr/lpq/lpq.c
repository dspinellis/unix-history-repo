/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
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
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lpq.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Spool Queue examination program
 *
 * lpq [-l] [-Pprinter] [user...] [job...]
 *
 * -l long output
 * -P used to identify printer as per lpr/lprm
 */

#include "lp.h"

char	*user[MAXUSERS];	/* users to process */
int	users;			/* # of users in user array */
int	requ[MAXREQUESTS];	/* job number of spool entries */
int	requests;		/* # of spool requests */

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

static
usage()
{
	puts("usage: lpq [-l] [-Pprinter] [user ...] [job ...]");
	exit(1);
}
