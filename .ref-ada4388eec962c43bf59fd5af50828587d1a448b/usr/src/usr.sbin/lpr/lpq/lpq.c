/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1983, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lpq.c	8.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Spool Queue examination program
 *
 * lpq [-a] [-l] [-Pprinter] [user...] [job...]
 *
 * -a show all non-null queues on the local machine
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
#include "pathnames.h"

int	 requ[MAXREQUESTS];	/* job number of spool entries */
int	 requests;		/* # of spool requests */
char	*user[MAXUSERS];	/* users to process */
int	 users;			/* # of users in user array */

static int ckqueue __P((char *));
void usage __P((void));

int
main(argc, argv)
	register int	argc;
	register char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	int	ch, aflag, lflag;
	char	*buf, *cp;

	name = *argv;
	if (gethostname(host, sizeof(host))) {
		perror("lpq: gethostname");
		exit(1);
	}
	openlog("lpd", 0, LOG_LPR);

	aflag = lflag = 0;
	while ((ch = getopt(argc, argv, "alP:")) != EOF)
		switch((char)ch) {
		case 'a':
			++aflag;
			break;
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

	if (!aflag && printer == NULL && (printer = getenv("PRINTER")) == NULL)
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

	if (aflag) {
		while (cgetnext(&buf, printcapdb) > 0) {
			if (ckqueue(buf) <= 0) {
				free(buf);
				continue;	/* no jobs */
			}
			for (cp = buf; *cp; cp++)
				if (*cp == '|' || *cp == ':') {
					*cp = '\0';
					break;
				}
			printer = buf;
			printf("%s:\n", printer);
			displayq(lflag);
			free(buf);
			printf("\n");
		}
	} else
		displayq(lflag);
	exit(0);
}

static int
ckqueue(cap)
	char *cap;
{
	register struct dirent *d;
	DIR *dirp;
	char *spooldir;

	if (cgetstr(cap, "sd", &spooldir) == -1)
		spooldir = _PATH_DEFSPOOL;
	if ((dirp = opendir(spooldir)) == NULL)
		return (-1);
	while ((d = readdir(dirp)) != NULL) {
		if (d->d_name[0] != 'c' || d->d_name[1] != 'f')
			continue;	/* daemon control files only */
		closedir(dirp);
		return (1);		/* found something */
	}
	closedir(dirp);
	return (0);
}

void
usage()
{
	puts("usage: lpq [-a] [-l] [-Pprinter] [user ...] [job ...]");
	exit(1);
}
