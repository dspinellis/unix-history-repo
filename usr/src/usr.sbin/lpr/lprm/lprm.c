/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lprm.c	5.9 (Berkeley) %G%";
#endif /* not lint */

/*
 * lprm - remove the current user's spool entry
 *
 * lprm [-] [[job #] [user] ...]
 *
 * Using information in the lock file, lprm will kill the
 * currently active daemon (if necessary), remove the associated files,
 * and startup a new daemon.  Priviledged users may remove anyone's spool
 * entries, otherwise one can only remove their own.
 */

#include <sys/param.h>

#include <syslog.h>
#include <dirent.h>
#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "lp.h"
#include "lp.local.h"

/*
 * Stuff for handling job specifications
 */
char	*person;		/* name of person doing lprm */
int	 requ[MAXREQUESTS];	/* job number of spool entries */
int	 requests;		/* # of spool requests */
char	*user[MAXUSERS];	/* users to process */
int	 users;			/* # of users in user array */

static char	luser[16];	/* buffer for person */

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	struct passwd *p;

	name = argv[0];
	gethostname(host, sizeof(host));
	openlog("lpd", 0, LOG_LPR);
	if ((p = getpwuid(getuid())) == NULL)
		fatal("Who are you?");
	if (strlen(p->pw_name) >= sizeof(luser))
		fatal("Your name is too long");
	strcpy(luser, p->pw_name);
	person = luser;
	while (--argc) {
		if ((arg = *++argv)[0] == '-')
			switch (arg[1]) {
			case 'P':
				if (arg[2])
					printer = &arg[2];
				else if (argc > 1) {
					argc--;
					printer = *++argv;
				}
				break;
			case '\0':
				if (!users) {
					users = -1;
					break;
				}
			default:
				usage();
			}
		else {
			if (users < 0)
				usage();
			if (isdigit(arg[0])) {
				if (requests >= MAXREQUESTS)
					fatal("Too many requests");
				requ[requests++] = atoi(arg);
			} else {
				if (users >= MAXUSERS)
					fatal("Too many users");
				user[users++] = arg;
			}
		}
	}
	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;

	rmjob();
	exit(0);
}

void
usage()
{
	fprintf(stderr, "usage: lprm [-] [-Pprinter] [[job #] [user] ...]\n");
	exit(2);
}
