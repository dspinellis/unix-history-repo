/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)finger.c	5.25 (Berkeley) %G%";
#endif /* not lint */

/*
 * Finger prints out information about users.  It is not portable since
 * certain fields (e.g. the full user name, office, and phone numbers) are
 * extracted from the gecos field of the passwd file which other UNIXes
 * may not have or may use for other things.
 *
 * There are currently two output formats; the short format is one line
 * per user and displays login name, tty, login time, real name, idle time,
 * and office location/phone number.  The long format gives the same
 * information (in a more legible format) as well as home directory, shell,
 * mail info, and .plan/.project files.
 */

#include <sys/param.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <utmp.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>
#include "finger.h"

DB *db;
time_t now;
int entries, lflag, mflag, pplan, sflag;
char tbuf[1024];

static void loginlist __P((void));
static void userlist __P((int, char **));

main(argc, argv)
	int argc;
	char **argv;
{
	int ch;

	while ((ch = getopt(argc, argv, "lmps")) != EOF)
		switch(ch) {
		case 'l':
			lflag = 1;		/* long format */
			break;
		case 'm':
			mflag = 1;		/* force exact match of names */
			break;
		case 'p':
			pplan = 1;		/* don't show .plan/.project */
			break;
		case 's':
			sflag = 1;		/* short format */
			break;
		case '?':
		default:
			(void)fprintf(stderr,
			    "usage: finger [-lmps] [login ...]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	(void)time(&now);
	setpassent(1);
	if (!*argv) {
		/*
		 * Assign explicit "small" format if no names given and -l
		 * not selected.  Force the -s BEFORE we get names so proper
		 * screening will be done.
		 */
		if (!lflag)
			sflag = 1;	/* if -l not explicit, force -s */
		loginlist();
		if (entries == 0)
			(void)printf("No one logged on.\n");
	} else {
		userlist(argc, argv);
		/*
		 * Assign explicit "large" format if names given and -s not
		 * explicitly stated.  Force the -l AFTER we get names so any
		 * remote finger attempts specified won't be mishandled.
		 */
		if (!sflag)
			lflag = 1;	/* if -s not explicit, force -l */
	}
	if (entries)
		if (lflag)
			lflag_print();
		else
			sflag_print();
	exit(0);
}

static void
loginlist()
{
	register PERSON *pn;
	DBT data, key;
	struct passwd *pw;
	struct utmp user;
	int r, sflag;
	char name[UT_NAMESIZE + 1];

	if (!freopen(_PATH_UTMP, "r", stdin))
		err("%s: %s", _PATH_UTMP, strerror(errno));
	name[UT_NAMESIZE] = NULL;
	while (fread((char *)&user, sizeof(user), 1, stdin) == 1) {
		if (!user.ut_name[0])
			continue;
		if ((pn = find_person(user.ut_name)) == NULL) {
			bcopy(user.ut_name, name, UT_NAMESIZE);
			if ((pw = getpwnam(name)) == NULL)
				continue;
			pn = enter_person(pw);
		}
		enter_where(&user, pn);
	}
	if (db && lflag)
		for (sflag = R_FIRST;; sflag = R_NEXT) {
			r = (*db->seq)(db, &key, &data, sflag);
			if (r == -1)
				err("db seq: %s", strerror(errno));
			if (r == 1)
				break;
			enter_lastlog(*(PERSON **)data.data);
		}
}

static void
userlist(argc, argv)
	register int argc;
	register char **argv;
{
	register PERSON *pn;
	DBT data, key;
	struct utmp user;
	struct passwd *pw;
	int r, sflag, *used, *ip;
	char **ap, **nargv, **np, **p;

	if ((nargv = malloc(argc * sizeof(char *))) == NULL ||
	    (used = calloc(argc, sizeof(int))) == NULL)
		err("%s", strerror(errno));

	/* Pull out all network requests. */
	for (ap = p = argv, np = nargv; *p; ++p)
		if (index(*p, '@'))
			*np++ = *p;
		else
			*ap++ = *p;

	*np++ = NULL;
	*ap++ = NULL;

	if (!*argv)
		goto net;

	/*
	 * Traverse the list of possible login names and check the login name
	 * and real name against the name specified by the user.
	 */
	if (mflag)
		for (p = argv; *p; ++p)
			if (pw = getpwnam(*p))
				enter_person(pw);
			else
				(void)fprintf(stderr,
				    "finger: %s: no such user\n", *p);
	else {
		while (pw = getpwent())
			for (p = argv, ip = used; *p; ++p, ++ip)
				if (match(pw, *p)) {
					enter_person(pw);
					*ip = 1;
				}
		for (p = argv, ip = used; *p; ++p, ++ip)
			if (!*ip)
				(void)fprintf(stderr,
				    "finger: %s: no such user\n", *p);
	}

	/* Handle network requests. */
net:	for (p = nargv; *p;)
		netfinger(*p++);

	if (entries == 0)
		return;

	/*
	 * Scan thru the list of users currently logged in, saving
	 * appropriate data whenever a match occurs.
	 */
	if (!freopen(_PATH_UTMP, "r", stdin))
		err("%s: %s", _PATH_UTMP, strerror(errno));
	while (fread((char *)&user, sizeof(user), 1, stdin) == 1) {
		if (!user.ut_name[0])
			continue;
		if ((pn = find_person(user.ut_name)) == NULL)
			continue;
		enter_where(&user, pn);
	}
	if (db)
		for (sflag = R_FIRST;; sflag = R_NEXT) {
			r = (*db->seq)(db, &key, &data, sflag);
			if (r == -1)
				err("db seq: %s", strerror(errno));
			if (r == 1)
				break;
			enter_lastlog(*(PERSON **)data.data);
		}
}
