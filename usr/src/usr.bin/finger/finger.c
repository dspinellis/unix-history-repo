/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tony Nardo of the Johns Hopkins University/Applied Physics Lab.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)finger.c	5.13 (Berkeley) %G%";
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
#include <sys/file.h>
#include <stdio.h>
#include "finger.h"
#include "pathnames.h"

struct utmp user;
PERSON *head;
time_t now;
int entries, lflag, sflag, mflag, pplan;
char tbuf[1024];

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	int ch;
	time_t time();

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
		if (!head)
			(void)printf("No one logged on.\n");
	} else {
		userlist(argv);
		/*
		 * Assign explicit "large" format if names given and -s not
		 * explicitly stated.  Force the -l AFTER we get names so any
		 * remote finger attempts specified won't be mishandled.
		 */
		if (!sflag)
			lflag = 1;	/* if -s not explicit, force -l */
	}
	if (head) {
		if (lflag)
			lflag_print();
		else
			sflag_print();
	}
	exit(0);
}

loginlist()
{
	register PERSON *pn;
	register int fd;
	struct passwd *pw;
	char name[UT_NAMESIZE + 1], *strdup(), *malloc();

	if ((fd = open(_PATH_UTMP, O_RDONLY, 0)) < 0) {
		(void)fprintf(stderr, "finger: can't read %s.\n", _PATH_UTMP);
		exit(2);
	}
	name[UT_NAMESIZE] = NULL;
	while(read(fd, (char *)&user, sizeof(user)) == sizeof(user)) {
		if (!user.ut_name[0])
			continue;
		bcopy(user.ut_name, name, UT_NAMESIZE);
		if (!(pw = getpwnam(name)))
			continue;
		if (!(pn = (PERSON *)malloc((u_int)sizeof(PERSON)))) {
			(void)fprintf(stderr, "finger: out of space.\n");
			exit(1);
		}
		++entries;
		pn->next = head;
		head = pn;
		utcopy(&user, pn);
		userinfo(pn, pw);
		find_idle_and_ttywrite(pn);
		pn->info = LOGGEDIN;
	}
	(void)close(fd);
}

#define	ARGIGNORE	(char *)0x01
userlist(argv)
	char **argv;
{
	register PERSON *nethead, *p, *pn;
	register struct passwd *pw;
	register char **a1, **a2;
	int fd, dolocal, nelem;
	char **sargv, **arglist, *malloc(), *rindex(), *strcpy();

	/* suppress duplicates while it's still easy */
	for (a1 = argv; *a1; ++a1)
		for (a2 = a1 + 1; *a2; ++a2)
			if (!strcasecmp(*a1, *a2)) {
				*a1 = ARGIGNORE;
				break;
			}

	/* pull out all network requests */
	for (sargv = argv, dolocal = 0, nethead = NULL; *argv; ++argv) {
		if (!index(*argv, '@')) {
			dolocal = 1;
			continue;
		}
		if (!(pn = (PERSON *)malloc((u_int)sizeof(PERSON)))) {
			(void)fprintf(stderr, "finger: out of space.\n");
			exit(1);
		}
		pn->next = nethead;
		nethead = pn;
		pn->name = *argv;
		*argv = ARGIGNORE;
	}

	if (!dolocal)
		goto net;

	/*
	 * traverse the list of possible login names and check the login name
	 * and real name against the name specified by the user.  Possible
	 * speedup would be to use getpwnam(3) if mflag set -- maybe not
	 * worthwhile, given that the default is the mflag off.
	 */
	nelem = argv - sargv + 1;
	if (!(arglist =
	    (char **)malloc((u_int)(nelem * sizeof(char *))))) {
		(void)fprintf(stderr, "finger: out of space.\n");
		exit(1);
	}
	bcopy((char *)sargv, (char *)arglist, nelem * sizeof(char *));
	while (pw = getpwent()) {
		for (argv = sargv; *argv; ++argv) {
			if (*argv == ARGIGNORE)
				continue;
			if (strcasecmp(pw->pw_name, *argv) &&
			    (mflag || !match(pw, *argv)))
				continue;
			if (!(pn = (PERSON *)malloc((u_int)sizeof(PERSON)))) {
				(void)fprintf(stderr,
				    "finger: out of space.\n");
				exit(1);
			}
			++entries;
			pn->next = head;
			head = pn;
			userinfo(pn, pw);
			pn->info = FOUND;
			arglist[argv - sargv] = ARGIGNORE;
			/* don't break, may be listed multiple times */
		}
	}

	/* list errors */
	for (; *arglist; ++arglist)
		if (*arglist != ARGIGNORE)
			(void)fprintf(stderr,
			    "finger: %s: no such user.\n", *arglist);

	/* handle network requests */
net:	for (pn = nethead; pn; pn = pn->next) {
		netfinger(pn->name);
		if (pn->next || entries)
			putchar('\n');
	}

	if (!head)
		return;

	/*
	 * Scan thru the list of users currently logged in, saving
	 * appropriate data whenever a match occurs.
	 */
	if ((fd = open(_PATH_UTMP, O_RDONLY, 0)) < 0) {
		(void)fprintf( stderr, "finger: can't read %s.\n", _PATH_UTMP);
		exit(1);
	}
	while (read(fd, (char *)&user, sizeof(user)) == sizeof(user)) {
		if (!user.ut_name[0])
			continue;
		for (pn = head; pn; pn = pn->next) {
			if (strncasecmp(pn->name, user.ut_name, UT_NAMESIZE))
				continue;
			if (pn->info == LOGGEDIN) {
				if (!(p =
				    (PERSON *)malloc((u_int)sizeof(PERSON)))) {
					(void)fprintf(stderr,
					    "finger: out of space.\n");
					exit(1);
				}
				++entries;
				p->name = pn->name;
				(void)strcpy(p->tty, pn->tty);
				/* link in so finds `real' entry first! */
				p->next = pn->next;
				pn->next = p;
				pn = p;
			}
			pn->info = LOGGEDIN;
			utcopy(&user, pn);
			find_idle_and_ttywrite(pn);
			/* don't break, may be listed multiple times... */
		}
	}
	(void)close(fd);
	for (pn = head; pn; pn = pn->next)
		if (pn->info == FOUND)
			find_when(pn);
}
