/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chown.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <ctype.h>

static int ischown, uid, gid, fflag, rflag, retval;
static char *myname;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register char *cp;
	int ch;
	char *index(), *rindex();

	myname = (cp = rindex(*argv, '/')) ? cp + 1 : *argv;
	ischown = myname[2] == 'o';

	while ((ch = getopt(argc, argv, "Rf")) != EOF)
		switch((char)ch) {
		case 'R':
			rflag++;
			break;
		case 'f':
			fflag++;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	if (argc < 2)
		usage();

	if (ischown) {
		if (cp = index(*argv, '.')) {
			*cp++ = '\0';
			setgid(cp);
		}
		else
			gid = -1;
		setuid(*argv);
	}
	else {
		uid = -1;
		setgid(*argv);
	}

	while (*++argv)
		change(*argv);
	exit(retval);
}

static
setgid(s)
	register char *s;
{
	register int ngroups;
	struct group *gr, *getgrnam();
	int groups[NGROUPS];
	char *beg;

	if (!*s) {
		gid = -1;			/* argument was "uid." */
		return;
	}
	for (beg = s; *s && isdigit(*s); ++s);
	if (!*s)
		gid = atoi(beg);
	else {
		if (!(gr = getgrnam(beg))) {
			if (fflag)
				exit(0);
			fprintf(stderr, "%s: unknown group id: %s\n",
			    myname, beg);
			exit(-1);
		}
		gid = gr->gr_gid;
	}
	/* check now; the kernel returns "EPERM" on later failure */
	ngroups = getgroups(NGROUPS, groups);
	while (--ngroups >= 0 && gid != groups[ngroups]);
	if (ngroups < 0) {
		if (fflag)
			exit(0);
		fprintf(stderr, "%s: you are not a member of group %s.\n",
		    myname, beg);
		exit(-1);
	}
}

static
setuid(s)
	register char *s;
{
	struct passwd *pwd, *getpwnam();
	char *beg;

	if (!*s) {
		uid = -1;			/* argument was ".gid" */
		return;
	}
	for (beg = s; *s && isdigit(*s); ++s);
	if (!*s)
		uid = atoi(beg);
	else {
		if (!(pwd = getpwnam(beg))) {
			if (fflag)
				exit(0);
			fprintf(stderr, "chown: unknown user id: %s\n", beg);
			exit(-1);
		}
		uid = pwd->pw_uid;
	}
}

static
change(file)
	char *file;
{
	register DIR *dirp;
	register struct direct *dp;
	struct stat buf;

	if (lstat(file, &buf) || chown(file, uid, gid)) {
		err(file);
		return;
	}
	if (rflag && ((buf.st_mode & S_IFMT) == S_IFDIR)) {
		if (chdir(file) < 0 || !(dirp = opendir("."))) {
			err(file);
			return;
		}
		for (dp = readdir(dirp); dp; dp = readdir(dirp)) {
			if (dp->d_name[0] == '.' && (!dp->d_name[1] ||
			    dp->d_name[1] == '.' && !dp->d_name[2]))
				continue;
			change(dp->d_name);
		}
		closedir(dirp);
		if (chdir("..")) {
			err("..");
			exit(fflag ? 0 : -1);
		}
	}
}

static
err(s)
	char *s;
{
	if (fflag)
		return;
	fputs(ischown ? "chown: " : "chgrp: ", stderr);
	perror(s);
	retval = -1;
}

static
usage()
{
	fprintf(stderr, "usage: %s [-Rf] %s file ...\n", myname,
	    ischown ? "owner[.group]" : "group");
	exit(-1);
}
