/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chown.c	5.11 (Berkeley) 6/18/88";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <ctype.h>

static int ischown, uid, gid, fflag, rflag, retval;
static char *gname, *myname;

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
	struct group *gr, *getgrnam();

	if (!*s) {
		gid = -1;			/* argument was "uid." */
		return;
	}
	for (gname = s; *s && isdigit(*s); ++s);
	if (!*s)
		gid = atoi(gname);
	else {
		if (!(gr = getgrnam(gname))) {
			if (fflag)
				exit(0);
			fprintf(stderr, "%s: unknown group id: %s\n",
			    myname, gname);
			exit(-1);
		}
		gid = gr->gr_gid;
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

	if (chown(file, uid, gid)) {
		chownerr(file);
		return;
	}
	if (!rflag)
		return;
	if (lstat(file, &buf)) {
		err(file);
		return;
	}
	if ((buf.st_mode & S_IFMT) == S_IFDIR) {
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
chownerr(file)
	char *file;
{
	static int euid = -1, ngroups = -1;

	/* check for chown without being root */
	if (uid != -1 && euid == -1 && (euid = geteuid())) {
		if (fflag)
			exit(0);
		err(file);
		exit(-1);
	}
	/* check group membership; kernel just returns EPERM */
	if (gid != -1 && ngroups == -1) {
		int groups[NGROUPS];

		ngroups = getgroups(NGROUPS, groups);
		while (--ngroups >= 0 && gid != groups[ngroups]);
		if (ngroups < 0) {
			if (fflag)
				exit(0);
			fprintf(stderr,
			    "%s: you are not a member of group %s.\n",
			    myname, gname);
			exit(-1);
		}
	}
	err(file);
}

static
err(s)
	char *s;
{
	if (fflag)
		return;
	fprintf(stderr, "%s: ", myname);
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
