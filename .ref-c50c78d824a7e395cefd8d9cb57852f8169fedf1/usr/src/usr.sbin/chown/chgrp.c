/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)chgrp.c	5.9 (Berkeley) %G%";
#endif not lint

/*
 * chgrp -fR gid file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include <sys/dir.h>

struct	group *gr, *getgrnam(), *getgrgid();
struct	passwd *getpwuid(), *pwd;
struct	stat stbuf;
int	gid, uid;
int	status;
int	fflag, rflag;
/* VARARGS */
int	fprintf();

main(argc, argv)
	int argc;
	char *argv[];
{
	register c, i;
	register char *cp;

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'f':
			fflag++;
			break;

		case 'R':
			rflag++;
			break;

		default:
			fatal(255, "unknown option: %c", *cp);
			/*NOTREACHED*/
		}
		argv++, argc--;
	}
	if (argc < 2) {
		fprintf(stderr, "usage: chgrp [-fR] gid file ...\n");
		exit(255);
	}
	uid = getuid();
	if (isnumber(argv[0])) {
		gid = atoi(argv[0]);
		gr = getgrgid(gid);
		if (uid && gr == NULL)
			fatal(255, "%s: unknown group", argv[0]);
	} else {
		gr = getgrnam(argv[0]);
		if (gr == NULL)
			fatal(255, "%s: unknown group", argv[0]);
		gid = gr->gr_gid;
	}
	pwd = getpwuid(uid);
	if (pwd == NULL)
		fatal(255, "Who are you?");
	if (uid && pwd->pw_gid != gid) {
		for (i=0; gr->gr_mem[i]; i++)
			if (!(strcmp(pwd->pw_name, gr->gr_mem[i])))
				goto ok;
		if (fflag)
			exit(0);
		fatal(255, "You are not a member of the %s group", argv[0]);
	}
ok:
	for (c = 1; c < argc; c++) {
		/* do stat for directory arguments */
		if (lstat(argv[c], &stbuf)) {
			status += Perror(argv[c]);
			continue;
		}
		if (uid && uid != stbuf.st_uid) {
			status += error("You are not the owner of %s", argv[c]);
			continue;
		}
		if (rflag && ((stbuf.st_mode & S_IFMT) == S_IFDIR)) {
			status += chownr(argv[c]);
			continue;
		}
		if (chown(argv[c], -1, gid)) {
			status += Perror(argv[c]);
			continue;
		}
	}
	exit(status);
}

isnumber(s)
	char *s;
{
	register int c;

	while (c = *s++)
		if (!isdigit(c))
			return (0);
	return (1);
}

chownr(dir)
	char *dir;
{
	register DIR *dirp;
	register struct direct *dp;
	struct stat st;
	char savedir[1024];
	int ecode;

	if (getwd(savedir) == 0)
		fatal(255, "%s", savedir);
	/*
	 * Change what we are given before doing its contents.
	 */
	if (chown(dir, -1, gid) < 0 && Perror(dir))
		return (1);
	if (chdir(dir) < 0) {
		Perror(dir);
		return (1);
	}
	if ((dirp = opendir(".")) == NULL) {
		Perror(dir);
		return (1);
	}
	dp = readdir(dirp);
	dp = readdir(dirp); /* read "." and ".." */
	ecode = 0;
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if (lstat(dp->d_name, &st) < 0) {
			ecode = Perror(dp->d_name);
			if (ecode)
				break;
			continue;
		}
		if (uid && uid != st.st_uid) {
			ecode = error("You are not the owner of %s",
				dp->d_name);
			continue;
		}
		if ((st.st_mode & S_IFMT) == S_IFDIR) {
			ecode = chownr(dp->d_name);
			if (ecode)
				break;
			continue;
		}
		if (chown(dp->d_name, -1, gid) < 0 &&
		    (ecode = Perror(dp->d_name)))
			break;
	}
	closedir(dirp);
	if (chdir(savedir) < 0)
		fatal(255, "can't change back to %s", savedir);
	return (ecode);
}

error(fmt, a)
	char *fmt, *a;
{

	if (!fflag) {
		fprintf(stderr, "chgrp: ");
		fprintf(stderr, fmt, a);
		putc('\n', stderr);
	}
	return (!fflag);
}

fatal(status, fmt, a)
	int status;
	char *fmt, *a;
{

	fflag = 0;
	(void) error(fmt, a);
	exit(status);
}

Perror(s)
	char *s;
{

	if (!fflag) {
		fprintf(stderr, "chgrp: ");
		perror(s);
	}
	return (!fflag);
}
