/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)chgrp.c	5.1 (Berkeley) %G%";
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
	register char *flags;

	argc--, argv++;
	if (*argv[0] == '-') {
		for (flags = argv[0]; *flags; ++flags)
			switch (*flags) {
			  case '-':			break;
			  case 'f': 	fflag++;	break;
			  case 'R':	rflag++;	break;
			  default:
				printf("chgrp: unknown option: %s\n", *flags);
				exit(255);
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
		if (uid && gr == NULL) {
			fprintf(stderr, "%s: unknown group\n", argv[0]);
			exit(255);
		}
	} else {
		gr = getgrnam(argv[0]);
		if (gr == NULL) {
			fprintf(stderr, "%s: unknown group\n", argv[0]);
			exit(255);
		}
		gid = gr->gr_gid;
	}
	pwd = getpwuid(uid);
	if (pwd == NULL) {
		fprintf(stderr, "Who are you?\n");
		exit(255);
	}
	if (uid && pwd->pw_gid != gid) {
		for (i=0; gr->gr_mem[i]; i++)
			if (!(strcmp(pwd->pw_name, gr->gr_mem[i])))
				goto ok;
		if (fflag)
			exit(0);
		fprintf(stderr, "You are not a member of the %s group.\n",
		    		argv[0]);
		exit(255);
	}
ok:
	for (c = 1; c < argc; c++) {
		if (lstat(argv[c], &stbuf)) {
			perror(argv[c]);
			continue;
		}
		if (uid && uid != stbuf.st_uid) {
			if (fflag)
				continue;
			fprintf(stderr, "You are not the owner of %s\n"
				      , argv[c]);
			++status;
			continue;
		}
		if (rflag && stbuf.st_mode & S_IFDIR)
			status += chownr(argv[c], stbuf.st_uid, gid);
		else if (chown(argv[c], stbuf.st_uid, gid) && !fflag)
			perror(argv[c]);
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

chownr(dir, uid, gid)
	char	*dir;
{
	register DIR		*dirp;
	register struct direct	*dp;
	register struct stat	st;
	char			savedir[1024];

	if (getwd(savedir) == 0) {
		fprintf(stderr, "chgrp: %s\n", savedir);
		exit(255);
	}
	if (chown(dir, uid, gid) < 0 && !fflag) {
		perror(dir);
		return(1);
	}

	chdir(dir);
	if ((dirp = opendir(".")) == NULL) {
		perror(dir);
		exit(status);
	}
	dp = readdir(dirp);
	dp = readdir(dirp); /* read "." and ".." */

	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
		if (lstat(dp->d_name, &st) < 0) {
			fprintf(stderr, "chgrp: can't access %s\n", dp->d_name);
			return(1);
		}
		if (st.st_mode & S_IFDIR)
			chownr(dp->d_name, st.st_uid, gid);
		else
			if (chown(dp->d_name, st.st_uid, gid) < 0 && !fflag) {
				perror(dp->d_name);
				return(1);
			}
	}
	closedir(dirp);
	chdir(savedir);
	return(0);
}
