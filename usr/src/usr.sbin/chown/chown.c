/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)chown.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * chown [-fR] uid[.gid] file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <sys/dir.h>
#include <grp.h>

struct	passwd	*pwd,*getpwnam();
struct	stat	stbuf;
int	uid;
int	status;
int	fflag, rflag;

main(argc, argv)
char *argv[];
{
	register int c, gid;
	register char *flags, *group;
	struct group *grp;

	if(argc < 3) {
		fprintf(stderr, "usage: chown [-fR] owner[.group] file ...\n");
		exit(-1);
	}
	if (*argv[1] == '-') {
		for (flags = argv[1]; *flags; ++flags)
			switch (*flags) {
			  case '-':			break;
			  case 'f': 	fflag++;	break;
			  case 'R':	rflag++;	break;
			  default:
				printf("chown: unknown option: %c\n", *flags);
				exit(-2);
			}
		argv++, argc--;
	}

	for (group = argv[1]; *group ; group++) {
		if (*group == '.') {
			*group = '\0';
			group++;
			if (isnumber(group))
				gid = atoi(group);
			else {
				if ((grp=getgrnam(group)) == NULL) {
					printf("unknown group: %s\n",group);
					exit(-3);
				}
				gid = grp -> gr_gid;
				endgrent();
			}
			goto owner;
		}
	}
	group = NULL;
	
owner:
	if (isnumber(argv[1])) {
		uid = atoi(argv[1]);
	} else {
		if ((pwd=getpwnam(argv[1])) == NULL) {
			printf("unknown user id: %s\n",argv[1]);
			exit(-4);
		}
		uid = pwd->pw_uid;
	}

	for(c=2; c<argc; c++) {
		if (lstat(argv[c], &stbuf) < 0) {
			printf("chown: couldn't stat %s\n", argv[c]);
			exit(-5);
		}
		if (group == NULL)
			gid = stbuf.st_gid;
		if (rflag && stbuf.st_mode & S_IFDIR)
			status += chownr(argv[c], group, uid, gid);
		else if (chown(argv[c], uid, gid) < 0 && !fflag) {
			perror(argv[c]);
			status++;
		}
	}
	exit(status);
}

isnumber(s)
char *s;
{
	register c;

	while(c = *s++)
		if(!isdigit(c))
			return(0);
	return(1);
}

chownr(dir, dogrp, uid, gid_save)
	char	*dir;
{
	register DIR		*dirp;
	register struct direct	*dp;
	register struct stat	st;
	char			savedir[1024];
	int			gid;

	if (getwd(savedir) == 0) {
		fprintf(stderr, "chown: %s\n", savedir);
		exit(-6);
	}
	if (chown(dir, uid, gid_save) < 0 && !fflag) {
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
			fprintf(stderr, "chown: can't access %s\n", dp->d_name);
			return(1);
		}
		if (dogrp)
			gid = gid_save;
		else
			gid = st.st_gid;
		if (st.st_mode & S_IFDIR)
			chownr(dp->d_name, dogrp, uid, gid);
		else
			if (chown(dp->d_name, uid, gid) < 0 && !fflag) {
				perror(dp->d_name);
				return(1);
			}
	}
	closedir(dirp);
	chdir(savedir);
	return(0);
}
