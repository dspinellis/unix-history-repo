/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)mkpasswd.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

char	buf[BUFSIZ];

struct	passwd *fgetpwent();

main(argc, argv)
	char *argv[];
{
	DBM *dp;
	datum key, content;
	register char *cp, *tp;
	register struct passwd *pwd;
	int verbose = 0, entries = 0, maxlen = 0;

	if (argc > 1 && strcmp(argv[1], "-v") == 0) {
		verbose++;
		argv++, argc--;
	}
	if (argc != 2) {
		fprintf(stderr, "usage: mkpasswd [ -v ] file\n");
		exit(1);
	}
	if (access(argv[1], R_OK) < 0) {
		fprintf(stderr, "mkpasswd: ");
		perror(argv[1]);
		exit(1);
	}
	umask(0);
	dp = dbm_open(argv[1], O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (dp == NULL) {
		fprintf(stderr, "mkpasswd: ");
		perror(argv[1]);
		exit(1);
	}
	setpwfile(argv[1]);
	while (pwd = getpwent()) {
		cp = buf;
#define	COMPACT(e)	tp = pwd->pw_/**/e; while (*cp++ = *tp++);
		COMPACT(name);
		COMPACT(passwd);
		bcopy((char *)&pwd->pw_uid, cp, sizeof (int));
		cp += sizeof (int);
		bcopy((char *)&pwd->pw_gid, cp, sizeof (int));
		cp += sizeof (int);
		bcopy((char *)&pwd->pw_quota, cp, sizeof (int));
		cp += sizeof (int);
		COMPACT(comment);
		COMPACT(gecos);
		COMPACT(dir);
		COMPACT(shell);
		content.dptr = buf;
		content.dsize = cp - buf;
		if (verbose)
			printf("store %s, uid %d\n", pwd->pw_name, pwd->pw_uid);
		key.dptr = pwd->pw_name;
		key.dsize = strlen(pwd->pw_name);
		if (dbm_store(dp, key, content, DBM_INSERT) < 0) {
			fprintf(stderr, "mkpasswd: ");
			perror("dbm_store failed");
			exit(1);
		}
		key.dptr = (char *)&pwd->pw_uid;
		key.dsize = sizeof (int);
		if (dbm_store(dp, key, content, DBM_INSERT) < 0) {
			fprintf(stderr, "mkpasswd: ");
			perror("dbm_store failed");
			exit(1);
		}
		entries++;
		if (cp - buf > maxlen)
			maxlen = cp - buf;
	}
	dbm_close(dp);
	printf("%d password entries, maximum length %d\n", entries, maxlen);
	exit(0);
}
