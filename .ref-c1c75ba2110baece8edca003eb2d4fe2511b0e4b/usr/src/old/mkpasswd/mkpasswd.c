/*
 * Copyright (c) 1980, 1983 Regents of the University of California.
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
"@(#) Copyright (c) 1980, 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkpasswd.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	DBM *dp;
	datum key, content;
	register char *cp, *tp;
	register struct passwd *pwd;
	int verbose = 0, entries = 0, maxlen = 0;
	char buf[BUFSIZ];

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
	(void)umask(0);
	dp = dbm_open(argv[1], O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (dp == NULL) {
		fprintf(stderr, "mkpasswd: ");
		perror(argv[1]);
		exit(1);
	}
	setpwfile(argv[1]);
	while (pwd = getpwent()) {
		cp = buf;
#define	COMPACT(e)	tp = pwd->e; while (*cp++ = *tp++);
		COMPACT(pw_name);
		COMPACT(pw_passwd);
		bcopy((char *)&pwd->pw_uid, cp, sizeof (int));
		cp += sizeof (int);
		bcopy((char *)&pwd->pw_gid, cp, sizeof (int));
		cp += sizeof (int);
		bcopy((char *)&pwd->pw_quota, cp, sizeof (int));
		cp += sizeof (int);
		COMPACT(pw_comment);
		COMPACT(pw_gecos);
		COMPACT(pw_dir);
		COMPACT(pw_shell);
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
