#ifndef lint
static	char *sccsid = "@(#)chgrp.c	4.3 82/03/31";
#endif

/*
 * chgrp gid file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>

struct	group *gr, *getgrnam(), *getgrgid();
struct	passwd *getpwuid(), *pwd;
struct	stat stbuf;
int	gid, uid;
int	status;
/* VARARGS */
int	fprintf();

main(argc, argv)
	int argc;
	char *argv[];
{
	register c, i;

	argc--, argv++;
	if (argc < 2) {
		printf("usage: chgrp gid file ...\n");
		exit(2);
	}
	uid = getuid();
	if (isnumber(argv[0])) {
		gid = atoi(argv[1]);
		gr = getgrgid(gid);
		if (uid && gr == NULL) {
			printf("%s: unknown group\n", argv[0]);
			exit(2);
		}
	} else {
		gr = getgrnam(argv[0]);
		if (gr == NULL) {
			printf("%s: unknown group\n", argv[0]);
			exit(2);
		}
		gid = gr->gr_gid;
	}
	pwd = getpwuid(uid);
	if (pwd == NULL) {
		fprintf(stderr, "Who are you?\n");
		exit(2);
	}
	if (uid && pwd->pw_gid != gid) {
		for (i=0; gr->gr_mem[i]; i++)
			if (!(strcmp(pwd->pw_name, gr->gr_mem[i])))
				goto ok;
		fprintf(stderr, "You are not a member of the %s group.\n",
		    argv[0]);
		exit(2);
	}
ok:
	for (c = 1; c < argc; c++) {
		if (stat(argv[c], &stbuf)) {
			perror(argv[c]);
			continue;
		}
		if (uid && uid != stbuf.st_uid) {
			fprintf(stderr, "You are not the owner of %s\n",
			    argv[c]);
			status = 1;
			continue;
		}
		if (chown(argv[c], stbuf.st_uid, gid))
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
