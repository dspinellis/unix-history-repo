/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)groups.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * groups
 */

#include <sys/param.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>

int	groups[NGROUPS];

main(argc, argv)
	int argc;
	char *argv[];
{
	int ngroups, i;
	char *sep = "";
	struct group *gr;

	if (argc > 1)
		showgroups(argv[1]);
	ngroups = getgroups(NGROUPS, groups);
	for (i = 0; i < ngroups; i++) {
		gr = getgrgid(groups[i]);
		if (gr == NULL)
			printf("%s%d", sep, groups[i]);
		else
			printf("%s%s", sep, gr->gr_name);
		sep = " ";
	}
	printf("\n");
	exit(0);
}

showgroups(user)
	register char *user;
{
	register struct group *gr;
	register struct passwd *pw;
	register char **cp;
	char *sep = "";

	if ((pw = getpwnam(user)) == NULL) {
		fprintf(stderr, "groups: no such user.\n");
		exit(1);
	}
	while (gr = getgrent()) {
		if (pw->pw_gid == gr->gr_gid) {
			printf("%s%s", sep, gr->gr_name);
			sep = " ";
			continue;
		}
		for (cp = gr->gr_mem; cp && *cp; cp++)
			if (strcmp(*cp, user) == 0) {
				printf("%s%s", sep, gr->gr_name);
				sep = " ";
				break;
			}
	}
	printf("\n");
	exit(0);
}
