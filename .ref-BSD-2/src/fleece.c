/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Look for a particular file name in everyone's home directory.
 *
 * Syntax: fleece name
 * Author: Kurt Shoens (UCB) 1/11/79
 */

char	*pwfile	=	"/etc/passwd";

main(argc, argv)
	char **argv;
{
	char namebuf[BUFSIZ], home[BUFSIZ], word[BUFSIZ];
	register char *cp;
	struct stat sbuf;
	extern char _sobuf[];

	if (argc < 2) {
		fprintf(stderr, "Usage: %s name ...\n", *argv);
		exit(1);
	}
	setbuf(stdout, _sobuf);
	strcpy(word, argv[1]);
	if (freopen(pwfile, "r", stdin) == NULL) {
		perror(pwfile);
		exit(1);
	}
	while (gets(namebuf) != NULL) {
		gethome(namebuf, home);
		cp = home + strlen(home);
		if (cp[-1] != '/')
			*cp++ = '/';
		strcpy(cp, word);
		if (stat(home, &sbuf) >= 0)
			puts(home);
	}
	exit(0);
}

/*
 * Find from the given passwd line the user's home directory
 * and copy right.
 */

gethome(pwline, home)
	char pwline[], home[];
{
	register char *cp, *cp2;
	register int c;

	for (cp = pwline, c = 0; c < 5 && *cp; c += *cp++ == ':')
		;
	for (cp2 = home; *cp && *cp != ':'; *cp2++ = *cp++)
		;
	*cp2 = 0;
}
