/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)getNAME.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * Get name sections from manual pages.
 *	-t	for building toc
 *	-i	for building intro entries
 *	other	apropos database
 */
#include <stdio.h>
#include <string.h>

int tocrc;
int intro;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	int ch;

	while ((ch = getopt(argc, argv, "it")) != EOF)
		switch(ch) {
		case 'i':
			intro = 1;
			break;
		case 't':
			tocrc = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (!*argv)
		usage();

	for (; *argv; ++argv)
		getfrom(*argv);
	exit(0);
}

getfrom(name)
	char *name;
{
	int i = 0;
	char headbuf[BUFSIZ];
	char linbuf[BUFSIZ];

	if (freopen(name, "r", stdin) == 0) {
		perror(name);
		return;
	}
	for (;;) {
		if (fgets(headbuf, sizeof headbuf, stdin) == NULL)
			return;
		if (headbuf[0] != '.')
			continue;
		if (headbuf[1] == 'T' && headbuf[2] == 'H')
			break;
		if (headbuf[1] == 't' && headbuf[2] == 'h')
			break;
	}
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			return;
		if (linbuf[0] != '.')
			continue;
		if (linbuf[1] == 'S' && linbuf[2] == 'H')
			break;
		if (linbuf[1] == 's' && linbuf[2] == 'h')
			break;
	}
	trimln(headbuf);
	if (tocrc)
		doname(name);
	if (!intro)
		printf("%s\t", headbuf);
	for (;;) {
		if (fgets(linbuf, sizeof linbuf, stdin) == NULL)
			break;
		if (linbuf[0] == '.') {
			if (linbuf[1] == 'S' && linbuf[2] == 'H')
				break;
			if (linbuf[1] == 's' && linbuf[2] == 'h')
				break;
		}
		trimln(linbuf);
		if (intro) {
			split(linbuf, name);
			continue;
		}
		if (i != 0)
			printf(" ");
		i++;
		printf("%s", linbuf);
	}
	printf("\n");
}

trimln(cp)
	register char *cp;
{

	while (*cp)
		cp++;
	if (*--cp == '\n')
		*cp = 0;
}

doname(name)
	char *name;
{
	register char *dp = name, *ep;

again:
	while (*dp && *dp != '.')
		putchar(*dp++);
	if (*dp)
		for (ep = dp+1; *ep; ep++)
			if (*ep == '.') {
				putchar(*dp++);
				goto again;
			}
	putchar('(');
	if (*dp)
		dp++;
	while (*dp)
		putchar (*dp++);
	putchar(')');
	putchar(' ');
}

split(line, name)
	char *line, *name;
{
	register char *cp, *dp;
	char *sp, *sep;

	cp = index(line, '-');
	if (cp == 0)
		return;
	sp = cp + 1;
	for (--cp; *cp == ' ' || *cp == '\t' || *cp == '\\'; cp--)
		;
	*++cp = '\0';
	while (*sp && (*sp == ' ' || *sp == '\t'))
		sp++;
	for (sep = "", dp = line; dp && *dp; dp = cp, sep = "\n") {
		cp = index(dp, ',');
		if (cp) {
			register char *tp;

			for (tp = cp - 1; *tp == ' ' || *tp == '\t'; tp--)
				;
			*++tp = '\0';
			for (++cp; *cp == ' ' || *cp == '\t'; cp++)
				;
		}
		printf("%s%s\t", sep, dp);
		dorefname(name);
		printf("\t%s", sp);
	}
}

dorefname(name)
	char *name;
{
	register char *dp = name, *ep;

again:
	while (*dp && *dp != '.')
		putchar(*dp++);
	if (*dp)
		for (ep = dp+1; *ep; ep++)
			if (*ep == '.') {
				putchar(*dp++);
				goto again;
			}
	putchar('.');
	if (*dp)
		dp++;
	while (*dp)
		putchar (*dp++);
}

usage()
{
	(void)fprintf(stderr, "usage: getNAME [-it] file ...\n");
	exit(1);
}
