/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)config.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <pwd.h>
#include "pathnames.h"

#define	MAXLINE		1024

extern char *progname;
char *pathbuf, **arorder;

static FILE *cfp;

/*
 * getpath --
 *	read in the configuration file, calling a function with the line
 *	from each matching section.
 */
char *
getpath(sects)
	char **sects;
{
	register char **av, *p;
	size_t len;
	char line[MAXLINE];
	static int openconfig();

	openconfig();
	while (fgets(line, sizeof(line), cfp)) {
		if (!index(line, '\n')) {
			(void)fprintf(stderr, "%s: config line too long.\n",
			    progname);
			exit(1);
		}
		p = strtok(line, " \t\n");
		if (!p || *p == '#')
			continue;
		for (av = sects; *av; ++av)
			if (!strcmp(p, *av))
				break;
		if (!*av)
			continue;
		while (p = strtok((char *)NULL, " \t\n")) {
			len = strlen(p);
			if (p[len - 1] == '/')
				for (av = arorder; *av; ++av)
                			cadd(p, len, *av);
			else
				cadd(p, len, (char *)NULL);
		}
	}
	return(pathbuf);
}

cadd(add1, len1, add2)
	char *add1, *add2;
	register size_t len1;
{
	static size_t buflen, boff;
	static char *bp, *endp;
	register size_t len2;

	len2 = add2 ? strlen(add2) : 0;
	if (bp == NULL || bp + len1 + len2 + 2 >= endp) {
		buflen += MAX(len1 + len2 + 2, 1024);
		boff = bp ? bp - pathbuf : 0;
		if ((pathbuf = realloc(pathbuf, buflen)) == NULL)
			enomem();
		bp = pathbuf + boff;
		endp = pathbuf + buflen;
	}
	bcopy(add1, bp, len1);
	bp += len1;
	if (len2) {
		bcopy(add2, bp, len2);
		bp += len2;
	}
	*bp++ = ':';
	*bp = '\0';
}

static
openconfig()
{
	if (cfp) {
		rewind(cfp);
		return;
	}
	if (!(cfp = fopen(_PATH_MANCONF, "r"))) {
		(void)fprintf(stderr, "%s: no configuration file %s.\n",
		    progname, _PATH_MANCONF);
		exit(1);
	}
}

char **
getdb()
{
	register char *p;
	int cnt, num;
	char **ar, line[MAXLINE];

	ar = NULL;
	num = 0;
	cnt = -1;
	openconfig();
	while (fgets(line, sizeof(line), cfp)) {
		if (!index(line, '\n')) {
			(void)fprintf(stderr, "%s: config line too long.\n",
			    progname);
			exit(1);
		}
		p = strtok(line, " \t\n");
#define	WHATDB	"_whatdb"
		if (!p || *p == '#' || strcmp(p, WHATDB))
			continue;
		while (p = strtok((char *)NULL, " \t\n")) {
			if (cnt == num - 1 &&
			    !(ar = realloc(ar, (num += 30) * sizeof(char **))))
				enomem();
			if (!(ar[++cnt] = strdup(p)))
				enomem();
		}
	}
	if (ar) {
		if (cnt == num - 1 &&
		    !(ar = realloc(ar, ++num * sizeof(char **))))
			enomem();
		ar[++cnt] = NULL;
	}
	return(ar);
}

char **
getorder()
{
	register char *p;
	int cnt, num;
	char **ar, line[MAXLINE];

	ar = NULL;
	num = 0;
	cnt = -1;
	openconfig();
	while (fgets(line, sizeof(line), cfp)) {
		if (!index(line, '\n')) {
			(void)fprintf(stderr, "%s: config line too long.\n",
			    progname);
			exit(1);
		}
		p = strtok(line, " \t\n");
#define	SUBDIR	"_subdir"
		if (!p || *p == '#' || strcmp(p, SUBDIR))
			continue;
		while (p = strtok((char *)NULL, " \t\n")) {
			if (cnt == num - 1 &&
			    !(ar = realloc(ar, (num += 30) * sizeof(char **))))
				enomem();
			if (!(ar[++cnt] = strdup(p)))
				enomem();
		}
	}
	if (ar) {
		if (cnt == num - 1 &&
		    !(ar = realloc(ar, ++num * sizeof(char **))))
			enomem();
		ar[++cnt] = NULL;
	}
	return(ar);
}

getsection(sect)
	char *sect;
{
	register char *p;
	char line[MAXLINE];

	openconfig();
	while (fgets(line, sizeof(line), cfp)) {
		if (!index(line, '\n')) {
			(void)fprintf(stderr, "%s: config line too long.\n",
			    progname);
			exit(1);
		}
		p = strtok(line, " \t\n");
		if (!p || *p == '#')
			continue;
		if (!strcmp(p, sect))
			return(1);
	}
	return(0);
}

enomem()
{
	(void)fprintf(stderr, "%s: %s\n", progname, strerror(ENOMEM));
	exit(1);
}
