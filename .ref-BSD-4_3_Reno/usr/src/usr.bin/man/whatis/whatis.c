/*
 * Copyright (c) 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whatis.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "../man/pathnames.h"

#define	MAXLINELEN	256			/* max line handled */

char *progname;

static int *found, foundman;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register char *beg, **p;
	int ch;
	char *p_augment, *p_path, **getdb();

	progname = "whatis";
	while ((ch = getopt(argc, argv, "M:m:P:")) != EOF)
		switch((char)ch) {
		case 'M':
		case 'P':		/* backward compatible */
			p_path = optarg;
			break;
		case 'm':
			p_augment = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	if (argc < 1)
		usage();

	/*NOSTRICT*/
	if (!(found = (int *)malloc((u_int)argc)))
		enomem();
	bzero((char *)found, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* trim full paths */
		if (beg = rindex(*p, '/'))
			*p = beg + 1;

	if (p_augment)
		whatis(argv, p_augment, 1);
	if (p_path || (p_path = getenv("MANPATH")))
		whatis(argv, p_path, 1);
	else
		for (p = getdb(); *p; ++p)
			whatis(argv, *p, 0);

	if (!foundman) {
		fprintf(stderr, "whatis: no %s file found.\n", _PATH_WHATIS);
		exit(1);
	}
	for (p = argv; *p; ++p)
		if (!found[p - argv])
			printf("%s: not found\n", *p);
}

whatis(argv, path, buildpath)
	char **argv, *path;
	int buildpath;
{
	register char *end, *name, **p;
	char buf[MAXLINELEN + 1], wbuf[MAXLINELEN + 1];

	for (name = path; name; name = end) {	/* through name list */
		if (end = index(name, ':'))
			*end++ = '\0';

		if (buildpath) {
			char hold[MAXPATHLEN + 1];

			(void)sprintf(hold, "%s/%s", name, _PATH_WHATIS);
			name = hold;
		}

		if (!freopen(name, "r", stdin))
			continue;

		foundman = 1;

		/* for each file found */
		while (fgets(buf, sizeof(buf), stdin)) {
			dashtrunc(buf, wbuf);
			for (p = argv; *p; ++p)
				if (match(wbuf, *p)) {
					printf("%s", buf);
					found[p - argv] = 1;

					/* only print line once */
					while (*++p)
						if (match(wbuf, *p))
							found[p - argv] = 1;
					break;
				}
		}
	}
}

/*
 * match --
 *	match a full word
 */
match(bp, str)
	register char *bp, *str;
{
	register int len;
	register char *start;

	if (!*str || !*bp)
		return(0);
	for (len = strlen(str);;) {
		for (; *bp && !isdigit(*bp) && !isalpha(*bp); ++bp);
		if (!*bp)
			break;
		for (start = bp++;
		    *bp && (*bp == '_' || isdigit(*bp) || isalpha(*bp)); ++bp);
		if (bp - start == len && !strncasecmp(start, str, len))
			return(1);
	}
	return(0);
}

/*
 * dashtrunc --
 *	truncate a string at " - "
 */
dashtrunc(from, to)
	register char *from, *to;
{
	register int ch;

	for (; (ch = *from) && ch != '\n' &&
	    (ch != ' ' || from[1] != '-' || from[2] != ' '); ++from)
		*to++ = ch;
	*to = '\0';
}

/*
 * usage --
 *	print usage message and die
 */
usage()
{
	(void)fprintf(stderr,
	    "usage: whatis [-M path] [-m path] command ...\n");
	exit(1);
}
