/*
 * Copyright (c) 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whatis.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "../man/pathnames.h"

#define	MAXLINELEN	256			/* max line handled */

int *found, foundman;
char *progname;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register char *beg, **p;
	int ch;
	char *p_augment, *p_path, *config(), *getenv(), *malloc();

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

	if (!p_path && !(p_path = getenv("MANPATH")))
		p_path = config();

	/*NOSTRICT*/
	if (!(found = (int *)malloc((u_int)argc)))
		enomem();
	bzero((char *)found, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* trim full paths */
		if (beg = rindex(*p, '/'))
			*p = beg + 1;

	if (p_augment)
		whatis(argv, p_augment);
	if (p_path)
		whatis(argv, p_path);

	if (!foundman) {
		fprintf(stderr, "whatis: no %s file found.\n", _PATH_WHATIS);
		exit(1);
	}
	for (p = argv; *p; ++p)
		if (!found[p - argv])
			printf("%s: not found\n", *p);
}

whatis(argv, path)
	char **argv, *path;
{
	register char *beg, *end, **p;
	char fname[MAXPATHLEN + 1];
	char buf[MAXLINELEN + 1], wbuf[MAXLINELEN + 1];

	for (beg = path; beg; beg = end) {	/* through path list */
		end = index(beg, ':');
		if (!end)
			(void)sprintf(fname, "%s/%s", beg, _PATH_WHATIS);
		else {
			(void)sprintf(fname, "%.*s/%s", end - beg, beg,
			    _PATH_WHATIS);
			++end;
		}
		if (!freopen(fname, "r", stdin))
			continue;

		/* for each file found */
		for (foundman = 1; fgets(buf, sizeof(buf), stdin);) {
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
