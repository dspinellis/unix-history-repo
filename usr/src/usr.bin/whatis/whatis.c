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
static char sccsid[] = "@(#)whatis.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include "pathnames.h"

#define	MAXLINELEN	1000			/* max line handled */
#define	WHATIS		"whatis"		/* database name */

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register char *beg, *end, **p;
	int ch, foundman = 0, *found;
	char *manpath, buf[MAXLINELEN + 1], fname[MAXPATHLEN + 1];
	char wbuf[MAXLINELEN + 1], *getenv(), *malloc();

	while ((ch = getopt(argc, argv, "M:P:")) != EOF)
		switch((char)ch) {
		case 'M':
		case 'P':		/* backward compatible */
			manpath = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;
	if (argc < 1)
		usage();

	if (!(manpath = getenv("MANPATH")))
		manpath = _PATH_DEFAULT;

	/*NOSTRICT*/
	if (!(found = (int *)malloc((u_int)argc))) {
		fprintf(stderr, "whatis: out of space.\n");
		exit(1);
	}
	bzero((char *)found, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* trim full paths */
		if (beg = rindex(*p, '/'))
			*p = beg + 1;

	for (beg = manpath; beg; beg = end) {	/* through path list */
		end = index(beg, ':');
		if (!end)
			(void)sprintf(fname, "%s/%s", beg, WHATIS);
		else {
			(void)sprintf(fname, "%.*s/%s", end - beg, beg, WHATIS);
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
	if (!foundman) {
		fprintf(stderr, "whatis: no %s file found in %s.\n",
		    WHATIS, manpath);
		exit(1);
	}
	for (p = argv; *p; ++p)
		if (!found[p - argv])
			printf("%s: not found\n", *p);
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
	fprintf(stderr, "usage: whatis [-M path] string ...\n");
	exit(1);
}
