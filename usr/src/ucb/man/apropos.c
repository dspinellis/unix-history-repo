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
static char sccsid[] = "@(#)apropos.c	5.6 (Berkeley) 6/29/88";
#endif /* not lint */

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>

#define	DEF_PATH	"/usr/man:/usr/new/man:/usr/local/man"
#define	MAXLINELEN	1000			/* max line handled */
#define	WHATIS		"whatis"		/* database name */

#define	NO	0				/* no/false */
#define	YES	1				/* yes/true */

static char *myname;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register char *beg, *end, **C;
	int ch, foundman = NO, *found, isapropos;
	int a_match(), w_match(), (*match)();
	char *manpath = NULL, buf[MAXLINELEN + 1], fname[MAXPATHLEN + 1];
	char wbuf[MAXLINELEN + 1], *getenv(), *malloc();

	myname = (beg = rindex(*argv, '/')) ? beg + 1 : *argv;
	if (!strcmp(myname, "apropos")) {
		isapropos = YES;
		match = a_match;
	}
	else {
		isapropos = NO;
		match = w_match;
	}
	while ((ch = getopt(argc, argv, "M:P:")) != EOF)
		switch((char)ch) {
			case 'M':
			case 'P':		/* backward contemptible */
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
		manpath = DEF_PATH;

	/*NOSTRICT*/
	if (!(found = (int *)malloc((u_int)argc))) {
		fprintf(stderr, "%s: out of space.\n", myname);
		exit(1);
	}
	bzero((char *)found, argc * sizeof(int));

	if (isapropos)
		for (C = argv; *C; ++C)		/* convert to lower-case */
			lowstr(*C, *C);
	else for (C = argv; *C; ++C)		/* trim full paths */
		if (beg = rindex(*C, '/'))
			*C = beg + 1;

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
		for (foundman = YES; gets(buf);) {
			if (isapropos)
				lowstr(buf, wbuf);
			else
				dashtrunc(buf, wbuf);
			for (C = argv; *C; ++C)
				if ((*match)(wbuf, *C)) {
					puts(buf);
					found[C - argv] = YES;

					/* only print line once */
					while (*++C)
						if ((*match)(wbuf, *C))
							found[C - argv] = YES;
					break;
				}
		}
	}
	if (!foundman) {
		fprintf(stderr, "%s: no %s file found in %s.\n", myname, WHATIS, manpath);
		exit(1);
	}
	for (C = argv; *C; ++C)
		if (!found[C - argv])
			printf("%s: %s\n", *C, isapropos ? "nothing appropriate" : "not found");
}

/*
 * a_match --
 *	match for apropos; anywhere the string appears
 */
static
a_match(bp, str)
	register char *bp, *str;
{
	register int len;
	register char test;

	if (!*bp)
		return(NO);
	/* backward compatible: everything matches empty string */
	if (!*str)
		return(YES);
	for (test = *str++, len = strlen(str); *bp;)
		if (test == *bp++ && !strncmp(bp, str, len))
			return(YES);
	return(NO);
}

/*
 * w_match --
 *	match for whatis; looks for full word match
 */
static
w_match(bp, str)
	register char *bp, *str;
{
	register int len;
	register char *start;

	if (!*str || !*bp)
		return(NO);
	for (len = strlen(str);;) {
		for (; *bp && !isdigit(*bp) && !isalpha(*bp); ++bp);
		if (!*bp)
			break;
		for (start = bp++; *bp && (isdigit(*bp) || isalpha(*bp)); ++bp);
		if (bp - start == len && !strncasecmp(start, str, len))
			return(YES);
	}
	return(NO);
}

/*
 * dashtrunc --
 *	truncate a string at " - "
 */
static
dashtrunc(from, to)
	register char *from, *to;
{
	do {
		if (from[0] == ' ' && from[1] == '-' && from[2] == ' ')
			break;
	} while (*to++ = *from++);
	*to = '\0';
}

/*
 * lowstr --
 *	convert a string to lower case
 */
static
lowstr(from, to)
	register char *from, *to;
{
	do {
		*to++ = isupper(*from) ? tolower(*from) : *from;
	} while (*from++);
}

/*
 * usage --
 *	print usage message and die
 */
static
usage()
{
	fprintf(stderr, "usage: %s [-M path] string ...\n", myname);
	exit(1);
}
