/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)apropos.c	5.3 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>

#define	DEF_PATH	"/usr/man:/usr/new/man:/usr/local/man"
#define	MAXLINELEN	1000			/* max line handled */
#define	NO		0			/* no/false */
#define	WHATIS		"whatis"		/* database name */
#define	YES		1			/* yes/true */

static char	*myname;

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	optind;
	register char	*beg, *end, **C;
	int	ch, foundman = NO, *found, isapropos,
		a_match(), w_match(), (*match)();
	char	*manpath = NULL,
		buf[MAXLINELEN + 1], fname[MAXPATHLEN + 1],
		wbuf[MAXLINELEN + 1],
		*getenv(), *index(), *malloc(), *rindex();

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
	bzero((char *)found, argc * sizeof(int));	/* calloc is silly */

	if (!isapropos)
		for (C = argv; *C; ++C) {		/* trim full paths */
			if (beg = rindex(*C, '/'))
				*C = beg + 1;
		}
	for (C = argv; *C; ++C)			/* convert to lower-case */
		lowstr(*C, *C);

	for (beg = manpath; beg; beg = end) {	/* through path list */
		end = index(beg, ':');
		if (!end)
			(void)sprintf(fname, "%s/%s", beg, WHATIS);
		else {
			(void)sprintf(fname, "%.*s/%s", end - beg, beg, WHATIS);
			++end;
		}
						/* for each file found */
		if (freopen(fname, "r", stdin)) {
			foundman = YES;
			while (gets(buf)) {	/* read & convert to lcase */
				lowstr(buf, wbuf);
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
	}
	if (!foundman) {
		fprintf(stderr, "%s: no %s file found in %s.\n", myname, WHATIS, manpath);
		exit(1);
	}
	for (C = argv; *C; C++)
		if (!found[C - argv])
			printf("%s: %s\n", *C, isapropos ? "nothing appropriate" : "not found");
}

static
a_match(bp, str)
	register char	*bp, *str;
{
	register char	test, *Cs, *Cb;

	if (!*bp)
		return(NO);
	/* backward contemptible: everything matches empty string */
	if (!*str)
		return(YES);
	for (test = *str++; *bp;)
		if (test == *bp++) {
			Cs = str;
			Cb = bp;
			do {
				if (!*Cs)
					return(YES);
			} while (*Cb++ == *Cs++);
		}
	return(NO);
}

static
w_match(bp, str)
	register char	*bp, *str;
{
	register char	test, *Cs, *Cb;

	if (!*str || !*bp)
		return(NO);
	for (test = *str++; *bp;)
		if (test == *bp++) {
			for (Cs = str, Cb = bp; *Cs == *Cb; ++Cs, ++Cb);
			if (!*Cs && (isspace(*Cb) || *Cb == '(' || *Cb == ','))
				return(YES);
		}
	return(NO);
}

static
lowstr(from, to)
	register char	*from, *to;
{
	for (; *from; ++from, ++to)
		*to = isupper(*from) ? tolower(*from) : *from;
	*to = '\0';
}

static
usage()
{
	fprintf(stderr, "usage: %s [-M path] string ...\n", myname);
	exit(1);
}
