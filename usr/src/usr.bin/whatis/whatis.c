/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1987, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)whatis.c	8.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/queue.h>

#include <ctype.h>
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../man/config.h"
#include "../man/pathnames.h"

#define	MAXLINELEN	256			/* max line handled */

static int *found, foundman;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	ENTRY *ep;
	TAG *tp;
	int ch, rv;
	char *beg, *conffile, **p, *p_augment, *p_path;

	conffile = NULL;
	p_augment = p_path = NULL;
	while ((ch = getopt(argc, argv, "C:M:m:P:")) != EOF)
		switch (ch) {
		case 'C':
			conffile = optarg;
			break;
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

	if ((found = malloc((u_int)argc * sizeof(int))) == NULL)
		err(1, NULL);
	memset(found, 0, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* trim full paths */
		if (beg = rindex(*p, '/'))
			*p = beg + 1;

	if (p_augment)
		whatis(argv, p_augment, 1);
	if (p_path || (p_path = getenv("MANPATH")))
		whatis(argv, p_path, 1);
	else {
		config(conffile);
		ep = (tp = getlist("_whatdb")) == NULL ?
		   NULL : tp->list.tqh_first;
		for (; ep != NULL; ep = ep->q.tqe_next)
			whatis(argv, ep->s, 0);
	}

	if (!foundman) {
		fprintf(stderr, "whatis: no %s file found.\n", _PATH_WHATIS);
		exit(1);
	}
	rv = 1;
	for (p = argv; *p; ++p)
		if (found[p - argv])
			rv = 0;
		else
			printf("%s: not found\n", *p);
	exit(rv);
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
	    "usage: whatis [-C file] [-M path] [-m path] command ...\n");
	exit(1);
}
