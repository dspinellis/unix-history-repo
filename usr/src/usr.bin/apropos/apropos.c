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
static char sccsid[] = "@(#)apropos.c	8.3 (Berkeley) %G%";
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

#define	MAXLINELEN	1024			/* max line handled */

static int *found, foundman;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	ENTRY *ep;
	int ch;
	char *conffile, **p, *p_augment, *p_path;

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

	if ((found = malloc((u_int)argc)) == NULL)
		err(1, NULL);
	memset(found, 0, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* convert to lower-case */
		lowstr(*p, *p);

	if (p_augment)
		apropos(argv, p_augment, 1);
	if (p_path || (p_path = getenv("MANPATH")))
		apropos(argv, p_path, 1);
	else {
		config(conffile);
		ep = getlist("_whatdb");
		if (ep != NULL)
			ep = ep->list.qe_next;
		for (; ep != NULL; ep = ep->list.qe_next)
			apropos(argv, ep->s, 0);
	}

	if (!foundman) {
		(void)fprintf(stderr,
		    "apropos: no %s file found.\n", _PATH_WHATIS);
		exit(1);
	}
	for (p = argv; *p; ++p)
		if (!found[p - argv])
			(void)printf("%s: nothing appropriate\n", *p);
}

apropos(argv, path, buildpath)
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
			if (!index(buf, '\n')) {
				(void)fprintf(stderr,
				    "apropos: %s line too long.\n", name);
				continue;
			}
			lowstr(buf, wbuf);
			for (p = argv; *p; ++p)
				if (match(wbuf, *p)) {
					(void)printf("%s", buf);
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
 *	match anywhere the string appears
 */
match(bp, str)
	register char *bp, *str;
{
	register int len;
	register char test;

	if (!*bp)
		return(0);
	/* backward compatible: everything matches empty string */
	if (!*str)
		return(1);
	for (test = *str++, len = strlen(str); *bp;)
		if (test == *bp++ && !strncmp(bp, str, len))
			return(1);
	return(0);
}

/*
 * lowstr --
 *	convert a string to lower case
 */
lowstr(from, to)
	register char *from, *to;
{
	register char ch;

	while ((ch = *from++) && ch != '\n')
		*to++ = isupper(ch) ? tolower(ch) : ch;
	*to = '\0';
}

/*
 * usage --
 *	print usage message and die
 */
usage()
{
	(void)fprintf(stderr,
	    "usage: apropos [-C file] [-M path] [-m path] keyword ...\n");
	exit(1);
}
