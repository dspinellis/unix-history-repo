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
static char sccsid[] = "@(#)apropos.c	5.10 (Berkeley) %G%";
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
	register char **p;
	int ch;
	char *p_augment, *p_path, *config(), *getenv(), *malloc();

	progname = "apropos";
	p_augment = p_path = NULL;
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

	for (p = argv; *p; ++p)			/* convert to lower-case */
		lowstr(*p, *p);

	if (p_augment)
		apropos(argv, p_augment);
	if (p_path)
		apropos(argv, p_path);
	if (!foundman) {
		(void)fprintf(stderr, "apropos: no %s file found.\n",
		    _PATH_WHATIS);
		exit(1);
	}
	for (p = argv; *p; ++p)
		if (!found[p - argv])
			(void)printf("%s: nothing appropriate\n", *p);
}

apropos(argv, path)
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
			if (!index(buf, '\n')) {
				(void)fprintf(stderr,
				    "apropos: %s line too long.\n", fname);
				exit(1);
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
	    "usage: apropos [-M path] [-m path] keyword ...\n");
	exit(1);
}
