/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)apropos.c	5.12 (Berkeley) 6/1/90";
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
	register char **p;
	int ch;
	char *p_augment, *p_path, **getdb();

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

	/*NOSTRICT*/
	if (!(found = (int *)malloc((u_int)argc)))
		enomem();
	bzero((void *)found, argc * sizeof(int));

	for (p = argv; *p; ++p)			/* convert to lower-case */
		lowstr(*p, *p);

	if (p_augment)
		apropos(argv, p_augment, 1);
	if (p_path || (p_path = getenv("MANPATH")))
		apropos(argv, p_path, 1);
	else
		for (p = getdb(); *p; ++p)
			apropos(argv, *p, 0);

	if (!foundman) {
		(void)fprintf(stderr,
		    "apropos: : no %s file found.\n", _PATH_WHATIS);
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
