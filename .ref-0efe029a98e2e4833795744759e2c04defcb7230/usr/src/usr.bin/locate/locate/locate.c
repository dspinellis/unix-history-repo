/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James A. Woods.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)locate.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Ref: Usenix ;login:, Vol 8, No 1, February/March, 1983, p. 8.
 *
 * Locate scans a file list for the full pathname of a file given only part
 * of the name.  The list has been processed with with "front-compression"
 * and bigram coding.  Front compression reduces space by a factor of 4-5,
 * bigram coding by a further 20-25%.
 *
 * The codes are:
 * 
 * 	0-28	likeliest differential counts + offset to make nonnegative
 *	30	switch code for out-of-range count to follow in next word
 *	128-255 bigram codes (128 most common, as determined by 'updatedb')
 *	32-127  single character (printable) ascii residue (ie, literal)
 * 
 * A novel two-tiered string search technique is employed:
 * 
 * First, a metacharacter-free subpattern and partial pathname is matched
 * BACKWARDS to avoid full expansion of the pathname list.  The time savings
 * is 40-50% over forward matching, which cannot efficiently handle
 * overlapped search patterns and compressed path residue.
 * 
 * Then, the actual shell glob-style regular expression (if in this form) is
 * matched against the candidate pathnames using the slower routines provided
 * in the standard 'find'.
 */

#include <sys/param.h>
#include <unistd.h>
#include <stdio.h>
#include "locate.h"
#include "pathnames.h"

FILE *fp;

main(argc, argv)
	int argc;
	char **argv;
{
	if (argc != 2) {
		(void)fprintf(stderr, "usage: locate pattern\n");
		exit(1);
	}
	if (!(fp = fopen(_PATH_FCODES, "r"))) {
		(void)fprintf(stderr, "locate: no database file %s.\n",
		    _PATH_FCODES);
		exit(1);
	}
	while (*++argv)
		fastfind(*argv);
	exit(0);
}

fastfind(pathpart)
	char *pathpart;
{
	register char *p, *s;
	register int c;
	int count, found, globflag;
	char *cutoff, *patend, *q, *index(), *patprep();
	char bigram1[NBG], bigram2[NBG], path[MAXPATHLEN];

	for (c = 0, p = bigram1, s = bigram2; c < NBG; c++)
		p[c] = getc(fp), s[c] = getc(fp);

	p = pathpart;
	globflag = index(p, '*') || index(p, '?') || index(p, '[');
	patend = patprep(p);

	found = 0;
	for (c = getc(fp), count = 0; c != EOF;) {
		count += ((c == SWITCH) ? getw(fp) : c) - OFFSET;
		/* overlay old path */
		for (p = path + count; (c = getc(fp)) > SWITCH;)
			if (c < PARITY)
				*p++ = c;
			else {		/* bigrams are parity-marked */
				c &= PARITY - 1;
				*p++ = bigram1[c], *p++ = bigram2[c];
			}
		*p-- = NULL;
		cutoff = (found ? path : path + count);
		for (found = 0, s = p; s >= cutoff; s--)
			if (*s == *patend) {	/* fast first char check */
				for (p = patend - 1, q = s - 1; *p != NULL;
				    p--, q--)
					if (*q != *p)
						break;
				if (*p == NULL) {	/* fast match success */
					found = 1;
					if (!globflag ||
					    fnmatch(pathpart, path, FNM_QUOTE))
						(void)printf("%s\n", path);
					break;
				}
			}
	}
}

/*
 * extract last glob-free subpattern in name for fast pre-match; prepend
 * '\0' for backwards match; return end of new pattern
 */
static char globfree[100];

char *
patprep(name)
	char *name;
{
	register char *endmark, *p, *subp;

	subp = globfree;
	*subp++ = '\0';
	p = name + strlen(name) - 1;
	/* skip trailing metacharacters (and [] ranges) */
	for (; p >= name; p--)
		if (index("*?", *p) == 0)
			break;
	if (p < name)
		p = name;
	if (*p == ']')
		for (p--; p >= name; p--)
			if (*p == '[') {
				p--;
				break;
			}
	if (p < name)
		p = name;
	/*
	 * if pattern has only metacharacters, check every path (force '/'
	 * search)
	 */
	if ((p == name) && index("?*[]", *p) != 0)
		*subp++ = '/';
	else {
		for (endmark = p; p >= name; p--)
			if (index("]*?", *p) != 0)
				break;
		for (++p;
		    (p <= endmark) && subp < (globfree + sizeof(globfree));)
			*subp++ = *p++;
	}
	*subp = '\0';
	return(--subp);
}
