/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cfgets.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * get nonblank, non-comment, (possibly continued) line. Alan S. Watt 
 */

#include <stdio.h>
#define COMMENT		'#'
#define CONTINUE	'\\'
#define EOLN		'\n'
#define EOS		'\0'

/*LINTLIBRARY*/

char *
cfgets(buf, siz, fil)
register char *buf;
int siz;
FILE *fil;
{
	register char *s;
	register i, c, len;
	char *fgets();

	for (i=0,s=buf; i = (fgets(s, siz-i, fil) != NULL); i = s - buf) {

		/* get last character of line */
		c = s[len = (strlen(s) - 1)];

		/* skip comments; make sure end of comment line seen */
		if (*s == COMMENT) {
			while (c != EOLN && c != EOF)
				c = getc(fil);
			*s = EOS;
		}

		/* skip blank lines */
		else if (*s != EOLN) {
			s += len;

			/* continue lines ending with CONTINUE */
			if (c != EOLN || *--s != CONTINUE)
				break;
		}
	}
	
	return i ? buf : NULL;
}

#ifdef TEST
main()
{
	char buf[512];

	while (cfgets(buf, sizeof buf, stdin))
		fputs(buf, stdout);
}
#endif TEST
