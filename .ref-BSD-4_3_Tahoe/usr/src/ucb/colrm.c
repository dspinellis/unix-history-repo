/*
 * Copyright (c) 1980 Regents of the University of California.
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
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)colrm.c	5.3 (Berkeley) 6/29/88";
#endif /* not lint */

#include <stdio.h>
/*
COLRM removes unwanted columns from a file
	Jeff Schriebman  UC Berkeley 11-74
*/


main(argc,argv)
char **argv;
{
	register c, ct, first, last;

	first = 0;
	last = 0;
	if (argc > 1)
		first = getn(*++argv);
	if (argc > 2)
		last = getn(*++argv);

start:
	ct = 0;
loop1:
	c = getc(stdin);
	if (feof(stdin))
		goto fin;
	if (c == '\t')
		ct = (ct + 8) & ~7;
	else if (c == '\b')
		ct = ct ? ct - 1 : 0;
	else
		ct++;
	if (c == '\n') {
		putc(c, stdout);
		goto start;
	}
	if (!first || ct < first) {
		putc(c, stdout);
		goto loop1;
	}

/* Loop getting rid of characters */
	while (!last || ct < last) {
		c = getc(stdin);
		if (feof(stdin))
			goto fin;
		if (c == '\n') {
			putc(c, stdout);
			goto start;
		}
		if (c == '\t')
			ct = (ct + 8) & ~7;
		else if (c == '\b')
			ct = ct ? ct - 1 : 0;
		else
			ct++;
	}

/* Output last of the line */
	for (;;) {
		c = getc(stdin);
		if (feof(stdin))
			break;
		putc(c, stdout);
		if (c == '\n')
			goto start;
	}
fin:
	fflush(stdout);
	exit(0);
}

getn(ap)
char *ap;
{
	register int n,c;
	register char *p;

	p = ap;
	n = 0;
	while ((c = *p++) >= '0' && c <= '9')
		n = n*10 + c - '0';
	return(n);
}
