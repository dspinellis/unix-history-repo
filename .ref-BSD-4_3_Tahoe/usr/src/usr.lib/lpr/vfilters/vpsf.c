/*
 * Copyright (c) 1983 Regents of the University of California.
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
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vpsf.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

/*
 * Versatec printer filter
 * 	make wide listings by placing pages side by side
 */

#include <stdio.h>
#include <sys/vcmd.h>

#define	LINELN 440
#define	PAGELN  86
#define	LMARG   10

int	pltmode[] = {VPLOT};
int	prtmode[] = {VPRINT};

char	screen[PAGELN][LINELN];
char	ul[PAGELN][LINELN];
char	anyul[PAGELN];
int	origin;		/* first column of a page */
int	origin_ind;	/* origin plus indent */
int	outline;	/* current line number */
int	outcol;		/* current column number */
int	npages;
int	width = 106;	/* default page width */
int	length = 86;	/* default page length */
int	indent = 0;	/* default indent */

int	literal;
char	*name;		/* user's login name */
char	*host;		/* user's machine name */
char	*acctfile;	/* accounting information file */

main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;

	while (--argc) {
		if (*(*++argv) == '-') {
			switch (argv[0][1]) {
			case 'n':
				argc--;
				name = *++argv;
				break;

			case 'h':
				argc--;
				host = *++argv;
				break;

			case 'w':
				if ((i = atoi(&argv[0][2])) > 0 && i <= LINELN)
					width = i;
				break;

			case 'l':
				if ((i = atoi(&argv[0][2])) > 0 && i <= PAGELN)
					length = i;
				break;

			case 'i':
				if ((i = atoi(&argv[0][2])) >= 0 &&
				    i < LINELN - 1)
					indent = i;
				break;

			case 'c':	/* Print input without throwing away
					   control chars and without putting
					   in page breaks. */
				literal++;
				break;
			}
		} else
			acctfile = *argv;
	}
	indent += literal ? 1 : LMARG;
	if (indent >= width)
		indent = width - 1;

	/*
	 * input file is open on file descriptor 0.
	 * vp should be open on file descriptor 1.
	 * The error log file is open on file descriptor 2.
	 */
	ioctl(1, VSETSTATE, prtmode);
	process();

	/*
	 * Put out an extra null to ensure versatec will get an even
	 * number of good characters.
	 */
	putchar('\0');

	if (ferror(stdout))
		exit(1);
	if (name && acctfile && access(acctfile, 02) >= 0 &&
	    freopen(acctfile, "a", stdout) != NULL) {
		if (host)
			printf("%7.2f\t%s:%s\n", (float)npages, host, name);
		else
			printf("%7.2f\t%s\n", (float)npages, name);
	}
	exit(0);
}

set_up()
{
	clear(screen, sizeof(screen));
	origin = 0;
	origin_ind = outcol = origin + indent;
	outline = 0;
	cutmark(origin);
}

process()
{
	register int c;

	set_up();

	while ((c = getchar()) != EOF)
		switch (c) {
		case ' ':
			outcol++;
			break;

		case '\t':
			outcol = ((outcol - origin_ind) | 07) + origin_ind + 1;
			break;

		case '\b':
			if (outcol > origin_ind)
				outcol--;
			break;

		case '\r':
			outcol = origin_ind;
			break;

		case '\f':
			outline = length;
			/* fall into ... */

		case '\n':
			if (++outline >= length) {
				origin += width + 1;
 				origin_ind += width + 1;
				cutmark(origin);
				if (origin + width + 1 >= LINELN) {
					oflush();
					break;
				}
				outline = 0;
			}
			outcol = origin_ind;
			break;

		default:
			outchar(c);
			break;
		}

	if (outline || origin) {
		cutmark(origin + width + 1);
		oflush();
	}
	printf("\n\n\n\n\n");
}

outchar(c)
	register int c;
{
	register char *cp;
	register int d;

	if (!literal && (c < 040 || c >= 0177))
		return;
	if (outcol >= origin + width + 1) {
		outcol++;
		return;
	}
	cp = &screen[outline][outcol];
	d = *cp;
	if (d != ' ') {
		if (d == '_' || c == '_') {
			if (c == d) {
				outcol++;
				return;
			}
			if (anyul[outline] == 0)
				clear(ul[outline], LINELN);
			anyul[outline] = 1;
			ul[outline][outcol] = 0377;
			if (c == '_')
				c = d;
		}
	}
	*cp = c;
	outcol++;
}

oflush()
{
	register char *cp, *dp;
	register int i, j, oc, dc, c;

	npages++;
	putchar('\n');
	for (i = 0; i < length; i++)
		putline(i);
	for (i = 0; i < LINELN; i++)
		putchar('_');
	putchar('\n');

	set_up();
}

clear(cp, i)
	register char *cp;
	register int i;
{
	if (i > 0)
		do
			*cp++ = ' ';
		while (--i);
}

cutmark(o)
	register int o;
{
	register int i;

	screen[0][o] = '|';
	screen[1][o] = '|';
	screen[length - 1][o] = '|';
	screen[length - 2][o] = '|';
}

putline(n)
	register int n;
{
	register char *cp;
	register int j;

	fwrite(screen[n], sizeof(char), sizeof(screen[0]), stdout);
	if (anyul[n]) {
		putchar('\n');
		putchar('\0');
		fflush(stdout);
		ioctl(1, VSETSTATE, pltmode);
		cp = ul[n];
		j = LINELN;
		do {
			putchar(*cp & 0377);
			putchar(*cp++ & 0377);
		} while (--j);
		fflush(stdout);
		ioctl(1, VSETSTATE, prtmode);
	} else
		putchar('\n');
	if (ferror(stdout))
		exit(1);
}
