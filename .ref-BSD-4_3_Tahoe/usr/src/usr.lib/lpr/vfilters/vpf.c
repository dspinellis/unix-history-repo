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
static char sccsid[] = "@(#)vpf.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

/*
 * Varian/Versatec printer filter
 */

#include <signal.h>
#include <stdio.h>
#include <sys/vcmd.h>

#define LINELN 440

int	pltmode[] = {VPLOT};
int	prtmode[] = {VPRINT};
char	linebuf[LINELN+1];
char	ovbuf[LINELN];
int	ov;
int	lineno;
int	varian = 1;	/* default is the varian */
int	width = 132;	/* default line length */
int	indent = 0;	/* default indent length */
int	length = 58;	/* 80 for 11" long paper */
int	npages = 1;
int	literal;
char	*name;		/* user's login name */
char	*host;		/* user's machine name */
char	*acctfile;	/* accounting information file */

main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;

	if (argv[0][strlen(argv[0])-1] == 'W') { /* Wide: the versatec. */
		varian = 0;
		width = 440;
		length = 66;
	}

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
				if ((i = atoi(&argv[0][2])) > 0 && i < LINELN)
					width = i;
				break;

			case 'l':
				length = atoi(&argv[0][2]);
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
	/*
	 * device should be open on file descriptor 1.
	 */
	ioctl(1, VSETSTATE, prtmode);
	send();
	if (name && acctfile && access(acctfile, 02) >= 0 &&
	    freopen(acctfile, "a", stdout) != NULL) {
		printf("%7.2f\t%s:%s\n", (float)npages, host, name);
	}
	exit(0);
}

send()
{
	lineno = 0;
	while (getline()) {
		if (varian && !literal && lineno >= length) {
			putline(1);
			lineno = 0;
		} else
			putline(0);
	}
	if (varian && lineno) {
		putchar('\f');	/* be sure to end on a page boundary */
		npages++;
	}
	/*
	 * Put out an extra null to ensure varian will get an even
	 * number of good characters.
	 */
	putchar('\0');
}

getline()
{
	register col, maxcol, c;

	ov = 0;
	for (col = 0; col < width; col++) {
		linebuf[col] = ' ';
		ovbuf[col] = 0;
	}
	col = indent;
	maxcol = 0;
	for (;;) switch (c = getchar()) {

	case EOF:
		return(0);

	case '\031':
		/*
		 * lpd needs to use a different filter to print data so
		 * stop what we are doing and wait for lpd to restart us.
		 */
		if ((c = getchar()) == '\1') {
			putchar('\0');		/* make sure even # sent */
			fflush(stdout);
			kill(getpid(), SIGSTOP);
			ioctl(1, VSETSTATE, prtmode);
			continue;
		}
		ungetc(c, stdin);
		c = '\031';
		/* fall through if not stop sequence */
	default:
		if (c >= ' ' || literal) {
			if (col < width) {
				if (linebuf[col] == '_') {
					ovbuf[col] = 0377;
					ov++;
				}
				linebuf[col++] = c;
				if (col > maxcol)
					maxcol = col;
			} else
				col++;
		}
		continue;

	case ' ':
		col++;
		continue;

	case '\t':
		col = (col|07) + 1;
		continue;

	case '\r':
		col = 0;
		continue;

	case '_':
		if (col < width) {
			if (linebuf[col] != ' ') {
				ovbuf[col] = 0377;
				ov++;
			} else
				linebuf[col] = c;
			col++;
			if (col > maxcol)
				maxcol = col;
		} else
			col++;
		continue;

	case '\f':
		/* Fall through, treating a ff as a line break, too... */
		lineno = length - 1;
	case '\n':
		if (maxcol > width)
			maxcol = width;
		linebuf[maxcol] = '\0';
		if (++lineno % length == 0)
			npages++;
		return(1);

	case '\b':
		if (col > 0)
			col--;
		continue;
	}
}

putline(ff)
int ff;
{
	register char *lp;
	register c, i;

	lp = linebuf;
	while (c = *lp++)
		putchar(c);
	if (ov) {
		putchar('\n');
		putchar('\0');
		fflush(stdout);
		ioctl(1, VSETSTATE, pltmode);
		for (lp = ovbuf, i = ov; ov--; ) {
			putchar(*lp & 0377);
			putchar(*lp++ & 0377);
		}
		if (ov & 1)
			putchar('\0');
		fflush(stdout);
		ioctl(1, VSETSTATE, prtmode);
	}
	if (ff)
		putchar('\f');
	else if (ov == 0)
		putchar('\n');
	if (ferror(stdout))
		exit(1);
}
