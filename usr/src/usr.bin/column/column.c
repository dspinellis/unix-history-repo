/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)column.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <strings.h>

int termwidth = 80;		/* default terminal width */

char **list;			/* array of pointers to records */
int entries;			/* number of records */
int maxlength;			/* longest record */

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int errno, optind;
	struct winsize win;
	FILE *fp;
	int ch, xflag;
	char *p, *getenv();

	if (ioctl(1, TIOCGWINSZ, &win) == -1 || !win.ws_col) {
		if (p = getenv("COLUMNS"))
			termwidth = atoi(p);
	} else
		termwidth = win.ws_col;

	xflag = 0;
	while ((ch = getopt(argc, argv, "c:x")) != EOF)
		switch(ch) {
		case 'c':
			termwidth = atoi(optarg);
			break;
		case 'x':
			xflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	fp = stdin;
	do {
		if (*argv)
			if (!(fp = fopen(*argv, "r"))) {
				(void)fprintf(stderr, "column: %s: %s\n",
				    *argv, strerror(errno));
				continue;
			} else
				++argv;
		input(fp);
		if (fp != stdin)
			(void)fclose(fp);
	} while (*argv);

	if (!entries)
		exit(0);

	if (maxlength >= termwidth)
		print();
	else if (xflag)
		c_columnate();
	else
		r_columnate();
	exit(0);
}

#define	TAB	8
c_columnate()
{
	register int chcnt, col, cnt, numcols;
	int endcol;
	char **lp;

	maxlength = (maxlength + TAB) & ~(TAB - 1);
	numcols = termwidth / maxlength;
	endcol = maxlength;
	for (chcnt = col = 0, lp = list;; ++lp) {
		chcnt += printf("%s", *lp);
		if (!--entries)
			break;
		if (++col == numcols) {
			chcnt = col = 0;
			endcol = maxlength;
			putchar('\n');
		} else {
			while ((cnt = (chcnt + TAB & ~(TAB - 1))) <= endcol) {
				(void)putchar('\t');
				chcnt = cnt;
			}
			endcol += maxlength;
		}
	}
	if (chcnt)
		putchar('\n');
}

r_columnate()
{
	register int base, chcnt, cnt, col;
	int endcol, numcols, numrows, row;

	maxlength = (maxlength + TAB) & ~(TAB - 1);
	numcols = termwidth / maxlength;
	numrows = entries / numcols;
	if (entries % numcols)
		++numrows;

	for (row = 0; row < numrows; ++row) {
		endcol = maxlength;
		for (base = row, chcnt = col = 0; col < numcols; ++col) {
			chcnt += printf("%s", list[base]);
			if ((base += numrows) >= entries)
				break;
			while ((cnt = (chcnt + TAB & ~(TAB - 1))) <= endcol) {
				(void)putchar('\t');
				chcnt = cnt;
			}
			endcol += maxlength;
		}
		putchar('\n');
	}
}

print()
{
	register int cnt;
	register char **lp;

	for (cnt = entries, lp = list; cnt--; ++lp)
		printf("%s\n", *lp);
}

#define	DEFNUM		1000
#define	MAXLINELEN	(2048 + 1)

input(fp)
	register FILE *fp;
{
	static u_int maxentry;
	register int len;
	register char *p;
	char buf[MAXLINELEN], *malloc(), *realloc();

	if (!list &&
	    !(list = (char **)malloc((maxentry = DEFNUM) * sizeof(char *))))
		nomem();
	while (fgets(buf, MAXLINELEN, fp)) {
		if (buf[0] == '\n')
			continue;
		if (entries == maxentry) {
			maxentry += DEFNUM;
			if (!(list =
			    (char **)realloc(list, maxentry * sizeof(char *))))
				nomem();
		}
		if (p = index(buf, '\n')) {
			*p = '\0';
			len = p - buf;
		}
		else
			len = strlen(buf);
		if (maxlength < len)
			maxlength = len;
		list[entries++] = strdup(buf);
	}
}

nomem()
{
	(void)fprintf(stderr, "column: out of memory.\n");
	exit(1);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: column [-x] [-c columns] [file ...]\n");
	exit(1);
}
