/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)column.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void  c_columnate __P((void));
void *emalloc __P((int));
void  input __P((FILE *));
void  maketbl __P((void));
void  nomem __P((void));
void  print __P((void));
void  r_columnate __P((void));
void  usage __P((void));

int termwidth = 80;		/* default terminal width */

int entries;			/* number of records */
int eval;			/* exit value */
int maxlength;			/* longest record */
char **list;			/* array of pointers to records */
char *separator = "\t ";	/* field separator for table option */

int
main(argc, argv)
	int argc;
	char **argv;
{
	struct winsize win;
	FILE *fp;
	int ch, tflag, xflag;
	char *p;

	if (ioctl(1, TIOCGWINSZ, &win) == -1 || !win.ws_col) {
		if (p = getenv("COLUMNS"))
			termwidth = atoi(p);
	} else
		termwidth = win.ws_col;

	tflag = xflag = 0;
	while ((ch = getopt(argc, argv, "c:s:tx")) != EOF)
		switch(ch) {
		case 'c':
			termwidth = atoi(optarg);
			break;
		case 's':
			separator = optarg;
			break;
		case 't':
			tflag = 1;
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

	if (!*argv)
		input(stdin);
	else for (; *argv; ++argv)
		if (fp = fopen(*argv, "r")) {
			input(fp);
			(void)fclose(fp);
		} else {
			(void)fprintf(stderr, "column: %s: %s\n", *argv,
			    strerror(errno));
			eval = 1;
		}

	if (!entries)
		exit(eval);

	if (tflag)
		maketbl();
	else if (maxlength >= termwidth)
		print();
	else if (xflag)
		c_columnate();
	else
		r_columnate();
	exit(eval);
}

#define	TAB	8
void
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

void
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

void
print()
{
	register int cnt;
	register char **lp;

	for (cnt = entries, lp = list; cnt--; ++lp)
		(void)printf("%s\n", *lp);
}

typedef struct _tbl {
	char **list;
	int cols, *len;
} TBL;
#define	DEFCOLS	25

void
maketbl()
{
	register TBL *t;
	register int coloff, cnt;
	register char *p, **lp;
	int *lens, maxcols;
	TBL *tbl;
	char **cols;

	t = tbl = emalloc(entries * sizeof(TBL));
	cols = emalloc((maxcols = DEFCOLS) * sizeof(char *));
	lens = emalloc(maxcols * sizeof(int));
	for (cnt = 0, lp = list; cnt < entries; ++cnt, ++lp, ++t) {
		for (coloff = 0, p = *lp; cols[coloff] = strtok(p, separator);
		    p = NULL)
			if (++coloff == maxcols) {
				if (!(cols = realloc(cols, (u_int)maxcols +
				    DEFCOLS * sizeof(char *))) ||
				    !(lens = realloc(lens,
				    (u_int)maxcols + DEFCOLS * sizeof(int))))
					nomem();
				bzero((char *)lens + maxcols * sizeof(int),
				    DEFCOLS * sizeof(int));
				maxcols += DEFCOLS;
			}
		t->list = emalloc(coloff * sizeof(char *));
		t->len = emalloc(coloff * sizeof(int));
		for (t->cols = coloff; --coloff >= 0;) {
			t->list[coloff] = cols[coloff];
			t->len[coloff] = strlen(cols[coloff]);
			if (t->len[coloff] > lens[coloff])
				lens[coloff] = t->len[coloff];
		}
	}
	for (cnt = 0, t = tbl; cnt < entries; ++cnt, ++t) {
		for (coloff = 0; coloff < t->cols  - 1; ++coloff)
			(void)printf("%s%*s", t->list[coloff],
			    lens[coloff] - t->len[coloff] + 2, " ");
		(void)printf("%s\n", t->list[coloff]);
	}
}

#define	DEFNUM		1000
#define	MAXLINELEN	(2048 + 1)

void
input(fp)
	register FILE *fp;
{
	static int maxentry;
	register int len;
	register char *p;
	char buf[MAXLINELEN];

	if (!list)
		list = emalloc((maxentry = DEFNUM) * sizeof(char *));
	while (fgets(buf, MAXLINELEN, fp)) {
		for (p = buf; *p && isspace(*p); ++p);
		if (!*p)
			continue;
		if (!(p = index(p, '\n'))) {
			(void)fprintf(stderr, "column: line too long.\n");
			eval = 1;
			continue;
		}
		*p = '\0';
		len = p - buf;
		if (maxlength < len)
			maxlength = len;
		if (entries == maxentry) {
			maxentry += DEFNUM;
			if (!(list = realloc(list,
			    (u_int)maxentry * sizeof(char *))))
				nomem();
		}
		list[entries++] = strdup(buf);
	}
}

void *
emalloc(size)
	int size;
{
	char *p;

	if (!(p = malloc(size)))
		nomem();
	bzero(p, size);
	return(p);
}

void
nomem()
{
	(void)fprintf(stderr, "column: out of memory.\n");
	exit(1);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: column [-tx] [-c columns] [file ...]\n");
	exit(1);
}
