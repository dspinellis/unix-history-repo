/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)rs.c	4.3	(Berkeley)	%G%";
#endif not lint

/*
 *	rs - reshape a data array
 *	Author:  John Kunze, Office of Comp. Affairs, UCB
 *		BEWARE: lots of unfinished edges
 */

#include <stdio.h>
#include <ctype.h>

long	flags;
#define	TRANSPOSE	000001
#define	MTRANSPOSE	000002
#define	ONEPERLINE	000004
#define	ONEISEPONLY	000010
#define	ONEOSEPONLY	000020
#define	NOTRIMENDCOL	000040
#define	SQUEEZE		000100
#define	SHAPEONLY	000200
#define	DETAILSHAPE	000400
#define	RIGHTADJUST	001000
#define	NULLPAD		002000
#define	RECYCLE		004000
#define	SKIPPRINT	010000
#define	ICOLBOUNDS	020000
#define	OCOLBOUNDS	040000
#define ONEPERCHAR	0100000
#define NOARGS		0200000

char	buf[BUFSIZ];
short	*colwidths;
short	*cord;
short	*icbd;
short	*ocbd;
int	nelem;
char	**elem;
char	**endelem;
char	*curline;
int	allocsize = BUFSIZ;
int	curlen;
int	irows, icols;
int	orows, ocols;
int	maxlen;
int	skip;
int	propgutter;
char	isep = ' ', osep = ' ';
int	owidth = 80, gutter = 2;

char	**getptrs();

main(argc, argv)
int	argc;
char	**argv;
{
	setbuf(stdout, buf);
	getargs(argc, argv);
	getfile();
	if (flags & SHAPEONLY) {
		printf("%d %d\n", irows, icols);
		exit(0);
	}
	prepfile();
	/*fprintf(stderr, "#irows %d icols %d orows %d ocols %d\n",irows,icols,orows,ocols);*/
	putfile();
	exit(0);
}

getfile()
{
	register char	*p;
	register char	*endp;
	register char	**ep = 0;
	int	multisep = (flags & ONEISEPONLY ? 0 : 1);
	int	nullpad = flags & NULLPAD;
	char	**padto;

	while (skip--) {
		getline();
		if (flags & SKIPPRINT)
			puts(curline);
	}
	getline();
	if (flags & NOARGS && curlen < owidth)
		flags |= ONEPERLINE;
	if (flags & ONEPERLINE)
		icols = 1;
	else				/* count cols on first line */
		for (p = curline, endp = curline + curlen; p < endp; p++) {
			if (*p == isep && multisep)
				continue;
			icols++;
			while (*p && *p != isep)
				p++;
		}
	ep = getptrs(elem);
	p = curline;
	do {
		if (flags & ONEPERLINE) {
			*ep++ = curline;
			if (maxlen < curlen)
				maxlen = curlen;
			irows++;
			continue;
		}
		for (p = curline, endp = curline + curlen; p < endp; p++) {
			if (*p == isep && multisep)
				continue;	/* eat up column separators */
			if (*p == isep)		/* must be an empty column */
				*ep = "";
			else			/* store column entry */
				*ep = p;
			while (p < endp && *p != isep)
				p++;		/* find end of entry */
			*p = '\0';		/* mark end of entry */
			if (maxlen < p - *ep)	/* update maxlen */
				maxlen = p - *ep;
			ep++;			/* prepare for next entry */
		}
		irows++;			/* update row count */
		if (nullpad) {			/* pad missing entries */
			padto = elem + irows * icols;
			while  (ep < padto)
				*ep++ = "";
		}
	if (ep > endelem)			/* if low on pointers */
		ep = getptrs(ep);		/* get some more */
	} while (getline() != EOF);
	*ep = 0;				/* mark end of pointers */
	nelem = ep - elem;
}

putfile()
{
	register char	**ep;
	register int	i;
	register int	j;

	ep = elem;
	if (flags & TRANSPOSE)
		for (i = 0; i < orows; i++) {
			for (j = i; j < nelem; j += orows)
				prints(ep[j], (j - i) / orows);
			putchar('\n');
		}
	else
		for (i = 0; i < orows; i++) {
			for (j = 0; j < ocols; j++)
				prints(*ep++, j);
			putchar('\n');
		}
}

prints(s, col)
char	*s;
int	col;
{
	register char	*p = s;
	register int	n;

	while (*p)
		p++;
	n = (flags & ONEOSEPONLY ? 1 : colwidths[col] - (p - s));
	if (flags & RIGHTADJUST)
		while (n-- > 0)
			putchar(osep);
	for (p = s; *p; p++)
		putchar(*p);
	while (n-- > 0)
		putchar(osep);
}

error(msg, s)
char	*msg;
char	*s;
{
	fprintf(stderr, "rs:  ");
	fprintf(stderr, msg, s);
	fprintf(stderr, "\nUsage:  rs [ -[csCS][x][kKgGw][N]tTeEnyjhHm ] [ rows [ cols ] ]\n");
	exit(1);
}

prepfile()
{
	register char	**ep;
	register int 	i;
	register int 	j;
	char	**lp;
	int	colw;
	int	max = 0;
	int	n;

	if (!nelem)
		exit(0);
	gutter += maxlen * propgutter / 100.0;
	colw = maxlen + gutter;
	if (flags & MTRANSPOSE) {
		orows = icols;
		ocols = irows;
	}
	else if (orows == 0 && ocols == 0) {	/* decide rows and cols */
		ocols = owidth / colw;
		if (ocols == 0)
			fprintf(stderr, "Display width %d is less than column width %d\n", owidth, colw);
		if (ocols > nelem)
			ocols = nelem;
		orows = nelem / ocols + (nelem % ocols ? 1 : 0);
	}
	else if (orows == 0)			/* decide on rows */
		orows = nelem / ocols + (nelem % ocols ? 1 : 0);
	else if (ocols == 0)			/* decide on cols */
		ocols = nelem / orows + (nelem % orows ? 1 : 0);
	lp = elem + orows * ocols;
	while (lp > endelem) {
		getptrs(elem + nelem);
		lp = elem + orows * ocols;
	}
	if (flags & RECYCLE) {
		for (ep = elem + nelem; ep < lp; ep++)
			*ep = *(ep - nelem);
		nelem = lp - elem;
	}
	if (!(colwidths = (short *) malloc(ocols * sizeof(short))))
		error("malloc:  No gutter space", "");
	if (flags & SQUEEZE) {
		if (flags & TRANSPOSE)
			for (ep = elem, i = 0; i < ocols; i++) {
				for (j = 0; j < orows; j++)
					if ((n = strlen(*ep++)) > max)
						max = n;
				colwidths[i] = max + gutter;
			}
		else
			for (i = 0; i < ocols; i++) {
				for (j = i; j < nelem; j += ocols)
					if ((n = strlen(ep[j])) > max)
						max = n;
				colwidths[i] = max + gutter;
			}
	}
	/*	for (i = 0; i < orows; i++) {
			for (j = i; j < nelem; j += orows)
				prints(ep[j], (j - i) / orows);
			putchar('\n');
		}
	else
		for (i = 0; i < orows; i++) {
			for (j = 0; j < ocols; j++)
				prints(*ep++, j);
			putchar('\n');
		}*/
	else
		for (i = 0; i < ocols; i++)
			colwidths[i] = colw;
	if (!(flags & NOTRIMENDCOL)) {
		if (flags & RIGHTADJUST)
			colwidths[0] -= gutter;
		else
			colwidths[ocols - 1] = 0;
	}
	n = orows * ocols;
	if (n > nelem && (flags & RECYCLE))
		nelem = n;
	/*for (i = 0; i < ocols; i++)
		fprintf(stderr, "%d ",colwidths[i]);
	fprintf(stderr, "is colwidths, nelem %d\n", nelem);*/
}

#define	BSIZE	2048
char	ibuf[BSIZE];		/* two screenfuls should do */

getline()	/* get line; maintain curline, curlen; manage storage */
{
	register char	*p;
	register int	c;
	register int	i;
	static	int	putlength;
	static	char	*endblock = ibuf + BSIZE;

	if (!irows) {
		curline = ibuf;
		putlength = flags & DETAILSHAPE;
	}
	else if (skip <= 0) {			/* don't waste storage */
		curline += curlen + 1;
		if (putlength)		/* print length, recycle storage */
			printf(" %d line %d\n", curlen, irows);
	}
	if (!putlength && endblock - curline < BUFSIZ) {   /* need storage */
		/*ww = endblock-curline; tt += ww;*/
		/*printf("#wasted %d total %d\n",ww,tt);*/
		if (!(curline = (char *) malloc(BSIZE)))
			error("File too large", "");
		endblock = curline + BSIZE;
		/*printf("#endb %d curline %d\n",endblock,curline);*/
	}
	for (p = curline, i = 1; i < BUFSIZ; *p++ = c, i++)
		if ((c = getchar()) == EOF || c == '\n')
			break;
	*p = '\0';
	curlen = i - 1;
	return(c);
}

char	**
getptrs(sp)
char	**sp;
{
	register char	**p;
	register char	**ep;

	for (;;) {
		allocsize += allocsize;
		if (!(p = (char **) malloc(allocsize * sizeof(char *)))) {
			perror("rs");
			exit(1);
		}
		if ((endelem = p + allocsize - icols) <= p) {
			free(p);
			continue;
		}
		if (elem != 0)
			free(elem);
		ep = elem;
		elem = p;
		while (ep < sp)
			*p++ = *ep++;
		return(p);
	}
}

getargs(ac, av)
int	ac;
char	**av;
{
	register char	*p;
	char	*getnum(), *getlist();

	if (ac == 1) {
		flags |= NOARGS | TRANSPOSE;
	}
	while (--ac && **++av == '-')
		for (p = *av+1; *p; p++)
			switch (*p) {
			case 'T':
				flags |= MTRANSPOSE;
			case 't':
				flags |= TRANSPOSE;
				break;
			case 'c':		/* input col. separator */
				flags |= ONEISEPONLY;
			case 's':		/* one or more allowed */
				if (p[1])
					isep = *++p;
				else
					isep = '\t';	/* default is ^I */
				break;
			case 'C':
				flags |= ONEOSEPONLY;
			case 'S':
				if (p[1])
					osep = *++p;
				else
					osep = '\t';	/* default is ^I */
				break;
			case 'w':		/* window width, default 80 */
				p = getnum(&owidth, p, 0);
				if (owidth <= 0)
					error("Width must be a positive integer", "");
				break;
			case 'K':			/* skip N lines */
				flags |= SKIPPRINT;
			case 'k':			/* skip, do not print */
				p = getnum(&skip, p, 0);
				if (!skip)
					skip = 1;
				break;
			case 'm':
				flags |= NOTRIMENDCOL;
				break;
			case 'g':		/* gutter space */
				p = getnum(&gutter, p, 0);
				break;
			case 'G':
				p = getnum(&propgutter, p, 0);
				break;
			case 'e':		/* each line is an entry */
				flags |= ONEPERLINE;
				break;
			case 'E':
				flags |= ONEPERCHAR;
				break;
			case 'j':			/* right adjust */
				flags |= RIGHTADJUST;
				break;
			case 'n':	/* null padding for missing values */
				flags |= NULLPAD;
				break;
			case 'y':
				flags |= RECYCLE;
				break;
			case 'H':			/* print shape only */
				flags |= DETAILSHAPE;
			case 'h':
				flags |= SHAPEONLY;
				break;
			case 'z':			/* squeeze col width */
				flags |= SQUEEZE;
				break;
			/*case 'p':
				ipagespace = atoi(++p);	(default is 1)
				break;*/
			case 'o':			/* col order */
				p = getlist(&cord, p);
				break;
			case 'b':
				flags |= ICOLBOUNDS;
				p = getlist(&icbd, p);
				break;
			case 'B':
				flags |= OCOLBOUNDS;
				p = getlist(&ocbd, p);
				break;
			default:
				error("Bad flag:  %.1s", p);
			}
	/*if (!osep)
		osep = isep;*/
	switch (ac) {
	/*case 3:
		opages = atoi(av[2]);*/
	case 2:
		ocols = atoi(av[1]);
	case 1:
		orows = atoi(av[0]);
	case 0:
		break;
	default:
		error("Too many arguments.  What do you mean by `%s'?", av[3]);
	}
}

char	*
getlist(list, p)
short	**list;
char	*p;
{
	register char	*t;
	register int	count = 1;

	for (t = p + 1; *t; t++) {
		if (!isdigit(*t))
			error("Option %.1s requires a list of unsigned numbers separated by commas", t);
		count++;
		while (*t && isdigit(*t))
			t++;
		if (*t != ',')
			break;
	}
	if (!(*list = (short *) malloc(count * sizeof(short))))
		error("No list space", "");
	count = 0;
	for (t = p + 1; *t; t++) {
		(*list)[count++] = atoi(t);
		printf("++ %d ", (*list)[count-1]);
		fflush(stdout);
		while (*t && isdigit(*t))
			t++;
		if (*t != ',')
			break;
	}
	(*list)[count] = 0;
	return(t - 1);
}

char	*
getnum(num, p, strict)	/* num = number p points to; if (strict) complain */
int	*num;				/* returns pointer to end of num */
char	*p;
int	strict;
{
	register char	*t = p;

	if (!isdigit(*++t)) {
		if (strict || *t == '-' || *t == '+')
			error("Option %.1s requires an unsigned integer", p);
		*num = 0;
		return(p);
	}
	*num = atoi(t);
	while (*++t)
		if (!isdigit(*t))
			break;
	return(--t);
}
