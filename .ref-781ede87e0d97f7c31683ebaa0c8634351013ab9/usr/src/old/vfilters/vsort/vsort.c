/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vsort.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include <stdio.h>
/*
 * vsort - Sort troff output for versatec to reduce amount of reverse leading
 */ 

#define NULL 0

double atof();
char *calloc();

FILE *in,*out;

int skipfirst = 1;	/* skip the first leading so start at top of page */
int cpsize = 02;	/*  Funny sizes  */
struct point_sizes
	{
	int stupid_code;
	int real_code;
	} point_sizes[] =
		{
		010, 6,
		0, 7,
		01, 8,
		07, 9,
		02, 10,
		03, 11,
		04, 12,
		05, 14,
		0211, 16,
		06, 18,
		0212, 20,
		0213, 22,
		0214, 24,
		0215, 28,
		0216, 36,
		0, 0
		};

int	pagelength = 144 * 11;	/* in Leading units */
int	pagemod;		/* horizontal page number (for versatec) */
#define	MODOFF 3672		/* 432 * 8.5 */

int esc, lead, back, verd, mcase, railmag;
int col, row;
int pstart = 0;	/*  Means a startline is pending  */

int oback, omcase, orailmag, ocol, orow;
int opsize = 02;

struct lstate
	{
	int col;
	int psize;
	char railmag;
	char verd;
	char back;
	char mcase;
	};

struct line
	{
	struct line *nextp;
	struct line *lastp;
	int len;
	int row;
	struct lstate start;
	struct lstate end;
	char *codep;
	};

struct line *head;
struct line *tail;
struct line cline;

#define TBUFLEN 1024
char *codep;
char tbuf[TBUFLEN];

char	wide = 0;
char	nocutmarks = 0;	/* Remove lines that seem to be cut marks. */

#define	iscutmark(ch)	(ch == 023 || ch == 040 || ch == 061)
main(argc, argv) 
	int argc;
	char *argv[];
	{
	register i;

	for(i = 3; i < 15; i++)
		close(i);
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
			case 'l': {
					float f = 144 * atof(argv[1] + 2);
					if (f < 144) {
						error("bad length");
						exit(1);
					}
					pagelength = f;
					break;
			}

			case 'W':
				wide++;
				break;

			case 'c':
				nocutmarks++;
				break;
		}
		argc--; argv++;
	}
	out = stdout;
	if(argc > 1)
		{
		while(--argc)
			{
			argv++;
			if((in=fopen(argv[0], "r")) == NULL)
				perror("vsort");
			else {
			     ofile();
			     fclose(in);
			     }
			}
		}
	   else
		{
		in = stdin;
		ofile();
		}
	exit(0);
}

ofile()
	{
	register int c;
	static int initialized;

	while((c = getch()) != -1) {
		if(!c)
			continue;
		if(c & 0200)		/* escape (left/right) */
			{
			if(!pstart)
				stuffc(c);
			esc += (~c) & 0177;
			continue;
			}
		if(esc)
			{
			if(back)
				esc = -esc;
			col += esc;
			esc = 0;
			}
		if((c & 0377) < 0100)	/*  Purely for efficiency  */
			goto normal_char;
		switch(c) {
			case 0100:
				if(initialized++) {
					termline();
					linesflush();	/* Omit trailing leading. */
					return;
				}
				row = 0;
				col = 0;	esc = 0;
				lead = 0;
				verd = 0;	back = 0;	mcase = 0;
				railmag = 0;
				ocol = 0;
				orow = 0;
				oback = 0;	omcase = 0;
				orailmag = 0;
				if(loadfont(railmag, cpsize) < 0)
					error("init");
				startline();
				putc(0100, out);	/*  Dont stuff it guys  */
				break;
			case 0101:	/* lower rail */
				crail(railmag &= ~01);
				if(!pstart)
					stuffc(c);
				break;
			case 0102:	/* upper rail */
				crail(railmag |= 01);
				if(!pstart)
					stuffc(c);
				break;
			case 0103:	/* upper mag */
				crail(railmag |= 02);
				if(!pstart)
					stuffc(c);
				break;
			case 0104:	/* lower mag */
				crail(railmag &= ~02);
				if(!pstart)
					stuffc(c);
				break;
			case 0105:	/* lower case */
				mcase = 0;
				if(!pstart)
					stuffc(c);
				break;
			case 0106:	/* upper case */
				mcase = 1;
				if(!pstart)
					stuffc(c);
				break;
			case 0107:	/* escape forward */
				back = 0;
				if(!pstart)
					stuffc(c);
				break;
			case 0110:	/* escape backwards */
				back = 1;
				if(!pstart)
					stuffc(c);
				break;
			case 0111:	/* stop */
				stuffc(c);
				break;
			case 0112:	/* lead forward */
				verd = 0;
				break;
			case 0113:	/* undefined */
				break;
			case 0114:	/* lead backward */
				verd = 1;
				break;
			case 0115:	/* undefined */
			case 0116:
			case 0117:
				break;
			default:
				if((c & 0340) == 0140)	/* leading */
					{
					termline();
					lead = (~c) & 037;
					if(verd)
						lead = -lead;
					if (skipfirst > 0) {
						skipfirst--;
						continue;
					}
					row += lead;
					if (row >= pagelength) {
						if (wide) {
							if (pagemod == 3) {
								allflush();
								col %= MODOFF;
								pagemod = 0;
							} else {
								pagemod++;
								col += MODOFF;
								row -= pagelength;
							}
						} else {
							allflush();
						}
					}
					if (wide && row < 0 && pagemod) {
						pagemod--;
						col -= MODOFF;
						row += pagelength;
					}
					pstart++;
					continue;
				}
				if((c & 0360) == 0120)	/* size change */
				{
					if(!pstart)
						stuffc(c);
					col += stupidadj(c & 017, cpsize);
					loadfont(railmag, c & 017);
					continue;
				}
				if(c & 0300)
					continue;
			normal_char:
				c = (c & 077);
				stuffc(c);
		}
	}	/* End of while loop reading chars. */
	termline();
	linesflush();	/*  don't put out trailing leading. */
}


int peekc;
getch() {
	register c;
	if(peekc) {
		c = peekc;
		peekc = 0;
		return(c);
	}
	return(getc(in));
}

ungetc(c) {
	peekc = c;
}


error(s)
	char *s;
{

	fflush(out);
	fprintf(stderr, stderr, "vsort: %s\n", s);
}

crail(nrail)
	register int nrail;
{
	register int psize;

	psize = cpsize;
	loadfont(nrail, psize);
}

loadfont(fnum, size)
	register int fnum;
	register int size;
{

	cpsize = size;
	return(0);
}

startline()
{

	if(pstart != 0) {
		cline.row = row;
		return;
	}
	cline.len = 0;
	cline.row = row;
	cline.start.col = col;
	cline.start.psize = cpsize;
	cline.start.mcase = mcase;
	cline.start.back = back;
	cline.start.verd = verd;
	cline.start.railmag = railmag;
	codep = tbuf;
}

termline()
{
	register struct line *linep;
	register char *allp;
	register char *cp;
	int i;

	if(pstart != 0)
		return;
	if((allp = calloc(sizeof *linep,1)) == ((char *)-1))
		error("alloc");
	linep = (struct line *)allp;
	linep->end.col = col;
	linep->end.psize = cpsize;
	linep->end.mcase = mcase;
	linep->end.back = back;
	linep->end.verd = verd;
	linep->end.railmag = railmag;
	linep->start.col = cline.start.col;
	linep->start.psize = cline.start.psize;
	linep->start.mcase = cline.start.mcase;
	linep->start.back = cline.start.back;
	linep->start.verd = cline.start.verd;
	linep->start.railmag = cline.start.railmag;
	linep->len = cline.len;
	linep->row = row;
	if((allp = calloc(cline.len,1)) == ((char *)-1))
		error("alloc");
	linep->codep = allp;
	cp = tbuf;
	for(i = 0; i < cline.len; i++)
		*allp++ = *cp++;
	sortin(linep);
	}

sortin(linep)
	register struct line *linep;
{
	register struct line *clp;

	if((clp = tail) == NULL) {
		head = tail = linep;
		linep->lastp = linep->nextp = NULL;
		return;
	}
	while(clp != NULL && clp->row > linep->row)
		clp = clp->lastp;
	if(clp == tail) {
		linep->lastp = tail;
		linep->nextp = NULL;
		tail->nextp = linep;
		tail = linep;
	} else
		if(clp == NULL)	/*  goes at head of list  */ {
			linep->lastp = NULL;
			linep->nextp = head;
			head->lastp = linep;
			head = linep;
		} else {
			linep->lastp = clp;
			linep->nextp = clp->nextp;
			clp->nextp->lastp = linep;
			clp->nextp = linep;
		}
}

stuffc(code)
	register int code;
{

	if(pstart != 0) {
		pstart = 0;
		startline();
	}
	if(cline.len > TBUFLEN) {
		termline();
		startline();
	}
	*codep++ = code;
	cline.len++;
}


allflush()	/* Flush all lines, then put out trailing leading. */
{

	linesflush();
	if (row > orow) {
		ptlead(row - orow);
	}
	row -= pagelength;
	orow = row;
}

linesflush()
{
	while(head != NULL)
		sendline();
}

sendline()
{
	register char *cp;
	register struct line *linep;
	register int i;
	register int remcutmark;

	if ((linep = head) == NULL)
		return;

	/* Heuristic: if cut marks are present, they are on lines whose
	 * row numbers are <= 24 or >= pagelength-4.
	 * Cut marks are formed from 023's, 040's, or 061's.
	 * There are 2 or 4 of them on a line, and no other characters
	 * are present.  cutmark(...) checks this.
	 * Cutmark lines are removed if nocutmarks is true.
	 * Three leading units are removed, too, to compensate for
	 * varian output not being exactly 1584 leading units long.
	 */
#ifdef DUMPLINE
	dumpline(linep);
#endif
	remcutmark = 0;
	if (nocutmarks
 	    && (linep->row <= 24 || linep->row >= pagelength-4)
	    && cutmark(linep)) {
		remcutmark++;
		orow = orow + 3;
	}
	if(linep->start.railmag != orailmag)
		ptrail(linep->start.railmag);
	if(linep->start.psize != opsize)
		ptsize(linep->start.psize);
	if(linep->start.mcase != omcase)
		ptmcase();
	if(linep->row > orow)	/*  lead forward  */
		ptlead(linep->row - orow);
	if(linep->start.col != ocol)
		ptesc(linep->start.col-ocol);
	if(linep->start.back != oback)
		ptback();
	cp = linep->codep;
	if (remcutmark) {
		for(i = 0; i < linep->len; i++) {
			if (!iscutmark(*cp))	/* iscutmark is a macro. */
				putc(*cp++, out);
			else
				cp++;
		}
	} else {
		for(i = 0; i < linep->len; i++)
			putc(*cp++, out);
	}

	orow = linep->row;
	orailmag = linep->end.railmag;
	opsize = linep->end.psize;
	omcase = linep->end.mcase;
	ocol = linep->end.col;
	oback = linep->end.back;
	head = linep->nextp;

	cfree(linep->codep);
	cfree(linep);
	if(head == NULL)
		tail = NULL;
	else
		head->lastp = NULL;
}

int
cutmark(linep)
register struct line *linep;
{
	register int i;
	register int ch;
	register int dashcount = 0;
	register int firstdash = 0;

	for (i = 0; i < linep->len; i++) {
		ch = linep->codep[i] & 0377;
		if (ch < 0100) {
			if (iscutmark(ch)) {
					if (firstdash == 0)
						firstdash = ch;
					if (ch != firstdash)
						return (0);
					dashcount++;
			} else
				return(0);
		}
	}
	/* Must have 2 or 4 dashes on a line. */
	return (dashcount == 4 || dashcount == 2);
}

ptrail(rlmg)
	register int rlmg;
{

	if((rlmg & 01) != (orailmag & 01))
		putc((rlmg & 01) ? 0102:0101, out);	/*  rail  */
	if((rlmg & 02) != (orailmag & 02))
		putc((rlmg & 02) ? 0103:0104, out);	/*  mag  */
}

ptback()
{

	putc(oback ? 0107:0110, out);
	oback = !oback;
}

ptsize(size)
	register int size;
{

	putc(0120 | (size & 017), out);
	ptesc(-stupidadj(size, opsize));
}

stupidadj(code, lcode)
	register int code;
	int lcode;
{
	register struct point_sizes *psp;
	register struct point_sizes *lpsp;

	psp = point_sizes;
	while(psp->real_code != 0) {
		if((psp->stupid_code & 017) == code)
			break;
		psp++;
	}
	lpsp = point_sizes;
	while(lpsp->real_code != 0) {
		if((lpsp->stupid_code & 017) == lcode)
			break;
		lpsp++;
	}
	code = 0;
	if(!(lpsp->stupid_code & 0200) && (psp->stupid_code & 0200))
		code = -55;
	else
		if((lpsp->stupid_code & 0200) && !(psp->stupid_code & 0200))
			code = 55;
	return(code);
}

ptmcase()
{

	putc(omcase ? 0105:0106, out);
}

ptesc(escc)
	register int escc;
{

	if((escc < 0 && !oback ) || (escc >= 0 && oback))
		ptback();
	escc = abs(escc);
	while(escc > 0177) {
		putc(0200, out);
		escc -= 0177;
	}
	if(escc)
		putc(0200 | ((~escc) & 0177), out);
}

ptlead(leadd)
	register int leadd;
{

	while(leadd > 037) {
		putc(0140, out);
		leadd -= 037;
	}
	if(leadd)
		putc(0140 | ((~leadd) & 037), out);
}

#ifdef DUMPLINE
dumpline(linep)
register struct line *linep;
{
	int i;

	fprintf(stderr, "row: %d\n", linep->row);
	fprintf(stderr, "start.col: %o  ", linep->start.col & 0377);
	fprintf(stderr, ".psize: %o  ", linep->start.psize);
	fprintf(stderr, ".railmag: %o  ", linep->start.railmag);
	fprintf(stderr, ".verd: %o  ", linep->start.verd);
	fprintf(stderr, ".back: %o  ", linep->start.back);
	fprintf(stderr, ".mcase: %o\n", linep->start.mcase);
	fprintf(stderr, "  end.col: %o  ", linep->end.col);
	fprintf(stderr, ".psize: %o  ", linep->end.psize);
	fprintf(stderr, ".railmag: %o  ", linep->end.railmag);
	fprintf(stderr, ".verd: %o  ", linep->end.verd);
	fprintf(stderr, ".back: %o  ", linep->end.back);
	fprintf(stderr, ".mcase: %o\n", linep->end.mcase);
	fprintf(stderr, "len: %d\t", linep->len);
	fprintf(stderr, "codep: ");
	for (i = 0; i < linep->len; i++)
		fprintf(stderr, "%o ", linep->codep[i] & 0377);
	fprintf(stderr, "\n\n");
}
#endif
