# include <stdio.h>
/*
 * vsort - Sort troff output for versatec to reduce amount of reverse leading
 */ 

#define cycle for(;;)
#define NULL 0

#define INTR	2
#define QUIT	3
#define HUP	1
#define BROKENPIPE 13

double atof();

FILE *inbuf,*outbuf;

int skmagic = 1;	/* skip the first leading so start at top of page */
int cpsize = 02;	/*  Funny sizes  */
struct point_sizes
	{
	int stupid_code;
	int real_code;
	} point_sizes[]
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

int	overflow = 144 * 11;
int	pagemod;
#define	MODOFF 3672		/* 432 * 8.5 */

int esc, lead, back, verd, mcase, railmag;
int col, row;
int pstart 0;	/*  Means a startline is pending  */

int oback, omcase, orailmag, ocol;
int opsize 02;

struct lstate
	{
	int row;
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
	struct lstate start;
	struct lstate end;
	char *codep;
	};

struct line *head;
struct line *tail;
struct line cline { -1,-1,-1,-1};

#define TBUFLEN 1024
char *codep;
char tbuf[TBUFLEN];

#ifdef MONITORING
extern etext();
int monbuf[1000];
#endif

main(argc, argv) 
	int argc;
	char *argv[];
	{
	register i;

#ifndef TESTING
	signal(BROKENPIPE, 1);
#endif

	for(i = 3; i < 15; i++)
		close(i);
#ifdef MONITORING
	monitor(2,etext,monbuf,sizeof monbuf/2,0);
#endif
	if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 'l') {
		float f = 144 * atof(argv[1] + 2);
		if (f < 144) {
			error("bad length");
			exit(1);
		}
		overflow = f;
		argc--, argv++;
	}
	outbuf = stdout;
	if(argc > 1)
		{
		while(--argc)
			{
			argv++;
			process(argv[0]);
			}
		}
	   else
		{
		inbuf = stdin;
		ofile();
		}
#ifdef MONITORING
	monitor(0);
#endif
	done();
}


process(name)
	char *name;
{
	if((inbuf=fopen(name, "r")) == NULL)
		error("cannot open data file");
	ofile();
}

ofile()
	{
	register int c;
	static int initialized;

	while((c = getch()) != -1) {
		if(!c)
			continue;
		if(c & 0200)
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
				if(initialized++)
					goto out;
				row = 0;
				col = 0;	esc = 0;
				lead = 0;
				verd = 0;	back = 0;	mcase = 0;
				railmag = 0;
				ocol = 0;
				oback = 0;	omcase = 0;
				orailmag = 0;
				if(loadfont(railmag, cpsize) < 0)
					error("init");
				startline();
				putc(0100, outbuf);	/*  Dont stuff it guys  */
				break;
			case 0101:	/* lower rail */
				crail(railmag =& ~01);
				if(!pstart)
					stuffc(c);
				break;
			case 0102:	/* upper rail */
				crail(railmag =| 01);
				if(!pstart)
					stuffc(c);
				break;
			case 0103:	/* upper mag */
				crail(railmag =| 02);
				if(!pstart)
					stuffc(c);
				break;
			case 0104:	/* lower mag */
				crail(railmag =& ~02);
				if(!pstart)
					stuffc(c);
				break;
			case 0105:	/* lower case */
				mcase = 0;
				if(!pstart)
					stuffc(c);
				break;
			case 0106:	/* upper case */
				mcase = 0100;
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
					if (skmagic > 0) {
						skmagic--;
						ptlead(lead);
						continue;
					}
					row += lead;
					if (row >= overflow) {
						if (pagemod == 3) {
							allflush();
							col %= MODOFF;
							pagemod = 0;
						} else {
							pagemod++;
							col += MODOFF;
							row -= overflow;
						}
					}
					if (row >= overflow)
						write(2, "URK\n", 4);
				/*
					if (row > overflow)
						miniflush();
				*/
					if (row < 0) {
						if (pagemod) {
							pagemod--;
							col -= MODOFF;
							row += overflow;
						}
					}
					if (row < 0)
						write(2, "URK2\n", 5);
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
	}
   out:
	termline();
	allflush();
	fclose(inbuf);
}

done() {
	termline();
	allflush();
	exit();
}

int peekc;
getch() {
	register c;
	if(peekc) {
		c = peekc;
		peekc = 0;
		return(c);
	}
	return(getc(inbuf));
}

ungetc(c) {
	peekc = c;
}


error(s)
	char *s;
{

	fflush(outbuf);
	fprintf(stderr, "Vsort: %s\n", s);
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
		cline.start.row = row;
		return;
	}
	cline.len = 0;
	cline.start.row = row;
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
	if((allp = calloc(sizeof *linep,1)) == -1)
		error("alloc");
	linep = allp;
	linep->end.row = row;
	linep->end.col = col;
	linep->end.psize = cpsize;
	linep->end.mcase = mcase;
	linep->end.back = back;
	linep->end.verd = verd;
	linep->end.railmag = railmag;
	linep->start.row = cline.start.row;
	linep->start.col = cline.start.col;
	linep->start.psize = cline.start.psize;
	linep->start.mcase = cline.start.mcase;
	linep->start.back = cline.start.back;
	linep->start.verd = cline.start.verd;
	linep->start.railmag = cline.start.railmag;
	linep->len = cline.len;
	if((allp = calloc(cline.len,1)) == -1)
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
	while(clp != NULL && clp->start.row > linep->start.row)
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

miniflush()
{

	sendline();
}

allflush()
{

	while(head != NULL)
		sendline();
	sendline();
	fflush(outbuf);
}

sendline()
{
	register char *cp;
	register struct line *linep;
	register int i;
	int drow;

	if((linep = head) == NULL) {
		while(row >= overflow) {
			/* fprintf(stderr, "spitting %d\n", overflow - row); */
			if (overflow - row)
				ptlead(overflow - row);
			updatelist(overflow);
		}
		return;
	}
	adjparms(linep);
	cp = linep->codep;
	for(i = 0; i < linep->len; i++)
		putc(*cp++, outbuf);
	setparms(linep);
	if(linep->nextp != NULL)
		drow = linep->nextp->start.row;
	else
		drow = overflow;
	head = linep->nextp;
	cfree(linep->codep);
	cfree(linep);
	if(head == NULL)
		tail = NULL;
	else
		head->lastp = NULL;
	ptlead(drow - row);
	row = drow;
	/* updatelist(drow); */
}

adjparms(linep)
	register struct line *linep;
{

	if(linep->start.railmag != orailmag)
		ptrail(linep->start.railmag);
	if(linep->start.psize != opsize)
		ptsize(linep->start.psize);
	if(linep->start.mcase != omcase)
		ptmcase();
	if(linep->start.row != row)	/*  lead forward  */
	{
		ptlead(linep->start.row - row);
		row = linep->start.row;
		/* updatelist(linep->start.row); */
	}
	if(linep->start.col != ocol)
		ptesc(linep->start.col-ocol);
	if(linep->start.back != oback)
		ptback();
}

ptrail(rlmg)
	register int rlmg;
{

	if((rlmg & 01) != (orailmag & 01))
		putc((rlmg & 01) ? 0102:0101, outbuf);	/*  rail  */
	if((rlmg & 02) != (orailmag & 02))
		putc((rlmg & 02) ? 0103:0104, outbuf);	/*  mag  */
}

ptback()
{

	putc(oback ? 0107:0110, outbuf);
	oback = !oback;
}

ptsize(size)
	register int size;
{

	putc(0120 | (size & 017), outbuf);
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

	putc(omcase ? 0105:0106, outbuf);
}

ptesc(escc)
	register int escc;
{

	if((escc < 0 && !oback ) || (escc >= 0 && oback))
		ptback();
	escc = abs(escc);
	while(escc > 0177) {
		putc(0200, outbuf);
		escc -= 0177;
	}
	if(escc)
		putc(0200 | ((~escc) & 0177), outbuf);
}

ptlead(leadd)
	register int leadd;
{

	while(leadd > 037) {
		putc(0140, outbuf);
		leadd -= 037;
	}
	if(leadd)
		putc(0140 | ((~leadd) & 037), outbuf);
}

setparms(linep)
	register struct line *linep;
{

	orailmag = linep->end.railmag;
	opsize = linep->end.psize;
	omcase = linep->end.mcase;
	ocol = linep->end.col;
	oback = linep->end.back;
}

updatelist(drow)
	register int drow;
{
	register struct line *clp;

	for(clp = head; clp != NULL; clp = clp->nextp)
		clp->start.row -= drow;
	row -= drow;
}
