/*	t6.c	1.8	(Berkeley)	85/04/30	*/
#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
#include "dev.h"
/*
troff6.c

width functions, sizes and fonts
*/

#include <sgtty.h>
#include "ext.h"
int	trflg;
/* fitab[f][c] is 0 if c is not on font f
	/* if it's non-zero, c is in fontab[f] at position
	/* fitab[f][c].
	*/
int	fontlab[NFONT+1];
short	*pstab;
int	cstab[NFONT+1], ccstab[NFONT+1];
int	bdtab[NFONT+1];
int	sbold = 0;

width(j)
tchar j;
{
	register i, k;

	k = 0;
	i = cbits(j);
	if (ismot(j)) {
		if (isvmot(j))
			goto rtn;
		k = absmot(j);
		if (isnmot(j))
			k = -k;
		goto rtn;
	}
	if (i == '\b') {
		k = -widthp;
		goto rtn;
	}
	if (i == PRESC)
		i = eschar;
	else if (i == ohc || iscontrol(i))
		goto rtn;
	if (sfbits(j) == oldbits) {
		xfont = pfont;
		xpts = ppts;
	} else 
		xbits(j);
	if (iszbit(j))
		goto rtn;
	if (!trflg)
		i = trtab[i];
	if ((i -= 32) < 0)
		goto rtn;
	k = getcw(i);
	if (bd)
		k += (bd - 1) * HOR;
	if (cs)
		k = cs;
	widthp = k;
rtn:
	xbitf = trflg = 0;
	return(k);
}


getcw(i)
register int	i;
{
	register int	k;
	register char	*p;
	int	x, j;

	bd = 0;
	if (i == 0) {	/* a blank */
		k = (fontab[xfont][0] * spacesz + 6) / 12;
		/* this nonsense because .ss cmd uses 1/36 em as its units */
		/* and default is 12 */
		goto g1;
	}
	if ((j = fitab[xfont][i] & BMASK) == 0) {	/* NOT current font */
		/* search through search list of xfont
		/* to see what font it ought to be on.
		/* for now, searches from current font in wraparound order. */

		int	ii, jj;
		for (ii=xfont, jj=0; jj < nfonts; jj++, ii=ii % nfonts + 1) {
			j = fitab[ii][i] & BMASK;
			if (j != 0) {
				p = fontab[ii];
				k = *(p + j);
				if (xfont == sbold)
					bd = bdtab[ii];
				if (setwdf)
					v.ct |= kerntab[ii][j];
				goto g1;
			}
		}
		code = 0;
		k = fontab[xfont][0];	/* leave a space-size space */
		goto g1;
	}
	p = fontab[xfont];
	if (setwdf)
		v.ct |= kerntab[xfont][j];
	k = *(p + j);
g1:
	if (!bd)
		bd = bdtab[xfont];
	if (cs = cstab[xfont]) {
		if (ccs = ccstab[xfont])
			x = ccs; 
		else 
			x = xpts;
		cs = (cs * EMPTS(x)) / 36;
	}
	return(((k&BMASK) * xpts + (Unitwidth / 2)) / Unitwidth);
	/* Unitwidth is Units/Point, where
	/* Units is the fundamental digitization
	/* of the character set widths, and
	/* Point is the number of goobies in a point
	/* e.g., for cat, Units=36, Point=6, so Unitwidth=36/6=6
	/* In effect, it's the size at which the widths
	/* translate directly into units.
	*/
}


xbits(i)
tchar i;
{
	register j, k;

	xfont = fbits(i);
	k = sbits(i);
	if (k) {
		xpts = pstab[--k];
		oldbits = sfbits(i);
		pfont = xfont;
		ppts = xpts;
		goto rtn;
	}
	switch (xbitf) {
	case 0:
		xfont = font;
		xpts = pts;
		break;
	case 1:
		xfont = pfont;
		xpts = ppts;
		break;
	case 2:
		xfont = mfont;
		xpts = mpts;
	}
rtn:
	xbitf = 0;
}


tchar setch()
{
	register j;
	char	temp[10];
	register char	*s;
	extern char	*chname;
	extern short	*chtab;
	extern int	nchtab;

	s = temp;
	if ((*s++ = getach()) == 0 || (*s++ = getach()) == 0)
		return(0);
	*s = '\0';
	for (j = 0; j < nchtab; j++)
		if (strcmp(&chname[chtab[j]], temp) == 0)
			return(j + 128 | chbits);
	return(0);
}


tchar absch()	/* absolute character number */
{
	fprintf(stderr, "troff: no \\C yet\n");
	return(0);
}


findft(i)
register int	i;
{
	register k;
	register char last;

	if ((k = (i & BMASK) - '0') >= 0 && k <= 9) {	/* digit - see if two */
	    if (last = cbits(i) >> BYTE)
		if ((last -= '0') < 0 || last > 9 || (k = k*10+last) > nfonts)
			return(-1);
	    return(k);		/* one digit (or two, if in range */
	}
	for (k = 0; fontlab[k] != i; k++)
		if (k > nfonts)
			return(-1);
	return(k);
}


caseps()
{
	register i;

	if (skip())
		i = apts1;
	else {
		noscale++;
		i = inumb(&apts);	/* this is a disaster for fractional point sizes */
		noscale = 0;
		if (nonumb)
			return;
	}
	casps1(i);
}


casps1(i)
register int	i;
{
	if (i <= 0)
		return;
	apts1 = apts;
	apts = i;
	pts1 = pts;
	pts = findps(i);
	mchbits();
}


findps(i)
register int	i;
{
	register j, k;

	for (j = 0; i > (k = pstab[j]); j++)
		if (!k) {
			k = pstab[--j];
			break;
		}
	return(k);
}


mchbits()
{
	register i, j, k;

	i = pts;
	for (j = 0; i > (k = pstab[j]); j++)
		if (!k) {
			k = pstab[--j];
			break;
		}
	chbits = 0;
	setsbits(chbits, ++j);
	setfbits(chbits, font);
	sps = width(' ' | chbits);
}


setps()
{
	register i, j;

	if (((i = cbits(getch())) == '+' || i == '-') && (j = cbits(ch = getch()) - '0') >= 0 && j <= 9) {
		if (i == '-')
			j = -j;
		ch = 0;
		casps1(apts + j);
		return;
	}
	if ((i -= '0') == 0) {
		casps1(apts1);
		return;
	}
	if (i > 0 && i <= 9) {
		/* removed if (i <= 3 && */
		/* didn't work!!!! */
		if (i <= 3 && (j = cbits(ch = getch()) - '0') >= 0 && j <= 9) {
			i = 10 * i + j;
			ch = 0;
		}
		casps1(i);
	}
}


tchar setht()		/* set character height from \H'...' */
{
	int n;
	tchar c;

	getch();
	n = inumb(&apts);
	getch();
	if (n == 0 || nonumb)
		n = apts;	/* does this work? */
	c = CHARHT;
	c |= ZBIT;
	setsbits(c, n);
	return(c);
}

tchar setslant()		/* set slant from \S'...' */
{
	int n;
	tchar c;

	getch();
	n = 0;
	n = inumb(&n);
	getch();
	if (nonumb)
		n = 0;
	c = SLANT;
	c |= ZBIT;
	setsfbits(c, n+180);
	return(c);
}


casest()
{
	register i, j;
	register char last;

	skip();
	i = getrq();
	if (!i || i == 'P') {
	    stip = stip1;
	    return;
	}
	if (i == '0')
	    goto sterr;

	last = cbits(i) >> BYTE;
	if ((j = (i & BMASK) - '0') >= 0 && j <= 9) {	/* digit - see if two */
	    if (last)
		if ((last -= '0') < 0 || last > 9)
		    goto sterr;
	    if (j > nstips)
		goto sterr;
	} else {					/* stipple name */
	    for (j = 0; stiplab[j] != i; j++)
		if (j > nstips)
		    goto sterr;
	}
	stip1 = stip;
	stip = j;
	return;
sterr:
	fprintf(stderr, "troff: Can't find stipple %c%c\n", i & BMASK, last);
}


caseft()
{
	skip();
	setfont(1);
}


setfont(a)
int	a;
{
	register i, j;

	if (a)
		i = getrq();
	else 
		i = getsn();
	if (!i || i == 'P') {
		j = font1;
		goto s0;
	}
	if (i == '0')
		return;
	if ((j = findft(i)) == -1)
		if ((j = setfp(0, i)) == -1)	/* try to put it in position 0 */
			return;
s0:
	font1 = font;
	font = j;
	mchbits();
}


setwd()
{
	register base, wid;
	tchar i;
	int	delim, em, k;
	int	savlevel, savhp, savapts, savapts1, savfont, savfont1, savpts, savpts1;
	tchar *savpinchar, *p, *q, tempinchar[LNSIZE];	/* XXX */

	base = v.st = v.sb = wid = v.ct = 0;
	if (ismot(i = getch()))
		return;
	delim = cbits(i);
	savhp = v.hp;
	savpinchar = pinchar;	/* XXX */
	for (p=inchar, q=tempinchar; p < pinchar; )	/* XXX */
		*q++ = *p++;	/* XXX */
	pinchar = inchar;	/* XXX */
	savlevel = level;
	v.hp = level = 0;
	savapts = apts;
	savapts1 = apts1;
	savfont = font;
	savfont1 = font1;
	savpts = pts;
	savpts1 = pts1;
	setwdf++;
	while (cbits(i = getch()) != delim && !nlflg) {
		wid += width(i);
		if (!ismot(i)) {
			em = POINT * xpts;
		} else if (isvmot(i)) {
			k = absmot(i);
			if (isnmot(i))
				k = -k;
			base -= k;
			em = 0;
		} else 
			continue;
		if (base < v.sb)
			v.sb = base;
		if ((k = base + em) > v.st)
			v.st = k;
	}
	nform = 0;
	setn1(wid);
	v.hp = savhp;
	pinchar = savpinchar;	/* XXX */
	for (p=inchar, q=tempinchar; p < pinchar; )	/* XXX */
		*p++ = *q++;	/* XXX */
	level = savlevel;
	apts = savapts;
	apts1 = savapts1;
	font = savfont;
	font1 = savfont1;
	pts = savpts;
	pts1 = savpts1;
	mchbits();
	setwdf = 0;
}


tchar vmot()
{
	dfact = lss;
	vflag++;
	return(mot());
}


tchar hmot()
{
	dfact = EM;
	return(mot());
}


tchar mot()
{
	register short j, n;
	tchar i;

	j = HOR;
	getch(); /*eat delim*/
	if (n = atoi()) {
		if (vflag)
			j = VERT;
		i = makem(quant(n, j));
	} else
		i = 0;
	getch();
	vflag = 0;
	dfact = 1;
	return(i);
}


tchar sethl(k)
int	k;
{
	register j;
	tchar i;

	j = EM / 2;
	if (k == 'u')
		j = -j;
	else if (k == 'r')
		j = -2 * j;
	vflag++;
	i = makem(j);
	vflag = 0;
	return(i);
}


tchar makem(i)
int	i;
{
	tchar j;

	if ((j = i) < 0)
		j = -j;
	j |= MOT;
	if (i < 0)
		j |= NMOT;
	if (vflag)
		j |= VMOT;
	return(j);
}


tchar getlg(i)
tchar i;
{
	tchar j, k;
	register int lf;

	if ((lf = fontbase[fbits(i)]->ligfont) == 0)	/* the font has no ligatures */
		return(i);
	j = getch0();
	if (cbits(j) == 'i' && (lf & LFI))
		j = LIG_FI;
	else if (cbits(j) == 'l' && (lf & LFL))
		j = LIG_FL;
	else if (cbits(j) == 'f' && (lf & LFF)) {
		if ((lf & (LFFI|LFFL)) && lg != 2) {
			k = getch0();
			if (cbits(k)=='i' && (lf&LFFI))
				j = LIG_FFI;
			else if (cbits(k)=='l' && (lf&LFFL))
				j = LIG_FFL;
			else {
				ch0 = k;
				j = LIG_FF;
			}
		} else 
			j = LIG_FF;
	} else {
		ch0 = j;
		j = i;
	}
	return(i & SFMASK | j);
}


caselg()
{

	lg = 1;
	if (skip())
		return;
	lg = atoi();
}


casefp()
{
	register i, j, last;

	skip();
	j = getrq();
	if ((i = (j & BMASK) - '0') >= 0 && i <= 9) {	/* digit - see if two */
	    if (last = cbits(j) >> BYTE)
		if ((last -= '0') < 0 || last > 9 || (i = i*10+last) > nfonts)
			i = -1;
	} else i = -1;
	if (i <= 0 || i > nfonts)
		fprintf(stderr, "troff: fp: bad font position %d\n", i);
	else if (skip() || !(j = getrq()))
		fprintf(stderr, "troff: fp: no font name\n"); 
	else {
		skip();
		setfp(i, j);
	}
}


setfp(pos, font)	/* mount font at position pos[0...nfonts] */
int pos, font;
{
	register i, j, file;
	int n;
	char	longname[NS], shortname[3];
	extern int	nchtab;

	shortname[0] = font & BMASK;
	shortname[1] = font >> BYTE;
	shortname[2] = '\0';

	for (i = 0; i < nfonts; i++)		/* first search through the */
	    if (fontlab[i] == font) {		/* list of fonts and see if */
		register char *c;		/* it's already here to swap */

		if (pos == i) return(i);	/* already there, forget it */

#define ptrswap(x, y) { c = x; x = y; y = c; }
#define fptrswap(x, y) { c = (char*) (x); x = y; y = (struct font *) c; }

		fptrswap(fontbase[pos], fontbase[i]);
		ptrswap(fontab[pos], fontab[i]);
		ptrswap(kerntab[pos], kerntab[i]);
		ptrswap(fitab[pos], fitab[i]);
		ptfpcmd(pos, shortname);
		fontlab[i] = fontlab[pos];
						/* tell driver about the swap */
		shortname[0] = fontlab[pos] & BMASK;
		shortname[1] = fontlab[pos] >> BYTE;
		ptfpcmd(i, shortname);
		fontlab[pos] = font;
		if (pos == 0)
			ch = (tchar) FONTPOS | (tchar) font << 16;
		return(pos);
	    }

	sprintf(longname, "%s/dev%s/%s.out", fontfile, devname, shortname);
	if ((file = open(longname, 0)) < 0) {
		fprintf(stderr, "troff: Can't open %s\n", longname);
		return(-1);
	}
	n = fontbase[pos]->nwfont & BMASK;
	read(file, fontbase[pos], 3*n + nchtab + 96 + sizeof(struct font));
	kerntab[pos] = (char *) fontab[pos] + (fontbase[pos]->nwfont & BMASK);
	/* have to reset the fitab pointer because the width may be different */
	fitab[pos] = (char *) fontab[pos] + 3 * (fontbase[pos]->nwfont & BMASK);
	if ((fontbase[pos]->nwfont & BMASK) > n) {
		fprintf(stderr, "troff: Font %s too big for position %d\n",
								shortname, pos);
		return(-1);
	}
	fontbase[pos]->nwfont = n;	/* for loading a larger one later */
	close(file);

	fontlab[pos] = font;
	bdtab[pos] = cstab[pos] = ccstab[pos] = 0;
		/* if position isn't zero, no place to store its value. */
		/* only time a FONTPOS is pushed back is if it's a */
		/* standard font on position 0 (i.e., mounted implicitly. */
		/* there's a bug here:  if there are several input lines */
		/* that look like .ft XX in short successtion, the output */
		/* will all be in the last one because the "x font ..." */
		/* comes out too soon.  pushing back FONTPOS doesn't work */
		/* with .ft commands because input is flushed after .xx cmds */
	ptfpcmd(pos, shortname);
	if (pos == 0)
		ch = (tchar) FONTPOS | (tchar) font << 16;
	return(pos);
}


casecs()
{
	register i, j;

	noscale++;
	skip();
	if (!(i = getrq()) || (i = findft(i)) < 0)
		goto rtn;
	skip();
	cstab[i] = atoi();
	skip();
	j = atoi();
	if (nonumb)
		ccstab[i] = 0;
	else
		ccstab[i] = findps(j);
rtn:
	noscale = 0;
}


casebd()
{
	register i, j, k;

	k = 0;
bd0:
	if (skip() || !(i = getrq()) || (j = findft(i)) == -1) {
		if (k)
			goto bd1;
		else 
			return;
	}
	if (k) {
		sbold = j;
		j = k;
	}
bd1:
	skip();
	noscale++;
	bdtab[j] = atoi();
	noscale = 0;
}


casevs()
{
	register i;

	skip();
	vflag++;
	dfact = INCH; /* default scaling is points! */
	dfactd = 72;
	res = VERT;
	i = inumb(&lss);
	if (nonumb)
		i = lss1;
	/*	if(i < VERT)i = VERT; */
	if (i < VERT) 
		i = 0;
	lss1 = lss;
	lss = i;
}


casess()
{
	register i;

	noscale++;
	skip();
	if (i = atoi()) {
		spacesz = i & 0177;
		sps = width(' ' | chbits);
	}
	noscale = 0;
}


tchar xlss()
{
	/* stores \x'...' into
	/* two successive tchars.
	/* the first contains HX, the second the value,
	/* encoded as a vertical motion.
	/* decoding is done in n2.c by pchar().
	*/
	int	i;
	tchar c;

	getch();
	dfact = lss;
	i = quant(atoi(), VERT);
	dfact = 1;
	getch();
	if (i >= 0)
		ch0 = MOT | VMOT | i;
	else
		ch0 = MOT | VMOT | NMOT | -i;
	c = HX;
	dummy();
	return(c);
}
