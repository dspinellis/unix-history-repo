#ifndef lint
static char sccsid[]="@(#)n6.c	1.1	(CWI)	86/08/14";
#endif

#include "tdef.h"
#include "tw.h"
#include "ext.h"
#include <ctype.h>

/*
 * n6.c -- width functions, sizes and fonts
*/

int	bdtab[NFONT+1] ={ 0, 0, 0, 3, 3, 0, };
int	sbold = 0;
int	fontlab[NFONT+1] = { 0, 'R', 'I', 'B', PAIR('B','I'), 'S', 0 };

width(j)
register tchar j;
{
	register i, k;

	if (j & (ZBIT|MOT)) {
		if (iszbit(j))
			return(0);
		if (isvmot(j))
			return(0);
		k = absmot(j);
		if (isnmot(j))
			k = -k;
		return(k);
	}
	i = cbits(j);
	if (i < ' ') {
		if (i == '\b')
			return(-widthp);
		if (i == PRESC)
			i = eschar;
		else if (iscontrol(i))
			return(0);
	}
	if (i==ohc)
		return(0);
	i = trtab[i];
	if (i < 32)
		return(0);
	k = t.width[i] * t.Char;
	widthp = k;
	return(k);
}


tchar setch()
{
	register j;
	char	temp[10];
	register char	*s;

	s = temp;
	if ((*s++ = getach()) == 0 || (*s++ = getach()) == 0)
		return(0);
	*s = '\0';
	if ((j = findch(temp)) > 0)
		return j | chbits;
	else
		return 0;
}

tchar setabs()		/* set absolute char from \C'...' */
{			/* for now, a no-op */
	int i, n, nf;

	getch();
	n = 0;
	n = inumb(&n);
	getch();
	return ' ';
}

findft(i)
register int	i;
{
	register k;

	if ((k = i - '0') >= 0 && k <= nfonts && k < smnt)
		return(k);
	for (k = 0; fontlab[k] != i; k++)
		if (k > nfonts)
			return(-1);
	return(k);
}


caseps()
{
}


mchbits()
{
	chbits = 0;
	setfbits(chbits, font);
	sps = width(' ' | chbits);
}


setps()
{
	register i, j;

	i = cbits(getch());
	if (isdigit(i)) {		/* \sd or \sdd */
		i -= '0';
		if (i == 0)		/* \s0 */
			;
		else if (i <= 3 && isdigit(j = cbits(ch=getch()))) {	/* \sdd */
			ch = 0;
		}
	} else if (i == '(') {		/* \s(dd */
		getch();
		getch();
	} else if (i == '+' || i == '-') {	/* \s+, \s- */
		j = cbits(getch());
		if (isdigit(j)) {		/* \s+d, \s-d */
			;
		} else if (j == '(') {		/* \s+(dd, \s-(dd */
			getch();
			getch();
		}
	}
}


oldsetps()
{
	int i, j, k;

	if (((i = cbits(getch())) == '+' || i == '-') && (j = cbits(ch = getch()) - '0') >= 0 && j <= 9) {
		if (i == '-')
			j = -j;
		ch = 0;
		return;
	}
	if ((i -= '0') == 0) {
		return;
	}
	if (i > 0 && i <= 9) {
		/* removed if (i <= 3 && */
		/* didn't work!!!! */
		if (i <= 3 && (j = cbits(ch = getch()) - '0') >= 0 && j <= 9) {
			i = 10 * i + j;
			ch = 0;
		}
	}
}


tchar setht()		/* set character height from \H'...' */
{
	int	n;
	tchar c;

	getch();
	n = inumb(&apts);
	getch();
	return(0);
}


tchar setslant()		/* set slant from \S'...' */
{
	int	n;
	tchar c;

	getch();
	n = 0;
	n = inumb(&n);
	getch();
	return(0);
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
	if (i == 'S' || i == '0')
		return;
	if ((j = findft(i, fontlab)) == -1)
		return;
s0:
	font1 = font;
	font = j;
	mchbits();
}


setwd()
{
	register base, wid;
	register tchar i;
	int	delim, emsz, k;
	int	savhp, savapts, savapts1, savfont, savfont1, savpts, savpts1;

	base = numtab[ST].val = numtab[ST].val = wid = numtab[CT].val = 0;
	if (ismot(i = getch()))
		return;
	delim = cbits(i);
	savhp = numtab[HP].val;
	numtab[HP].val = 0;
	savapts = apts;
	savapts1 = apts1;
	savfont = font;
	savfont1 = font1;
	savpts = pts;
	savpts1 = pts1;
	setwdf++;
	while (cbits(i = getch()) != delim && !nlflg) {
		k = width(i);
		wid += k;
		numtab[HP].val += k;
		if (!ismot(i)) {
			emsz = (INCH * pts + 36) / 72;
		} else if (isvmot(i)) {
			k = absmot(i);
			if (isnmot(i))
				k = -k;
			base -= k;
			emsz = 0;
		} else 
			continue;
		if (base < numtab[SB].val)
			numtab[SB].val = base;
		if ((k = base + emsz) > numtab[ST].val)
			numtab[ST].val = k;
	}
	setn1(wid, 0, (tchar) 0);
	numtab[HP].val = savhp;
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
	register int j, n;
	register tchar i;

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

	j = t.Halfline;
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
	register tchar j;

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
tchar	i;
{
	return(i);
}


caselg()
{
}


casefp()
{
	register i, j;

	skip();
	if ((i = cbits(getch()) - '0') < 0 || i > nfonts)
		return;
	if (skip() || !(j = getrq()))
		return;
	fontlab[i] = j;
}


casecs()
{
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
	if (j == smnt) {
		k = smnt;
		goto bd0;
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
	dfact = INCH; /*default scaling is points!*/
	dfactd = 72;
	res = VERT;
	i = inumb(&lss);
	if (nonumb)
		i = lss1;
	if (i < VERT)
		i = 0;	/* was VERT */
	lss1 = lss;
	lss = i;
}




casess()
{
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

	getch();
	dfact = lss;
	i = quant(atoi(), VERT);
	dfact = 1;
	getch();
	if (i >= 0)
		*pbp++ = MOT | VMOT | i;
	else
		*pbp++ = MOT | VMOT | NMOT | -i;
	return(HX);
}
