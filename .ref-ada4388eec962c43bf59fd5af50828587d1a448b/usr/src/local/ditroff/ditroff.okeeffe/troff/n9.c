#ifndef lint
static char sccsid[] = "@(#)n9.c	2.2 (CWI) 93/02/25";
#endif lint
#include "tdef.h"
#ifdef NROFF
#include "tw.h"
#endif
#include <sgtty.h>
#include "ext.h"

/*
 * troff9.c
 * 
 * misc functions
 */

tchar setz()
{
	tchar i;

	if (!ismot(i = getch()))
		i |= ZBIT;
	return(i);
}

setline()
{
	register tchar *i;
	tchar c;
	int	length;
	int	w, cnt, delim, rem, temp;
	tchar linebuf[NC];

	if (ismot(c = getch()))
		return;
	delim = cbits(c);
	vflag = 0;
	dfact = EM;
	length = quant(atoi(), HOR);
	dfact = 1;
	if (!length) {
		eat(delim);
		return;
	}
s0:
	if ((cbits(c = getch())) == delim) {
		ch = c;
		c = RULE | chbits;
	} else if (cbits(c) == FILLER)
		goto s0;
	w = width(c);
	i = linebuf;
	if (length < 0) {
		*i++ = makem(length);
		length = -length;
	}
	if (!(cnt = length / w)) {
		*i++ = makem(-(temp = ((w - length) / 2)));
		*i++ = c;
		*i++ = makem(-(w - length - temp));
		goto s1;
	}
	if (rem = length % w) {
		if (cbits(c) == RULE || cbits(c) == UNDERLINE || cbits(c) == ROOTEN)
			*i++ = c | ZBIT;
		*i++ = makem(rem);
	}
	if (cnt) {
		*i++ = RPT;
		*i++ = cnt;
		*i++ = c;
	}
s1:
	*i++ = 0;
	eat(delim);
	pushback(linebuf);
}


eat(c)
register int	c;
{
	register i;

	while ((i = cbits(getch())) != c &&  (i != '\n'))
		;
	return(i);
}


setov()
{
	register j, k;
	tchar i, o[NOV+1];
	int delim, w[NOV+1];

	if (ismot(i = getch()))
		return;
	delim = cbits(i);
	for (k = 0; (k < NOV) && ((j = cbits(i = getch())) != delim) &&  (j != '\n'); k++) {
		o[k] = i;
		w[k] = width(i);
	}
	o[k] = w[k] = 0;
	if (o[0])
		for (j = 1; j; ) {
			j = 0;
			for (k = 1; o[k] ; k++) {
				if (w[k-1] < w[k]) {
					j++;
					i = w[k];
					w[k] = w[k-1];
					w[k-1] = i;
					i = o[k];
					o[k] = o[k-1];
					o[k-1] = i;
				}
			}
		}
	else 
		return;
	*pbp++ = makem(w[0] / 2);
	for (k = 0; o[k]; k++)
		;
	while (k>0) {
		k--;
		*pbp++ = makem(-((w[k] + w[k+1]) / 2));
		*pbp++ = o[k];
	}
}


setbra()
{
	register k;
	tchar i, *j, dwn;
	int	cnt, delim;
	tchar brabuf[NC];

	if (ismot(i = getch()))
		return;
	delim = cbits(i);
	j = brabuf + 1;
	cnt = 0;
#ifdef NROFF
	dwn = (2 * t.Halfline) | MOT | VMOT;
#endif
#ifndef NROFF
	dwn = EM | MOT | VMOT;
#endif
	while (((k = cbits(i = getch())) != delim) && (k != '\n') &&  (j <= (brabuf + NC - 4))) {
		*j++ = i | ZBIT;
		*j++ = dwn;
		cnt++;
	}
	if (--cnt < 0)
		return;
	else if (!cnt) {
		ch = *(j - 2);
		return;
	}
	*j = 0;
#ifdef NROFF
	*--j = *brabuf = (cnt * t.Halfline) | MOT | NMOT | VMOT;
#endif
#ifndef NROFF
	*--j = *brabuf = (cnt * EM) / 2 | MOT | NMOT | VMOT;
#endif
	*--j &= ~ZBIT;
	pushback(brabuf);
}


setvline()
{
	register i;
	tchar c, rem, ver, neg;
	int	cnt, delim, v;
	tchar vlbuf[NC];
	register tchar *vlp;

	if (ismot(c = getch()))
		return;
	delim = cbits(c);
	dfact = lss;
	vflag++;
	i = quant(atoi(), VERT);
	dfact = 1;
	if (!i) {
		eat(delim);
		vflag = 0;
		return;
	}
	if ((cbits(c = getch())) == delim) {
		c = BOXRULE | chbits;	/*default box rule*/
	} else 
		getch();
	c |= ZBIT;
	neg = 0;
	if (i < 0) {
		i = -i;
		neg = NMOT;
	}
#ifdef NROFF
	v = 2 * t.Halfline;
#endif
#ifndef NROFF
	v = EM;
#endif
	cnt = i / v;
	rem = makem(i % v) | neg;
	ver = makem(v) | neg;
	vlp = vlbuf;
	if (!neg)
		*vlp++ = ver;
	if (absmot(rem) != 0) {
		*vlp++ = c;
		*vlp++ = rem;
	}
	while ((vlp < (vlbuf + NC - 3)) && cnt--) {
		*vlp++ = c;
		*vlp++ = ver;
	}
	*(vlp - 2) &= ~ZBIT;
	if (!neg)
		vlp--;
	*vlp++ = 0;
	pushback(vlbuf);
	vflag = 0;
}

#define	NPAIR	(NC/2-6)	/* max pairs in spline, etc. */

setdraw()	/* generate internal cookies for a drawing function */
{
	int i, j, k, dx[NPAIR], dy[NPAIR], delim, type;
	tchar c, drawbuf[NC];

	/* input is \D'f dx dy dx dy ... c' (or at least it had better be) */
	/* this does drawing function f with character c and the */
	/* specified dx,dy pairs interpreted as appropriate */
	/* pairs are deltas from last point, except for radii */

	/* t x		set line thickness to x */
	/* s x		set line style to bit-map x (x BETTER be in "u")*/
	/* l dx dy:	line from here by dx,dy */
	/* c x:		circle of diameter x, left side here */
	/* e x y:	ellipse of diameters x,y, left side here */
	/* a dx1 dy1 dx2 dy2:
			ccw arc: ctr at dx1,dy1, then end at dx2,dy2 from there */
	/* [Pp] s x y ...:	for polygons filled with stipple s */
	/* ~ x y ...:	wiggly line  -or-  */
	/* g x y ...:	for gremlin-style curves */
	/* ~ dx1 dy1 dx2 dy2...:
			spline to dx1,dy1 to dx2,dy2 ... */
	/* f dx dy ...:	f is any other char:  like spline */

	if (ismot(c = getch()))
		return;
	delim = cbits(c);
	type = cbits(getch());
	for (i = 0; i < NPAIR ; i++) {
		if (nlflg)
			break;
		c = getch();
		if (cbits(c) == delim)
			break;
	/* ought to pick up optional drawing character */
		if (cbits(c) != ' ')
			ch = c;
		vflag = 0;
		if (i == 0 && (type == DRAWPOLY || type == DRAWUBPOLY)) {
			dfact = 1;
			dx[0] = quant(atoi(), 1);
			if (dx[0] < 0 || dx[0] > MAXMOT)
				dx[0] = 0;
			dy[0] = 0;
			continue;
		}
		dfact = EM;
		dx[i] = quant(atoi(), HOR);
		if (dx[i] > MAXMOT)
			dx[i] = MAXMOT;
		else if (dx[i] < -MAXMOT)
			dx[i] = -MAXMOT;
		if (cbits((c = getch())) == delim) {	/* spacer */
			dy[i++] = 0;
			break;
		}
		vflag = 1;
		dfact = lss;
		dy[i] = quant(atoi(), VERT);
		if (dy[i] > MAXMOT)
			dy[i] = MAXMOT;
		else if (dy[i] < -MAXMOT)
			dy[i] = -MAXMOT;
	}
	dfact = 1;
	vflag = 0;
#ifndef NROFF
	drawbuf[0] = DRAWFCN | chbits | ZBIT;
	drawbuf[1] = type | chbits | ZBIT;
	for (k = 0, j = 2; k < i; k++) {
		drawbuf[j++] = MOT | ((dx[k] >= 0) ? dx[k] : (NMOT | -dx[k]));
		drawbuf[j++] = MOT | VMOT | ((dy[k] >= 0) ? dy[k] : (NMOT | -dy[k]));
	}
	if (type == DRAWELLIPSE) {
		drawbuf[4] = drawbuf[3] | NMOT;	/* so the net vertical is zero */
		j = 5;
	} else if (type == DRAWTHICK || type == DRAWSTYLE) {
		drawbuf[3] = drawbuf[2] | NMOT;	/* so net horizontal is zero */
	}
	drawbuf[j++] = '.' | chbits | ZBIT;	/* marks end for ptout */
	drawbuf[j] = 0;
	pushback(drawbuf);
#endif
}


casefc()
{
	register i;
	tchar j;

	gchtab[fc] &= ~FCBIT;
	fc = IMP;
	padc = ' ';
	if (skip() || ismot(j = getch()) || (i = cbits(j)) == '\n')
		return;
	fc = i;
	gchtab[fc] |= FCBIT;
	if (skip() || ismot(ch) || (ch = cbits(ch)) == fc)
		return;
	padc = ch;
}


tchar
setfield(x)
int	x;
{
	register tchar ii, jj, *fp;
	register i, j;
	int length, ws, npad, temp, type;
	tchar **pp, *padptr[NPP];
	tchar fbuf[FBUFSZ];
	int savfc, savtc, savlc;
	tchar rchar;
	int savepos;

	if (x == tabch) 
		rchar = tabc | chbits;
	else if (x ==  ldrch) 
		rchar = dotc | chbits;
	temp = npad = ws = 0;
	savfc = fc;
	savtc = tabch;
	savlc = ldrch;
	tabch = ldrch = fc = IMP;
	savepos = numtab[HP].val;
	gchtab[tabch] &= ~TABBIT;
	gchtab[ldrch] &= ~LDRBIT;
	gchtab[fc] &= ~FCBIT;
	gchtab[IMP] |= TABBIT|LDRBIT|FCBIT;
	for (j = 0; ; j++) {
		if ((tabtab[j] & TABMASK) == 0) {
			if (x == savfc)
				errprint("zero field width.");
			jj = 0;
			goto rtn;
		}
		if ((length = ((tabtab[j] & TABMASK) - numtab[HP].val)) > 0 )
			break;
	}
	type = tabtab[j] & (~TABMASK);
	fp = fbuf;
	pp = padptr;
	if (x == savfc) {
		while (1) {
			j = cbits(ii = getch());
			jj = width(ii);
			widthp = jj;
			numtab[HP].val += jj;
			if (j == padc) {
				npad++;
				*pp++ = fp;
				if (pp > (padptr + NPP - 1))
					break;
				goto s1;
			} else if (j == savfc) 
				break;
			else if (j == '\n') {
				temp = j;
				nlflg = 0;
				break;
			}
			ws += jj;
s1:
			*fp++ = ii;
			if (fp > (fbuf + FBUFSZ - 3))
				break;
		}
		if (!npad) {
			npad++;
			*pp++ = fp;
			*fp++ = 0;
		}
		*fp++ = temp;
		*fp++ = 0;
		temp = i = (j = length - ws) / npad;
		i = (i / HOR) * HOR;
		if ((j -= i * npad) < 0)
			j = -j;
		ii = makem(i);
		if (temp < 0)
			ii |= NMOT;
		for (; npad > 0; npad--) {
			*(*--pp) = ii;
			if (j) {
				j -= HOR;
				(*(*pp)) += HOR;
			}
		}
		pushback(fbuf);
		jj = 0;
	} else if (type == 0) {
		/*plain tab or leader*/
		if ((j = width(rchar)) > 0) {
			int nchar = length / j;
			while (nchar-->0 && pbp < &pbbuf[NC-3]) {
				numtab[HP].val += j;
				widthp = j;
				*pbp++ = rchar;
			}
			length %= j;
		}
		if (length)
			jj = length | MOT;
		else 
			jj = getch0();
	} else {
		/*center tab*/
		/*right tab*/
		while (((j = cbits(ii = getch())) != savtc) &&  (j != '\n') && (j != savlc)) {
			jj = width(ii);
			ws += jj;
			numtab[HP].val += jj;
			widthp = jj;
			*fp++ = ii;
			if (fp > (fbuf + FBUFSZ - 3)) 
				break;
		}
		*fp++ = ii;
		*fp++ = 0;
		if (type == RTAB)
			length -= ws;
		else 
			length -= ws / 2; /*CTAB*/
		pushback(fbuf);
		if ((j = width(rchar)) != 0 && length > 0) {
			int nchar = length / j;
			while (nchar-- > 0 && pbp < &pbbuf[NC-3])
				*pbp++ = rchar;
			length %= j;
		}
		length = (length / HOR) * HOR;
		jj = makem(length);
		nlflg = 0;
	}
rtn:
	gchtab[fc] &= ~FCBIT;
	gchtab[tabch] &= ~TABBIT;
	gchtab[ldrch] &= ~LDRBIT;
	fc = savfc;
	tabch = savtc;
	ldrch = savlc;
	gchtab[fc] |= FCBIT;
	gchtab[tabch] = TABBIT;
	gchtab[ldrch] |= LDRBIT;
	numtab[HP].val = savepos;
	return(jj);
}


