#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
#ifdef NROFF
extern
#include "tw.h"
#endif
/*
troff9.c

misc functions
*/

#include <sgtty.h>
#include "ext.h"

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
	i = cbuf;
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
	cp = cbuf;
}


eat(c)
int	c;
{
	register i;

	while ((i = cbits(getch())) != c &&  (i != '\n'))
		;
	return(i);
}


setov()
{
	register j, k;
	tchar i, *p, o[NOV+1];
	int	delim, w[NOV+1];

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
	p = cbuf;
	for (k = 0; o[k]; k++) {
		*p++ = o[k];
		*p++ = makem(-((w[k] + w[k+1]) / 2));
	}
	*p++ = makem(w[0] / 2);
	*p = 0;
	cp = cbuf;
}


setbra()
{
	register k;
	tchar i, *j, dwn;
	int	cnt, delim;

	if (ismot(i = getch()))
		return;
	delim = cbits(i);
	j = cbuf + 1;
	cnt = 0;
#ifdef NROFF
	dwn = (2 * t.Halfline) | MOT | VMOT;
#endif
#ifndef NROFF
	dwn = EM | MOT | VMOT;
#endif
	while (((k = cbits(i = getch())) != delim) && (k != '\n') &&  (j <= (cbuf + NC - 4))) {
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
	*--j = *cbuf = (cnt * t.Halfline) | MOT | NMOT | VMOT;
#endif
#ifndef NROFF
	*--j = *cbuf = (cnt * EM) / 2 | MOT | NMOT | VMOT;
#endif
	*--j &= ~ZBIT;
	cp = cbuf;
}


setvline()
{
	register i;
	tchar c, *k, rem, ver, neg;
	int	cnt, delim, v;

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
	k = cbuf;
	if (!neg)
		*k++ = ver;
	if (absmot(rem) != 0) {
		*k++ = c;
		*k++ = rem;
	}
	while ((k < (cbuf + NC - 3)) && cnt--) {
		*k++ = c;
		*k++ = ver;
	}
	*(k - 2) &= ~ZBIT;
	if (!neg)
		k--;
	*k = 0;
	cp = cbuf;
	vflag = 0;
}


setdraw()	/* generate internal cookies for a drawing function */
{
	int i, j, k, dx[NC / 2], dy[NC / 2], delim, type, temp;
	tchar c;
	/* input is \D'f x y x y ... c' (or at least it had better be) */
	/* this does drawing function f with character c and the */
	/* specified x,y pairs interpreted as appropriate */

	/* t x		set line thickness to x */
	/* s x		set line style to bit-map x (x BETTER be in "u")*/
	/* l x y:	line from here by x,y */
	/* c x:		circle of diameter x, left side here */
	/* e x y:	ellipse of diameters x,y, left side here */
	/* a x y r:	arc to x,y with radius r (ccw) */
	/* ~ x y ...:	wiggly line  -or-  */
	/* g x y ...:	for gremlin-style curves */
	/* p s x y ...:	for polygons filled with stipple s */

	if (ismot(c = getch()))
		return;
	delim = cbits(c);
	type = cbits(getch());
	for (i = 0; ; i++) {
		if (i > (NC / 2 - 3))
			i--;
		if (nlflg)
			break;
		c = getch();
		if (cbits(c) == delim)
			break;
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
	cbuf[0] = DRAWFCN | chbits | ZBIT;
	cbuf[1] = type | chbits | ZBIT;
	for (k = 0, j = 2; k < i; k++) {
		cbuf[j++] = MOT | ((dx[k] >= 0) ? dx[k] : (NMOT | -dx[k]));
		cbuf[j++] = MOT | VMOT | ((dy[k] >= 0) ? dy[k] : (NMOT | -dy[k]));
	}
	if (type == DRAWELLIPSE) {
		cbuf[4] = cbuf[3] | NMOT;	/* so net vertical is zero */
		j = 5;
	} else if (type == DRAWTHICK || type == DRAWSTYLE) {
		cbuf[3] = cbuf[2] | NMOT;	/* so net horizontal is zero */
	}
	cbuf[j++] = '.' | chbits | ZBIT;	/* marks end for ptout */
	cbuf[j] = 0;
	cp = cbuf;
#endif
}


casefc()
{
	register i;
	tchar j;

	fc = IMP;
	padc = ' ';
	if (skip() || ismot(j = getch()) || (i = cbits(j)) == '\n')
		return;
	fc = i;
	if (skip() || ismot(ch) || (ch = cbits(ch)) == fc)
		return;
	padc = ch;
}


tchar setfield(x)
int	x;
{
	tchar ii, jj, *fp;
	register i, j;
	int	length, ws, npad, temp, type;
	tchar * *pp, *padptr[NPP];
	static tchar fbuf[FBUFSZ];
	int	savfc, savtc, savlc;

	if (x == tabch) 
		rchar = tabc | chbits;
	else if (x ==  ldrch) 
		rchar = dotc | chbits;
	temp = npad = ws = 0;
	savfc = fc;
	savtc = tabch;
	savlc = ldrch;
	tabch = ldrch = fc = IMP;
	for (j = 0; ; j++) {
		if ((tabtab[j] & TMASK) == 0) {
			if (x == savfc)
				fprintf(stderr, "troff: zero field width.\n");
			jj = 0;
			goto rtn;
		}
		v.hp = sumhp();	/* XXX */
		if ((length = ((tabtab[j] & TMASK) - v.hp)) > 0 )
			break;
	}
	type = tabtab[j] & (~TMASK);
	fp = fbuf;
	pp = padptr;
	if (x == savfc) {
		while (1) {
			if (((j = cbits(ii = getch()))) == padc) {
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
			ws += width(ii);
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
		cp = fbuf;
		jj = 0;
	} else if (type == 0) {
		/*plain tab or leader*/
		if ((j = width(rchar)) == 0)
			nchar = 0;
		else {
			nchar = length / j;
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
			ws += width(ii);
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
		if (((j = width(rchar)) == 0) || (length <= 0))
			nchar = 0;
		else {
			nchar = length / j;
			length %= j;
		}
		length = (length / HOR) * HOR;
		jj = makem(length);
		cp = fbuf;
		nlflg = 0;
	}
rtn:
	fc = savfc;
	tabch = savtc;
	ldrch = savlc;
	return(jj);
}


