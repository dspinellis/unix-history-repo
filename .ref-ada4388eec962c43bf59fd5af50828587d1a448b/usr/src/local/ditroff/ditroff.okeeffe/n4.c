#ifndef lint
static char sccsid[] = "@(#)n4.c	2.1 (CWI) 85/07/18";
#endif lint
/*      @(#)n4.c	1.1     */
#include	<ctype.h>
#include "tdef.h"
#ifdef NROFF
#include "tw.h"
#endif
#include "ext.h"
#include <sgtty.h>
/*
 * troff4.c
 * 
 * number registers, conversion, arithmetic
 */


int	regcnt = NNAMES;
int	falsef	= 0;	/* on if inside false branch of if */
#define	NHASH(i)	((i>>6)^i)&0177
struct	numtab	*nhash[128];	/* 128 == the 0177 on line above */

setn()
{
	register i, j;
	register tchar ii;
	int	f;

	f = nform = 0;
	if ((i = cbits(ii = getach())) == '+')
		f = 1;
	else if (i == '-')
		f = -1;
	else 
		ch = ii;
	if (falsef)
		f = 0;
	if ((i = getsn()) == 0)
		return;
	if ((i & 0177) == '.')
		switch (i >> BYTE) {
		case 's': 
			i = pts;	
			break;
		case 'v': 
			i = lss;		
			break;
		case 'f': 
			i = font;	
			break;
		case 'p': 
			i = pl;		
			break;
		case 't':  
			i = findt1();	
			break;
		case 'o': 
			i = po;		
			break;
		case 'l': 
			i = ll;		
			break;
		case 'i': 
			i = in;		
			break;
		case '$': 
			i = frame->nargs;		
			break;
		case 'A': 
			i = ascii;		
			break;
		case 'c': 
			i = numtab[CD].val;		
			break;
		case 'n': 
			i = lastl;		
			break;
		case 'a': 
			i = ralss;		
			break;
		case 'h': 
			i = dip->hnl;	
			break;
		case 'd':
			if (dip != d)
				i = dip->dnl; 
			else 
				i = numtab[NL].val;
			break;
		case 'u': 
			i = fi;		
			break;
		case 'j': 
			i = ad + 2 * admod;	
			break;
		case 'w': 
			i = widthp;
			break;
		case 'x': 
			i = nel;	
			break;
		case 'y': 
			i = un;		
			break;
		case 'T': 
			i = dotT;		
			break; /*-Tterm used in nroff*/
		case 'V': 
			i = VERT;		
			break;
		case 'H': 
			i = HOR;		
			break;
		case 'k': 
			i = ne;		
			break;
		case 'P': 
			i = print;		
			break;
		case 'L': 
			i = ls;		
			break;
		case 'R': 
			i = NN - regcnt;	
			break;
		case 'z': 
			i = dip->curd;
			*pbp++ = (i >> BYTE) & BYTEMASK;
			*pbp++ = i & BYTEMASK;
			return;
		case 'b': 
			i = bdtab[font];
			break;
		case 'F':
			cpushback(cfname[ifi]);
			return;
		case 'D':
			i = hyalg;	/* "Dialect" register (jaap) */
			break;
		case 'e':
			i = thresh;	/* (jaap) */
			break;

		default:
			goto s0;
		}
	else {
s0:
		if ((j = findr(i)) == -1)
			i = 0;
		else {
			i = numtab[j].val = (numtab[j].val+numtab[j].inc*f);
			nform = numtab[j].fmt;
		}
	}
	setn1(i, nform, (tchar) 0);
}

tchar	numbuf[17];
tchar	*numbufp;

wrc(i)
tchar i;
{
	if (numbufp >= &numbuf[16])
		return(0);
	*numbufp++ = i;
	return(1);
}



/* insert into input number i, in format form, with size-font bits bits */
setn1(i, form, bits)
int	i;
tchar bits;
{
	extern int	wrc();

	numbufp = numbuf;
	nrbits = bits;
	nform = form;
	fnumb(i, wrc);
	*numbufp = 0;
	pushback(numbuf);
}


nrehash()
{
	register struct numtab *p;
	register i;

	for (i=0; i<128; i++)
		nhash[i] = 0;
	for (p=numtab; p < &numtab[NN]; p++)
		p->link = 0;
	for (p=numtab; p < &numtab[NN]; p++) {
		if (p->r == 0)
			continue;
		i = NHASH(p->r);
		p->link = nhash[i];
		nhash[i] = p;
	}
}

nunhash(rp)
register struct numtab *rp;
{	
	register struct numtab *p;
	register struct numtab **lp;

	if (rp->r == 0)
		return;
	lp = &nhash[NHASH(rp->r)];
	p = *lp;
	while (p) {
		if (p == rp) {
			*lp = p->link;
			p->link = 0;
			return;
		}
		lp = &p->link;
		p = p->link;
	}
}

findr(i)
register int	i;
{
	register struct numtab *p;
	register h = NHASH(i);

	if (i == 0)
		return(-1);
	for (p = nhash[h]; p; p = p->link)
		if (i == p->r)
			return(p - numtab);
	for (p = numtab; p < &numtab[NN]; p++) {
		if (p->r == 0) {
			p->r = i;
			p->link = nhash[h];
			nhash[h] = p;
			regcnt++;
			return(p - numtab);
		}
	}
	errprint("too many number registers (%d).", NN);
	done2(04); 
	/* NOTREACHED */
}

usedr(i)	/* returns -1 if nr i has never been used */
register int	i;
{
	register struct numtab *p;

	if (i == 0)
		return(-1);
	for (p = nhash[NHASH(i)]; p; p = p->link)
		if (i == p->r)
			return(p - numtab);
	return -1;
}


fnumb(i, f)
register int	i, (*f)();
{
	register j;

	j = 0;
	if (i < 0) {
		j = (*f)('-' | nrbits);
		i = -i;
	}
	switch (nform) {
	default:
	case '1':
	case 0: 
		return decml(i, f) + j;
		break;
	case 'i':
	case 'I': 
		return roman(i, f) + j;
		break;
	case 'a':
	case 'A': 
		return abc(i, f) + j;
		break;
	}
}


decml(i, f)
register int	i, (*f)();
{
	register j, k;

	k = 0;
	nform--;
	if ((j = i / 10) || (nform > 0))
		k = decml(j, f);
	return(k + (*f)((i % 10 + '0') | nrbits));
}


roman(i, f)
int	i, (*f)();
{

	if (!i)
		return((*f)('0' | nrbits));
	if (nform == 'i')
		return(roman0(i, f, "ixcmz", "vldw"));
	else 
		return(roman0(i, f, "IXCMZ", "VLDW"));
}


roman0(i, f, onesp, fivesp)
int	i, (*f)();
char	*onesp, *fivesp;
{
	register q, rem, k;

	k = 0;
	if (!i)
		return(0);
	k = roman0(i / 10, f, onesp + 1, fivesp + 1);
	q = (i = i % 10) / 5;
	rem = i % 5;
	if (rem == 4) {
		k += (*f)(*onesp | nrbits);
		if (q)
			i = *(onesp + 1);
		else 
			i = *fivesp;
		return(k += (*f)(i | nrbits));
	}
	if (q)
		k += (*f)(*fivesp | nrbits);
	while (--rem >= 0)
		k += (*f)(*onesp | nrbits);
	return(k);
}


abc(i, f)
int	i, (*f)();
{
	if (!i)
		return((*f)('0' | nrbits));
	else 
		return(abc0(i - 1, f));
}


abc0(i, f)
int	i, (*f)();
{
	register j, k;

	k = 0;
	if (j = i / 26)
		k = abc0(j - 1, f);
	return(k + (*f)((i % 26 + nform) | nrbits));
}

long	atoi0()
{
	register c, k, cnt;
	register tchar ii;
	long	i, acc;
	extern long	ckph();

	i = 0; 
	acc = 0;
	nonumb = 0;
	cnt = -1;
a0:
	cnt++;
	ii = getch();
	c = cbits(ii);
	switch (c) {
	default:
		ch = ii;
		if (cnt)
			break;
	case '+':
		i = ckph();
		if (nonumb)
			break;
		acc += i;
		goto a0;
	case '-':
		i = ckph();
		if (nonumb)
			break;
		acc -= i;
		goto a0;
	case '*':
		i = ckph();
		if (nonumb)
			break;
		acc *= i;
		goto a0;
	case '/':
		i = ckph();
		if (nonumb)
			break;
		if (i == 0) {
			flusho();
			errprint("divide by zero.");
			acc = 0;
		} else 
			acc /= i;
		goto a0;
	case '%':
		i = ckph();
		if (nonumb)
			break;
		acc %= i;
		goto a0;
	case '&':	/*and*/
		i = ckph();
		if (nonumb)
			break;
		if ((acc > 0) && (i > 0))
			acc = 1; 
		else 
			acc = 0;
		goto a0;
	case ':':	/*or*/
		i = ckph();
		if (nonumb)
			break;
		if ((acc > 0) || (i > 0))
			acc = 1; 
		else 
			acc = 0;
		goto a0;
	case '=':
		if (cbits(ii = getch()) != '=')
			ch = ii;
		i = ckph();
		if (nonumb) {
			acc = 0; 
			break;
		}
		if (i == acc)
			acc = 1;
		else 
			acc = 0;
		goto a0;
	case '>':
		k = 0;
		if (cbits(ii = getch()) == '=')
			k++; 
		else 
			ch = ii;
		i = ckph();
		if (nonumb) {
			acc = 0; 
			break;
		}
		if (acc > (i - k))
			acc = 1; 
		else 
			acc = 0;
		goto a0;
	case '<':
		k = 0;
		if (cbits(ii = getch()) == '=')
			k++; 
		else 
			ch = ii;
		i = ckph();
		if (nonumb) {
			acc = 0; 
			break;
		}
		if (acc < (i + k))
			acc = 1; 
		else 
			acc = 0;
		goto a0;
	case ')': 
		break;
	case '(':
		acc = atoi0();
		goto a0;
	}
	return(acc);
}


long	ckph()
{
	register tchar i;
	register long	j;
	extern long	atoi0();
	extern long	atoi1();

	if (cbits(i = getch()) == '(')
		j = atoi0();
	else {
		j = atoi1(i);
	}
	return(j);
}


long	atoi1(ii)
register tchar ii;
{
	register i, j, digits;
	register long	acc;
	int	neg, abs, field;

	neg = abs = field = digits = 0;
	acc = 0;
	for (;;) {
		i = cbits(ii);
		switch (i) {
		default:
			break;
		case '+':
			ii = getch();
			continue;
		case '-':
			neg = 1;
			ii = getch();
			continue;
		case '|':
			abs = 1 + neg;
			neg = 0;
			ii = getch();
			continue;
		}
		break;
	}
a1:
	while (i >= '0' && i <= '9') {
		field++;
		digits++;
		acc = 10 * acc + i - '0';
		ii = getch();
		i = cbits(ii);
	}
	if (i == '.') {
		field++;
		digits = 0;
		ii = getch();
		i = cbits(ii);
		goto a1;
	}
	if (!field) {
		ch = ii;
		goto a2;
	}
	switch (i) {
	case 'u':
		i = j = 1;	/* should this be related to HOR?? */
		break;
	case 'v':	/*VSs - vert spacing*/
		j = lss;
		i = 1;
		break;
	case 'm':	/*Ems*/
		j = EM;
		i = 1;
		break;
	case 'n':	/*Ens*/
		j = EM;
#ifndef NROFF
		i = 2;
#endif
#ifdef NROFF
		i = 1;	/*Same as Ems in NROFF*/
#endif
		break;
	case 'p':	/*Points*/
		j = INCH;
		i = 72;
		break;
	case 'i':	/*Inches*/
		j = INCH;
		i = 1;
		break;
	case 'c':	/*Centimeters*/
		/* if INCH is too big, this will overflow */
		j = INCH * 50;
		i = 127;
		break;
	case 'P':	/*Picas*/
		j = INCH;
		i = 6;
		break;
	default:
		j = dfact;
		ch = ii;
		i = dfactd;
	}
	if (neg) 
		acc = -acc;
	if (!noscale) {
		acc = (acc * j) / i;
	}
	if ((field != digits) && (digits > 0))
		while (digits--)
			acc /= 10;
	if (abs) {
		if (dip != d)
			j = dip->dnl; 
		else 
			j = numtab[NL].val;
		if (!vflag) {
			j = numtab[HP].val;
		}
		if (abs == 2)
			j = -j;
		acc -= j;
	}
a2:
	nonumb = !field;
	return(acc);
}


caserr()
{
	register i, j;
	register struct numtab *p;

	lgf++;
	while (!skip() && (i = getrq()) ) {
		j = usedr(i);
		if (j < 0)
			continue;
		p = &numtab[j];
		nunhash(p);
		p->r = p->val = p->inc = p->fmt = 0;
		regcnt--;
	}
}


casenr()
{
	register i, j;

	lgf++;
	skip();
	if ((i = findr(getrq())) == -1)
		goto rtn;
	skip();
	j = inumb(&numtab[i].val);
	if (nonumb)
		goto rtn;
	numtab[i].val = j;
	skip();
	j = atoi();
	if (nonumb)
		goto rtn;
	numtab[i].inc = j;
rtn:
	return;
}


caseaf()
{
	register i, k;
	register tchar j;

	lgf++;
	if (skip() || !(i = getrq()) || skip())
		return;
	k = 0;
	j = getch();
	if (!isalpha(cbits(j))) {
		ch = j;
		while ((j = cbits(getch())) >= '0' &&  j <= '9')
			k++;
	}
	if (!k)
		k = j;
	numtab[findr(i)].fmt = k & BYTEMASK;
}

setaf()	/* return format of number register */
{
	register int i, j;

	i = usedr(getsn());
	if (i == -1)
		return;
	if (numtab[i].fmt > 20)	/* it was probably a, A, i or I */
		*pbp++ = numtab[i].fmt;
	else
		for (j = (numtab[i].fmt ? numtab[i].fmt : 1); j; j--)
			*pbp++ = '0';
}


vnumb(i)
int	*i;
{
	vflag++;
	dfact = lss;
	res = VERT;
	return(inumb(i));
}


hnumb(i)
int	*i;
{
	dfact = EM;
	res = HOR;
	return(inumb(i));
}


inumb(n)
int	*n;
{
	register i, j, f;
	register tchar ii;

	f = 0;
	if (n) {
		if ((j = cbits(ii = getch())) == '+')
			f = 1;
		else if (j == '-')
			f = -1;
		else 
			ch = ii;
	}
	i = atoi();
	if (n && f)
		i = *n + f * i;
	i = quant(i, res);
	vflag = 0;
	res = dfactd = dfact = 1;
	if (nonumb)
		i = 0;
	return(i);
}


quant(n, m)
int	n, m;
{
	register i, neg;

	neg = 0;
	if (n < 0) {
		neg++;
		n = -n;
	}
	/* better as i = ((n + (m/2))/m)*m */
	i = n / m;
	if ((n - m * i) > (m / 2))
		i += 1;
	i *= m;
	if (neg)
		i = -i;
	return(i);
}


