#include	<ctype.h>
#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
#ifdef NROFF
extern
#include "tw.h"
#endif
#include "s.h"
/*
troff4.c

number registers, conversion, arithmetic
*/

#include <sgtty.h>
#include "ext.h"

int	regcnt = NNAMES;
int	falsef	= 0;	/* on if inside false branch of if */

setn()
{
	register i, j;
	tchar ii;
	int	f;

	f = nform = 0;
	lgf++;					/* don't catch ligatures */
	if ((i = cbits(ii = getch())) == '+')
		f = 1;
	else if (i == '-')
		f = -1;
	else 
		ch = ii;
	lgf--;					/* ok, catch `em again */
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
			i = v.cd;		
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
				i = v.nl;
			break;
		case 'u': 
			i = fi;		
			break;
		case 'j': 
			i = ad + 2 * admod;	
			break;
		case 'w': 
			i = width(*(pinchar-1));	/* XXX */
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
			cbuf[0] = i & BMASK;
			cbuf[1] = (i >> BYTE) & BMASK;
			cbuf[2] = 0;
			cp = cbuf;
			return;
		case 'b': 
			i = bdtab[font];
			break;

		default:
			goto s0;
		}
	else {
s0:
		if ((j = findr(i)) == -1)
			i = 0;
		else {
			i = (vlist[j] = (vlist[j] + inc[j] * f));
			nform = fmt[j];
		}
	}
	setn1(i);
	cp = cbuf;
}


setn1(i)
int	i;
{
	extern int	wrc();

	cp = cbuf;
	nrbits = 0;
	fnumb(i, wrc);
	*cp = 0;
	cp = cbuf;
}


findr(i)
register int	i;
{
	register j;
	register int *p;

	if (i == 0)
		return(-1);
	for (p = r; p < &r[NN]; p++) {
		if (i == *p)
			break;
	}
	if (p != &r[NN])
		return(p - r);
	for (p = r; p < &r[NN]; p++) {
		if (*p == 0) {
			*p = i;
			regcnt++;
			break;
		}
	}
	if (p == &r[NN]) {
		fprintf(stderr, "troff: too many number registers (%d).\n", NN);
		done2(04); 
	}
	return(p - r);
}

usedr(i)	/* returns -1 if nr i has never been used */
register int	i;
{
	register j;
	register int *p;

	if (i == 0)
		return(-1);
	for (p = r; p < &r[NN]; p++) {
		if (i == *p)
			break;
	}
	if (p != &r[NN])
		return(p - r);
	else
		return -1;
}


fnumb(i, f)
int	i, (*f)();
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
		return(decml(i, f) + j);
	case 'i':
	case 'I': 
		return(roman(i, f) + j);
	case 'a':
	case 'A': 
		return(abc(i, f) + j);
	}
}


decml(i, f)
int	i, (*f)();
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


wrc(i)
tchar i;
{
	if (cp >= &cbuf[NC])
		return(0);
	*cp++ = i;
	return(1);
}


long	atoi0()
{
	register c, k, cnt;
	tchar ii;
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
			fprintf(stderr, "troff: divide by zero.\n");
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
	tchar i;
	long	j;
	extern long	atoi0();
	extern long	atoi1();

	if (cbits(i = getch()) == '(')
		j = atoi0();
	else {
		ch = i;
		j = atoi1();
	}
	return(j);
}


long	atoi1()
{
	register i, j, digits;
	tchar ii;
	long	acc;
	int	neg, abs, field;

	neg = abs = field = digits = 0;
	acc = 0;
a0:
	ii = getch();
	i = cbits(ii);
	switch (i) {
	default:
		ch = ii;
		break;
	case '+':
		goto a0;
	case '-':
		neg = 1;
		goto a0;
	case '|':
		abs = 1 + neg;
		neg = 0;
		goto a0;
	}
a1:
	while (((j = (cbits(ii = getch())) - '0') >= 0) && (j <= 9)) {
		field++;
		digits++;
		acc = 10 * acc + j;
	}
	if (cbits(ii) == '.') {
		field++;
		digits = 0;
		goto a1;
	}
	ch = ii;
	if (!field)
		goto a2;
	ii = getch();
	switch (i = cbits(ii)) {
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
			j = v.nl;
		if (!vflag) {
			j = v.hp = sumhp();	/* XXX */
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

	lgf++;
	while (!skip() && (i = getrq()) ) {
		for (j = NNAMES; j < NN; j++) {  /*NNAMES predefined names*/
			if (i == r[j])
				break;
		}
		if (j != NN) {
			r[j] = vlist[j] = inc[j] = fmt[j] = 0;
			regcnt--;
		}
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
	j = inumb(&vlist[i]);
	if (nonumb)
		goto rtn;
	vlist[i] = j;
	skip();
	j = atoi();
	if (nonumb)
		goto rtn;
	inc[i] = j;
rtn:
	return;
}


caseaf()
{
	register i, k;
	tchar j;

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
	fmt[findr(i)] = k & BMASK;
}

setaf()	/* return format of number register */
{
	register int i, j;

	i = usedr(getsn());
	if (i == -1)
		return;
	cp = cbuf;
	if (fmt[i] > 20)	/* it was probably a, A, i or I */
		*cp++ = fmt[i];
	else
		for (j = (fmt[i] ? fmt[i] : 1); j; j--)
			*cp++ = '0';
	*cp = 0;
	cp = cbuf;
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
	tchar ii;

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


