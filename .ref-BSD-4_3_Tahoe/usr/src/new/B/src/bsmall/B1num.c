/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1num.c,v 1.1 84/06/28 00:48:56 timo Exp $ */

/* B numbers, small version */

	/*
	 * THIS VERSION SHOULD ONLY BE USED IF
	 * THE SYSTEM IS TOO LARGE OTHERWISE.
	 * IT USES FLOATING POINT ARITHMETIC FOR EXACT NUMBERS
	 * INSTEAD OF ARBITRARY LENGTH RATIONAL ARITHMETIC.
	 */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b2syn.h" /* for def of Keymark() only */
#include "B1num.h"

value numerator(v) value v; {
	Checknum(v);
	if (!Exact(v)) error("*/ on approximate number");
	return mk_int(Numerator(v));
}

value denominator(v) value v; {
	Checknum(v);
	if (!Exact(v)) error("/* on approximate number"); /* */
	return mk_int(Denominator(v));
}

double numval(v) value v; {
	Checknum(v);
	return Numval(v);
}

checkint(v) value v; {
	Checknum(v);
	if (Denominator(v) != One) error("number not an integer");
}

bool large(v) value v; {
	checkint(v);
	if (Numerator(v) < -Maxint || Numerator(v) > Maxint) return Yes;
	return No;
}
	
int intval(v) value v; {
	checkint(v);
	return (int)Numerator(v);
}

intlet propintlet(i) int i; {
	if (i < -Maxintlet || i > Maxintlet)
		error("exceedingly large integer");
	return i;
}

integer gcd(i, j) integer i, j; {
	integer k;
	if (i == Zero && j == Zero) syserr("gcd(0, 0)");
	if (i != floor(i) || j != floor(j))
		syserr("gcd called with non-integer");
	if (i < Zero) i= -i; if (j < Zero) j= -j;
	if (i < j) {
		k= i; i= j; j= k;
	}
	while (j >= One) {
		k= i-j*floor(i/j);
		i= j; j= k;
	}
	if (j != Zero) error(
		"arithmetic overflow while simplifying exact number");
	if (i != floor(i)) syserr("gcd returns non-integer");
	return i;
}

value b_zero, b_one, b_minus_one, zero, one;

value mk_exact(p, q, len) register integer p, q; intlet len; {
	value v; integer d;
	if (q == One && len ==0) {
		if (p == Zero) return copy(b_zero);
		if (p == One)  return copy(b_one);
		if (p == -One) return copy(b_minus_one);
	}
	v= grab_num(len);
	if (q == One) {
		Numerator(v)= p; Denominator(v)= q;
		return v;
	}
	if (q == Zero) error("attempt to make exact number with denominator 0");
	if (q < Zero) {p= -p; q= -q;}
	d= (q == One ? One : p == One ? One : gcd(p, q));
	Numerator(v)= p/d; Denominator(v)= q/d;
	return v;
}

bool integral(v) value v; {
	return Integral(v);
}

value mk_integer(p) int p; {
	return mk_exact((integer)p, One, 0);
}

value mk_int(p) integer p; {
	return mk_exact(p, One, 0);
}

value mk_approx(x) register double x; {
	value v= grab_num(0);
	Approxval(v)= x; Denominator(v)= Zero;
	return v;
}

initnum() {
	b_zero= grab_num(0);
		Numerator(b_zero)= Zero; Denominator(b_zero)= One;
	b_one= grab_num(0);
		Numerator(b_one)= One; Denominator(b_one)= One;
	b_minus_one= grab_num(0);
		Numerator(b_minus_one)= -One; Denominator(b_minus_one)= One;
	zero= mk_integer(0);
	one= mk_integer(1);
}

value approximate(v) value v; {
	if (!Exact(v)) return copy(v);
	return mk_approx(Numerator(v)/Denominator(v));
}

numcomp(v, w) value v, w; {
	double vv= Numval(v), ww= Numval(w);
	if (vv < ww) return -1;
	if (vv > ww) return  1;
	if (Exact(v) && Exact(w)) return 0;
	if (Exact(v)) return -1; /* 1 < 1E0 */
	if (Exact(w)) return  1; /* 1E0 > 1 */
	return 0;
}

double numhash(v) value v; {
	number *n= (number *)Ats(v);
	return .123*n->p + .777*n->q;
}

#define CONVBUFSIZ 100
char convbuf[CONVBUFSIZ];

string convnum(v) value v; {
	double x; string bp; bool prec_loss= No;
	Checknum(v);
	x= Numval(v);
 conv:	if (!prec_loss && Exact(v) && fabs(x) <= LONG &&
		fabs(Numerator(v)) < BIG && fabs(Denominator(v)) < BIG) {
		intlet len= 0 < Length(v) && Length(v) <= MAXNUMDIG ? Length(v) : 0;
		intlet dcnt, sigcnt; bool sig;
		if (Denominator(v) != One) {
			intlet k; double p= 1.0, q;
			prec_loss= Yes;
			for (k= 1; k < MAXNUMDIG; k++) {
				p*= 10.0;
				q= p/Denominator(v);
				if (k >= len && q == floor(q)) {
					prec_loss= No;
					break;
				}
			}
			len= k;
		}
	convex:	sprintf(convbuf, "%.*f", len, x);
		dcnt= sigcnt= 0; sig= No;
		for (bp= convbuf; *bp != '\0'; bp++) 
			if ('0' <= *bp && *bp <= '9') {
				dcnt++;
				if (*bp != '0') sig= Yes;
				if (sig) sigcnt++;
			}
		if (sigcnt < MINNUMDIG && prec_loss) goto conv;
		if (dcnt > MAXNUMDIG) {
			if (len <= 0) syserr("conversion error 1");
			if (Denominator(v) == One) len= 0;
			else len-= dcnt-MAXNUMDIG;
			if (len < 0) syserr("conversion error 2");
			goto convex;
		}
	} else { /*approx etc*/
		sprintf(convbuf, "%.*e", MAXNUMDIG-5, x);
		for (bp= convbuf; *bp != '\0'; bp++)
		if (*bp == 'e') {
			*bp= 'E';
			break;
		}
	}
	return convbuf;
}

value numconst(tx, q) txptr tx, q; {
	bool dig= No; double ex= 0, ap= 1; intlet ndap, len= 0;
	while (tx < q && '0' <= *tx && *tx <= '9') {
		dig= Yes;
		ex= 10*ex+(*tx++ - '0');
	}
	if (tx < q && *tx == '.') {
		tx++; ndap= 0;
		while (tx < q && '0' <= *tx && *tx <= '9') {
			dig= Yes; ndap++;
			len= *tx == '0' ? ndap : 0;
			ex= 10*ex+(*tx++ - '0'); ap*= 10;
		}
		if (!dig) syserr("numconst[1]");
	}
	if (tx < q && *tx == 'E') {
		intlet sign= 1; double expo= 0;
		tx++;
		if (!('0' <= *tx && *tx <= '9') && Keymark(*tx)) {
			tx--;
			goto exact;
		}
		if (!dig) ex= 1;
		if (tx < q && (*tx == '+' || *tx == '-'))
			if (*tx++ == '-') sign= -1;
		dig= No;
		while (tx < q && '0' <= *tx && *tx <= '9') {
			dig= Yes;
			expo= 10*expo+(*tx++ - '0');
		}
		if (!dig) syserr("numconst[2]");
		return mk_approx(ex/ap*exp(sign*expo*log(10.0)));
	}
exact:	return mk_exact(ex, ap, len);
}

printnum(f1, v) FILE *f1; value v; {
	FILE *f= f1 ? f1 : stdout;
	if (!Exact(v) || Denominator(v) == One) {
		if (!Exact(v))
			fputc('~', f);
		fputs(convnum(v), f);
	}
	else {
		value w = numerator(v);
		fputs(convnum(w), f);
		release(w);
		fputc('/', f);
		w = denominator(v);
		fputs(convnum(w), f);
		release(w);
	}
	if (!f1) fputc('\n', f); /* Flush buffer for sdb */
}
