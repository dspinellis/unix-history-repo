/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1nuC.c,v 1.4 85/08/22 16:50:36 timo Exp $
*/

#include <ctype.h>
#include "b.h"
#include "b0con.h"
#include "b0fea.h"
#include "b1obj.h"
#include "b1mem.h"
#include "b1num.h"
#include "b2syn.h" /* temporary until numconst is fixed */

char *sprintf(); /* OS */
extern value tento();
extern integer int_tento();

#define EXPDIGITS 10	/* Extra positions to allow for exponent part */
			/* -- must be larger than tenlogBASE */
#define MAXDIGITS (MAXNUMDIG-1)	/* Max precision for fixed/floating numbers */
#define CONVBUFSIZE (MAXDIGITS+4)
			/* Maximum number of digits to print in integer notation */
			/* (4 is the size of 'e+00' added by sprintf) */


/* Convert an integer to a C character string.
   The character string is overwritten on each next call.
   It assumes BASE is a power of 10. */

Hidden char *convint(v) register integer v; {
	static char *buffer, shortbuffer[tenlogBASE+3];
	static char fmt[10];
	register char *cp;
	register int i;
	bool neg = No;

	if (IsSmallInt(v)) {
		sprintf(shortbuffer, "%d", SmallIntVal(v));
		return shortbuffer;
	}

	if (Digit(v, Length(v)-1) < 0) {
		neg = Yes;
		v = int_neg(v);
	}
	if (buffer) freemem(buffer);
	buffer = getmem((unsigned)(Length(v)*tenlogBASE + 1 + neg));
	cp = buffer;
	if (neg) *cp++ = '-';
	sprintf(cp, "%d", Msd(v));
	if (!IsSmallInt(v)) {
		if (!*fmt) sprintf(fmt, "%%0%dd", tenlogBASE);
		while (*cp) ++cp;
		for (i = Length(v)-2; i >= 0; --i, cp += tenlogBASE)
			sprintf(cp, fmt, Digit(v, i));
		if (neg) release((value) v);
	}
	return buffer;
}

#ifdef EXT_RANGE

/* This is terrible.  But never mind, it'll all change (sometimes). */

Hidden bool hugenumber(v) value v; {
	bool huge;
	real w = (real) approximate(v);
	huge = Expo(w) > Maxexpo || Expo(w) < Minexpo && Frac(w) != 0;
	release((value)w);
	return huge;
}


Hidden string convapp(v) value v; {
	value absv, tenlogv, expo, tentoexpo, frac;
	static char buf[100];
	char fmt[15];
	int precision;
	double fracval, expoval, i;

	absv = absval(v);
	tenlogv = log2((value)int_10, absv), release(absv);
	expo = floorf(tenlogv), release(tenlogv);
	expoval = numval(expo), release(expo);
	if (expoval*tenlogBASE >= Maxintlet || expoval*tenlogBASE <= -Maxintlet) {
		expo = (value) mk_approx(expoval, 0.0);
		tentoexpo = power((value)int_10, expo), release(expo);
	}
	else
		tentoexpo = tento((int)expoval);
	frac = quot(v, tentoexpo), release(tentoexpo);
	fracval = numval(frac), release(frac);
	while (fabs(fracval) >= 10) fracval /= 10, ++expoval;
	while (fabs(fracval) < 1) fracval *= 10, --expoval;
	precision = MAXDIGITS;
	i = expoval < 0 ? -expoval : expoval;
	while (i >= 10 && precision > 2) --precision, i /= 10;
		/* Loose precision for large exponents! */
		/* :-( But keep some too! )-: */
	sprintf(fmt, "%%.%dlgE%%s%%2.0lf", precision);
	sprintf(buf, fmt, fracval, expoval >= 0 ? "+" : "", expoval);
	return buf;
}

#endif EXT_RANGE

/* Convert a numeric value to a C character string.
   The character string is overwritten on each next call. */

Visible string convnum(v) register value v; {
	static char convbuf[3+CONVBUFSIZE+EXPDIGITS];
		/* 3 extra for things (sign, 0.) to be stuck on front of it */
	static char fmt[10];
	char *bufstart = convbuf+3;
	register char *cp = bufstart;
	double x;

	if (Integral(v)) return convint((integer)v);
#ifdef EXT_RANGE
	if (hugenumber(v)) return convapp(v);
#endif

	/* Reasonably-sized reals and rationals are treated alike.
	   However, not-too-large rationals resulting from
	   'n round x' are transformed to f-format. */

	x = numval(v);
	if (!*fmt) sprintf(fmt, "%%.%dlg", MAXDIGITS);
	sprintf(bufstart, fmt, x);

	for (cp = bufstart; *cp != '\0'; ++cp)
		if (*cp == 'e') {	/* change sprintf's 'e' to 'E' */
			*cp = 'E';
			break;
		}

#ifdef IBMPC
	if (*cp != 'E') {
		/* Delete trailing zeros after decimal pt; don't rely on %g */
		for (cp = bufstart; *cp != '\0' && *cp != '.'; ++cp)
			;
		if (*cp == '.') {
			char *ep;
			for (; *cp != '\0' && *cp != 'E'; ++cp)
				;
			ep = cp;
			while (*--cp == '0')
				;
			if (++cp < ep) {
				while (*ep != '\0')
					*cp++ = *ep++;
				*cp = '\0';
			}
		}
	}
#endif IBMPC

	if (Rational(v) && Roundsize(v) > 0 && *cp != 'E') {
		int i = Roundsize(v);
		int j = 1;
			/* Counts digits allowed beyond MAXDIGITS, 1 for '.' */

		for (cp = bufstart; *cp == '0'; ++cp)
			++j; /* Allow a trailing zero for each leading zero */

		for (; *cp != '\0' && *cp != '.'; ++cp)
			; /* Find '.' or end of string */

		if (*cp == '\0') {
			*cp = '.'; /* Append '.' if not found */
			*++cp = '\0';
		}
		else {
			while (*++cp == '0')
				/* Allow more precision if leading zeros */
				++j, --i;
			while (*cp != '\0')
				--i, ++cp; /* Find last digit */
		}

		/* Append extra zeros (but don't show more precision
		   than sprintf can!) */
		while (--i >= 0 && cp < bufstart+MAXDIGITS+j)
			*cp++ = '0';

		*cp = '\0'; /* Append new terminating null byte */
	}

	return bufstart;
}


/* Convert a string to a number (assume it's syntactically correct!).
   Pointers to the first and last+1 characters are given.
   Again, BASE must be a power of 10.
   ********** NEW **********
   If E_EXACT is defined, all numbers input are made exact, even if
   E-notation is used.
   ********** WARNING **********
   This routine must be fixed, because it accesses the source buffer
   and it shouldn't because it's in the wrong place in the hierarchy
*/

Visible value numconst(text, end) register txptr text, end; {
	register txptr tp;
	register int numdigs, fraclen;
	integer a;
	register digit accu;
	value c;

	if (Char(text) == 'E') a = int_1;
	else {
		while (text<end && Char(text)=='0') ++text; /* Skip leading zeros */

		for (tp = text; tp<end && isdigit(Char(tp)); ++tp)
			; /* Count integral digits */
		numdigs = tp-text;
		fraclen = 0;
		if (tp<end && Char(tp)=='.') {
			++tp;
			for (; tp<end && isdigit(Char(tp)); ++tp)
				++fraclen; /* Count fractional digits */
			numdigs += fraclen;
		}
		a = (integer) grab_num((numdigs+tenlogBASE-1) / tenlogBASE);
		if (!a) return Vnil; /* Recovered error */
		accu = 0;
		/* Integer part: */
		for (; text<end && isdigit(Char(text)); ++text) {
			accu = accu*10 + Char(text)-'0';
			--numdigs;
			if (numdigs%tenlogBASE == 0) {
				Digit(a, numdigs/tenlogBASE) = accu;
				accu = 0;
			}
		}
		/* Fraction: */
		if (text < end && Char(text) == '.') {
			++text;
			for (; text<end && isdigit(Char(text)); ++text) {
				accu = accu*10 + Char(text)-'0';
				--numdigs;
				if (numdigs%tenlogBASE == 0) {
					Digit(a, numdigs/tenlogBASE) = accu;
					accu = 0;
				}
			}
		}
		if (numdigs != 0) syserr(MESS(800, "numconst: can't happen"));
		a = int_canon(a);
	}

	/* Exponent: */
	if (text >= end || Char(text) != 'E') {
		integer b = int_tento(fraclen);
		c = mk_exact(a, b, fraclen);
		release((value) b);
	}
	else {
		double expo = 0;
		int sign = 1;
		value b;
		++text;
		if (text < end) {
			if (Char(text) == '+') ++text;
			else if (Char(text) == '-') {
				++text;
				sign = -1;
			}
		}
		for (; text<end && isdigit(Char(text)); ++text) {
			expo = expo*10 + Char(text)-'0';
			if (expo > Maxint) {
				error(MESS(801, "excessive exponent in E-notation"));
				expo = 0;
				break;
			}
		}
		b = tento((int)expo * sign - fraclen);
#ifndef E_EXACT
		/* Make approximate number if E-notation used */
		c = approximate(b);
		release(b);
		b = c;
#endif
		if (a == int_1) c = b;
		else c = prod((value)a, b), release(b);
	}
	release((value) a);
	return c;
}


/*
 * printnum(f, v) writes a number v on file f in such a way that it
 * can be read back identically, assuming integral powers of ~2 can be
 * computed exactly.  (This is necessary for the permanent environment.)
 */

Visible Procedure printnum(f, v) FILE *f; value v; {
	if (Approximate(v)) {
#ifdef PRINT_APPROX
		if (Frac((real)v) == 0) fprintf(f, "~0");
		else {
			static char fmt[25];
			if (!*fmt)
			    sprintf(fmt, "%%.%dlgE0*~2**%%.0lf", MAXDIGITS+2);
			fprintf(f, fmt, Frac((real)v), Expo((real)v));
		}
		return;
#else
		fputc('~', f);
#endif
	}
	if (Rational(v) && Denominator((rational)v) != int_1) {
		int i = Roundsize(v);
		fputs(convnum((value)Numerator((rational)v)), f);
		if (i > 0 && i <= MAXDIGITS) {
			/* The assumption here is that in u/v, the Roundsize
			   of the result is the sum of that of the operands. */
			putc('.', f);
			do putc('0', f); while (--i > 0);
		}
		putc('/', f);
		v = (value) Denominator((rational)v);
	}
	fputs(convnum(v), f);
}
