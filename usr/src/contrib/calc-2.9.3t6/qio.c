/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Scanf and printf routines for arbitrary precision rational numbers
 */

#include "stdarg.h"
#include "qmath.h"


#define	PUTCHAR(ch)		math_chr(ch)
#define	PUTSTR(str)		math_str(str)
#define	PRINTF1(fmt, a1)	math_fmt(fmt, a1)
#define	PRINTF2(fmt, a1, a2)	math_fmt(fmt, a1, a2)

int tilde_ok = TRUE;	/* FALSE => don't print '~' for rounded value */

#if 0
static long	etoalen;
static char	*etoabuf = NULL;
#endif

static long	scalefactor;
static ZVALUE	scalenumber = { 0, 0, 0 };


/*
 * Print a formatted string containing arbitrary numbers, similar to printf.
 * ALL numeric arguments to this routine are rational NUMBERs.
 * Various forms of printing such numbers are supplied, in addition
 * to strings and characters.  Output can actually be to any FILE
 * stream or a string.
 */
#ifdef VARARGS
# define VA_ALIST1 fmt, va_alist
# define VA_DCL1 char *fmt; va_dcl
#else
# if defined(__STDC__) && __STDC__ == 1
#  define VA_ALIST1 char *fmt, ...
#  define VA_DCL1
# else
#  define VA_ALIST1 fmt
#  define VA_DCL1 char *fmt;
# endif
#endif
/*VARARGS*/
void
qprintf(VA_ALIST1)
	VA_DCL1
{
	va_list ap;
	NUMBER *q;
	int ch, sign;
	long width, precision;

#ifdef VARARGS
	va_start(ap);
#else
	va_start(ap, fmt);
#endif
	while ((ch = *fmt++) != '\0') {
		if (ch == '\\') {
			ch = *fmt++;
			switch (ch) {
				case 'n': ch = '\n'; break;
				case 'r': ch = '\r'; break;
				case 't': ch = '\t'; break;
				case 'f': ch = '\f'; break;
				case 'v': ch = '\v'; break;
				case 'b': ch = '\b'; break;
				case 0:
					va_end(ap);
					return;
			}
			PUTCHAR(ch);
			continue;
		}
		if (ch != '%') {
			PUTCHAR(ch);
			continue;
		}
		ch = *fmt++;
		width = 0; precision = 8; sign = 1;
percent:	;
		switch (ch) {
			case 'd':
				q = va_arg(ap, NUMBER *);
				qprintfd(q, width);
				break;
			case 'f':
				q = va_arg(ap, NUMBER *);
				qprintff(q, width, precision);
				break;
			case 'e':
				q = va_arg(ap, NUMBER *);
				qprintfe(q, width, precision);
				break;
			case 'r':
			case 'R':
				q = va_arg(ap, NUMBER *);
				qprintfr(q, width, (BOOL) (ch == 'R'));
				break;
			case 'N':
				q = va_arg(ap, NUMBER *);
				zprintval(q->num, 0L, width);
				break;
			case 'D':
				q = va_arg(ap, NUMBER *);
				zprintval(q->den, 0L, width);
				break;
			case 'o':
				q = va_arg(ap, NUMBER *);
				qprintfo(q, width);
				break;
			case 'x':
				q = va_arg(ap, NUMBER *);
				qprintfx(q, width);
				break;
			case 'b':
				q = va_arg(ap, NUMBER *);
				qprintfb(q, width);
				break;
			case 's':
				PUTSTR(va_arg(ap, char *));
				break;
			case 'c':
				PUTCHAR(va_arg(ap, int));
				break;
			case 0:
				va_end(ap);
				return;
			case '-':
				sign = -1;
				ch = *fmt++;
			default:
		if (('0' <= ch && ch <= '9') || ch == '.' || ch == '*') {
			if (ch == '*') {
				q = va_arg(ap, NUMBER *);
				width = sign * qtoi(q);
				ch = *fmt++;
			} else if (ch != '.') {
				width = ch - '0';
				while ('0' <= (ch = *fmt++) && ch <= '9')
					width = width * 10 + ch - '0';
				width *= sign;
			}
			if (ch == '.') {
				if ((ch = *fmt++) == '*') {
					q = va_arg(ap, NUMBER *);
					precision = qtoi(q);
					ch = *fmt++;
				} else {
					precision = 0;
					while ('0' <= (ch = *fmt++) && ch <= '9')
						precision = precision * 10 + ch - '0';
				}
			}
			goto percent;
		}
		}
	}
	va_end(ap);
}


#if 0
/*
 * Read a number from the specified FILE stream (NULL means stdin).
 * The number can be an integer, a fraction, a real number, an
 * exponential number, or a hex, octal or binary number.  Leading blanks
 * are skipped.  Illegal numbers return NULL.  Unrecognized characters
 * remain to be read on the line.
 *	q = qreadval(fp);
 */
NUMBER *
qreadval(fp)
	FILE *fp;		/* file stream to read from (or NULL) */
{
	NUMBER *r;		/* returned number */
	char *cp; 		/* current buffer location */
	long savecc;		/* characters saved in buffer */
	long scancc; 		/* characters parsed correctly */
	int ch;			/* current character */

	if (fp == NULL)
		fp = stdin;
	if (etoabuf == NULL) {
		etoabuf = (char *)malloc(OUTBUFSIZE + 2);
		if (etoabuf == NULL)
			return NULL;
		etoalen = OUTBUFSIZE;
	}
	cp = etoabuf;
	ch = fgetc(fp);
	while ((ch == ' ') || (ch == '\t'))
		ch = fgetc(fp);
	savecc = 0;
	for (;;) {
		if (ch == EOF)
			return NULL;
		if (savecc >= etoalen)
		{
			cp = (char *)realloc(etoabuf, etoalen + OUTBUFSIZE + 2);
			if (cp == NULL)
				return NULL;
			etoabuf = cp;
			etoalen += OUTBUFSIZE;
			cp += savecc;
		}
		*cp++ = (char)ch;
		*cp = '\0';
		scancc = qparse(etoabuf, QPF_SLASH);
		if (scancc != ++savecc)
			break;
		ch = fgetc(fp);
	}
	ungetc(ch, fp);
	if (scancc < 0)
		return NULL;
	r = atoq(etoabuf);
	if (ziszero(r->den)) {
		qfree(r);
		r = NULL;
	}
	return r;
}
#endif


/*
 * Print a number in the specified output mode.
 * If MODE_DEFAULT is given, then the default output mode is used.
 * Any approximate output is flagged with a leading tilde.
 * Integers are always printed as themselves.
 */
void
qprintnum(q, outmode)
	int outmode;
	NUMBER *q;
{
	NUMBER tmpval;
	long prec, exp;

	if (outmode == MODE_DEFAULT)
		outmode = _outmode_;
	if ((outmode == MODE_FRAC) || ((outmode == MODE_REAL) && qisint(q))) {
		qprintfr(q, 0L, FALSE);
		return;
	}
	switch (outmode) {
		case MODE_INT:
			if (tilde_ok && qisfrac(q))
				PUTCHAR('~');
			qprintfd(q, 0L);
			break;

		case MODE_REAL:
			prec = qplaces(q);
			if ((prec < 0) || (prec > _outdigits_)) {
				prec = _outdigits_;
				if (tilde_ok) {
				    PUTCHAR('~');
				}
			}
			qprintff(q, 0L, prec);
			break;

		case MODE_EXP:
			if (qiszero(q)) {
				PUTCHAR('0');
				return;
			}
			tmpval = *q;
			tmpval.num.sign = 0;
			exp = qilog10(&tmpval);
			if (exp == 0) {		/* in range to output as real */
				qprintnum(q, MODE_REAL);
				return;
			}
			tmpval.num = _one_;
			tmpval.den = _one_;
			if (exp > 0)
				ztenpow(exp, &tmpval.den);
			else
				ztenpow(-exp, &tmpval.num);
			q = qmul(q, &tmpval);
			zfree(tmpval.num);
			zfree(tmpval.den);
			qprintnum(q, MODE_REAL);
			qfree(q);
			PRINTF1("e%ld", exp);
			break;

		case MODE_HEX:
			qprintfx(q, 0L);
			break;

		case MODE_OCTAL:
			qprintfo(q, 0L);
			break;

		case MODE_BINARY:
			qprintfb(q, 0L);
			break;

		default:
			math_error("Bad mode for print");
	}
}


/*
 * Print a number in floating point representation.
 * Example:  193.784
 */
void
qprintff(q, width, precision)
	NUMBER *q;
	long width;
	long precision;
{
	ZVALUE z, z1;

	if (precision != scalefactor) {
		if (scalenumber.v)
			zfree(scalenumber);
		ztenpow(precision, &scalenumber);
		scalefactor = precision;
	}
	if (scalenumber.v)
		zmul(q->num, scalenumber, &z);
	else
		z = q->num;
	if (qisfrac(q)) {
		zquo(z, q->den, &z1);
		if (z.v != q->num.v)
			zfree(z);
		z = z1;
	}
	if (qisneg(q) && ziszero(z))
		PUTCHAR('-');
	zprintval(z, precision, width);
	if (z.v != q->num.v)
		zfree(z);
}


/*
 * Print a number in exponential notation.
 * Example: 4.1856e34
 */
/*ARGSUSED*/
void
qprintfe(q, width, precision)
	register NUMBER *q;
	long width;
	long precision;
{
	long exponent;
	NUMBER q2;
	ZVALUE num, den, tenpow, tmp;

	if (qiszero(q)) {
		PUTSTR("0.0");
		return;
	}
	num = q->num;
	den = q->den;
	num.sign = 0;
	exponent = zdigits(num) - zdigits(den);
	if (exponent > 0) {
		ztenpow(exponent, &tenpow);
		zmul(den, tenpow, &tmp);
		zfree(tenpow);
		den = tmp;
	}
	if (exponent < 0) {
		ztenpow(-exponent, &tenpow);
		zmul(num, tenpow, &tmp);
		zfree(tenpow);
		num = tmp;
	}
	if (zrel(num, den) < 0) {
		zmuli(num, 10L, &tmp);
		if (num.v != q->num.v)
			zfree(num);
		num = tmp;
		exponent--;
	}
	q2.num = num;
	q2.den = den;
	q2.num.sign = q->num.sign;
	qprintff(&q2, 0L, precision);
	if (exponent)
		PRINTF1("e%ld", exponent);
	if (num.v != q->num.v)
		zfree(num);
	if (den.v != q->den.v)
		zfree(den);
}


/*
 * Print a number in rational representation.
 * Example: 397/37
 */
void
qprintfr(q, width, force)
	NUMBER *q;
	long width;
	BOOL force;
{
	zprintval(q->num, 0L, width);
	if (force || qisfrac(q)) {
		PUTCHAR('/');
		zprintval(q->den, 0L, width);
	}
}


/*
 * Print a number as an integer (truncating fractional part).
 * Example: 958421
 */
void
qprintfd(q, width)
	NUMBER *q;
	long width;
{
	ZVALUE z;

	if (qisfrac(q)) {
		zquo(q->num, q->den, &z);
		zprintval(z, 0L, width);
		zfree(z);
	} else
		zprintval(q->num, 0L, width);
}


/*
 * Print a number in hex.
 * This prints the numerator and denominator in hex.
 */
void
qprintfx(q, width)
	NUMBER *q;
	long width;
{
	zprintx(q->num, width);
	if (qisfrac(q)) {
		PUTCHAR('/');
		zprintx(q->den, 0L);
	}
}


/*
 * Print a number in binary.
 * This prints the numerator and denominator in binary.
 */
void
qprintfb(q, width)
	NUMBER *q;
	long width;
{
	zprintb(q->num, width);
	if (qisfrac(q)) {
		PUTCHAR('/');
		zprintb(q->den, 0L);
	}
}


/*
 * Print a number in octal.
 * This prints the numerator and denominator in octal.
 */
void
qprintfo(q, width)
	NUMBER *q;
	long width;
{
	zprinto(q->num, width);
	if (qisfrac(q)) {
		PUTCHAR('/');
		zprinto(q->den, 0L);
	}
}


/*
 * Convert a string to a number in rational, floating point,
 * exponential notation, hex, or octal.
 *	q = atoq(string);
 */
NUMBER *
atoq(s)
	register char *s;
{
	register NUMBER *q;
	register char *t;
	ZVALUE div, newnum, newden, tmp;
	long decimals, exp;
	BOOL hex, negexp;

	q = qalloc();
	decimals = 0;
	exp = 0;
	negexp = FALSE;
	hex = FALSE;
	t = s;
	if ((*t == '+') || (*t == '-'))
		t++;
	if ((*t == '0') && ((t[1] == 'x') || (t[1] == 'X'))) {
		hex = TRUE;
		t += 2;
	}
	while (((*t >= '0') && (*t <= '9')) || (hex &&
		(((*t >= 'a') && (*t <= 'f')) || ((*t >= 'A') && (*t <= 'F')))))
			t++;
	if (*t == '/') {
		t++;
		atoz(t, &q->den);
	} else if ((*t == '.') || (*t == 'e') || (*t == 'E')) {
		if (*t == '.') {
			t++;
			while ((*t >= '0') && (*t <= '9')) {
				t++;
				decimals++;
			}
		}
		/*
		 * Parse exponent if any
		 */
		if ((*t == 'e') || (*t == 'E')) {
			t++;
			if (*t == '+')
				t++;
			else if (*t == '-') {
				negexp = TRUE;
				t++;
			}
			while ((*t >= '0') && (*t <= '9')) {
				exp = (exp * 10) + *t++ - '0';
				if (exp > 1000000)
					math_error("Exponent too large");
			}
		}
		ztenpow(decimals, &q->den);
	}
	atoz(s, &q->num);
	if (qiszero(q)) {
		qfree(q);
		return qlink(&_qzero_);
	}
	/*
	 * Apply the exponential if any
	 */
	if (exp) {
		ztenpow(exp, &tmp);
		if (negexp) {
			zmul(q->den, tmp, &newden);
			zfree(q->den);
			q->den = newden;
		} else {
			zmul(q->num, tmp, &newnum);
			zfree(q->num);
			q->num = newnum;
		}
		zfree(tmp);
	}
	/*
	 * Reduce the fraction to lowest terms
	 */
	if (zisunit(q->num) || zisunit(q->den))
		return q;
	zgcd(q->num, q->den, &div);
	if (zisunit(div))
		return q;
	zquo(q->num, div, &newnum);
	zfree(q->num);
	zquo(q->den, div, &newden);
	zfree(q->den);
	q->num = newnum;
	q->den = newden;
	return q;
}


/*
 * Parse a number in any of the various legal forms, and return the count
 * of characters that are part of a legal number.  Numbers can be either a
 * decimal integer, possibly two decimal integers separated with a slash, a
 * floating point or exponential number, a hex number beginning with "0x",
 * a binary number beginning with "0b", or an octal number beginning with "0".
 * The flags argument modifies the end of number testing for ease in handling
 * fractions or complex numbers.  Minus one is returned if the number format
 * is definitely illegal.
 */
long
qparse(cp, flags)
	int flags;
	register char *cp;
{
	char *oldcp;

	oldcp = cp;
	if ((*cp == '+') || (*cp == '-'))
		cp++;
	if ((*cp == '+') || (*cp == '-'))
		return -1;
	if ((*cp == '0') && ((cp[1] == 'x') || (cp[1] == 'X'))) {	/* hex */
		cp += 2;
		while (((*cp >= '0') && (*cp <= '9')) ||
			((*cp >= 'a') && (*cp <= 'f')) ||
			((*cp >= 'A') && (*cp <= 'F')))
				cp++;
		goto done;
	}
	if ((*cp == '0') && ((cp[1] == 'b') || (cp[1] == 'B'))) {	/* binary */
		cp += 2;
		while ((*cp == '0') || (*cp == '1'))
			cp++;
		goto done;
	}
	if ((*cp == '0') && (cp[1] >= '0') && (cp[1] <= '9')) { /* octal */
		while ((*cp >= '0') && (*cp <= '7'))
			cp++;
		goto done;
	}
	/*
	 * Number is decimal, but can still be a fraction or real or exponential.
	 */
	while ((*cp >= '0') && (*cp <= '9'))
		cp++;
	if (*cp == '/' && flags & QPF_SLASH) {	/* fraction */
		cp++;
		while ((*cp >= '0') && (*cp <= '9'))
			cp++;
		goto done;
	}
	if (*cp == '.') {	/* floating point */
		cp++;
		while ((*cp >= '0') && (*cp <= '9'))
			cp++;
	}
	if ((*cp == 'e') || (*cp == 'E')) {	/* exponential */
		cp++;
		if ((*cp == '+') || (*cp == '-'))
			cp++;
		if ((*cp == '+') || (*cp == '-'))
			return -1;
		while ((*cp >= '0') && (*cp <= '9'))
			cp++;
	}

done:
	if (((*cp == 'i') || (*cp == 'I')) && (flags & QPF_IMAG))
		cp++;
	if ((*cp == '.') || ((*cp == '/') && (flags & QPF_SLASH)) ||
		((*cp >= '0') && (*cp <= '9')) ||
		((*cp >= 'a') && (*cp <= 'z')) ||
		((*cp >= 'A') && (*cp <= 'Z')))
			return -1;
	return (cp - oldcp);
}

/* END CODE */
