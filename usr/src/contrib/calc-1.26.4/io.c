/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Scanf and printf routines for extended precision numbers
 */

#include <stdio.h>
#include "stdarg.h"
#include "math.h"

#define	OUTBUFSIZE	200		/* realloc size for output buffers */


#define	PUTCHAR(ch)		math_chr(ch)
#define	PUTSTR(str)		math_str(str)
#define	PRINTF1(fmt, a1)	math_fmt(fmt, a1)
#define	PRINTF2(fmt, a1, a2)	math_fmt(fmt, a1, a2)


long	_outdigits_ = 20;		/* default digits for output */
int	_outmode_ = MODE_INITIAL;	/* default output mode */


/*
 * Output state that has been saved when diversions are done.
 */
typedef struct iostate IOSTATE;
struct iostate {
	IOSTATE *oldiostates;		/* previous saved state */
	long outdigits;			/* digits for output */
	int outmode;			/* output mode */
	FILE *outfp;			/* file unit for output (if any) */
	char *outbuf;			/* output string buffer (if any) */
	long outbufsize;		/* current size of string buffer */
	long outbufused;		/* space used in string buffer */
	BOOL outputisstring;		/* TRUE if output is to string buffer */
};


static IOSTATE	*oldiostates = NULL;	/* list of saved output states */
static FILE	*outfp = stdout;	/* file unit for output */
static char	*outbuf = NULL;		/* current diverted buffer */
static long	scalefactor;
static long	outbufsize;
static long	outbufused;
static BOOL	outputisstring;
static ZVALUE	scalenumber = { 0, 0, 0 };

#if 0
static long	etoalen;
static char	*etoabuf = NULL;
#endif

static void zprintx proto((ZVALUE z, long width));
static void zprintb proto((ZVALUE z, long width));
static void zprinto proto((ZVALUE z, long width));
static void zprintval proto((ZVALUE z, long decimals, long width));

static void atoz proto((char * s, ZVALUE * res));

static void qprintff();
static void qprintfd();
static void qprintfe();
static void qprintfr();
static void qprintfo();
static void qprintfb();
#ifdef CODE
extern void qprintfx();
#else
static void qprintfx();
#endif


/*
 * Routine to output a character either to a FILE
 * handle or into a string.
 */
void
math_chr(ch)
{
	char	*cp;

	if (!outputisstring) {
		fputc(ch, outfp);
		return;
	}
	if (outbufused >= outbufsize) {
		cp = (char *)realloc(outbuf, outbufsize + OUTBUFSIZE + 1);
		if (cp == NULL)
			error("Cannot realloc output string");
		outbuf = cp;
		outbufsize += OUTBUFSIZE;
	}
	outbuf[outbufused++] = (char)ch;
}


/*
 * Routine to output a null-terminated string either
 * to a FILE handle or into a string.
 */
void
math_str(str)
	char	*str;
{
	char	*cp;
	int	len;

	if (!outputisstring) {
		fputs(str, outfp);
		return;
	}
	len = strlen(str);
	if ((outbufused + len) > outbufsize) {
		cp = (char *)realloc(outbuf, outbufsize + len + OUTBUFSIZE + 1);
		if (cp == NULL)
			error("Cannot realloc output string");
		outbuf = cp;
		outbufsize += (len + OUTBUFSIZE);
	}
	memcpy(&outbuf[outbufused], str, len);
	outbufused += len;
}


/*
 * Routine to output a printf-style formatted string either
 * to a FILE handle or into a string.
 */
#ifdef VARARGS
# define VA_ALIST fmt, va_alist
# define VA_DCL char *fmt; va_dcl
#else
# ifdef __STDC__
#  define VA_ALIST char *fmt, ...
#  define VA_DCL
# else
#  define VA_ALIST fmt
#  define VA_DCL char *fmt;
# endif
#endif
/*VARARGS*/
void
math_fmt(VA_ALIST)
	VA_DCL
{
	va_list ap;
	char buf[200];

#ifdef VARARGS
	va_start(ap);
#else
	va_start(ap, fmt);
#endif
	vsprintf(buf, fmt, ap);
	va_end(ap);
	math_str(buf);
}


/*
 * Flush the current output stream.
 */
void
math_flush()
{
	if (!outputisstring)
		fflush(outfp);
}


/*
 * Divert further output so that it is saved into a string that will be
 * returned later when the diversion is completed.  The current state of
 * output is remembered for later restoration.  Diversions can be nested.
 * Output diversion is only intended for saving output to "stdout".
 */
void
divertio()
{
	register IOSTATE *sp;

	sp = (IOSTATE *) malloc(sizeof(IOSTATE));
	if (sp == NULL)
		error("No memory for diverting output");
	sp->oldiostates = oldiostates;
	sp->outdigits = _outdigits_;
	sp->outmode = _outmode_;
	sp->outfp = outfp;
	sp->outbuf = outbuf;
	sp->outbufsize = outbufsize;
	sp->outbufused = outbufused;
	sp->outputisstring = outputisstring;

	outbufused = 0;
	outbufsize = 0;
	outbuf = (char *) malloc(OUTBUFSIZE + 1);
	if (outbuf == NULL)
		error("Cannot allocate divert string");
	outbufsize = OUTBUFSIZE;
	outputisstring = TRUE;
	oldiostates = sp;
}


/*
 * Undivert output and return the saved output as a string.  This also
 * restores the output state to what it was before the diversion began.
 * The string needs freeing by the caller when it is no longer needed.
 */
char *
getdivertedio()
{
	register IOSTATE *sp;
	char *cp;

	sp = oldiostates;
	if (sp == NULL)
		error("No diverted state to restore");
	cp = outbuf;
	cp[outbufused] = '\0';
	oldiostates = sp->oldiostates;
	_outdigits_ = sp->outdigits;
	_outmode_ = sp->outmode;
	outfp = sp->outfp;
	outbuf = sp->outbuf;
	outbufsize = sp->outbufsize;
	outbufused = sp->outbufused;
	outbuf = sp->outbuf;
	outputisstring = sp->outputisstring;
	return cp;
}


/*
 * Clear all diversions and set output back to the original destination.
 * This is called when resetting the global state of the program.
 */
void
cleardiversions()
{
	while (oldiostates)
		free(getdivertedio());
}


/*
 * Set the output routines to output to the specified FILE stream.
 * This interacts with output diversion in the following manner.
 *	STDOUT	diversion	action
 *	----	---------	------
 *	yes	yes		set output to diversion string again.
 *	yes	no		set output to stdout.
 *	no	yes		set output to specified file.
 *	no	no		set output to specified file.
 */
void
setfp(newfp)
	FILE *newfp;
{
	outfp = newfp;
	outputisstring = (oldiostates && (newfp == stdout));
}


/*
 * Set the output mode for numeric output.
 */
void
set_mode(newmode)
{
	if ((newmode <= MODE_DEFAULT) || (newmode > MODE_MAX))
		error("Setting illegal output mode");
	_outmode_ = newmode;
}


/*
 * Set the number of digits for float or exponential output.
 */
void
setdigits(newdigits)
	long newdigits;
{
	if (newdigits < 0)
		error("Setting illegal number of digits");
	_outdigits_ = newdigits;
}


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
# ifdef __STDC__
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
	if (iszero(r->den)) {
		qfree(r);
		r = NULL;
	}
	return r;
}
#endif


/*
 * Print a complex number in rational representation.
 * Example:  2/3-4i/5
 */
void
cprintfr(c)
	COMPLEX *c;
{
	NUMBER *r;
	NUMBER *i;

	r = c->real;
	i = c->imag;
	if (!qiszero(r) || qiszero(i))
		qprintfr(r, 0L, FALSE);
	if (qiszero(i))
		return;
	if (!qiszero(r) && !qisneg(i))
		PUTCHAR('+');
	zprintval(i->num, 0L, 0L);
	PUTCHAR('i');
	if (qisfrac(i)) {
		PUTCHAR('/');
		zprintval(i->den, 0L, 0L);
	}
}


/*
 * Print a number in the specified output mode.
 * If MODE_DEFAULT is given, then the default output mode is used.
 * Any approximate output is flagged with a leading tilde.
 * Integers are always printed as themselves.
 */
void
qprintnum(q, outmode)
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
			if (qisfrac(q))
				PUTCHAR('~');
			qprintfd(q, 0L);
			break;

		case MODE_REAL:
			prec = qplaces(q);
			if ((prec < 0) || (prec > _outdigits_)) {
				prec = _outdigits_;
				PUTCHAR('~');
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
			freeh(tmpval.num.v);
			freeh(tmpval.den.v);
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
			error("Bad mode for print");
	}
}


/*
 * Print a number in floating point representation.
 * Example:  193.784
 */
static void
qprintff(q, width, precision)
	NUMBER *q;
	long width;
	long precision;
{
	ZVALUE z, z1;

	if (precision != scalefactor) {
		if (scalenumber.v)
			freeh(scalenumber.v);
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
			freeh(z.v);
		z = z1;
	}
	if (qisneg(q) && iszero(z))
		PUTCHAR('-');
	zprintval(z, precision, width);
	if (z.v != q->num.v)
		freeh(z.v);
}


/*
 * Print a number in exponential notation.
 * Example: 4.1856e34
 */
/*ARGSUSED*/
static void
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
		freeh(tenpow.v);
		den = tmp;
	}
	if (exponent < 0) {
		ztenpow(-exponent, &tenpow);
		zmul(num, tenpow, &tmp);
		freeh(tenpow.v);
		num = tmp;
	}
	if (zrel(num, den) < 0) {
		zmuli(num, 10L, &tmp);
		if (num.v != q->num.v)
			freeh(num.v);
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
		freeh(num.v);
	if (den.v != q->den.v)
		freeh(den.v);
}


/*
 * Print a number in rational representation.
 * Example: 397/37
 */
static void
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
static void
qprintfd(q, width)
	NUMBER *q;
	long width;
{
	ZVALUE z;

	if (qisfrac(q)) {
		zquo(q->num, q->den, &z);
		zprintval(z, 0L, width);
		freeh(z.v);
	} else
		zprintval(q->num, 0L, width);
}


/*
 * Print a number in hex.
 * This prints the numerator and denominator in hex.
 */
#ifndef CODE
static 
#endif
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
static void
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
static void
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
 * Print an integer value as a hex number.
 * The special characters 0x appear to indicate the number is hex.
 */
/*ARGSUSED*/
static void
zprintx(z, width)
	ZVALUE z;
	long width;
{
	register HALF *hp;	/* current word to print */
	int len;		/* number of halfwords to type */

	len = z.len - 1;
	if (isneg(z))
		PUTCHAR('-');
	if ((len == 0) && (*z.v <= (FULL) 9)) {
		len = '0' + *z.v;
		PUTCHAR(len);
		return;
	}
	hp = z.v + len;
	PRINTF1("0x%x", (FULL) *hp--);
	while (--len >= 0)
		PRINTF1("%04x", (FULL) *hp--);
}


/*
 * Print an integer value as a binary number.
 * The special characters 0b appear to indicate the number is binary.
 */
/*ARGSUSED*/
static void
zprintb(z, width)
	ZVALUE z;
	long width;
{
	register HALF *hp;	/* current word to print */
	int len;		/* number of halfwords to type */
	HALF val;		/* current value */
	HALF mask;		/* current mask */
	int didprint;		/* nonzero if printed some digits */
	int ch;			/* current char */

	len = z.len - 1;
	if (isneg(z))
		PUTCHAR('-');
	if ((len == 0) && (*z.v <= (FULL) 1)) {
		len = '0' + *z.v;
		PUTCHAR(len);
		return;
	}
	hp = z.v + len;
	didprint = 0;
	PUTSTR("0b");
	while (len-- >= 0) {
		val = *hp--;
		mask = (1 << (BASEB - 1));
		while (mask) {
			ch = '0' + ((mask & val) != 0);
			if (didprint || (ch != '0')) {
				PUTCHAR(ch);
				didprint = 1;
			}
			mask >>= 1;
		}
	}
}


/*
 * Print an integer value as an octal number.
 * The number begins with a leading 0 to indicate that it is octal.
 */
/*ARGSUSED*/
static void
zprinto(z, width)
	ZVALUE z;
	long width;
{
	register HALF *hp;	/* current word to print */
	int len;		/* number of halfwords to type */
	int num1, num2;		/* numbers to type */
	int rem;		/* remainder number of halfwords */

	if (isneg(z))
		PUTCHAR('-');
	len = z.len;
	if ((len == 1) && (*z.v <= (FULL) 7)) {
		num1 = '0' + *z.v;
		PUTCHAR(num1);
		return;
	}
	hp = z.v + len - 1;
	rem = len % 3;
	switch (rem) {	/* handle odd amounts first */
		case 0:
			num1 = (((FULL) hp[0]) << 8) + (((FULL) hp[-1]) >> 8);
			num2 = (((FULL) (hp[-1] & 0xff)) << 16) + ((FULL) hp[-2]);
			rem = 3;
			break;
		case 1:
			num1 = 0;
			num2 = (FULL) hp[0];
			break;
		case 2:
			num1 = (((FULL) hp[0]) >> 8);
			num2 = (((FULL) (hp[0] & 0xff)) << 16) + ((FULL) hp[-1]);
			break;
	}
	if (num1)
		PRINTF2("0%o%08o", num1, num2);
	else
		PRINTF1("0%o", num2);
	len -= rem;
	hp -= rem;
	while (len > 0) {	/* finish in groups of 3 halfwords */
		num1 = (((FULL) hp[0]) << 8) + (((FULL) hp[-1]) >> 8);
		num2 = (((FULL) (hp[-1] & 0xff)) << 16) + ((FULL) hp[-2]);
		PRINTF2("%08o%08o", num1, num2);
		hp -= 3;
		len -= 3;
	}
}


/*
 * Print a decimal integer to the terminal.
 * This works by dividing the number by 10^2^N for some N, and
 * then doing this recursively on the quotient and remainder.
 * Decimals supplies number of decimal places to print, with a decimal
 * point at the right location, with zero meaning no decimal point.
 * Width is the number of columns to print the number in, including the
 * decimal point and sign if required.  If zero, no extra output is done.
 * If positive, leading spaces are typed if necessary. If negative, trailing
 * spaces are typed if necessary.  As examples of the effects of these values,
 * (345,0,0) = "345", (345,2,0) = "3.45", (345,5,8) = "  .00345".
 */
static void
zprintval(z, decimals, width)
	ZVALUE z;		/* number to be printed */
	long decimals;		/* number of decimal places */
	long width;		/* number of columns to print in */
{
	int depth;		/* maximum depth */
	int n;			/* current index into array */
	int i;			/* number to print */
	long leadspaces;	/* number of leading spaces to print */
	long putpoint;		/* digits until print decimal point */
	long digits;		/* number of digits of raw number */
	BOOL output;		/* TRUE if have output something */
	BOOL neg;		/* TRUE if negative */
	ZVALUE quo, rem;	/* quotient and remainder */
	ZVALUE leftnums[32];	/* left parts of the number */
	ZVALUE rightnums[32];	/* right parts of the number */

	if (decimals < 0)
		decimals = 0;
	if (width < 0)
		width = 0;
	neg = (z.sign != 0);

	leadspaces = width - neg - (decimals > 0);
	z.sign = 0;
	/*
	 * Find the 2^N power of ten which is greater than or equal
	 * to the number, calculating it the first time if necessary.
	 */
	_tenpowers_[0] = _ten_;
	depth = 0;
	while ((_tenpowers_[depth].len < z.len) || (zrel(_tenpowers_[depth], z) <= 0)) {
		depth++;
		if (_tenpowers_[depth].len == 0)
			zsquare(_tenpowers_[depth-1], &_tenpowers_[depth]);
	}
	/*
	 * Divide by smaller 2^N powers of ten until the parts are small
	 * enough to output.  This algorithm walks through a binary tree
	 * where each node is a piece of the number to print, and such that
	 * we visit left nodes first.  We do the needed recursion in line.
	 */
	digits = 1;
	output = FALSE;
	n = 0;
	putpoint = 0;
	rightnums[0].len = 0;
	leftnums[0] = z;
	for (;;) {
		while (n < depth) {
			i = depth - n - 1;
			zdiv(leftnums[n], _tenpowers_[i], &quo, &rem);
			if (!iszero(quo))
				digits += (1L << i);
			n++;
			leftnums[n] = quo;
			rightnums[n] = rem;
		}
		i = leftnums[n].v[0];
		if (output || i || (n == 0)) {
			if (!output) {
				output = TRUE;
				if (decimals > digits)
					leadspaces -= decimals;
				else
					leadspaces -= digits;
				while (--leadspaces >= 0)
					PUTCHAR(' ');
				if (neg)
					PUTCHAR('-');
				if (decimals) {
					putpoint = (digits - decimals);
					if (putpoint <= 0) {
						PUTCHAR('.');
						while (++putpoint <= 0)
							PUTCHAR('0');
						putpoint = 0;
					}
				}
			}
			i += '0';
			PUTCHAR(i);
			if (--putpoint == 0)
				PUTCHAR('.');
		}
		while (rightnums[n].len == 0) {
			if (n <= 0)
				return;
			if (leftnums[n].len)
				freeh(leftnums[n].v);
			n--;
		}
		freeh(leftnums[n].v);
		leftnums[n] = rightnums[n];
		rightnums[n].len = 0;
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
					error("Exponent too large");
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
			freeh(q->den.v);
			q->den = newden;
		} else {
			zmul(q->num, tmp, &newnum);
			freeh(q->num.v);
			q->num = newnum;
		}
		freeh(tmp.v);
	}
	/*
	 * Reduce the fraction to lowest terms
	 */
	if (isunit(q->num) || isunit(q->den))
		return q;
	zgcd(q->num, q->den, &div);
	if (isunit(div))
		return q;
	zquo(q->num, div, &newnum);
	freeh(q->num.v);
	zquo(q->den, div, &newden);
	freeh(q->den.v);
	q->num = newnum;
	q->den = newden;
	return q;
}


/*
 * Read an integer value in decimal, hex, octal, or binary.
 * Hex numbers are indicated by a leading "0x", binary with a leading "0b",
 * and octal by a leading "0".  Periods are skipped over, but any other
 * extraneous character stops the scan.
 */
static void
atoz(s, res)
	register char *s;
	ZVALUE *res;
{
	ZVALUE z, ztmp, digit;
	HALF digval;
	BOOL minus;
	long shift;

	minus = FALSE;
	shift = 0;
	if (*s == '+')
		s++;
	else if (*s == '-') {
		minus = TRUE;
		s++;
	}
	if (*s == '0') {		/* possibly hex, octal, or binary */
		s++;
		if ((*s >= '0') && (*s <= '7')) {
			shift = 3;
		} else if ((*s == 'x') || (*s == 'X')) {
			shift = 4;
			s++;
		} else if ((*s == 'b') || (*s == 'B')) {
			shift = 1;
			s++;
		}
	}
	digit.v = &digval;
	digit.len = 1;
	digit.sign = 0;
	z = _zero_;
	while (*s) {
		digval = *s++;
		if ((digval >= '0') && (digval <= '9'))
			digval -= '0';
		else if ((digval >= 'a') && (digval <= 'f') && shift)
			digval -= ('a' - 10);
		else if ((digval >= 'A') && (digval <= 'F') && shift)
			digval -= ('A' - 10);
		else if (digval == '.')
			continue;
		else
			break;
		if (shift)
			zshift(z, shift, &ztmp);
		else
			zmuli(z, 10L, &ztmp);
		freeh(z.v);
		zadd(ztmp, digit, &z);
		freeh(ztmp.v);
	}
	trim(&z);
	if (minus && !iszero(z))
		z.sign = 1;
	*res = z;
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
