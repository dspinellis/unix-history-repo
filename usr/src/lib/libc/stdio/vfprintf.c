/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)vfprintf.c	5.30 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <varargs.h>
#include <stdio.h>
#include <ctype.h>

/* 11-bit exponent (VAX G floating point) is 308 decimal digits */
#define	MAXEXP		308
/* 128 bit fraction takes up 39 decimal digits; max reasonable precision */
#define	MAXFRACT	39

#define	BUF		(MAXEXP+MAXFRACT+1)	/* + decimal point */

#define	PUTC(ch)	(void) putc(ch, fp)

#define	ARG() \
	_ulong = flags&LONGINT ? va_arg(argp, long) : \
	    flags&SHORTINT ? va_arg(argp, short) : va_arg(argp, int);

#define	todigit(c)	((c) - '0')
#define	tochar(n)	((n) + '0')

/* have to deal with the negative buffer count kludge */
#define	NEGATIVE_COUNT_KLUDGE

#define	LONGINT		0x01		/* long integer */
#define	LONGDBL		0x02		/* long double; unimplemented */
#define	SHORTINT	0x04		/* short integer */
#define	ALT		0x08		/* alternate form */
#define	LADJUST		0x10		/* left adjustment */
#define	ZEROPAD		0x20		/* zero (as opposed to blank) pad */
#define	HEXPREFIX	0x40		/* add 0x or 0X prefix */

_doprnt(fmt0, argp, fp)
	u_char *fmt0;
	va_list argp;
	register FILE *fp;
{
	register u_char *fmt;	/* format string */
	register int ch;	/* character from fmt */
	register int cnt;	/* return value accumulator */
	register int n;		/* random handy integer */
	register char *t;	/* buffer pointer */
	double _double;		/* double precision arguments %[eEfgG] */
	u_long _ulong;		/* integer arguments %[diouxX] */
	int flags;		/* flags as above */
	int dprec;		/* decimal precision in [diouxX] */
	int fpprec;		/* `extra' floating precision in [eEfgG] */
	int width;		/* width from format (%8d), or 0 */
	int prec;		/* precision from format (%.3d), or -1 */
	int size;		/* size of converted field or string */
	int fieldsz;		/* field size expanded by sign, etc */
	int realsz;		/* field size expanded by decimal precision */
	char sign;		/* sign prefix (+ - or \0) */
	int base;		/* base for [diouxX] conversion */
	char *digs;		/* digits for [diouxX] conversion */
	char buf[BUF];		/* space for %c, %[diouxX], %[eEfgG] */
	char *_cvt();		/* handles [eEfgG] formats */

	if (fp->_flag & _IORW) {
		fp->_flag |= _IOWRT;
		fp->_flag &= ~(_IOEOF|_IOREAD);
	}
	if ((fp->_flag & _IOWRT) == 0)
		return (EOF);

	fmt = fmt0;
	digs = "0123456789abcdef";
	for (cnt = 0;; ++fmt) {
		n = fp->_cnt;
		for (t = (char *)fp->_ptr; (ch = *fmt) && ch != '%';
		     ++cnt, ++fmt)
			if (--n < 0
#ifdef NEGATIVE_COUNT_KLUDGE
			    && (!(fp->_flag & _IOLBF) || -n >= fp->_bufsiz)
#endif
			    || ch == '\n' && fp->_flag & _IOLBF) {
				fp->_cnt = n;
				fp->_ptr = t;
				(void) _flsbuf((u_char)ch, fp);
				n = fp->_cnt;
				t = (char *)fp->_ptr;
			} else
				*t++ = ch;
		fp->_cnt = n;
		fp->_ptr = t;
		if (!ch)
			return (cnt);

		flags = dprec = fpprec = width = 0;
		prec = -1;
		sign = '\0';

rflag:		switch (*++fmt) {
		case ' ':
			sign = ' ';
			goto rflag;
		case '#':
			flags |= ALT;
			goto rflag;
		case '*':
			/*
			 * ``A negative field width argument is taken as a
			 * - flag followed by a  positive field width.''
			 *	-- ANSI X3J11
			 * They don't exclude field widths read from args.
			 */
			if ((width = va_arg(argp, int)) >= 0)
				goto rflag;
			width = -width;
			/* FALLTHROUGH */
		case '-':
			flags |= LADJUST;
			goto rflag;
		case '+':
			sign = '+';
			goto rflag;
		case '.':
			if (*++fmt == '*')
				n = va_arg(argp, int);
			else {
				n = 0;
				while (isascii(*fmt) && isdigit(*fmt))
					n = 10 * n + todigit(*fmt++);
				--fmt;
			}
			prec = n < 0 ? -1 : n;
			goto rflag;
		case '0':
			/*
			 * ``Note that 0 is taken as a flag, not as the
			 * beginning of a field width.''
			 *	-- ANSI X3J11
			 */
			flags |= ZEROPAD;
			goto rflag;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			n = 0;
			do {
				n = 10 * n + todigit(*fmt);
			} while (isascii(*++fmt) && isdigit(*fmt));
			width = n;
			--fmt;
			goto rflag;
		case 'L':
			flags |= LONGDBL;
			goto rflag;
		case 'h':
			flags |= SHORTINT;
			goto rflag;
		case 'l':
			flags |= LONGINT;
			goto rflag;
		case 'c':
			*(t = buf) = va_arg(argp, int);
			size = 1;
			sign = '\0';
			goto pforw;
		case 'd':
		case 'i':
			ARG();
			if ((long)_ulong < 0) {
				_ulong = -_ulong;
				sign = '-';
			}
			base = 10;
			goto number;
		case 'e':
		case 'E':
		case 'f':
		case 'g':
		case 'G':
			_double = va_arg(argp, double);
			/*
			 * don't bother to do unrealistic precision; just
			 * pad it with zeroes later.  This keeps buffer size
			 * rational.
			 */
			if (prec > MAXFRACT) {
				if (*fmt != 'g' && *fmt != 'G' || (flags&ALT))
					fpprec = prec - MAXFRACT;
				prec = MAXFRACT;
			}
			t = buf;
			size = _cvt(_double, prec, flags, *fmt, &sign,
				    t, t + sizeof(buf)) - t;
			goto pforw;
		case 'n':
			if (flags & LONGINT)
				*va_arg(argp, long *) = cnt;
			else if (flags & SHORTINT)
				*va_arg(argp, short *) = cnt;
			else
				*va_arg(argp, int *) = cnt;
			break;
		case 'o':
			ARG();
			base = 8;
			goto nosign;
		case 'p':
			/*
			 * ``The argument shall be a pointer to void.  The
			 * value of the pointer is converted to a sequence
			 * of printable characters, in an implementation-
			 * defined manner.''
			 *	-- ANSI X3J11
			 */
			/* NOSTRICT */
			_ulong = (u_long)va_arg(argp, void *);
			base = 16;
			goto nosign;
		case 's':
			if (!(t = va_arg(argp, char *)))
				t = "(null)";
			if (prec >= 0) {
				/*
				 * can't use strlen; can only look for the
				 * NUL in the first `prec' characters, and
				 * strlen() will go further.
				 */
				char *p, *memchr();

				if (p = memchr(t, 0, prec)) {
					size = p - t;
					if (size > prec)
						size = prec;
				} else
					size = prec;
			} else
				size = strlen(t);
			sign = '\0';
			goto pforw;
		case 'u':
			ARG();
			base = 10;
			goto nosign;
		case 'X':
			digs = "0123456789ABCDEF";
			/* FALLTHROUGH */
		case 'x':
			ARG();
			base = 16;
			/* leading 0x/X only if non-zero */
			if (flags & ALT && _ulong != 0)
				flags |= HEXPREFIX;

			/* unsigned conversions */
nosign:			sign = '\0';
			/*
			 * ``... diouXx conversions ... if a precision is
			 * specified, the 0 flag will be ignored.''
			 *	-- ANSI X3J11
			 */
number:			if ((dprec = prec) >= 0)
				flags &= ~ZEROPAD;

			/*
			 * ``The result of converting a zero value with an
			 * explicit precision of zero is no characters.''
			 *	-- ANSI X3J11
			 */
			t = buf + BUF;
			if (_ulong != 0 || prec != 0) {
				do {
					*--t = digs[_ulong % base];
					_ulong /= base;
				} while (_ulong);
				digs = "0123456789abcdef";
				if (flags & ALT && base == 8 && *t != '0')
					*--t = '0'; /* octal leading 0 */
			}
			size = buf + BUF - t;

pforw:
			/*
			 * All reasonable formats wind up here.  At this
			 * point, `t' points to a string which (if not
			 * flags&LADJUST) should be padded out to `width'
			 * places.  If flags&ZEROPAD, it should first be
			 * prefixed by any sign or other prefix; otherwise,
			 * it should be blank padded before the prefix is
			 * emitted.  After any left-hand padding and
			 * prefixing, emit zeroes required by a decimal
			 * [diouxX] precision, then print the string proper,
			 * then emit zeroes required by any leftover floating
			 * precision; finally, if LADJUST, pad with blanks.
			 */

			/* compute actual size, so we know how much to pad */
			/* this code is not terribly satisfactory */
			/* fieldsz excludes decimal prec; realsz includes it */
			fieldsz = size + fpprec;
			if (sign)
				fieldsz++;
			if (flags & HEXPREFIX)
				fieldsz += 2;
			realsz = dprec > fieldsz ? dprec : fieldsz;

			/* right-adjusting blank padding */
			if ((flags & (LADJUST|ZEROPAD)) == 0 && width)
				for (n = realsz; n < width; n++)
					PUTC(' ');
			/* prefix */
			if (sign)
				PUTC(sign);
			if (flags & HEXPREFIX) {
				PUTC('0');
				PUTC((char)*fmt);
			}
			/* right-adjusting zero padding */
			if ((flags & (LADJUST|ZEROPAD)) == ZEROPAD)
				for (n = realsz; n < width; n++)
					PUTC('0');
			/* leading zeroes from decimal precision */
			for (n = fieldsz; n < dprec; n++)
				PUTC('0');

			/* the string or number proper */
			if (fp->_cnt - (n = size) >= 0 &&
			    (fp->_flag & _IOLBF) == 0) {
				fp->_cnt -= n;
				bcopy(t, (char *)fp->_ptr, n);
				fp->_ptr += n;
			} else
				while (--n >= 0)
					PUTC(*t++);
			/* trailing f.p. zeroes */
			while (--fpprec >= 0)
				PUTC('0');
			/* left-adjusting padding (always blank) */
			if (flags & LADJUST)
				for (n = realsz; n < width; n++)
					PUTC(' ');

			/* finally, adjust cnt */
			cnt += width > realsz ? width : realsz;
			break;
		case '\0':	/* "%?" prints ?, unless ? is NULL */
			return (cnt);
		default:
			PUTC((char)*fmt);
			cnt++;
		}
	}
	/* NOTREACHED */
}

#define	EFORMAT	0x01
#define	FFORMAT	0x02
#define	GFORMAT	0x04
#define	DEFPREC	6

static char *
_cvt(number, prec, flags, fmtch, sign, startp, endp)
	double number;
	register int prec;
	int flags;
	u_char fmtch;
	char *sign, *startp, *endp;
{
	register char *p, *t;
	register int expcnt, format;
	double fract, integer, tmp, modf();
	int decpt;
	char *savep, exponent[MAXEXP];

	if (prec == -1)
		prec = DEFPREC;

	if (number < 0) {
		*sign = '-';
		number = -number;
	}

	switch(fmtch) {
	case 'e':
	case 'E':
		format = EFORMAT;
		break;
	case 'f':
		format = FFORMAT;
		break;
	case 'g':
	case 'G':
		format = GFORMAT;
		fmtch -= 2;
	}

	/*
	 * if the alternate flag is set, or, at least one digit of precision
	 * was requested, add a decimal point, unless it's the g/G format
	 * in which case we require two digits of precision, as it counts
	 * precision differently.
	 */
	decpt = flags&ALT || prec > (format&GFORMAT ? 1 : 0);

	expcnt = 0;
	p = endp - 1;
	fract = modf(number, &integer);
	if (integer) {
		/* get integer part of number; count decimal places */
		for (; integer; ++expcnt) {
			tmp = modf(integer / 10, &integer);
			*p-- = tochar((int)((tmp + .03) * 10));
		}

		/* copy, in reverse order, to start of buffer */
		t = startp;
		*t++ = *++p;

		/*
		 * if the format is g/G, and the resulting exponent will be
		 * greater than the precision, use e/E format.  If e/E format,
		 * put in a decimal point as needed, and decrement precision
		 * count for each digit after the decimal point.
		 */
		if (format&GFORMAT && expcnt - 1 > prec || format&EFORMAT) {
			if (format&GFORMAT) {
				format |= EFORMAT;

				/* first digit is precision for g/G format */
				if (prec)
					--prec;
			}
			if (decpt)
				*t++ = '.';
			for (; ++p < endp && prec; --prec, *t++ = *p);

			/* precision ran out, round */
			if (p < endp) {
				if (*p > '4') {
					for (savep = t--;; *t-- = '0') {
						if (*t == '.')
							--t;
						if (++*t <= '9')
							break;
					}
					t = savep;
				}
				fract = 0;
			}
		}
		/*
		 * g/G in f format; if out of precision, replace digits with
		 * zeroes, note, have to round first.
		 */
		else if (format&GFORMAT) {
			for (; ++p < endp && prec; --prec, *t++ = *p);
			/* precision ran out; round and then add zeroes */
			if (p < endp) {
				if (*p > '4') {
					for (savep = t--; ++*t > '9';
					    *t-- = '0');
					t = savep;
				}
				do {
					*t++ = '0';
				} while (++p < endp);
				fract = 0;
			}
			if (decpt)
				*t++ = '.';
		}
		/* f format */
		else {
			for (; ++p < endp; *t++ = *p);
			if (decpt)
				*t++ = '.';
		}
		p = t;
	}
	/*
	 * if no fraction, the number was zero, and if no precision, can't
	 * show anything after the decimal point.
	 */
	else if (!fract || !prec) {
		*startp++ = '0';
		if (decpt && !(format&GFORMAT))
			*startp++ = '.';
		*startp = '\0';
		return(startp);
	}
	/*
	 * if the format is g/G, and the resulting exponent will be less than
	 * -4 use e/E format.  If e/E format, compute exponent value.
	 */
	else if (format&GFORMAT && fract < .0001 || format&EFORMAT) {
		format |= EFORMAT;
		if (fract)
			for (p = startp; fract;) {
				fract = modf(fract * 10, &tmp);
				if (!tmp) {
					--expcnt;
					continue;
				}
				*p++ = tochar((int)tmp);
				break;
			}
		else
			*p++ = '0';

		/* g/G format, decrement precision for first digit */
		if (format&GFORMAT && prec)
			--prec;

		/* add decimal after first non-zero digit */
		if (decpt)
			*p++ = '.';
	}
	/*
	 * f format or g/G printed as f format; don't worry about decimal
	 * point, if g/G format doesn't need it, will get stripped later.
	 */
	else {
		p = startp;
		*p++ = '0';
		*p++ = '.';
	}

	/* finish out requested precision */
	while (fract && prec-- > 0) {
		fract = modf(fract * 10, &tmp);
		*p++ = tochar((int)tmp);
	}
	while (prec-- > 0)
		*p++ = '0';

	/*
	 * if any fractional value left, "round" it back up to the beginning
	 * of the number, fixing the exponent as necessary, and avoiding the
	 * decimal point.
	 */
	if (fract) {
		(void)modf(fract * 10, &tmp);
		if (tmp > 4) {
			for (savep = p--;; *p-- = '0') {
				if (*p == '.')
					--p;
				if (p == startp) {
					*p = '1';
					++expcnt;
					break;
				}
				if (++*p <= '9')
					break;
			}
			p = savep;
		}
	}

	/*
	 * if a g/G format and not alternate flag, lose trailing zeroes,
	 * if e/E or g/G format, and last char is decimal point, lose it.
	 */
	if (!(flags&ALT)) {
		if (format&GFORMAT)
			for (; p[-1] == '0'; --p);
		if (format&(GFORMAT|EFORMAT) && p[-1] == '.')
			--p;
	}

	/* if an e/E format, add exponent */
	if (format&EFORMAT) {
		*p++ = fmtch;
		if (--expcnt < 0) {
			expcnt = -expcnt;
			*p++ = '-';
		}
		else
			*p++ = '+';
		t = exponent + MAXEXP;
		if (expcnt > 9) {
			do {
				*--t = tochar(expcnt % 10);
			} while ((expcnt /= 10) > 9);
			*--t = tochar(expcnt);
			for (; t < exponent + MAXEXP; *p++ = *t++);
		}
		else {
			*p++ = '0';
			*p++ = tochar(expcnt);
		}
	}
	return(p);
}
