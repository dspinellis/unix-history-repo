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
static char sccsid[] = "@(#)vfprintf.c	5.23 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <varargs.h>
#include <stdio.h>
#include <ctype.h>

#define	MAXBUF		512

#define	PUTC(ch)	{++cnt; putc((char)ch, fp);}

#define	ARG() \
	_ulong = flags&LONGINT ? va_arg(argp, long) : \
	    flags&SHORTINT ? va_arg(argp, short) : va_arg(argp, int);

/* have to deal with the negative buffer count kludge */
#define	NEGATIVE_COUNT_KLUDGE

#define	LONGINT		0x01		/* long integer */
#define	LONGDBL		0x02		/* long double; unimplemented */
#define	SHORTINT	0x04		/* short integer */
#define	ALT		0x08		/* alternate form */
#define	LADJUST		0x10		/* left adjustment */

_doprnt(fmt0, argp, fp)
	u_char *fmt0;
	va_list argp;
	register FILE *fp;
{
	register u_char *fmt;
	register int ch, cnt, n;
	register char *t;
	double _double;
	u_long _ulong;
	int base, flags, prec, size, width;
	char padc, sign, *digs, buf[MAXBUF], *_cvt();

	fmt = fmt0;
	digs = "0123456789abcdef";
	for (cnt = 0;; ++fmt) {
		n = fp->_cnt;
		for (t = fp->_ptr; (ch = *fmt) && ch != '%'; ++cnt, ++fmt)
			if (--n < 0
#ifdef NEGATIVE_COUNT_KLUDGE
			    && (!(fp->_flag & _IOLBF) || -n >= fp->_bufsiz)
#endif
			    || ch == '\n' && fp->_flag&_IOLBF) {
				fp->_cnt = n;
				fp->_ptr = t;
				(void)_flsbuf(ch, fp);
				n = fp->_cnt;
				t = fp->_ptr;
			}
			else
				*t++ = ch;
		fp->_cnt = n;
		fp->_ptr = t;
		if (!ch)
			return(cnt);

		flags = width = 0;
		prec = -1;
		padc = ' ';
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
			/*FALLTHROUGH*/
		case '-':
			flags |= LADJUST;
			goto rflag;
		case '+':
			sign = '+';
			goto rflag;
		case '.':
			if (*++fmt == '*')
				n = va_arg(argp, int);
			else if (isascii(*fmt) && isdigit(*fmt)) {
				n = 0;
				do {
					n = 10 * n + *fmt - '0';
				} while (isascii(*++fmt) && isdigit(*fmt));
				--fmt;
			}
			else {
				--fmt;
				prec = 0;
				goto rflag;
			}
			prec = n < 0 ? -1 : n;
			goto rflag;
		case '0':
			padc = '0';
			/*FALLTHROUGH*/
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			n = 0;
			do {
				n = 10 * n + *fmt - '0';
			} while (isascii(*++fmt) && isdigit(*fmt));
			width = n;
			--fmt;
			goto rflag;
		case 'L':
			/*
			 * C doesn't have a long double; use long for now.
			 * flags |= LONGDBL;
			 */
			flags |= LONGINT;
			goto rflag;
		case 'h':
			flags |= SHORTINT;
			goto rflag;
		case 'l':
			flags |= LONGINT;
			goto rflag;
		case 'c':
			buf[0] = va_arg(argp, int);
			size = 1;
			t = buf;
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
			size = _cvt(_double, prec, flags, *fmt, padc, &sign,
			    buf, buf + sizeof(buf)) - buf;
			t = buf;
			/*
			 * zero-padded sign put out here; blank padded sign
			 * placed in number in _cvt().
			 */
			if (sign && padc == '0') {
				PUTC(sign);
				--width;
			}
			goto pforw;
		case 'n':
			if (flags&LONGDBL || flags&LONGINT)
				*va_arg(argp, long *) = cnt;
			else if (flags&SHORTINT)
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
			/*NOSTRICT*/
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
				}
				else
					size = prec;
			}
			else
				size = strlen(t);
pforw:			if (!(flags&LADJUST) && width)
				for (n = size; n++ < width;)
					PUTC(padc);
			if (fp->_cnt - (n = size) >= 0) {
				cnt += n;
				fp->_cnt -= n;
				bcopy(t, fp->_ptr, n);
				fp->_ptr += n;
			}
			else for (; n--; ++t)
				PUTC(*t);
			if (flags&LADJUST)
				while (width-- > size)
					PUTC(' ');
			break;
		case 'u':
			ARG();
			base = 10;
			goto nosign;
		case 'X':
			digs = "0123456789ABCDEF";
			/*FALLTHROUGH*/
		case 'x':
			ARG();
			base = 16;
			/* leading 0x/X only if non-zero */
			if (!_ulong)
				flags &= ~ALT;

			/* unsigned conversions */
nosign:			sign = NULL;
			/*
			 * ``The result of converting a zero value with an
			 * explicit precision of zero is no characters.''
			 *	-- ANSI X3J11
			 */
number:			if (!_ulong && !prec)
				break;

			t = buf + MAXBUF - 1;
			do {
				*t-- = digs[_ulong % base];
				_ulong /= base;
			} while(_ulong);
			for (size = buf + MAXBUF - 1 - t; size < prec; ++size)
				*t-- = '0';

			/* alternate mode for hex and octal numbers */
			if (flags&ALT)
				switch (base) {
				case 16:
					/* avoid "00000x35" */
					if (padc == ' ') {
						*t-- = *fmt;
						*t-- = '0';
					}
					else {
						PUTC('0');
						PUTC(*fmt);
					}
					width -= 2;
					break;
				case 8:
					if (t[1] != '0') {
						*t-- = '0';
						--width;
					}
					break;
				}

			if (sign) {
				/* avoid "0000-3" */
				if (padc == ' ')
					*t-- = sign;
				else
					PUTC(sign);
				--width;
			}

			if (!(flags&LADJUST))
				while (size++ < width)
					PUTC(padc);
			while (++t < buf + MAXBUF)
				PUTC(*t);
			while (width-- > size)
				PUTC(' ');
			digs = "0123456789abcdef";
			break;
		case '\0':		/* "%?" prints ?, unless ? is NULL */
			return(cnt);
		default:
			PUTC(*fmt);
		}
	}
	/*NOTREACHED*/
}

#define	EFORMAT	0x01
#define	FFORMAT	0x02
#define	GFORMAT	0x04
#define	DEFPREC	6

static char *
_cvt(number, prec, flags, fmtch, padc, sign, startp, endp)
	double number;
	register int prec;
	int flags;
	u_char fmtch;
	char padc, *sign, *startp, *endp;
{
	register char *p;
	register int expcnt, format;
	double fract, integer, tmp, modf();
	int decpt;
	char *savep;

	if (prec == -1)
		prec = DEFPREC;

	if (number < 0) {
		*sign = '-';
		number = -number;
	}

	/* if blank padded, add sign in as part of the number */
	if (*sign && padc == ' ')
		*startp++ = *sign;

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
		register char *p2;

		/* get integer part of number; count decimal places */
		for (; integer; ++expcnt) {
			tmp = modf(integer / 10, &integer);
			*p-- = (int)((tmp + .03) * 10) + '0';
		}

		/* copy, in reverse order, to start of buffer */
		p2 = startp;
		*p2++ = *++p;

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
				*p2++ = '.';
			for (; ++p < endp && prec; --prec, *p2++ = *p);

			/* precision ran out, round */
			if (p < endp) {
				if (*p > '4') {
					for (savep = p2--;; *p2-- = '0') {
						if (*p2 == '.')
							--p2;
						if (++*p2 <= '9')
							break;
					}
					p2 = savep;
				}
				fract = 0;
			}
		}
		/*
		 * g/G in f format; if out of precision, replace digits with
		 * zeroes, note, have to round first.
		 */
		else if (format&GFORMAT) {
			for (; ++p < endp && prec; --prec, *p2++ = *p);
			/* precision ran out; round and then add zeroes */
			if (p < endp) {
				if (*p > '4') {
					for (savep = p2--; ++*p2 > '9';
					    *p2-- = '0');
					p2 = savep;
				}
				do {
					*p2++ = '0';
				} while (++p < endp);
				fract = 0;
			}
			if (decpt)
				*p2++ = '.';
		}
		/* f format */
		else {
			for (; ++p < endp; *p2++ = *p);
			if (decpt)
				*p2++ = '.';
		}
		p = p2;
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
				*p++ = (int)tmp + '0';
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
		*p++ = (int)tmp + '0';
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
		*p++ = expcnt / 10 + '0';
		*p++ = expcnt % 10 + '0';
	}
	return(p);
}
