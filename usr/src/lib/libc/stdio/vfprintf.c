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
static char sccsid[] = "@(#)vfprintf.c	5.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <varargs.h>
#include <stdio.h>
#include <ctype.h>

#define	MAXBUF		120
#define	DEFPREC		6

#define	PUTC(ch, fd)	{++cnt; putc(ch, fd);}

#define	EFORMAT		0x01
#define	FFORMAT		0x02
#define	GFORMAT		0x04

#define	LONGINT		0x01
#define	LONGDBL		0x02
#define	SHORTINT	0x04
#define	GETARG(r) \
	r = argsize&LONGINT ? va_arg(argp, long) : \
	    argsize&SHORTINT ? va_arg(argp, short) : va_arg(argp, int);

static int alternate;
static char printsign;

x_doprnt(fmt, argp, fp)
	register char *fmt;
	va_list argp;
	register FILE *fp;
{
	register u_long reg_ulong;
	register long reg_long;
	register int base;
	register char *digs, *bp, *t, padc;
	double _double;
	char argsize, *_cvt(), buf[MAXBUF];
	int cnt, n, ladjust, width, prec, size;

	digs = "0123456789abcdef";
	for (cnt = 0; *fmt; ++fmt) {
		if (*fmt != '%') {
			PUTC(*fmt, fp);
			continue;
		}

		alternate = ladjust = width = 0;
		prec = -1;
		padc = ' ';
		argsize = printsign = '\0';

flags:		switch (*++fmt) {
		case '#':
			alternate = 1;
			goto flags;
		case '*':
			/*
			 * ``A negative field width argument is taken as a
			 * - flag followed by a  positive field width.''
			 *	-- ANSI X3J11
			 * They don't exclude field widths read from args.
			 */
			if ((width = va_arg(argp, int)) >= 0)
				goto flags;
			width = -width;
			/*FALLTHROUGH*/
		case '-':
			ladjust = 1;
			goto flags;
		case '+':
			printsign = '+';
			goto flags;
		case '.':
			if (*++fmt == '*')
				prec = va_arg(argp, int);
			else if (isdigit(*fmt)) {
				prec = 0;
				do {
					prec = 10 * prec + *fmt - '0';
				} while isdigit(*++fmt);
				--fmt;
			}
			else {
				prec = 0;
				--fmt;
				goto flags;
			}
			if (prec < 0)
				prec = -1;
			goto flags;
		case '0':
			padc = '0';
			/*FALLTHROUGH*/
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			do {
				width = 10 * width + *fmt - '0';
			} while isdigit(*++fmt);
			--fmt;
		case 'L':
			argsize |= LONGDBL;
			goto flags;
		case 'h':
			argsize |= SHORTINT;
			goto flags;
		case 'l':
			argsize |= LONGINT;
			goto flags;
		case '%':			/* "%#%" prints as "%" */
			PUTC('%', fp);
			break;
		case 'c': {
			char ch;

			ch = va_arg(argp, int);
			PUTC(ch, fp);
			break;
		}
		case 'd':
		case 'i':
			GETARG(reg_long);
			if (reg_long < 0) {
				reg_ulong = -reg_long;
				printsign = '-';
			}
			else {
				reg_ulong = reg_long;
			}
			if (printsign)
				PUTC(printsign, fp);
			base = 10;
			goto num1;
		case 'E':
		case 'e':
			_double = va_arg(argp, double);
			bp = _cvt(_double, prec, EFORMAT, buf,
			    buf + sizeof(buf), *fmt);
			goto pbuf;
		case 'f':
			_double = va_arg(argp, double);
			bp = _cvt(_double, prec, FFORMAT, buf,
			    buf + sizeof(buf), 'f');
			goto pbuf;
		case 'G':
		case 'g':
			_double = va_arg(argp, double);
			bp = _cvt(_double, prec, GFORMAT, buf,
			    buf + sizeof(buf), *fmt - 2);
pbuf:			size = bp - buf;
			if (size < width && !ladjust)
				do {
					PUTC(padc, fp);
				} while (--width > size);
			for (t = buf; t < bp; ++t)
				PUTC(*t, fp);
			for (; width > size; --width)
				PUTC(padc, fp);
			break;
		case 'n':
			*(va_arg(argp, int *)) = cnt;
			break;
		case 'o':
			GETARG(reg_ulong);
			base = 8;
			if (!reg_ulong || !alternate)
				goto num1;
			bp = buf + sizeof(buf) - 1;
			do {
				*bp-- = digs[reg_ulong % base];
				reg_ulong /= base;
			} while(reg_ulong);
			size = &buf[sizeof(buf) - 1] - bp;
			if (size < --width && !ladjust)
				do {
					PUTC(padc, fp);
				} while (--width > size);
			PUTC('0', fp);
			goto num2;
		case 'p':
		case 's':
			if (!(bp = va_arg(argp, char *)))
				bp = "(null)";
			if (width > 0 && !ladjust) {
				char *savep;

				savep = bp;
				for (n = 0; *bp && (prec < 0 || n < prec);
				    n++, bp++);
				bp = savep;
				while (n++ < width)
					PUTC(' ', fp);
			}
			for (n = 0; *bp; ++bp) {
				if (++n > prec && prec >= 0)
					break;
				PUTC(*bp, fp);
			}
			if (n < width && ladjust)
				do {
					PUTC(' ', fp);
				} while (++n < width);
			break;
		case 'u':
			GETARG(reg_ulong);
			base = 10;
			goto num1;
		case 'X':
			digs = "0123456789ABCDEF";
			/*FALLTHROUGH*/
		case 'x':
			GETARG(reg_ulong);
			if (alternate && reg_ulong) {
				PUTC('0', fp);
				PUTC(*fmt, fp);
			}
			base = 16;
num1:			bp = buf + sizeof(buf) - 1;
			do {
				*bp-- = digs[reg_ulong % base];
				reg_ulong /= base;
			} while(reg_ulong);
			size = &buf[sizeof(buf) - 1] - bp;
			for (; size < prec; *bp-- = '0', ++size);
			if (size < width && !ladjust)
				do {
					PUTC(padc, fp);
				} while (--width > size);
num2:			while (++bp != &buf[MAXBUF])
				PUTC(*bp, fp);
			for (; width > size; --width)
				PUTC(padc, fp);
			digs = "0123456789abcdef";
			break;
		case '\0':		/* "%?" prints ?, unless ? is NULL */
			return(ferror(fp) ? -1 : cnt);
		default:
			PUTC(*fmt, fp);
		}
	}
	return(ferror(fp) ? -1 : cnt);
}

char *
_cvt(number, prec, format, startp, endp, fmtch)
	double number;
	int prec, format;
	char *startp, *endp, fmtch;
{
	register char *p;
	double fract, integer, tmp, modf();
	int decpt, expcnt;
	char *savep;

	if (prec == -1)				/* set default precision */
		prec = DEFPREC;

	p = endp - 1;
	if (number < 0) {			/* set sign */
		*startp++ = '-';
		number = -number;
	}
	else if (printsign)
		*startp++ = '+';

	/*
	 * if the alternate flag is set, or, at least one digit of precision
	 * was requested, add a decimal point, unless it's the g/G format
	 * in which case we require two digits of precision, since it counts
	 * precision differently.
	 */
	decpt = alternate || prec > 1 || !(format&GFORMAT) && prec;

	expcnt = 0;
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

			/* precision ran out; round number */
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
		 * g/G in f format; if run out of precision, replace digits
		 * with zeroes, note, have to round first, otherwise lose
		 * rounding point.
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
	 * it's unclear from the ANSI X3J11 spec if the g/G format should
	 * just result in an empty string, because it's supposed to remove
	 * trailing zeroes.  That seems counter-intuitive, so here it does
	 * what f and e/E do; if no fraction, the number was zero, and if
	 * no precision can't show anything after the decimal point.
	 */
	else if (!fract || !prec) {
		*startp++ = '0';
		if (decpt)
			*startp++ = '.';
		*startp++ = '\0';
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

	/* finish out requested precision from fractional value */
	while (prec--)
		if (fract) {
			fract = modf(fract * 10, &tmp);
			*p++ = (int)tmp + '0';
		}
		else
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
	if (!alternate) {
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
