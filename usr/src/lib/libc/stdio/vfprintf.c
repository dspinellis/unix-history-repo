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
static char sccsid[] = "@(#)vfprintf.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <varargs.h>
#include <stdio.h>
#include <ctype.h>

#define	MAXBUF		120
#define	DEFPREC		6

#define	PUTC(ch, fd)	{++cnt; putc(ch, fd);}

#define	EFORMAT		1
#define	FFORMAT		2
#define	GFORMAT		3

#define	LONGINT		0x01
#define	LONGDBL		0x02
#define	SHORTINT	0x04
#define	GETARG(r) \
	r = argsize&LONGINT ? va_arg(argp, long) : \
	    argsize&SHORTINT ? va_arg(argp, short) : va_arg(argp, int);

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
	char argsize, printsign, *_cvt(), buf[MAXBUF];
	int alternate, cnt, n, ladjust, width, prec, size;

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
			bp = _cvt(_double, prec, buf, EFORMAT, *fmt,
			    printsign, alternate);
			goto pbuf;
		case 'f':
			_double = va_arg(argp, double);
			bp = _cvt(_double, prec, buf, FFORMAT, 'f',
			    printsign, alternate);
			goto pbuf;
		case 'G':
		case 'g':
			_double = va_arg(argp, double);
			bp = _cvt(_double, prec, buf, GFORMAT, *fmt - 2,
			    printsign, alternate);
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
_cvt(number, prec, bp, format, fmtch, printsign, alternate)
	double number;
	int prec, format, alternate;
	register char *bp;
	char fmtch, printsign;
{
	int sign, decpt;
	register char *t;
	register int n;
	double fabs();
	char *ecvt(), *fcvt();

	if (prec == -1)
		prec = DEFPREC;
	t = fabs(number) < 1 ? ecvt(number, prec + 1, &decpt, &sign) :
	    fcvt(number, prec + 1, &decpt, &sign);

	if (sign)
		*bp++ = '-';
	else if (printsign)
		*bp++ = printsign;

	/* E format */
	/* use 'e' format if exponent > precision or less than -4 */
	if (format == EFORMAT ||
	    format == GFORMAT && (decpt > prec || decpt < -3)) {
		*bp++ = *t ? *t++ : '0';
		if (format != GFORMAT && prec || prec > 1) {
			*bp++ = '.';
			while(prec--)
				*bp++ = *t ? *t++ : '0';
		}
		else if (alternate)
			*bp++ = '.';
		if (*t && *t > '4')
			++bp[-1];
		if (format == GFORMAT && !alternate) {
			for (; bp[-1] == '0'; --bp);
			if (bp[-1] == '.')
				--bp;
		}
		*bp++ = fmtch;
		if (--decpt < 0) {
			decpt = -decpt;
			*bp++ = '-';
		}
		else
			*bp++ = '+';
		*bp++ = decpt / 10 + '0';
		*bp++ = decpt % 10 + '0';
	}
	/* F format */
	else {
		if (decpt <= 0) {
			*bp++ = '0';
			if (prec) {
				*bp++ = '.';
				if (format == FFORMAT)
					while (decpt++ < 0 && prec--)
						*bp++ = '0';
				else while (decpt++ < 0)
					*bp++ = '0';
			}
			else if (alternate)
				*bp++ = '.';
		}
		else {
			for (n = 1; n <= decpt; n++)
				*bp++ = *t++;
			if (prec || alternate)
				*bp++ = '.';
		}
		for (n = 1; n <= prec; n++)
			*bp++ = *t ? *t++ : '0';
		if (format == GFORMAT && !alternate) {
			for (; bp[-1] == '0'; --bp);
			if (bp[-1] == '.')
				--bp;
		}
	}
	return(bp);
}
