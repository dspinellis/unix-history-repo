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
 *
 * Originally derived from code posted by Steve Summit to USENET,
 * dated 3/25/87
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)vfprintf.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <varargs.h>
#include <stdio.h>
#include <ctype.h>

#define	MAXBUF		(sizeof(long) * 8)	 /* enough for binary */
#define	PUTC(ch, fd)	{ ++cnt; putc(ch, fd); }
#define	todigit(ch)	((ch) - '0')

doprnt(fmt, argp, fd)
	register char *fmt;
	va_list argp;
	register FILE *fd;
{
	register u_long reg_ulong;
	register long reg_long;
	register int base;
	register char *digs, *p, padc;
	char printsign, buf[MAXBUF];
	int alternate, cnt, n, ladjust, length, setlong, prec, size;

	for (cnt = 0; *fmt; ++fmt) {
		if (*fmt != '%') {
			PUTC(*fmt, fd);
			continue;
		}

		alternate = ladjust = length = setlong = 0;
		prec = -1;
		padc = ' ';
		printsign = '\0';

flags:		switch (*++fmt) {
		case '#':
			alternate = 1;
			goto flags;
		case '%':			/* "%#%" prints as "%" */
			PUTC('%', fd);
			continue;
		case '*':
			if ((length = va_arg(argp, int)) < 0) {
				ladjust = !ladjust;
				length = -length;
			}
			goto flags;
		case '+':
			printsign = '+';
			goto flags;
		case '-':
			ladjust = 1;
			goto flags;
		case '.':
			if (isdigit(*++fmt)) {
				do {
					prec = 10 * prec + todigit(*fmt);
				} while isdigit(*++fmt);
				--fmt;
			}
			else if (*fmt == '*')
				prec = va_arg(argp, int);
			goto flags;
		case '0':
			padc = '0';
			goto flags;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			do {
				length = 10 * length + todigit(*fmt);
			} while isdigit(*++fmt);
			--fmt;
		case 'l':
			setlong = 1;
			goto flags;
		}

		digs = "0123456789abcdef";

		switch (*fmt) {
		case 'c':
			PUTC(va_arg(argp, int), fd);
			break;
		case 'd':
			if (setlong)
				reg_long = va_arg(argp, long);
			else
				reg_long = va_arg(argp, int);
			if (reg_long < 0) {
				reg_ulong = -reg_long;
				printsign = '-';
			}
			else {
				reg_ulong = reg_long;
			}
			if (printsign)
				PUTC(printsign, fd);
			base = 10;
			goto donum;
		case 'o':
			if (setlong)
				reg_ulong = va_arg(argp, long);
			else
				reg_ulong = va_arg(argp, int);
			base = 8;
			goto donum;
		case 's':
			if (!(p = va_arg(argp, char *)))
				p = "(null)";
			if (length > 0 && !ladjust) {
				char *savep;

				savep = p;
				for (n = 0; *p && (prec == -1 || n < prec);
				    n++, p++);
				p = savep;
				while (n++ < length)
					PUTC(' ', fd);
			}
			for (n = 0; *p; ++p) {
				if (++n > prec && prec != -1)
					break;
				PUTC(*p, fd);
			}
			if (n < length && ladjust)
				do {
					PUTC(' ', fd);
				} while (++n < length);
			break;
		case 'u':
			if (setlong)
				reg_ulong = va_arg(argp, long);
			else
				reg_ulong = va_arg(argp, int);
			base = 10;
			goto donum;
		case 'X':
			digs = "0123456789ABCDEF";
			/*FALLTHROUGH*/
		case 'x':
			if (setlong)
				reg_ulong = va_arg(argp, long);
			else
				reg_ulong = va_arg(argp, int);
			if (alternate && reg_ulong) {
				PUTC('0', fd);
				PUTC(*fmt, fd);
			}
			base = 16;
donum:			p = &buf[sizeof(buf) - 1];
			do {
				*p-- = digs[reg_ulong % base];
				reg_ulong /= base;
			} while(reg_ulong);
			size = &buf[sizeof(buf) - 1] - p;
			if (reg_ulong && alternate && *fmt == 'o') {
				if (size < --length && !ladjust)
					do {
						PUTC(padc, fd);
					} while (--length > size);
				PUTC('0', fd);
				while (++p != &buf[MAXBUF])
					PUTC(*p, fd);
				if (size < length)	/* must be ladjust */
					for (; length > size; --length)
						PUTC(padc, fd);
			}
			else {
				if (size < length && !ladjust)
					do {
						PUTC(padc, fd);
					} while (--length > size);
				while (++p != &buf[MAXBUF])
					PUTC(*p, fd);
				if (size < length)	/* must be ladjust */
					for (; length > size; --length)
						PUTC(padc, fd);
			}
			break;
		case '\0':		/* "%?" prints ?, unless ? is NULL */
			return(cnt);
		default:
			PUTC(*fmt, fd);
		}
	}
	return(cnt);
}
