/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)clnp_sprintf.c	7.1 (Berkeley) %G%
 */

/*
 * CLNP needs a version of sprintf in the kernel.  If anything else
 * ever needs it, this can trivially be dropped into kern/subr_prf.c.
 */
#include "param.h"

/*
 * Note that stdarg.h and the ANSI style va_start macro is used for both
 * ANSI and traditional C compilers.
 */
#include <machine/stdarg.h>

int    sprintf __P((char *, const char *, ...));

/*
 * Scaled down version of sprintf(3).
 */
#ifdef __STDC__
sprintf(char *buf, const char *fmt, ...)
#else
sprintf(buf, fmt /*, va_alist */)
	char *buf, *fmt;
#endif
{
	register char *p, *bp;
	register int ch, base;
	u_long ul;
	int lflag;				/* hold a long in base 8 */
	char num[(sizeof(long) * NBBY / 3) + 1];
	va_list ap;

	va_start(ap, fmt);
	for (bp = buf;;) {
		while ((ch = *fmt++) != '%') {
			if ((*bp = ch) == '\0')
				return(bp - buf);
			*bp++ = ch;
		}
		lflag = 0;
reswitch:	switch (ch = *fmt++) {
		case 'l':
			lflag = 1;
			goto reswitch;
		case 'c':
			*bp++ = va_arg(ap, int);
			break;
		case 's':
			p = va_arg(ap, char *);
			while (*bp++ = *p++);
			--bp;
			break;
		case 'D':
			lflag = 1;
			/* FALLTHROUGH */
		case 'd':
			ul = lflag ?
			    va_arg(ap, long) : va_arg(ap, int);
			if ((long)ul < 0) {
				*bp++ = '-';
				ul = -(long)ul;
			}
			base = 10;
			goto number;
			break;
		case 'O':
			lflag = 1;
			/* FALLTHROUGH */
		case 'o':
			ul = lflag ?
			    va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 8;
			goto number;
			break;
		case 'U':
			lflag = 1;
			/* FALLTHROUGH */
		case 'u':
			ul = lflag ?
			    va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 10;
			goto number;
			break;
		case 'X':
			lflag = 1;
			/* FALLTHROUGH */
		case 'x':
			ul = lflag ?
			    va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 16;
number:			p = num;
			do {
				*p++ = "0123456789abcdef"[ul % base];
			} while (ul /= base);
			do {
				*bp++ = *--p;
			} while (p > num);
			break;
		default:
			*bp++ = '%';
			if (lflag)
				*bp++ = 'l';
			*bp++ = ch;
		}
	}
	va_end(ap);
}
