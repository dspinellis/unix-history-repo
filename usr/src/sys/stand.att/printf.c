/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)printf.c	5.3 (Berkeley) %G%
 */

/*
 * Scaled down version of printf(3).
 *
 * Used to print diagnostic information directly on the console tty.  Since
 * it is not interrupt driven, all system activities are suspended.  Printf
 * should not be used for chit-chat.
 *
 * One additional format: %b is supported to decode error registers.
 * Its usage is:
 *
 *	printf("reg=%b\n", regval, "<base><arg>*");
 *
 * where <base> is the output base expressed as a control character, e.g.
 * \10 gives octal; \20 gives hex.  Each arg is a sequence of characters,
 * the first of which gives the bit number to be inspected (origin 1), and
 * the next characters (up to a control character, i.e. a character <= 32),
 * give the name of the register.  Thus:
 *
 *	printf("reg=%b\n", 3, "\10\2BITTWO\1BITONE\n");
 *
 * would produce output:
 *
 *	reg=3<BITTWO,BITONE>
 */

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static void abort(){}				/* Needed by stdarg macros. */
static void number();

void
#if __STDC__
printf(const char *fmt, ...)
#else
printf(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	register char *p;
	register int ch, n;
	unsigned long ul;
	int lflag, set;

	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	for (;;) {
		while ((ch = *fmt++) != '%') {
			if (ch == '\0')
				return;
			putchar(ch);
		}
		lflag = 0;
reswitch:	switch (ch = *fmt++) {
		case 'l':
			lflag = 1;
			goto reswitch;
		case 'b':
			ul = va_arg(ap, int);
			p = va_arg(ap, char *);
			number(ul, *p++);

			if (!ul)
				break;

			for (set = 0; n = *p++;) {
				if (ul & (1 << (n - 1))) {
					putchar(set ? ',' : '<');
					for (; (n = *p) > ' '; ++p)
						putchar(n);
					set = 1;
				} else
					for (; *p > ' '; ++p);
			}
			if (set)
				putchar('>');
			break;
		case 'c':
			ch = va_arg(ap, int);
				putchar(ch & 0x7f);
			break;
		case 's':
			p = va_arg(ap, char *);
			while (ch = *p++)
				putchar(ch);
			break;
		case 'd':
			ul = lflag ?
			    va_arg(ap, long) : va_arg(ap, int);
			if ((long)ul < 0) {
				putchar('-');
				ul = -(long)ul;
			}
			number(ul, 10);
			break;
		case 'o':
			ul = lflag ?
			    va_arg(ap, long) : va_arg(ap, unsigned int);
			number(ul, 8);
			break;
		case 'u':
			ul = lflag ?
			    va_arg(ap, long) : va_arg(ap, unsigned int);
			number(ul, 10);
			break;
		case 'x':
			ul = lflag ?
			    va_arg(ap, long) : va_arg(ap, unsigned int);
			number(ul, 16);
			break;
		default:
			putchar('%');
			if (lflag)
				putchar('l');
			putchar(ch);
		}
	}
	va_end(ap);
}

static void
number(ul, base)
	unsigned long ul;
	int base;
{
	char *p, buf[11];			/* hold 2^32 in base 8 */

	p = buf;
	do {
		*p++ = "0123456789abcdef"[ul % base];
	} while (ul /= base);
	do {
		putchar(*--p);
	} while (p > buf);
}
