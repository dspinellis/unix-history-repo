/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prf.c	7.3 (Berkeley) %G%
 */

#include "sys/param.h"

/*
 * Scaled down version of C Library printf.
 * Used to print diagnostic information directly on console tty.
 * Since it is not interrupt driven, all system activities are
 * suspended.  Printf should not be used for chit-chat.
 *
 * One additional format: %b is supported to decode error registers.
 * Usage is:
 *	printf("reg=%b\n", regval, "<base><arg>*");
 * Where <base> is the output base expressed as a control character,
 * e.g. \10 gives octal; \20 gives hex.  Each arg is a sequence of
 * characters, the first of which gives the bit number to be inspected
 * (origin 1), and the next characters (up to a control character, i.e.
 * a character <= 32), give the name of the register.  Thus
 *	printf("reg=%b\n", 3, "\10\2BITTWO\1BITONE\n");
 * would produce output:
 *	reg=2<BITTWO,BITONE>
 */
/*VARARGS1*/
printf(fmt, x1)
	char *fmt;
	unsigned x1;
{

	prf(0, fmt, &x1);
}

/*VARARGS1*/
romprintf(fmt, x1)
	char *fmt;
	unsigned x1;
{

	prf(1, fmt, &x1);
}

prf(userom, fmt, adx)
	register char *fmt;
	register u_int *adx;
{
	register int b, c, i;
	char *s;
	int any;

loop:
	while ((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(userom, c);
	}
again:
	c = *fmt++;
	/* THIS CODE IS VAX DEPENDENT IN HANDLING %l? AND %c */
	switch (c) {

	case 'l':
		goto again;
	case 'x': case 'X':
		b = 16;
		goto number;
	case 'd': case 'D':
	case 'u':		/* what a joke */
		b = 10;
		goto number;
	case 'o': case 'O':
		b = 8;
number:
		printn(userom, (u_long)*adx, b);
		break;
	case 'c':
		b = *adx;
		for (i = 24; i >= 0; i -= 8)
			if (c = (b >> i) & 0x7f)
				putchar(userom, c);
		break;
	case 'b':
		b = *adx++;
		s = (char *)*adx;
		printn(userom, (u_long)b, *s++);
		any = 0;
		if (b) {
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					putchar(userom, any? ',' : '<');
					any = 1;
					for (; (c = *s) > 32; s++)
						putchar(userom, c);
				} else
					for (; *s > 32; s++)
						;
			}
			if (any)
				putchar(userom, '>');
		}
		break;

	case 's':
		s = (char *)*adx;
		while (c = *s++)
			putchar(userom, c);
		break;
	}
	adx++;
	goto loop;
}

/*
 * Printn prints a number n in base b.
 * We don't use recursion to avoid deep kernel stacks.
 */
printn(userom, n, b)
	u_long n;
{
	char prbuf[11];
	register char *cp;

	if (b == 10 && (int)n < 0) {
		putchar(userom, '-');
		n = (unsigned)(-(int)n);
	}
	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
	do
		putchar(userom, *--cp);
	while (cp > prbuf);
}

/*
 * Print a character on console.
 */
putchar(userom, c)
	register c;
{
#ifdef ROMPRF
	if (userom) {
		romputchar(c);
		return;
	}
#endif
	cnputc(c);
	if(c == '\n')
		cnputc('\r');
}

peekchar()
{
	register c;

	c = cngetc();
	if (c == ('c'&037)) {
		printf("^C");
		_stop("");
		/* NOTREACHED */
	}
	return(c);
}

getchar()
{
	register c;

	while((c = cngetc()) == 0)
		;
	if (c == '\r')
		c = '\n';
	else if (c == ('c'&037)) {
		printf("^C");
		_stop("");
		/* NOTREACHED */
	}
	putchar(0, c);
	return(c);
}

gets(buf)
	char *buf;
{
	register char *lp;
	register c;

	lp = buf;
	for (;;) {
		c = getchar() & 0177;
		switch(c) {
		case '\n':
		case '\r':
			c = '\n';
			*lp++ = '\0';
			return;
		case '\b':
			if (lp > buf) {
				lp--;
				putchar(0, ' ');
				putchar(0, '\b');
			}
			continue;
		case '#':
		case '\177':
			lp--;
			if (lp < buf)
				lp = buf;
			continue;
		case '@':
		case 'u'&037:
			lp = buf;
			putchar(0, '\n');
			continue;
		default:
			*lp++ = c;
		}
	}
}

