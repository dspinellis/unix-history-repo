/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)subr_prf.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <luna68k/stand/romvec.h>

#define TOCONS	0x1
#define TOTTY	0x2
#define TOLOG	0x4

/*
 * In case console is off,
 * panicstr contains argument to last
 * call to panic.
 */
char	*panicstr;

extern	cnputc();			/* standard console putc */
int	(*v_putc)() = cnputc;		/* routine to putc on virtual console */

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
 *	reg=3<BITTWO,BITONE>
 *
 * Another additional format: %r is used to pass an additional format string
 * and argument list recursively.  Usage is typically:
 *
 * fn(otherstuff, fmt [, arg1, ... ] )
 *	char *fmt;
 *	u_int arg1, ...;
 *
 *	printf("prefix: %r, other stuff\n", fmt, &arg1);
 */
#if defined(tahoe)
int	consintr;
#endif

/*VARARGS1*/
printf(fmt, x1)
	char *fmt;
	unsigned x1;
{
#if defined(tahoe)
	register int savintr;

	savintr = consintr, consintr = 0;	/* disable interrupts */
#endif
	prf(fmt, &x1, TOCONS        , (struct tty *)NULL);


#if defined(tahoe)
	consintr = savintr;			/* reenable interrupts */
#endif
}

prf(fmt, adx, flags, ttyp)
	register char *fmt;
	register u_int *adx;
	struct tty *ttyp;
{
	register int b, c, i;
	char *s;
	int any;

loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0')
			return;
		putchar(c, flags, ttyp);
	}
again:
	c = *fmt++;
	/* THIS CODE IS MACHINE DEPENDENT IN HANDLING %l? AND %c */
	switch (c) {

	case 'l':
		goto again;
	case 'x': case 'X':
		b = 16;
		goto number;
	case 'd': case 'D':
		b = -10;
		goto number;
	case 'u':
		b = 10;
		goto number;
	case 'o': case 'O':
		b = 8;
number:
		printn((u_long)*adx, b, flags, ttyp);
		break;
	case 'c':
		b = *adx;
#if BYTE_ORDER == LITTLE_ENDIAN
		for (i = 24; i >= 0; i -= 8)
			if (c = (b >> i) & 0x7f)
				putchar(c, flags, ttyp);
#endif
#if BYTE_ORDER == BIG_ENDIAN
		if (c = (b & 0x7f))
			putchar(c, flags, ttyp);
#endif
		break;
	case 'b':
		b = *adx++;
		s = (char *)*adx;
		printn((u_long)b, *s++, flags, ttyp);
		any = 0;
		if (b) {
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					putchar(any ? ',' : '<', flags, ttyp);
					any = 1;
					for (; (c = *s) > 32; s++)
						putchar(c, flags, ttyp);
				} else
					for (; *s > 32; s++)
						;
			}
			if (any)
				putchar('>', flags, ttyp);
		}
		break;

	case 's':
		s = (char *)*adx;
		while (c = *s++)
			putchar(c, flags, ttyp);
		break;

	case 'r':
		s = (char *)*adx++;
		prf(s, (u_int *)*adx, flags, ttyp);
		break;

	case '%':
		putchar('%', flags, ttyp);
		break;

	default:
		break;
	}
	adx++;
	goto loop;
}

/*
 * Printn prints a number n in base b.
 * We don't use recursion to avoid deep kernel stacks.
 */
printn(n, b, flags, ttyp)
	u_long n;
	struct tty *ttyp;
{
	char prbuf[11];
	register char *cp;

	if (b == -10) {
		if ((int)n < 0) {
			putchar('-', flags, ttyp);
			n = (unsigned)(-(int)n);
		}
		b = -b;
	}
	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
	do
		putchar(*--cp, flags, ttyp);
	while (cp > prbuf);
}

/*
 * Panic is called on unresolvable fatal errors.
 * It prints "panic: mesg", and then reboots.
 * If we are called twice, then we avoid trying to
 * sync the disks as this often leads to recursive panics.
 */
panic(s)
	char *s;
{
	if (!panicstr) {
		panicstr = s;
	}
	printf("panic: %s\n", s);

	ROM_abort();
}

/*
 * Print a character on console or users terminal.
 * If destination is console then the last MSGBUFS characters
 * are saved in msgbuf for inspection later.
 */
/*ARGSUSED*/
putchar(c, flags, ttyp)
	register int c;
	struct tty *ttyp;
{


























		(*v_putc)(c);
}
