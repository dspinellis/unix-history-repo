/*	prf.c	1.5	89/04/25	*/
/*	prf.c	4.3	81/05/05	*/

#include "machine/mtpr.h"

#include "param.h"
#include "../tahoe/cp.h"

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

	prf(fmt, &x1);
}

prf(fmt, adx)
	register char *fmt;
	register u_int *adx;
{
	register int b, c, i;
	char *s;
	int any;

loop:
	while ((c = *fmt++) != '%') {
		if (c == '\0')
			return;
		putchar(c);
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
		printn((u_long)*adx, b);
		break;
	case 'c':
		b = *adx;
		for (i = 24; i >= 0; i -= 8)
			if (c = (b >> i) & 0x7f)
				putchar(c);
		break;
	case 'b':
		b = *adx++;
		s = (char *)*adx;
		printn((u_long)b, *s++);
		any = 0;
		if (b) {
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					putchar(any? ',' : '<');
					any = 1;
					for (; (c = *s) > 32; s++)
						putchar(c);
				} else
					for (; *s > 32; s++)
						;
			}
			if (any)
				putchar('>');
		}
		break;

	case 's':
		s = (char *)*adx;
		while (c = *s++)
			putchar(c);
		break;
	}
	adx++;
	goto loop;
}

/*
 * Print a character on console.
 */
struct	cpdcb_o cpout;
struct	cpdcb_i cpin;

/* console requires even parity */
#define EVENP

putchar(c)
	char c;
{
	int time;
#ifdef EVENP
	register mask, par;

	for (par = 0, mask = 1; mask != 0200; mask <<= 1, par <<= 1)
		par ^= c&mask;
	c |= par;
#endif EVENP
	cpout.cp_hdr.cp_unit = CPCONS;	/* Resets done bit */
	cpout.cp_hdr.cp_comm = CPWRITE;
	cpout.cp_hdr.cp_count = 1;
	cpout.cp_buf[0] = c;
	mtpr(CPMDCB, &cpout);
	time = 100000;				/* Delay loop */
	while (time--) {
		uncache(&cpout.cp_hdr.cp_unit);
		if (cpout.cp_hdr.cp_unit & CPDONE)
			break;
	}
	if (c == '\n')
		putchar ('\r');
}

getchar()
{
	char c;

	cpin.cp_hdr.cp_unit = CPCONS;	/* Resets done bit */
	cpin.cp_hdr.cp_comm = CPREAD;
	cpin.cp_hdr.cp_count = 1;
	mtpr(CPMDCB, &cpin);
	while ((cpin.cp_hdr.cp_unit & CPDONE) == 0) 
		uncache(&cpin.cp_hdr.cp_unit);
	uncache(&cpin.cpi_buf[0]);
	c = cpin.cpi_buf[0] & 0x7f;
	if (c == '\r')
		c = '\n';
	if (c != '\b' && c != '\177')
		putchar(c);
	return (c);
}

trap(ps)
	int ps;
{
	printf("Trap %o\n", ps);
	for (;;)
		;
}

uncache (addr)
	char *addr;
{
	/* Return *(addr-0x4000); DIRTY assumes this address is valid */
	mtpr(PDCS, addr);
}
