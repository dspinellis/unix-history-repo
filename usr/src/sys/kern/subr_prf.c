/*	subr_prf.c	4.20	82/01/14	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/seg.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/mtpr.h"
#include "../h/reboot.h"
#include "../h/vm.h"
#include "../h/msgbuf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"

/*
 * In case console is off,
 * panicstr contains argument to last
 * call to panic.
 */
char	*panicstr;

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

	prf(fmt, &x1, 0);
}

/*
 * Uprintf prints to the current user's terminal,
 * guarantees not to sleep (so can be called by interrupt routines)
 * and does no watermark checking - (so no verbose messages).
 */
/*VARARGS1*/
uprintf(fmt, x1)
	char *fmt;
	unsigned x1;
{

	prf(fmt, &x1, 2);
}

prf(fmt, adx, touser)
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
		putchar(c, touser);
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
		printn((u_long)*adx, b, touser);
		break;
	case 'c':
		b = *adx;
		for (i = 24; i >= 0; i -= 8)
			if (c = (b >> i) & 0x7f)
				putchar(c, touser);
		break;
	case 'b':
		b = *adx++;
		s = (char *)*adx;
		printn((u_long)b, *s++, touser);
		any = 0;
		if (b) {
			putchar('<', touser);
			while (i = *s++) {
				if (b & (1 << (i-1))) {
					if (any)
						putchar(',', touser);
					any = 1;
					for (; (c = *s) > 32; s++)
						putchar(c, touser);
				} else
					for (; *s > 32; s++)
						;
			}
			if (any)
				putchar('>', touser);
		}
		break;

	case 's':
		s = (char *)*adx;
		while (c = *s++)
			putchar(c, touser);
		break;

	case '%':
		putchar('%', touser);
		break;
	}
	adx++;
	goto loop;
}

/*
 * Printn prints a number n in base b.
 * We don't use recursion to avoid deep kernel stacks.
 */
printn(n, b, touser)
	u_long n;
{
	char prbuf[11];
	register char *cp;

	if (b == 10 && (int)n < 0) {
		putchar('-', touser);
		n = (unsigned)(-(int)n);
	}
	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
	do
		putchar(*--cp, touser);
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
	int bootopt = panicstr ? RB_AUTOBOOT : RB_AUTOBOOT|RB_NOSYNC;

	panicstr = s;
	printf("panic: %s\n", s);
	(void) spl0();
	boot(RB_PANIC, bootopt);
}

/*
 * Warn that a system table is full.
 */
tablefull(tab)
	char *tab;
{

	printf("%s: table is full\n", tab);
}

/*
 * Hard error is the preface to plaintive error messages
 * about failing disk transfers.
 */
harderr(bp, cp)
	struct buf *bp;
	char *cp;
{

	printf("%s%d%c: hard error sn%d ", cp,
	    dkunit(bp), 'a'+(minor(bp->b_dev)&07), bp->b_blkno);
}

/*
 * Print a character on console or users terminal.
 * If destination is console then the last MSGBUFS characters
 * are saved in msgbuf for inspection later.
 */
/*ARGSUSED*/
putchar(c, touser)
	register int c;
{

	if (touser) {
		register struct tty *tp = u.u_ttyp;

		if (tp && (tp->t_state&TS_CARR_ON)) {
			register s = spl6();
			if (c == '\n')
				(void) ttyoutput('\r', tp);
			(void) ttyoutput(c, tp);
			ttstart(tp);
			splx(s);
		}
		return;
	}
	if (c != '\0' && c != '\r' && c != 0177 && mfpr(MAPEN)) {
		if (msgbuf.msg_magic != MSG_MAGIC) {
			msgbuf.msg_bufx = 0;
			msgbuf.msg_magic = MSG_MAGIC;
		}
		if (msgbuf.msg_bufx < 0 || msgbuf.msg_bufx >= MSG_BSIZE)
			msgbuf.msg_bufx = 0;
		msgbuf.msg_bufc[msgbuf.msg_bufx++] = c;
	}
	if (c == 0)
		return;
	cnputc(c);
}
