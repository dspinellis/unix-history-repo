/*	subr_prf.c	3.3	%G%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/seg.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/mtpr.h"

#ifdef TRACE
#define	TRCBUFS	4096
char	trcbuf[TRCBUFS];
char	*trcbufp = trcbuf;
int	trcwrap;
int	trcprt = TRCBUFS;
#endif

/*
 * In case console is off,
 * panicstr contains argument to last
 * call to panic.
 */

char	*panicstr;

/*
 * Scaled down version of C Library printf.
 * Only %s %u %d (==%u) %o %x %D are recognized.
 * Used to print diagnostic information
 * directly on console tty.
 * Since it is not interrupt driven,
 * all system activities are pretty much
 * suspended.
 * Printf should not be used for chit-chat.
 */
/*VARARGS1*/
printf(fmt, x1)
register char *fmt;
unsigned x1;
{

	prf(fmt, &x1, 0);
}

#ifdef TRACE
trace(fmt, x1)
register char *fmt;
unsigned x1;
{

	prf(fmt, &x1, 1);
}

#endif

prf(fmt, adx, trace)
register char *fmt;
register unsigned int *adx;
{
	register c;
	char *s;

loop:
	while((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(c, trace);
	}
	c = *fmt++;
	if (c == 'X')
		printx((long)*adx, trace);
	else if (c == 'd' || c == 'u' || c == 'o' || c == 'x')
		printn((long)*adx, c=='o'? 8: (c=='x'? 16:10), trace);
	else if (c == 's') {
		s = (char *)*adx;
		while (c = *s++)
#ifdef TRACE
			if (trace) {
				*trcbufp++ = c;
				if (trcbufp >= &trcbuf[TRCBUFS]) {
					trcbufp = trcbuf;
					trcwrap = 1;
				}
			} else
#endif
				putchar(c, trace);
	} else if (c == 'D') {
		printn(*(long *)adx, 10, trace);
		adx += (sizeof(long) / sizeof(int)) - 1;
	}
	adx++;
	goto loop;
}

printx(x, trace)
long x;
{
	int i;

	for (i = 0; i < 8; i++)
		putchar("0123456789ABCDEF"[(x>>((7-i)*4))&0xf], trace);
}

/*
 * Print an unsigned integer in base b.
 */
printn(n, b, trace)
long n;
{
	register long a;

	if (n<0) {	/* shouldn't happen */
		putchar('-', trace);
		n = -n;
	}
	if(a = n/b)
		printn(a, b, trace);
	putchar("0123456789ABCDEF"[(int)(n%b)], trace);
}

/*
 * Panic is called on unresolvable
 * fatal errors.
 * It syncs, prints "panic: mesg" and
 * then loops.
 */
panic(s)
char *s;
{
	panicstr = s;
	update();
	printf("panic: %s\n", s);
	spl0();
	for(;;)
		;
}

/*
 * prdev prints a warning message of the
 * form "mesg on dev x/y".
 * x and y are the major and minor parts of
 * the device argument.
 */
prdev(str, dev)
char *str;
dev_t dev;
{

	printf("%s on dev %u/%u\n", str, major(dev), minor(dev));
}

/*
 * deverr prints a diagnostic from
 * a device driver.
 * It prints the device, block number,
 * and an octal word (usually some error
 * status register) passed as argument.
 */
deverror(bp, o1, o2)
register struct buf *bp;
{

	prdev("err", bp->b_dev);
	printf("bn=%d er=%x,%x\n", bp->b_blkno, o1,o2);
}

#ifdef TRACE
dumptrc()
{
	register char *cp;
	register int pos, nch;

	nch = trcprt;
	if (nch < 0 || nch > TRCBUFS)
		nch = TRCBUFS;
	pos = (trcbufp - trcbuf) - nch;
	if (pos < 0)
		if (trcwrap)
			pos += TRCBUFS;
		else {
			nch += pos;
			pos = 0;
		}
	for (cp = &trcbuf[pos]; nch > 0; nch--) {
		putchar(*cp++, 0);
		if (cp >= &trcbuf[TRCBUFS])
			cp = trcbuf;
	}
}
#else
/*ARGSUSED*/
dumptrc(nch)
	int nch;
{

}
#endif

char	*msgbufp = msgbuf;	/* Next saved printf character */
/*
 * Print a character on console or in internal trace buffer.
 * If destination is console then the last MSGBUFS characters
 * are saved in msgbuf for inspection later.
 */
putchar(c, trace)
register c;
{
	register s, timo;

#ifdef TRACE
	if (trace) {
		*trcbufp++ = c;
		if (trcbufp >= &trcbuf[TRCBUFS]) {
			trcbufp = trcbuf;
			trcwrap = 1;
		}
		return;
	}
#endif
	if (c != '\0' && c != '\r' && c != 0177) {
		*msgbufp++ = c;
		if (msgbufp >= &msgbuf[MSGBUFS])
			msgbufp = msgbuf;
	}
	if (c == 0)
		return;
	cnputc(c);
}
