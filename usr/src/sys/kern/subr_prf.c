/*	subr_prf.c	3.1	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/seg.h"
#include "../h/buf.h"
#include "../h/conf.h"

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
	register c;
	register unsigned int *adx;
	char *s;

	adx = &x1;
loop:
	while((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(c);
	}
	c = *fmt++;
	if(c == 'X')
		printx((long)*adx);
	else if(c == 'd' || c == 'u' || c == 'o' || c == 'x')
		printn((long)*adx, c=='o'? 8: (c=='x'? 16:10));
	else if(c == 's') {
		s = (char *)*adx;
		while(c = *s++)
			putchar(c);
	} else if (c == 'D') {
		printn(*(long *)adx, 10);
		adx += (sizeof(long) / sizeof(int)) - 1;
	}
	adx++;
	goto loop;
}

printx(x)
long x;
{
	int i;

	for (i = 0; i < 8; i++)
		putchar("0123456789ABCDEF"[(x>>((7-i)*4))&0xf]);
}

/*
 * Print an unsigned integer in base b.
 */
printn(n, b)
long n;
{
	register long a;

	if (n<0) {	/* shouldn't happen */
		putchar('-');
		n = -n;
	}
	if(a = n/b)
		printn(a, b);
	putchar("0123456789ABCDEF"[(int)(n%b)]);
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
