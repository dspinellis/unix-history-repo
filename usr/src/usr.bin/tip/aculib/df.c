/*	df.c	4.4	81/11/20	*/
/*
 * Dial the DF02-AC or DF03-AC
 */

#if defined(DF02) || defined(DF03)
#include "tip.h"
#include <setjmp.h>

static jmp_buf	Sjbuf;
static timeout();

#if DF02
df02_dialer(num, acu)
	char *num, *acu;
{
	return(df_dialer(num, acu, 0));
}
#endif

#if DF03
df03_dialer(num, acu)
	char *num, *acu;
{
	return(df_dialer(num, acu, 1));
}
#endif

df_dialer(num, acu, df03)
	char *num, *acu;
	int df03;
{
	register int f = FD;
	struct sgttyb buf;
	int speed = 0, c = 0;
#ifdef TIOCMSET
	int st = MST;		/* Secondary Transmit flag, for speed select */
#endif

	ioctl(f, TIOCHPCL, 0);		/* make sure it hangs up when done */
	if (setjmp(Sjbuf)) {
		printf("connection timed out\r\n");
		df_disconnect();
		return(0);
	}
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	fflush(stdout);
#ifdef TIOCMSET
	if (df03) {
		ioctl(f, TIOCGETP, &buf);
		if (buf.sg_ospeed != B1200) {	/* must dial at 1200 baud */
			speed = buf.sg_ospeed;
			buf.sg_ospeed = buf.sg_ispeed = B1200;
			ioctl(f, TIOCSETP, &buf);
			ioctl(f, TIOCMBIC, &st); /* clear ST for 300 baud */
		} else
			ioctl(f, TIOCMBIS, &st); /* set ST for 1200 baud */
	}
#endif
	signal(SIGALRM, timeout);
	alarm(5 * strlen(num) + 10);
	ioctl(f, TIOCFLUSH, 0);
	write(f, "\001", 1);
	sleep(1);
	write(f, "\002", 1);
	write(f, num, strlen(num));
	read(f, (char *)&c, 1);
#ifdef TIOCMSET
	if (df03 && speed) {
		buf.sg_ispeed = buf.sg_ospeed = speed;
		ioctl(f, TIOCSETP, &buf);
	}
#endif
	return(c == 'A');
}


df_disconnect()
{
	write(FD, "\001", 1);
	sleep(1);
	ioctl(FD, TIOCFLUSH, 0);
}


df_abort()
{
	write(FD, "\001", 1);
	sleep(1);
	ioctl(FD, TIOCFLUSH, 0);
}


static
timeout()
{
	longjmp(Sjbuf, 1);
}
#endif
