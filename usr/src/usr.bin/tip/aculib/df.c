/*	df.c	4.1	81/05/09	*/
/*
 * Dial the DF02-AC
 */

#include "tip.h"
#if DF02
#include <setjmp.h>

static jmp_buf	Sjbuf;
static timeout();

df_dialer(num, acu)
char *num, *acu;
{
	register int f = FD;
	int c;

	ioctl(f, TIOCHPCL, 0);		/* make sure it hangs up when done */
	if (setjmp(Sjbuf)) {
		printf("connection timed out\r\n");
		df_disconnect();
		return(0);
	}
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	fflush(stdout);
	signal(SIGALRM, timeout);
	alarm(5 * strlen(num) + 10);
	ioctl(f, TIOCFLUSH, 0);
	write(f, "\001", 1);
	sleep(0);		/* this must waste 70 ms. */
	write(f, "\002", 1);
	write(f, num, strlen(num));
	c = 0;
	read(f, (char *)&c, 1);
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
