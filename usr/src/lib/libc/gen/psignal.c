/*	@(#)psignal.c	4.1 (Berkeley) %G%	*/
/*
 * Print the name of the signal indicated
 * along with the supplied message.
 */
#include <signal.h>

extern	char *sys_siglist[];

psignal(sig, s)
	unsigned sig;
	char *s;
{
	register char *c;
	register n;

	c = "Unknown signal";
	if (sig < NSIG)
		c = sys_siglist[sig];
	n = strlen(s);
	if (n) {
		write(2, s, n);
		write(2, ": ", 2);
	}
	write(2, c, strlen(c));
	write(2, "\n", 1);
}
