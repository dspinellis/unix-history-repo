/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)psignal.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Print the name of the signal indicated
 * along with the supplied message.
 */
#include <signal.h>

extern	char *sys_siglist[];

psignal(sig, s)
	unsigned int sig;
	char *s;
{
	register char *c;
	register int n;

	if (sig < NSIG)
		c = sys_siglist[sig];
	else
		c = "Unknown signal";
	n = strlen(s);
	if (n) {
		(void)write(2, s, n);
		(void)write(2, ": ", 2);
	}
	(void)write(2, c, strlen(c));
	(void)write(2, "\n", 1);
}
