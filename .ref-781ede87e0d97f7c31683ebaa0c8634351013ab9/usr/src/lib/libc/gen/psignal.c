/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)psignal.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Print the name of the signal indicated
 * along with the supplied message.
 */
#include <sys/signal.h>
#include <unistd.h>

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
		(void)write(STDERR_FILENO, s, n);
		(void)write(STDERR_FILENO, ": ", 2);
	}
	(void)write(STDERR_FILENO, c, strlen(c));
	(void)write(STDERR_FILENO, "\n", 1);
}
