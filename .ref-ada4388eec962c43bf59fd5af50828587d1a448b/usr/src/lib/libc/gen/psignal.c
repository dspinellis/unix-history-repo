/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)psignal.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Print the name of the signal indicated
 * along with the supplied message.
 */
#include <sys/signal.h>
#include <string.h>
#include <unistd.h>

void
psignal(sig, s)
	unsigned int sig;
	const char *s;
{
	register const char *c;
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
