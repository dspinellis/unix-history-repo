/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)siginterrupt.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <signal.h>

/*
 * Set signal state to prevent restart of system calls
 * after an instance of the indicated signal.
 */
siginterrupt(sig, flag)
	int sig, flag;
{
	extern sigset_t _sigintr;
	struct sigaction sa;
	int ret;

	if ((ret = sigaction(sig, (struct sigaction *)0, &sa)) < 0)
		return (ret);
	if (flag) {
		sigaddset(&_sigintr, sig);
		sa.sa_flags &= ~SA_RESTART;
	} else {
		sigdelset(&_sigintr, sig);
		sa.sa_flags |= SA_RESTART;
	}
	return (sigaction(sig, &sa, (struct sigaction *)0));
}
