/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)signal.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Almost backwards compatible signal.
 */
#include <signal.h>

sigset_t _sigintr;		/* shared with siginterrupt */

sig_t
signal(s, a)
	int s;
	sig_t a;
{
	struct sigaction sa, osa;

	sa.sa_handler = a;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	if (!sigismember(&_sigintr, s))
		sa.sa_flags |= SA_RESTART;
	if (sigaction(s, &sa, &osa) < 0)
		return (BADSIG);
	return (osa.sa_handler);
}
