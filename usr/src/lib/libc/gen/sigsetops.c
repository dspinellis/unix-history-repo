/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sigsetops.c	5.5 (Berkeley) %G%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sigsetops.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <signal.h>

#undef sigemptyset
#undef sigfillset
#undef sigaddset
#undef sigdelset
#undef sigismember

sigemptyset(set)
	sigset_t *set;
{
	*set = 0;
	return (0);
}

sigfillset(set)
	sigset_t *set;
{
	*set = ~(sigset_t)0;
	return (0);
}

sigaddset(set, signo)
	sigset_t *set;
	int signo;
{
	*set |= sigmask(signo);
	return (0);
}

sigdelset(set, signo)
	sigset_t *set;
	int signo;
{
	*set &= ~sigmask(signo);
	return (0);
}

sigismember(set, signo)
	const sigset_t *set;
	int signo;
{
	return ((*set & ~sigmask(signo)) != 0);
}
