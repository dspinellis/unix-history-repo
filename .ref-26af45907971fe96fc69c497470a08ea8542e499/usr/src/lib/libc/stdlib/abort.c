/*
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)abort.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/signal.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>

void
abort()
{
	sigset_t mask;

	sigfillset(&mask);
	/*
	 * don't block SIGABRT to give any handler a chance; we ignore
	 * any errors -- X311J doesn't allow abort to return anyway.
	 */
	sigdelset(&mask, SIGABRT);
	(void)sigprocmask(SIG_SETMASK, &mask, (sigset_t *)NULL);
	(void)kill(getpid(), SIGABRT);

	/*
	 * if SIGABRT ignored, or caught and the handler returns, do
	 * it again, only harder.
	 */
	(void)signal(SIGABRT, SIG_DFL);
	(void)sigprocmask(SIG_SETMASK, &mask, (sigset_t *)NULL);
	(void)kill(getpid(), SIGABRT);
	exit(1);
}
