/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)sigset.c	5.2 (Berkeley) 7/1/90
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sigset.c	5.2 (Berkeley) 7/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/signal.h>

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
	sigset_t *set;
	int signo;
{
	return ((*set & ~sigmask(signo)) != 0);
}
