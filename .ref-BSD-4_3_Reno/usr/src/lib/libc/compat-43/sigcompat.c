/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sigcompat.c	5.2 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/signal.h>

sigvec(signo, sv, osv)
	int signo;
	struct sigvec *sv, *osv;
{
	int ret;

	if (sv)
		sv->sv_flags ^= SV_INTERRUPT;	/* !SA_INTERRUPT */
	ret = sigaction(signo, (struct sigaction *)sv, (struct sigaction *)osv);
	if (ret == 0 && osv)
		osv->sv_flags ^= SV_INTERRUPT;	/* !SA_INTERRUPT */
	return (ret);
}

sigsetmask(mask)
	int mask;
{
	int omask, n;

	n = sigprocmask(SIG_SETMASK, (sigset_t *) &mask, (sigset_t *) &omask);
	if (n)
		return (n);
	return (omask);
}

sigblock(mask)
	int mask;
{
	int omask, n;

	n = sigprocmask(SIG_BLOCK, (sigset_t *) &mask, (sigset_t *) &omask);
	if (n)
		return (n);
	return (omask);
}

sigpause(mask)
	int mask;
{

	return (sigsuspend((sigset_t *)&mask));
}
