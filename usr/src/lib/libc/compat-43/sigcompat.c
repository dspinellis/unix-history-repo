/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)sigcompat.c	5.1 (Berkeley) %G%";
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
