/*
 * Copyright (c) 1989 Regents of the University of California.
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
static char sccsid[] = "@(#)siginterrupt.c	5.5 (Berkeley) 6/1/90";
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
