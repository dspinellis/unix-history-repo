/*
 * Copyright (c) 1985 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)abort.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/signal.h>

abort()
{
	(void)sigblock(~0L);
	(void)sigsetmask(~sigmask(SIGABRT));
	/* leave catch function active to give program a crack at it */
	(void)kill(getpid(), SIGABRT);
	/* if we got here, it was no good; reset to default and stop */
	(void)signal(SIGABRT, SIG_DFL);
	(void)sigsetmask(~sigmask(SIGABRT));
	(void)kill(getpid(), SIGABRT);
}
