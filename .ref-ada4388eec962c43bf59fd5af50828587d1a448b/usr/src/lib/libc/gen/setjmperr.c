/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setjmperr.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * This routine is called from longjmp() when an error occurs.
 * Programs that wish to exit gracefully from this error may
 * write their own versions.
 * If this routine returns, the program is aborted.
 */

#include <setjmp.h>
#include <unistd.h>

void
longjmperror()
{
#define	ERRMSG	"longjmp botch.\n"
	(void)write(STDERR_FILENO, ERRMSG, sizeof(ERRMSG) - 1);
}
