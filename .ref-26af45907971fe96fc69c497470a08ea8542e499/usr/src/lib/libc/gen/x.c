/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)x.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <setjmp.h>

int
sigsetjmp(jmpbuf, savemask)
	sigjmp_buf	jmpbuf;
	int		savemask;
{

	jmpbuf[_JBLEN] = savemask;
	if (savemask)
		return (setjmp(jmpbuf));
	return (_setjmp(jmpbuf));
}

void
siglongjmp(jmpbuf, retval)
	sigjmp_buf	jmpbuf;
	int		retval;
{

	if (jmpbuf[_JBLEN])
		return (longjmp(jmpbuf, retval));
	return (_longjmp(jmpbuf, retval));
}
