/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)stty.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Writearound to old stty system call.
 */

#include <sgtty.h>

stty(fd, ap)
	struct sgtty *ap;
{

	return(ioctl(fd, TIOCSETP, ap));
}
