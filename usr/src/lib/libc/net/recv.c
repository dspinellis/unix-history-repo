/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)recv.c	8.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/socket.h>

#include <stddef.h>

ssize_t
recv(s, buf, len, flags)
	int s, flags;
	size_t len;
	void *buf;
{
	return (recvfrom(s, buf, len, flags, NULL, 0));
}
