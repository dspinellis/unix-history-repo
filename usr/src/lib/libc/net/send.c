/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)send.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>

send(s, msg, len, flags)
	int s, len, flags;
	const void *msg;
{
	return(sendto(s, msg, len, flags, (struct sockaddr *)NULL, 0));
}
