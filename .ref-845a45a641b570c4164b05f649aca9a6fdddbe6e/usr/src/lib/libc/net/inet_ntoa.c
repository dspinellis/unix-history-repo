/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)inet_ntoa.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * Convert network-format internet address
 * to base 256 d.d.d.d representation.
 */
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>

char *
inet_ntoa(in)
	struct in_addr in;
{
	static char b[18];
	register char *p;

	p = (char *)&in;
#define	UC(b)	(((int)b)&0xff)
	(void)snprintf(b, sizeof(b),
	    "%d.%d.%d.%d", UC(p[0]), UC(p[1]), UC(p[2]), UC(p[3]));
	return (b);
}
