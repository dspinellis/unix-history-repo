/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getproto.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <netdb.h>

extern int _proto_stayopen;

struct protoent *
getprotobynumber(proto)
	register int proto;
{
	register struct protoent *p;

	setprotoent(_proto_stayopen);
	while (p = getprotoent())
		if (p->p_proto == proto)
			break;
	if (!_proto_stayopen)
		endprotoent();
	return (p);
}
