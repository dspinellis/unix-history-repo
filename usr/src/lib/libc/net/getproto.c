/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getproto.c	5.4 (Berkeley) %G%";
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
