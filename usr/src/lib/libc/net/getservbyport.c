/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getservbyport.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <netdb.h>

struct servent *
getservbyport(port, proto)
	int port;
	char *proto;
{
	register struct servent *p;

	setservent(0);
	while (p = getservent()) {
		if (p->s_port != port)
			continue;
		if (proto == 0 || strcmp(p->s_proto, proto) == 0)
			break;
	}
	endservent();
	return (p);
}
