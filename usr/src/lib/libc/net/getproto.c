/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getproto.c	5.1 (Berkeley) %G%";
#endif not lint

#include <netdb.h>

struct protoent *
getprotobynumber(proto)
	register int proto;
{
	register struct protoent *p;

	setprotoent(0);
	while (p = getprotoent())
		if (p->p_proto == proto)
			break;
	endprotoent();
	return (p);
}
