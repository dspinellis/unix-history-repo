/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getnetbyaddr.c	5.1 (Berkeley) %G%";
#endif not lint

#include <netdb.h>

struct netent *
getnetbyaddr(net, type)
	register int net, type;
{
	register struct netent *p;

	setnetent(0);
	while (p = getnetent())
		if (p->n_addrtype == type && p->n_net == net)
			break;
	endnetent();
	return (p);
}
