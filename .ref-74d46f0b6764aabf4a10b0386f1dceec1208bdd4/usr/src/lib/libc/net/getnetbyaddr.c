/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getnetbyaddr.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <netdb.h>

extern int _net_stayopen;

struct netent *
getnetbyaddr(net, type)
	register long net;
	register int type;
{
	register struct netent *p;

	setnetent(_net_stayopen);
	while (p = getnetent())
		if (p->n_addrtype == type && p->n_net == net)
			break;
	if (!_net_stayopen)
		endnetent();
	return (p);
}
