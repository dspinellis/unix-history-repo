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
static char sccsid[] = "@(#)getnetbyaddr.c	5.5 (Berkeley) %G%";
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
