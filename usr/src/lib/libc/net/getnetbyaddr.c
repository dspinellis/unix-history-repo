/*	getnetbyaddr.c	4.3	82/10/06	*/

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
