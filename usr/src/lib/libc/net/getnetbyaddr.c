/*	getnetbyaddr.c	4.2	82/10/05	*/

#include <netdb.h>

struct netent *
getnetbyaddr(net)
	register int net;
{
	register struct netent *p;

	setnetent(0);
	while (p = getnetent())
		if (p->n_net == net)
			break;
	endnetent();
	return (p);
}
