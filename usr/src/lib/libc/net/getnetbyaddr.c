/*	getnetbyaddr.c	4.1	82/08/25	*/

#include <netdb.h>

struct netent *
getnetaddr(net)
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
