/*	getproto.c	4.2	82/10/05	*/

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
