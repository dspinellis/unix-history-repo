/*	getproto.c	4.1	82/08/25	*/

#include <netdb.h>

struct protoent *
getproto(proto)
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
