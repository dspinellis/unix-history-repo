/*	getservbyport.c	4.1	82/08/25	*/

#include <netdb.h>

struct servent *
getservport(port, proto)
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
