/*	gethostbyaddr.c	4.3	82/10/06	*/

#include <netdb.h>

struct hostent *
gethostbyaddr(addr, len, type)
	char *addr;
	register int len, type;
{
	register struct hostent *p;

	sethostent(0);
	while (p = gethostent()) {
		if (p->h_addrtype != type || p->h_length != len)
			continue;
		if (bcmp(p->h_addr, addr, len) == 0)
			break;
	}
	endhostent();
	return (p);
}
