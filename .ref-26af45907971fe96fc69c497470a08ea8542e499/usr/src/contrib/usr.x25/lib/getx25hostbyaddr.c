/*
 * Fetch netdb entry from /etc/x25hosts for a
 * host given its X.121 address.
 *
 * Copyright 1985 Frank Pronk
 */

#include <sys/types.h>
#include <netccitt/x25.h>
#include <netdb.h>

struct hostent *
getx25hostbyaddr (addr)
char *addr;
{
	register struct hostent *p;
	struct hostent *getx25hostent ();

	setx25hostent(0);
	while ((p = getx25hostent ()) &&
		strcmp (((struct sockaddr_x25 *)p->h_addr)->x25_addr, addr))
			;
	endx25hostent ();
	return (p);
}
