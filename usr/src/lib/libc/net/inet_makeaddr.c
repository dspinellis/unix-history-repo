/*	inet_makeaddr.c	4.4	85/06/02	*/

#include <sys/types.h>
#include <netinet/in.h>

/*
 * Formulate an Internet address from network + host.  Used in
 * building addresses stored in the ifnet structure.
 */
struct in_addr
inet_makeaddr(net, host)
	int net, host;
{
	u_long addr;

	if (net < 128)
		addr = (net << IN_CLASSA_NSHIFT) | (host & IN_CLASSA_HOST);
	else if (net < 65536)
		addr = (net << IN_CLASSB_NSHIFT) | (host & IN_CLASSB_HOST);
	else
		addr = (net << IN_CLASSC_NSHIFT) | (host & IN_CLASSC_HOST);
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}
