/*	inet_makeaddr.c	4.1	82/10/07	*/

#include <sys/types.h>
#include <net/in.h>

/*
 * Formulate an Internet address from network + host.  Used in
 * building addresses stored in the ifnet structure.
 */
struct in_addr
if_makeaddr(net, host)
	int net, host;
{
	u_long addr;

	if (net < 128)
		addr = (net << 24) | host;
	else if (net < 65536)
		addr = (net << 16) | host;
	else
		addr = (net << 8) | host;
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}
