#ifndef lint
static char sccsid[] = "@(#)af.c	4.6 %G%";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include "router.h"
#include "rip.h"

extern char packet[MAXPACKETSIZE], *sys_errlist[];
extern int trace, errno;
#define	tprintf	if (trace) printf

/*
 * Address family support routines
 */
int	null_hash(), null_netmatch(), null_output(),
	null_portmatch(), null_portcheck(),
	null_checkhost(), null_canon();
int	inet_hash(), inet_netmatch(), inet_output(),
	inet_portmatch(), inet_portcheck(),
	inet_checkhost(), inet_canon();
#define NIL \
	{ null_hash,		null_netmatch,		null_output, \
	  null_portmatch,	null_portcheck,		null_checkhost, \
	  null_canon }
#define	INET \
	{ inet_hash,		inet_netmatch,		inet_output, \
	  inet_portmatch,	inet_portcheck,		inet_checkhost, \
	  inet_canon }

struct afswitch afswitch[AF_MAX] =
	{ NIL, NIL, INET, INET, NIL, NIL, NIL, NIL, NIL, NIL, NIL };

inet_hash(sin, hp)
	register struct sockaddr_in *sin;
	struct afhash *hp;
{

	hp->afh_nethash = IN_NETOF(sin->sin_addr);
	hp->afh_hosthash = sin->sin_addr.s_addr;
#if vax || pdp11
	hp->afh_hosthash = ntohl(hp->afh_hosthash);
#endif
	hp->afh_hosthash &= 0x7fffffff;
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{

	return (IN_NETOF(sin1->sin_addr) == IN_NETOF(sin2->sin_addr));
}

/*
 * Verify the message is from the right port.
 */
inet_portmatch(sin)
	struct sockaddr_in *sin;
{
	int port = sin->sin_port;
	
#if vax || pdp11
	port = ntohs(port);
#endif
	return (port == IPPORT_ROUTESERVER || port == IPPORT_ROUTESERVER+1);
}

/*
 * Verify the message is from a "trusted" port.
 */
inet_portcheck(sin)
	struct sockaddr_in *sin;
{
	int port = sin->sin_port;

#if vax || pdp11
	port = ntohs(port);
#endif
	return (port <= IPPORT_RESERVED);
}

/*
 * Internet output routine.
 */
inet_output(s, sin, size)
	int s;
	struct sockaddr_in *sin;
	int size;
{
	struct sockaddr_in dst;

	dst = *sin;
	sin = &dst;
	if (sin->sin_port == 0) {
		sin->sin_port = IPPORT_ROUTESERVER;
#if vax || pdp11
		sin->sin_port = htons(sin->sin_port);
#endif
	}
	if (send(s, sin, packet, size) < 0)
		tprintf("send to %x: %s\n", sin->sin_addr, sys_errlist[errno]);
}

/*
 * Return 1 if the address is for an Internet host,
 * otherwise assume it's a network address (broadcast).
 */
inet_checkhost(sin)
	struct sockaddr_in *sin;
{
	extern struct in_addr if_makeaddr();
	struct in_addr netaddr;

	netaddr = if_makeaddr(IN_NETOF(sin->sin_addr), INADDR_ANY);
	return (netaddr.s_addr != sin->sin_addr.s_addr);
}

inet_canon(sin)
	struct sockaddr_in *sin;
{
	sin->sin_port = 0;
}

/*ARGSUSED*/
null_hash(addr, hp)
	struct sockaddr *addr;
	struct afhash *hp;
{

	hp->afh_nethash = hp->afh_hosthash = 0;
}

/*ARGSUSED*/
null_netmatch(a1, a2)
	struct sockaddr *a1, *a2;
{

	return (0);
}

/*ARGSUSED*/
null_output(s, a1, n)
	int s;
	struct sockaddr *a1;
	int n;
{

	;
}

/*ARGSUSED*/
null_portmatch(a1)
	struct sockaddr *a1;
{

	return (0);
}

/*ARGSUSED*/
null_portcheck(a1)
	struct sockaddr *a1;
{

	return (0);
}

/*ARGSUSED*/
null_checkhost(a1)
	struct sockaddr *a1;
{

	return (0);
}

/*ARGSUSED*/
null_canon(a1)
	struct sockaddr *a1;
{

	;
}
