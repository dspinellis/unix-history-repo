#ifndef lint
static char sccsid[] = "@(#)af.c	4.1 %G%";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>

#include "router.h"
#include "rip.h"

/*
 * Address family support routines
 */
int	null_hash(), null_netmatch(), null_output(),
	null_portmatch(), null_checkhost(), null_canon();
int	inet_hash(), inet_netmatch(), inet_output(),
	inet_portmatch(), inet_checkhost(), inet_canon();
#define NIL \
	{ null_hash,		null_netmatch,		null_output, \
	  null_portmatch,	null_checkhost,		null_canon }
#define	INET \
	{ inet_hash,		inet_netmatch,		inet_output, \
	  inet_portmatch,	inet_checkhost,		inet_canon }

struct afswitch afswitch[AF_MAX] =
	{ NIL, NIL, INET, INET, NIL, NIL, NIL, NIL, NIL, NIL, NIL };

inet_hash(sin, hp)
	register struct sockaddr_in *sin;
	struct afhash *hp;
{

	hp->afh_nethash = sin->sin_addr.s_net;
	hp->afh_hosthash = ntohl(sin->sin_addr.s_addr);
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{

	return (sin1->sin_addr.s_net == sin2->sin_addr.s_net);
}

/*
 * Verify the message is from the right port.
 */
inet_portmatch(sin)
	struct sockaddr_in *sin;
{
	int port = ntohs(sin->sin_port);

	return (port == IPPORT_ROUTESERVER);
}

/*
 * Internet output routine.
 */
inet_output(sin, size)
	struct sockaddr_in *sin;
	int size;
{
	extern int s;
	extern char packet[MAXPACKETSIZE];
	struct sockaddr_in dst;

	dst = *sin;
	sin = &dst;
	if (sin->sin_port == 0)
		sin->sin_port = htons(IPPORT_ROUTESERVER);
	if (send(s, sin, packet, size) < 0)
		perror("send");
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

	netaddr = if_makeaddr((int)sin->sin_addr.s_net, INADDR_ANY);
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
null_output(a1, n)
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
