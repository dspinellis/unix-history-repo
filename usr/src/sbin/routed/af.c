#ifndef lint
static char sccsid[] = "@(#)af.c	4.9 %G%";
#endif

#include "router.h"

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

	hp->afh_nethash = inet_netof(sin->sin_addr);
	hp->afh_hosthash = sin->sin_addr.s_addr;
#if vax || pdp11
	hp->afh_hosthash = ntohl(hp->afh_hosthash);
#endif
	hp->afh_hosthash &= 0x7fffffff;
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{

	return (inet_netof(sin1->sin_addr) == inet_netof(sin2->sin_addr));
}

/*
 * Verify the message is from the right port.
 */
inet_portmatch(sin)
	register struct sockaddr_in *sin;
{
	
#if vax || pdp11
	sin->sin_port = ntohs(sin->sin_port);
#endif
	return (sin->sin_port == sp->s_port || sin->sin_port == sp->s_port+1);
}

/*
 * Verify the message is from a "trusted" port.
 */
inet_portcheck(sin)
	register struct sockaddr_in *sin;
{

#if vax || pdp11
	sin->sin_port = ntohs(sin->sin_port);
#endif
	return (sin->sin_port <= IPPORT_RESERVED);
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
	if (sin->sin_port == 0)
		sin->sin_port = htons(sp->s_port);
	if (send(s, sin, packet, size) < 0)
		perror("send");
}

/*
 * Return 1 if the address is believed
 * for an Internet host -- THIS IS A KLUDGE.
 */
inet_checkhost(sin)
	struct sockaddr_in *sin;
{
	return (inet_lnaof(sin->sin_addr) != 0);
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
