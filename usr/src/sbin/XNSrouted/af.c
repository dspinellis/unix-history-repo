#ifndef lint
static char rcsid[] = "$Header$";
#endif

#include "defs.h"

/*
 * Address family support routines
 */
int	null_hash(), null_netmatch(), null_output(),
	null_portmatch(), null_portcheck(),
	null_checkhost(), null_ishost(), null_canon();
int	xnnet_hash(), xnnet_netmatch(), xnnet_output(),
	xnnet_portmatch();
	xnnet_checkhost(), xnnet_ishost(), xnnet_canon();
#define NIL \
	{ null_hash,		null_netmatch,		null_output, \
	  null_portmatch,	null_portcheck,		null_checkhost, \
	  null_ishost,		null_canon }
#define	XNSNET \
	{ xnnet_hash,		xnnet_netmatch,		xnnet_output, \
	  xnnet_portmatch,	xnnet_portmatch,	xnnet_checkhost, \
	  xnnet_ishost,		xnnet_canon }

struct afswitch afswitch[AF_MAX] =
	{ NIL, NIL, NIL, NIL, NIL, NIL, XNSNET, NIL, NIL, NIL, NIL };

struct sockaddr_xn xnnet_default = { AF_XNS };

xnnet_hash(sxn, hp)
	register struct sockaddr_xn *sxn;
	struct afhash *hp;
{
	hp->afh_nethash = xnnet(sxn->sxn_addr.xn_net);
	hp->afh_hosthash = *(int *)sxn->sxn_addr.xn_host;
}

xnnet_netmatch(sxn1, sxn2)
	struct sockaddr_xn *sxn1, *sxn2;
{

	return (xnnet(sxn1->sxn_addr.xn_net) == xnnet(sxn2->sxn_addr.xn_net));
}

/*
 * Verify the message is from the right port.
 */
xnnet_portmatch(sxn)
	register struct sockaddr_xn *sxn;
{
	
	return (ntohs(sxn->sxn_addr.xn_socket) == IDPPORT_RIF );
}


/*
 * xns output routine.
 */
#ifdef DEBUG
int do_output = 0;
#endif
xnnet_output(s, flags, sxn, size)
	int s, flags;
	struct sockaddr_xn *sxn;
	int size;
{
	struct sockaddr_xn dst;

	dst = *sxn;
	sxn = &dst;
	if (sxn->sxn_addr.xn_socket == 0)
		sxn->sxn_addr.xn_socket = htons(IDPPORT_RIF);
#ifdef DEBUG
	if(do_output || ntohs(msg->rip_cmd) == RIPCMD_REQUEST)
#endif	
	if (sendto(s, msg, size, flags, sxn, sizeof (*sxn)) < 0)
		perror("sendto");
}

/*
 * Return 1 if the address is believed
 *  -- THIS IS A KLUDGE.
 */
xnnet_checkhost(sxn)
	struct sockaddr_xn *sxn;
{
	return (1);
}

/*
 * Return 1 if the address is
 * for a host, 0 for a network.
 */
xnnet_ishost(sxn)
	struct sockaddr_xn *sxn;
{
	register int i;

	for (i = 0; i < 6; i++)
		if (sxn->sxn_addr.xn_host[i] != 0) return (1);
	return (0);
}

xnnet_canon(sxn)
	struct sockaddr_xn *sxn;
{

	sxn->sxn_addr.xn_socket = 0;
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
null_output(s, f, a1, n)
	int s, f;
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
null_ishost(a1)
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
