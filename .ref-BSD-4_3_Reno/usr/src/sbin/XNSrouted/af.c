/*
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This file includes significant work done at Cornell University by
 * Bill Nesheim.  That work included by permission.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)af.c	5.10 (Berkeley) 6/1/90";
#endif /* not lint */

#include "defs.h"

/*
 * Address family support routines
 */
int	null_hash(), null_netmatch(), null_output(),
	null_portmatch(), null_portcheck(),
	null_checkhost(), null_ishost(), null_canon();
int	xnnet_hash(), xnnet_netmatch(), xnnet_output(),
	xnnet_portmatch(),
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

struct sockaddr_ns xnnet_default = { sizeof(struct sockaddr_ns), AF_NS };

union ns_net ns_anynet;
union ns_net ns_zeronet;

xnnet_hash(sns, hp)
	register struct sockaddr_ns *sns;
	struct afhash *hp;
{
	register long hash = 0;
	register u_short *s = sns->sns_addr.x_host.s_host;
	union ns_net_u net;

	net.net_e = sns->sns_addr.x_net;
	hp->afh_nethash = net.long_e;
	hash = *s++; hash <<= 8; hash += *s++; hash <<= 8; hash += *s;
	hp->afh_hosthash = hash;
}

xnnet_netmatch(sxn1, sxn2)
	struct sockaddr_ns *sxn1, *sxn2;
{
	return (ns_neteq(sxn1->sns_addr, sxn2->sns_addr));
}

/*
 * Verify the message is from the right port.
 */
xnnet_portmatch(sns)
	register struct sockaddr_ns *sns;
{
	
	return (ntohs(sns->sns_addr.x_port) == IDPPORT_RIF );
}


/*
 * xns output routine.
 */
#ifdef DEBUG
int do_output = 0;
#endif
xnnet_output(flags, sns, size)
	int flags;
	struct sockaddr_ns *sns;
	int size;
{
	struct sockaddr_ns dst;

	dst = *sns;
	sns = &dst;
	if (sns->sns_addr.x_port == 0)
		sns->sns_addr.x_port = htons(IDPPORT_RIF);
#ifdef DEBUG
	if(do_output || ntohs(msg->rip_cmd) == RIPCMD_REQUEST)
#endif	
	/*
	 * Kludge to allow us to get routes out to machines that
	 * don't know their addresses yet; send to that address on
	 * ALL connected nets
	 */
	 if (ns_neteqnn(sns->sns_addr.x_net, ns_zeronet)) {
	 	extern  struct interface *ifnet;
	 	register struct interface *ifp;
		
		for (ifp = ifnet; ifp; ifp = ifp->int_next) {
			sns->sns_addr.x_net = 
				satons_addr(ifp->int_addr).x_net;
			(void) sendto(s, msg, size, flags, sns, sizeof (*sns));
		}
		return;
	}
	
	(void) sendto(s, msg, size, flags, sns, sizeof (*sns));
}

/*
 * Return 1 if we want this route.
 * We use this to disallow route net G entries for one for multiple
 * point to point links.
 */
xnnet_checkhost(sns)
	struct sockaddr_ns *sns;
{
	register struct interface *ifp = if_ifwithnet(sns);
	/*
	 * We want this route if there is no more than one 
	 * point to point interface with this network.
	 */
	if (ifp == 0 || (ifp->int_flags & IFF_POINTOPOINT)==0) return (1);
	return (ifp->int_sq.n == ifp->int_sq.p);
}

/*
 * Return 1 if the address is
 * for a host, 0 for a network.
 */
xnnet_ishost(sns)
struct sockaddr_ns *sns;
{
	register u_short *s = sns->sns_addr.x_host.s_host;

	if ((s[0]==0xffff) && (s[1]==0xffff) && (s[2]==0xffff))
		return (0);
	else
		return (1);
}

xnnet_canon(sns)
	struct sockaddr_ns *sns;
{

	sns->sns_addr.x_port = 0;
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
