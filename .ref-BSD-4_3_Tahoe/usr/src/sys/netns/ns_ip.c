/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ns_ip.c	7.3 (Berkeley) 6/29/88
 */

/*
 * Software interface driver for encapsulating ns in ip.
 */

#ifdef NSIP
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "ioctl.h"
#include "protosw.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#include "../machine/mtpr.h"

#include "../netns/ns.h"
#include "../netns/ns_if.h"
#include "../netns/idp.h"

struct ifnet_en {
	struct ifnet ifen_ifnet;
	struct route ifen_route;
	struct in_addr ifen_src;
	struct in_addr ifen_dst;
};

int	nsipoutput(), nsipioctl();
#define LOMTU	(1024+512);

struct ifnet nsipif;
struct mbuf *nsip_list;		/* list of all hosts and gateways or
					broadcast addrs */

struct mbuf *
nsipattach()
{
	register struct mbuf *m = m_getclr(M_DONTWAIT, MT_PCB);
	register struct ifnet *ifp;

	if (m == NULL) return (NULL);
	m->m_off = MMINOFF;
	m->m_len = sizeof(struct ifnet_en);
	m->m_next = nsip_list;
	nsip_list = m;
	ifp = mtod(m, struct ifnet *);

	ifp->if_name = "nsip";
	ifp->if_mtu = LOMTU;
	ifp->if_ioctl = nsipioctl;
	ifp->if_output = nsipoutput;
	ifp->if_flags = IFF_POINTOPOINT;
	ifp->if_unit = nsipif.if_unit++;
	if_attach(ifp);
	return (dtom(ifp));
}


/*
 * Process an ioctl request.
 */
/* ARGSUSED */
nsipioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	int error = 0;
	struct ifreq *ifr;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		/* fall into: */

	case SIOCSIFDSTADDR:
		/*
		 * Everything else is done at a higher level.
		 */
		break;

	case SIOCSIFFLAGS:
		ifr = (struct ifreq *)data;
		if ((ifr->ifr_flags & IFF_UP) == 0)
			error = nsip_free(ifp);


	default:
		error = EINVAL;
	}
	return (error);
}

struct mbuf *nsip_badlen;
struct mbuf *nsip_lastin;
int nsip_hold_input;

idpip_input(m, ifp)
	register struct mbuf *m;
	struct ifnet *ifp;
{
	register struct ip *ip;
	register struct idp *idp;
	register struct ifqueue *ifq = &nsintrq;
	int len, s;

	if (nsip_hold_input) {
		if (nsip_lastin) {
			m_freem(nsip_lastin);
		}
		nsip_lastin = m_copy(m, 0, (int)M_COPYALL);
	}
	/*
	 * Get IP and IDP header together in first mbuf.
	 */
	nsipif.if_ipackets++;
	s = sizeof (struct ip) + sizeof (struct idp);
	if ((m->m_off > MMAXOFF || m->m_len < s) &&
	    (m = m_pullup(m, s)) == 0) {
		nsipif.if_ierrors++;
		return;
	}
	ip = mtod(m, struct ip *);
	if (ip->ip_hl > (sizeof (struct ip) >> 2)) {
		ip_stripoptions(ip, (struct mbuf *)0);
		if (m->m_len < s) {
			if ((m = m_pullup(m, s)) == 0) {
				nsipif.if_ierrors++;
				return;
			}
			ip = mtod(m, struct ip *);
		}
	}

	/*
	 * Make mbuf data length reflect IDP length.
	 * If not enough data to reflect IDP length, drop.
	 */
	m->m_off += sizeof (struct ip);
	m->m_len -= sizeof (struct ip);
	idp = mtod(m, struct idp *);
	len = ntohs(idp->idp_len);
	if (len & 1) len++;		/* Preserve Garbage Byte */
	if (ip->ip_len != len) {
		if (len > ip->ip_len) {
			nsipif.if_ierrors++;
			if (nsip_badlen) m_freem(nsip_badlen);
			nsip_badlen = m;
			return;
		}
		/* Any extra will be trimmed off by the NS routines */
	}

	/*
	 * Place interface pointer before the data
	 * for the receiving protocol.
	 */
	if (m->m_off >= MMINOFF + sizeof(struct ifnet *)) {
		m->m_off -= sizeof(struct ifnet *);
		m->m_len += sizeof(struct ifnet *);
	} else {
		struct mbuf *n;

		n = m_get(M_DONTWAIT, MT_HEADER);
		if (n == (struct mbuf *)0)
			goto bad;
		n->m_off = MMINOFF;
		n->m_len = sizeof(struct ifnet *);
		n->m_next = m;
		m = n;
	}
	*(mtod(m, struct ifnet **)) = ifp;

	/*
	 * Deliver to NS
	 */
	s = splimp();
	if (IF_QFULL(ifq)) {
		IF_DROP(ifq);
bad:
		m_freem(m);
		splx(s);
		return;
	}
	IF_ENQUEUE(ifq, m);
	schednetisr(NETISR_NS);
	splx(s);
	return;
}

/* ARGSUSED */
nsipoutput(ifn, m0, dst)
	struct ifnet_en *ifn;
	struct mbuf *m0;
	struct sockaddr *dst;
{

	register struct mbuf *m = dtom(ifn);
	register struct ip *ip;
	register struct route *ro = &(ifn->ifen_route);
	register int len = 0;
	register struct idp *idp = mtod(m0, struct idp *);
	int error;

	if (m->m_len != sizeof(struct ifnet_en)) {
		printf("nsipoutput: bad dst ifp %x\n", ifn);
		goto bad;
	}
	ifn->ifen_ifnet.if_opackets++;
	nsipif.if_opackets++;


	/*
	 * Calculate data length and make space
	 * for IP header.
	 */
	len =  ntohs(idp->idp_len);
	if (len & 1) len++;		/* Preserve Garbage Byte */
	m = m0;
	if (m->m_off < MMINOFF + sizeof (struct ip)) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m_freem(m0);
			return (ENOBUFS);
		}
		m->m_off = MMAXOFF - sizeof (struct ip);
		m->m_len = sizeof (struct ip);
		m->m_next = m0;
	} else {
		m->m_off -= sizeof (struct ip);
		m->m_len += sizeof (struct ip);
	}
	/*
	 * Fill in IP header.
	 */
	ip = mtod(m, struct ip *);
	*(long *)ip = 0;
	ip->ip_p = IPPROTO_IDP;
	ip->ip_src = ifn->ifen_src;
	ip->ip_dst = ifn->ifen_dst;
	ip->ip_len = (u_short)len + sizeof (struct ip);
	ip->ip_ttl = MAXTTL;

	/*
	 * Output final datagram.
	 */
	error =  (ip_output(m, (struct mbuf *)0, ro, SO_BROADCAST));
	if (error) {
		ifn->ifen_ifnet.if_oerrors++;
		ifn->ifen_ifnet.if_ierrors = error;
	}
	return (error);
bad:
	m_freem(m0);
	return (ENETUNREACH);
}

struct ifreq ifr = {"nsip0"};

nsip_route(m)
	register struct mbuf *m;
{
	register struct nsip_req *rq = mtod(m, struct nsip_req *);
	struct sockaddr_ns *ns_dst = (struct sockaddr_ns *)&rq->rq_ns;
	struct sockaddr_in *ip_dst = (struct sockaddr_in *)&rq->rq_ip;
	struct route ro;
	struct ifnet_en *ifn;
	struct sockaddr_in *src;

	/*
	 * First, make sure we already have an ns address:
	 */
	if (ns_hosteqnh(ns_thishost, ns_zerohost))
		return (EADDRNOTAVAIL);
	/*
	 * Now, determine if we can get to the destination
	 */
	bzero((caddr_t)&ro, sizeof (ro));
	ro.ro_dst = *(struct sockaddr *)ip_dst;
	rtalloc(&ro);
	if (ro.ro_rt == 0 || ro.ro_rt->rt_ifp == 0) {
		return (ENETUNREACH);
	}

	/*
	 * And see how he's going to get back to us:
	 * i.e., what return ip address do we use?
	 */
	{
		register struct in_ifaddr *ia;
		struct ifnet *ifp = ro.ro_rt->rt_ifp;

		for (ia = in_ifaddr; ia; ia = ia->ia_next)
			if (ia->ia_ifp == ifp)
				break;
		if (ia == 0)
			ia = in_ifaddr;
		if (ia == 0) {
			RTFREE(ro.ro_rt);
			return (EADDRNOTAVAIL);
		}
		src = (struct sockaddr_in *)&ia->ia_addr;
	}

	/*
	 * Is there a free (pseudo-)interface or space?
	 */
	for (m = nsip_list; m; m = m->m_next) {
		struct ifnet *ifp = mtod(m, struct ifnet *);
		if ((ifp->if_flags & IFF_UP) == 0)
			break;
	}
	if (m == (struct mbuf *) 0)
		m = nsipattach();
	if (m == NULL) {
		RTFREE(ro.ro_rt);
		return (ENOBUFS);
	}
	ifn = mtod(m, struct ifnet_en *);

	ifn->ifen_route = ro;
	ifn->ifen_dst =  ip_dst->sin_addr;
	ifn->ifen_src = src->sin_addr;

	/*
	 * now configure this as a point to point link
	 */
	ifr.ifr_name[4] = '0' + nsipif.if_unit - 1;
	ifr.ifr_dstaddr = * (struct sockaddr *) ns_dst;
	(void)ns_control((struct socket *)0, (int)SIOCSIFDSTADDR, (caddr_t)&ifr,
			(struct ifnet *)ifn);
	satons_addr(ifr.ifr_addr).x_host = ns_thishost;
	return (ns_control((struct socket *)0, (int)SIOCSIFADDR, (caddr_t)&ifr,
			(struct ifnet *)ifn));
}

nsip_free(ifp)
struct ifnet *ifp;
{
	register struct ifnet_en *ifn = (struct ifnet_en *)ifp;
	struct route *ro = & ifn->ifen_route;

	if (ro->ro_rt) {
		RTFREE(ro->ro_rt);
		ro->ro_rt = 0;
	}
	ifp->if_flags &= ~IFF_UP;
	return (0);
}

nsip_ctlinput(cmd, sa)
	int cmd;
	struct sockaddr *sa;
{
	extern u_char inetctlerrmap[];
	struct sockaddr_in *sin;
	int in_rtchange();

	if ((unsigned)cmd >= PRC_NCMDS)
		return;
	if (sa->sa_family != AF_INET && sa->sa_family != AF_IMPLINK)
		return;
	sin = (struct sockaddr_in *)sa;
	if (sin->sin_addr.s_addr == INADDR_ANY)
		return;

	switch (cmd) {

	case PRC_ROUTEDEAD:
	case PRC_REDIRECT_NET:
	case PRC_REDIRECT_HOST:
	case PRC_REDIRECT_TOSNET:
	case PRC_REDIRECT_TOSHOST:
		nsip_rtchange(&sin->sin_addr);
		break;
	}
}

nsip_rtchange(dst)
	register struct in_addr *dst;
{
	register struct mbuf *m;
	register struct ifnet_en *ifn;

	for (m = nsip_list; m; m = m->m_next) {
		ifn = mtod(m, struct ifnet_en *);
		if (ifn->ifen_dst.s_addr == dst->s_addr &&
			ifn->ifen_route.ro_rt) {
				RTFREE(ifn->ifen_route.ro_rt);
				ifn->ifen_route.ro_rt = 0;
		}
	}
}
#endif
