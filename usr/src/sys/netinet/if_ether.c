/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ether.c	7.19 (Berkeley) %G%
 */

/*
 * Ethernet address resolution protocol.
 * TODO:
 *	add "inuse/lock" bit (or ref. count) along with valid bit
 */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "socket.h"
#include "time.h"
#include "kernel.h"
#include "errno.h"
#include "ioctl.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/if_dl.h"
#include "../net/route.h"

#include "in.h"
#include "in_systm.h"
#include "in_var.h"
#include "ip.h"
#include "if_ether.h"

#define SIN(s) ((struct sockaddr_in *)s)
#define SDL(s) ((struct sockaddr_dl *)s)
#define SRP(s) ((struct sockaddr_inarp *)s)

/*
 * ARP trailer negotiation.  Trailer protocol is not IP specific,
 * but ARP request/response use IP addresses.
 */
#define ETHERTYPE_IPTRAILERS ETHERTYPE_TRAIL


/* timer values */
int	arpt_prune = (5*60*1);	/* walk list every 5 minutes */
int	arpt_keep = (20*60);	/* once resolved, good for 20 more minutes */
int	arpt_down = 20;		/* once declared down, don't send for 20 secs */
#define RTF_USETRAILERS	RTF_PROTO1
#define rt_expire rt_rmx.rmx_expire

extern struct ifnet loif;
extern struct timeval time;
struct llinfo_arp *arplookup(), llinfo_arp = {&llinfo_arp, &llinfo_arp};
struct ifqueue arpintrq = {0, 0, 0, 50};
int	arp_inuse, arp_allocated, arp_intimer;
int	arp_maxtries = 5;
int	useloopback = 1;	/* use loopback interface for local traffic */
int	arpinit_done = 0;

/*
 * Timeout routine.  Age arp_tab entries periodically.
 */
arptimer()
{
	int s = splnet();
	register struct llinfo_arp *la = llinfo_arp.la_next;

	timeout(arptimer, (caddr_t)0, arpt_prune * hz);
	while (la != &llinfo_arp) {
		register struct rtentry *rt = la->la_rt;
		la = la->la_next;
		if (rt->rt_expire && rt->rt_expire <= time.tv_sec)
			arptfree(la->la_prev); /* timer has expired, clear */
	}
	splx(s);
}

/*
 * Parallel to llc_rtrequest. 
 */
arp_rtrequest(req, rt, sa)
	int req;
	register struct rtentry *rt;
	struct sockaddr *sa;
{
	register struct sockaddr *gate = rt->rt_gateway;
	register struct llinfo_arp *la = (struct llinfo_arp *)rt->rt_llinfo;
	static struct sockaddr_dl null_sdl = {sizeof(null_sdl), AF_LINK};

	if (!arpinit_done) {
		arpinit_done = 1;
		timeout(arptimer, (caddr_t)0, hz);
	}
	if (rt->rt_flags & RTF_GATEWAY)
		return;
	switch (req) {
	case RTM_ADD:
	case RTM_RESOLVE:
		if ((rt->rt_flags & RTF_HOST) == 0) /* Route to IF? XXX*/
			rt->rt_flags |= RTF_CLONING;
		if (rt->rt_flags & RTF_CLONING) {
			/*
			 * Case 1: This route should come from a route to iface.
			 */
			rt_setgate(rt, rt_key(rt), &null_sdl);
			gate = rt->rt_gateway;
			SDL(gate)->sdl_type = rt->rt_ifp->if_type;
			SDL(gate)->sdl_index = rt->rt_ifp->if_index;
			rt->rt_expire = time.tv_sec;
			break;
		}
		if (gate->sa_family != AF_LINK ||
		    gate->sa_len < sizeof(null_sdl)) {
			log(LOG_DEBUG, "arp_rtrequest: bad gateway value");
			break;
		}
		SDL(gate)->sdl_type = rt->rt_ifp->if_type;
		SDL(gate)->sdl_index = rt->rt_ifp->if_index;
		if (la != 0)
			break; /* This happens on a route change */
		/*
		 * Case 2:  This route may come from cloning, or a manual route
		 * add with a LL address.
		 */
		R_Malloc(la, struct llinfo_arp *, sizeof(*la));
		rt->rt_llinfo = (caddr_t)la;
		if (la == 0) {
			log(LOG_DEBUG, "arp_rtrequest: malloc failed\n");
			break;
		}
		arp_inuse++, arp_allocated++;
		Bzero(la, sizeof(*la));
		la->la_rt = rt;
		rt->rt_flags |= RTF_LLINFO;
		insque(la, &llinfo_arp);
		if (SIN(rt_key(rt))->sin_addr.s_addr ==
		    (IA_SIN(rt->rt_ifa))->sin_addr.s_addr) {
		    /*
		     * This test used to be
		     *	if (loif.if_flags & IFF_UP)
		     * It allowed local traffic to be forced
		     * through the hardware by configuring the loopback down.
		     * However, it causes problems during network configuration
		     * for boards that can't receive packets they send.
		     * It is now necessary to clear "useloopback" and remove
		     * the route to force traffic out to the hardware.
		     */
			rt->rt_expire = 0;
			Bcopy(((struct arpcom *)rt->rt_ifp)->ac_enaddr,
				LLADDR(SDL(gate)), SDL(gate)->sdl_alen = 6);
			if (useloopback)
				rt->rt_ifp = &loif;
				
		}
		break;

	case RTM_DELETE:
		if (la == 0)
			break;
		arp_inuse--;
		remque(la);
		rt->rt_llinfo = 0;
		rt->rt_flags &= ~RTF_LLINFO;
		if (la->la_hold)
			m_freem(la->la_hold);
		Free((caddr_t)la);
	}
}

/*
 * Broadcast an ARP packet, asking who has addr on interface ac.
 */
void
arpwhohas(ac, addr)
	register struct arpcom *ac;
	struct in_addr *addr;
{
	register struct mbuf *m;
	register struct ether_header *eh;
	register struct ether_arp *ea;
	struct sockaddr sa;

	if ((m = m_gethdr(M_DONTWAIT, MT_DATA)) == NULL)
		return;
	m->m_len = sizeof(*ea);
	m->m_pkthdr.len = sizeof(*ea);
	MH_ALIGN(m, sizeof(*ea));
	ea = mtod(m, struct ether_arp *);
	eh = (struct ether_header *)sa.sa_data;
	bzero((caddr_t)ea, sizeof (*ea));
	bcopy((caddr_t)etherbroadcastaddr, (caddr_t)eh->ether_dhost,
	    sizeof(eh->ether_dhost));
	eh->ether_type = ETHERTYPE_ARP;		/* if_output will swap */
	ea->arp_hrd = htons(ARPHRD_ETHER);
	ea->arp_pro = htons(ETHERTYPE_IP);
	ea->arp_hln = sizeof(ea->arp_sha);	/* hardware address length */
	ea->arp_pln = sizeof(ea->arp_spa);	/* protocol address length */
	ea->arp_op = htons(ARPOP_REQUEST);
	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
	   sizeof(ea->arp_sha));
	bcopy((caddr_t)&ac->ac_ipaddr, (caddr_t)ea->arp_spa,
	   sizeof(ea->arp_spa));
	bcopy((caddr_t)addr, (caddr_t)ea->arp_tpa, sizeof(ea->arp_tpa));
	sa.sa_family = AF_UNSPEC;
	sa.sa_len = sizeof(sa);
	(*ac->ac_if.if_output)(&ac->ac_if, m, &sa, (struct rtentry *)0);
}

int	useloopback = 1;	/* use loopback interface for local traffic */

/*
 * Resolve an IP address into an ethernet address.  If success, 
 * desten is filled in.  If there is no entry in arptab,
 * set one up and broadcast a request for the IP address.
 * Hold onto this mbuf and resend it once the address
 * is finally resolved.  A return value of 1 indicates
 * that desten has been filled in and the packet should be sent
 * normally; a 0 return indicates that the packet has been
 * taken over here, either now or for later transmission.
 */
arpresolve(ac, rt, m, dst, desten, usetrailers)
	register struct arpcom *ac;
	register struct rtentry *rt;
	struct mbuf *m;
	register struct sockaddr *dst;
	register u_char *desten;
	int *usetrailers;
{
	register struct llinfo_arp *la;
	register struct in_ifaddr *ia;
	struct sockaddr_dl *sdl;

	*usetrailers = 0;
	if (m->m_flags & M_BCAST) {	/* broadcast */
		bcopy((caddr_t)etherbroadcastaddr, (caddr_t)desten,
		    sizeof(etherbroadcastaddr));
		return (1);
	}
	if (rt)
		la = (struct llinfo_arp *)rt->rt_llinfo;
	else {
		if (la = arplookup(SIN(dst)->sin_addr.s_addr, 1, 0)) 
			rt = la->la_rt;
	}
	if (la == 0 || rt == 0) {
		log(LOG_DEBUG, "arpresolve: can't allocate llinfo");
		m_freem(m);
		return (0);
	}
	sdl = SDL(rt->rt_gateway);
	/*
	 * Check the address family and length is valid, the address
	 * is resolved; otherwise, try to resolve.
	 */
	if ((rt->rt_expire == 0 || rt->rt_expire > time.tv_sec) &&
	    sdl->sdl_family == AF_LINK && sdl->sdl_alen != 0) {
		bcopy(LLADDR(sdl), desten, sdl->sdl_alen);
		*usetrailers = rt->rt_flags & RTF_USETRAILERS;
		return 1;
	}
	/*
	 * There is an arptab entry, but no ethernet address
	 * response yet.  Replace the held mbuf with this
	 * latest one.
	 */
	if (la->la_hold)
		m_freem(la->la_hold);
	la->la_hold = m;
	if (rt->rt_expire) {
		rt->rt_flags &= ~RTF_REJECT;
		if (la->la_asked == 0 || rt->rt_expire != time.tv_sec) {
			rt->rt_expire = time.tv_sec;
			if (la->la_asked++ < arp_maxtries)
				arpwhohas(ac, &(SIN(dst)->sin_addr));
			else {
				rt->rt_flags |= RTF_REJECT;
				rt->rt_expire += arpt_down;
				la->la_asked = 0;
			}

		}
	}
	return (0);
}

/*
 * Common length and type checks are done here,
 * then the protocol-specific routine is called.
 */
arpintr()
{
	register struct mbuf *m;
	register struct arphdr *ar;
	int s;

	while (arpintrq.ifq_head) {
		s = splimp();
		IF_DEQUEUE(&arpintrq, m);
		splx(s);
		if (m == 0 || (m->m_flags & M_PKTHDR) == 0)
			panic("arpintr");
		if (m->m_len >= sizeof(struct arphdr) &&
		    (ar = mtod(m, struct arphdr *)) &&
		    ntohs(ar->ar_hrd) == ARPHRD_ETHER &&
		    m->m_len >=
		      sizeof(struct arphdr) + 2 * ar->ar_hln + 2 * ar->ar_pln)

			    switch (ntohs(ar->ar_pro)) {

			    case ETHERTYPE_IP:
			    case ETHERTYPE_IPTRAILERS:
				    in_arpinput(m);
				    continue;
			    }
		m_freem(m);
	}
}

/*
 * ARP for Internet protocols on 10 Mb/s Ethernet.
 * Algorithm is that given in RFC 826.
 * In addition, a sanity check is performed on the sender
 * protocol address, to catch impersonators.
 * We also handle negotiations for use of trailer protocol:
 * ARP replies for protocol type ETHERTYPE_TRAIL are sent
 * along with IP replies if we want trailers sent to us,
 * and also send them in response to IP replies.
 * This allows either end to announce the desire to receive
 * trailer packets.
 * We reply to requests for ETHERTYPE_TRAIL protocol as well,
 * but don't normally send requests.
 */
void
in_arpinput(m)
	struct mbuf *m;
{
	register struct ether_arp *ea;
	register struct arpcom *ac = (struct arpcom *)m->m_pkthdr.rcvif;
	struct ether_header *eh;
	register struct llinfo_arp *la = 0;
	register struct rtentry *rt;
	struct in_ifaddr *ia, *maybe_ia = 0;
	struct mbuf *mcopy = 0;
	struct sockaddr_dl *sdl;
	struct sockaddr sa;
	struct in_addr isaddr, itaddr, myaddr;
	int proto, op, completed = 0, sendtrailers;

	if (ac->ac_if.if_flags & IFF_NOARP)
		goto out;
	ea = mtod(m, struct ether_arp *);
	proto = ntohs(ea->arp_pro);
	op = ntohs(ea->arp_op);
	bcopy((caddr_t)ea->arp_spa, (caddr_t)&isaddr, sizeof (isaddr));
	bcopy((caddr_t)ea->arp_tpa, (caddr_t)&itaddr, sizeof (itaddr));
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if (ia->ia_ifp == &ac->ac_if) {
			maybe_ia = ia;
			if ((itaddr.s_addr == ia->ia_addr.sin_addr.s_addr) ||
			     (isaddr.s_addr == ia->ia_addr.sin_addr.s_addr))
				break;
		}
	if (maybe_ia == 0)
		goto out;
	myaddr = ia ? ia->ia_addr.sin_addr : maybe_ia->ia_addr.sin_addr;
	if (!bcmp((caddr_t)ea->arp_sha, (caddr_t)ac->ac_enaddr,
	    sizeof (ea->arp_sha)))
		goto out;	/* it's from me, ignore it. */
	if (!bcmp((caddr_t)ea->arp_sha, (caddr_t)etherbroadcastaddr,
	    sizeof (ea->arp_sha))) {
		log(LOG_ERR,
		    "arp: ether address is broadcast for IP address %x!\n",
		    ntohl(isaddr.s_addr));
		goto out;
	}
	if (isaddr.s_addr == myaddr.s_addr) {
		log(LOG_ERR,
		   "duplicate IP address %x!! sent from ethernet address: %s\n",
		   ntohl(isaddr.s_addr), ether_sprintf(ea->arp_sha));
		itaddr = myaddr;
		if (op == ARPOP_REQUEST)
			goto reply;
		goto out;
	}
	s = splimp();
	la = arplookup(isaddr.s_addr, itaddr.s_addr == myaddr.s_addr, 0);
	if (la && (rt = la->la_rt) && (sdl = SDL(rt->rt_gateway))) {
		if (sdl->sdl_alen &&
		    bcmp((caddr_t)ea->arp_sha, LLADDR(sdl), sdl->sdl_alen))
			log(LOG_INFO, "arp info overwritten for %x by %s\n",
			    isaddr.s_addr, ether_sprintf(ea->arp_sha));
		completed = 1; 
		bcopy((caddr_t)ea->arp_sha, LLADDR(sdl),
			    sdl->sdl_alen = sizeof(ea->arp_sha));
		if (rt->rt_expire)
			rt->rt_expire = time.tv_sec + arpt_keep;
		rt->rt_flags &= ~RTF_REJECT;
		la->la_asked = 0;
		if (la->la_hold) {
			(*ac->ac_if.if_output)(&ac->ac_if, la->la_hold,
				rt_key(rt), rt);
			la->la_hold = 0;
		}
	}
reply:
	switch (proto) {

	case ETHERTYPE_IPTRAILERS:
		/* partner says trailers are OK */
		if (la)
			la->la_rt->rt_flags |= RTF_USETRAILERS;
		/*
		 * Reply to request iff we want trailers.
		 */
		if (op != ARPOP_REQUEST || ac->ac_if.if_flags & IFF_NOTRAILERS)
			goto out;
		break;

	case ETHERTYPE_IP:
		/*
		 * Reply if this is an IP request,
		 * or if we want to send a trailer response.
		 * Send the latter only to the IP response
		 * that completes the current ARP entry.
		 */
		if (op != ARPOP_REQUEST &&
		    (completed == 0 || ac->ac_if.if_flags & IFF_NOTRAILERS))
			goto out;
	}
	if (itaddr.s_addr == myaddr.s_addr) {
		/* I am the target */
		bcopy((caddr_t)ea->arp_sha, (caddr_t)ea->arp_tha,
		    sizeof(ea->arp_sha));
		bcopy((caddr_t)ac->ac_enaddr, (caddr_t)ea->arp_sha,
		    sizeof(ea->arp_sha));
		sendtrailers = !(ac->ac_if.if_flags & IFF_NOTRAILERS);
	} else {
		la = arplookup(itaddr.s_addr, 0, SIN_PROXY);
		if (la == NULL)
			goto out;
		bcopy((caddr_t)ea->arp_sha, (caddr_t)ea->arp_tha,
		    sizeof(ea->arp_sha));
		sdl = SDL(la->la_rt->rt_gateway);
		bcopy(LLADDR(sdl), (caddr_t)ea->arp_sha, sizeof(ea->arp_sha));
		sendtrailers = rt->rt_flags & RTF_USETRAILERS;
	}

	bcopy((caddr_t)ea->arp_spa, (caddr_t)ea->arp_tpa,
	    sizeof(ea->arp_spa));
	bcopy((caddr_t)&itaddr, (caddr_t)ea->arp_spa,
	    sizeof(ea->arp_spa));
	ea->arp_op = htons(ARPOP_REPLY); 
	/*
	 * If incoming packet was an IP reply,
	 * we are sending a reply for type IPTRAILERS.
	 * If we are sending a reply for type IP
	 * and we want to receive trailers,
	 * send a trailer reply as well.
	 */
	if (op == ARPOP_REPLY)
		ea->arp_pro = htons(ETHERTYPE_IPTRAILERS);
	else if (proto == ETHERTYPE_IP && sendtrailers)
		mcopy = m_copy(m, 0, (int)M_COPYALL);
	eh = (struct ether_header *)sa.sa_data;
	bcopy((caddr_t)ea->arp_tha, (caddr_t)eh->ether_dhost,
	    sizeof(eh->ether_dhost));
	eh->ether_type = ETHERTYPE_ARP;
	sa.sa_family = AF_UNSPEC;
	sa.sa_len = sizeof(sa);
	(*ac->ac_if.if_output)(&ac->ac_if, m, &sa, (struct rtentry *)0);
	if (mcopy) {
		ea = mtod(mcopy, struct ether_arp *);
		ea->arp_pro = htons(ETHERTYPE_IPTRAILERS);
		(*ac->ac_if.if_output)(&ac->ac_if,
					mcopy, &sa, (struct rtentry *)0);
	}
	return;
out:
	m_freem(m);
	return;
}

/*
 * Free an arp entry.
 */
arptfree(la)
	register struct llinfo_arp *la;
{
	register struct rtentry *rt = la->la_rt;
	register struct sockaddr_dl *sdl;
	if (rt == 0)
		panic("arptfree");
	if (rt->rt_refcnt > 0 && (sdl = SDL(rt->rt_gateway)) &&
	    sdl->sdl_family == AF_LINK) {
		sdl->sdl_alen = 0;
		la->la_asked = 0;
		rt->rt_flags &= ~RTF_REJECT;
		return;
	}
	rtrequest(RTM_DELETE, rt_key(rt), (struct sockaddr *)0, rt_mask(rt),
			0, (struct rtentry **)0);
}
int arpdebug = 0;
/*
 * Lookup or enter a new address in arptab.
 */
struct llinfo_arp *
arplookup(addr, create, proxy)
	u_long addr;
{
	register struct rtentry *rt;
	static struct sockaddr_inarp sin = {sizeof(sin), AF_INET };

	sin.sin_addr.s_addr = addr;
	sin.sin_other = proxy ? SIN_PROXY : 0;
	rt = rtalloc1((struct sockaddr *)&sin, create);
	if (rt == 0)
		return (0);
	rt->rt_refcnt--;
	if ((rt->rt_flags & RTF_GATEWAY) || !(rt->rt_flags & RTF_LLINFO) ||
		rt->rt_gateway->sa_family != AF_LINK) {
		arpcatchme();
		if (arpdebug)
			log(LOG_DEBUG, "arptnew failed on %x\n", ntohl(addr));
		return (0);
	}
	return ((struct llinfo_arp *)rt->rt_llinfo);
}

arpcatchme(){}

arpioctl(cmd, data)
	int cmd;
	caddr_t data;
{
	return (EOPNOTSUPP);
}
