/*      ns_ip.c     6.1     85/05/30     */

/*
 * Software interface driver for encapsulating ns in ip.
 */

#ifdef NSIP
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "errno.h"
#include "ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif

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
union ns_net nsip_net;
struct mbuf *nsip_list;		/* list of all hosts and gateways or
					broadcast addrs */

struct mbuf *
nsipattach()
{
	register struct mbuf *m = m_getclr(M_DONTWAIT, MT_PCB);
	register struct ifnet *ifp;
	register struct sockaddr_in *sin;

	if (m==0) return (0);
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
	return(dtom(ifp));
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

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		/*
		 * Everything else is done at a higher level.
		 */
		break;

	default:
		error = EINVAL;
	}
	return (error);
}

struct mbuf *nsip_badlen;
struct mbuf *nsip_lastin;
int nsip_hold_input;

idpip_input(m0)
	struct mbuf *m0;
{
	register struct ip *ip;
	register struct idp *idp;
	register struct mbuf *m;
	register struct ifqueue *ifq = &nsintrq;
	int len, s;

	if(nsip_hold_input) {
		if(nsip_lastin) {
			m_free(nsip_lastin);
		}
		nsip_lastin = m_copy(m0, 0, M_COPYALL);
	}
	/*
	 * Get IP and IDP header together in first mbuf.
	 */
	nsipif.if_ipackets++;
	m = m0;
	s = sizeof (struct ip) + sizeof (struct idp);
	if ((m->m_off > MMAXOFF || m->m_len < s) &&
	    (m = m_pullup(m, s))==0) {
		nsipif.if_ierrors++;
		return;
	}
	ip = mtod(m, struct ip *);
	if (ip->ip_hl > (sizeof (struct ip) >> 2))
		ip_stripoptions(ip, (struct mbuf *)0);

	/*
	 * Make mbuf data length reflect IDP length.
	 * If not enough data to reflect IDP length, drop.
	 */
	m->m_off += sizeof (struct ip);
	m->m_len -= sizeof (struct ip);
	idp = mtod(m, struct idp *);
	len = ntohs(idp->idp_len);
	if (ip->ip_len != len) {
		if (len > ip->ip_len) {
			nsipif.if_ierrors++;
			if(nsip_badlen) m_freem(nsip_badlen);
			nsip_badlen = m;
			return;
		}
		m_adj(m, len - ip->ip_len);
		/* ip->ip_len = len; */
	}
	/*
	 * Deliver to NS
	 */
	s = splimp();
	if (IF_QFULL(ifq)) {
		IF_DROP(ifq);
		m_freem(m0);
		splx(s);
		return (ENOBUFS);
	}
	IF_ENQUEUE(ifq, m0);
	schednetisr(NETISR_NS);
	splx(s);
	return (0);
bad:
	m_freem(m);
	return (0);
}

nsipoutput(ifn, m0, dst)
	struct ifnet_en *ifn;
	struct mbuf *m0;
	struct sockaddr *dst;
{

	register struct mbuf *m = dtom(ifn);
	register struct ip *ip;
	register struct route *ro = &(ifn->ifen_route);
	register int len = 0;
	struct in_addr in_src, in_dst;
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
	m = m0;
	if(m->m_off < MMINOFF + sizeof (struct ip)) {
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
	ip->ip_p = IPPROTO_PUP;
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
	return(ENETUNREACH);
}

struct ifreq ifr = {"nsip0"};

nsip_route(m)
	register struct mbuf *m;
{
	register struct nsip_req *rq = mtod(m, struct nsip_req *);
	struct sockaddr_ns *ns_dst = (struct sockaddr_ns *)&rq->rq_ns;
	struct sockaddr_in *ip_dst = (struct sockaddr_in *)&rq->rq_ip;
	int flags = rq->rq_flags;
	struct ifnet *ifp;
	struct route ro;
	struct ifnet_en *ifn;
	int error;
	struct sockaddr_in *dst;
	struct sockaddr_in *src;
	/*
	 * First, determine if we can get to the destination
	 */
	bzero((caddr_t)&ro, sizeof (ro));
	ro.ro_dst = *(struct sockaddr *)ip_dst;
	dst = (struct sockaddr_in *)& ro.ro_dst;
	rtalloc(&ro);
	if (ro.ro_rt == 0 || (ifp = ro.ro_rt->rt_ifp) == 0) {
		return (ENETUNREACH);
	}
	/*
	 * And see how he's going to get back to us:
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
			return (EADDRNOTAVAIL);
		}
		src = (struct sockaddr_in *)&ia->ia_addr;
	}
	/*
	 * Is there space?
	 */
	m = nsipattach();
	if (m==NULL) {return (ENOBUFS);}
	ifn = mtod(m, struct ifnet_en *);

	ro.ro_rt->rt_use++;
	ifn->ifen_route = ro;
	ifn->ifen_dst =  ip_dst->sin_addr;
	ifn->ifen_src = src->sin_addr;

	/*
	 * now configure this as a point to point link
	 */
	ifr.ifr_name[4] = '0' + nsipif.if_unit - 1;
	ifr.ifr_dstaddr = * (struct sockaddr *) ns_dst;
	return(ns_control((struct socket *)0, SIOCSIFADDR, (caddr_t)&ifr,
			(struct ifnet *)ifn));
}
#endif
