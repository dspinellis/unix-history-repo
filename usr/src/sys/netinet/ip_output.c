/*	ip_output.c	1.33	82/06/12	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/mtpr.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/route.h"
#include <errno.h>

int	ipnorouteprint = 0;

ip_output(m, opt, ro, allowbroadcast)
	struct mbuf *m;
	struct mbuf *opt;
	struct route *ro;
	int allowbroadcast;
{
	register struct ip *ip = mtod(m, struct ip *);
	register struct ifnet *ifp;
	int len, hlen = sizeof (struct ip), off, error = 0;
	struct route iproute;
	struct sockaddr *dst;

COUNT(IP_OUTPUT);
	if (opt)				/* XXX */
		(void) m_free(opt);		/* XXX */
	/*
	 * Fill in IP header.
	 */
	ip->ip_v = IPVERSION;
	ip->ip_hl = hlen >> 2;
	ip->ip_off &= IP_DF;
	ip->ip_id = htons(ip_id++);

	/*
	 * Route packet.
	 */
	if (ro == 0) {
		ro = &iproute;
		bzero((caddr_t)ro, sizeof (*ro));
	}
	dst = &ro->ro_dst;
	if (ro->ro_rt == 0) {
		ro->ro_dst.sa_family = AF_INET;
		((struct sockaddr_in *)&ro->ro_dst)->sin_addr = ip->ip_dst;
		/*
		 * If routing to interface only, short circuit routing lookup.
		 */
		if (ro == &routetoif) {
			/* check ifp is AF_INET??? */
			ifp = if_ifonnetof(ip->ip_dst.s_net);
			if (ifp == 0)
				goto unreachable;
			goto gotif;
		}
		rtalloc(ro);
	}
	if (ro->ro_rt == 0 || (ifp = ro->ro_rt->rt_ifp) == 0)
		goto unreachable;
	ro->ro_rt->rt_use++;
	if (ro->ro_rt->rt_flags & RTF_GATEWAY)
		dst = &ro->ro_rt->rt_gateway;
gotif:
	/*
	 * If source address not specified yet, use address
	 * of outgoing interface.
	 */
	if (ip->ip_src.s_addr == 0)
		ip->ip_src.s_addr =
		    ((struct sockaddr_in *)&ifp->if_addr)->sin_addr.s_addr;

	/*
	 * Have interface for packet.  Allow
	 * broadcasts only by authorized users.
	 */
	if (!allowbroadcast && (ifp->if_flags & IFF_BROADCAST)) {
		struct sockaddr_in *sin;

		sin = (struct sockaddr_in *)&ifp->if_broadaddr;
		if (sin->sin_addr.s_addr == ip->ip_dst.s_addr) {
			error = EACCES;
			goto bad;
		}
	}

	/*
	 * If small enough for interface, can just send directly.
	 */
	if (ip->ip_len <= ifp->if_mtu) {
#if vax
		ip->ip_len = htons((u_short)ip->ip_len);
		ip->ip_off = htons((u_short)ip->ip_off);
#endif
		ip->ip_sum = 0;
		ip->ip_sum = in_cksum(m, hlen);
		error = (*ifp->if_output)(ifp, m, dst);
		goto done;
	}

	/*
	 * Too large for interface; fragment if possible.
	 * Must be able to put at least 8 bytes per fragment.
	 */
	if (ip->ip_off & IP_DF) {
		error = EMSGSIZE;
		goto bad;
	}
	len = (ifp->if_mtu - hlen) &~ 7;
	if (len < 8) {
		error = EMSGSIZE;
		goto bad;
	}

	/*
	 * Discard IP header from logical mbuf for m_copy's sake.
	 * Loop through length of segment, make a copy of each
	 * part and output.
	 */
	m->m_len -= sizeof (struct ip);
	m->m_off += sizeof (struct ip);
	for (off = 0; off < ip->ip_len-hlen; off += len) {
		struct mbuf *mh = m_get(M_DONTWAIT);
		struct ip *mhip;

		if (mh == 0) {
			error = ENOBUFS;
			goto bad;
		}
		mh->m_off = MMAXOFF - hlen;
		mhip = mtod(mh, struct ip *);
		*mhip = *ip;
		if (hlen > sizeof (struct ip)) {
			int olen = ip_optcopy(ip, mhip, off);
			mh->m_len = sizeof (struct ip) + olen;
		} else
			mh->m_len = sizeof (struct ip);
		mhip->ip_off = off >> 3;
		if (off + len >= ip->ip_len-hlen)
			len = mhip->ip_len = ip->ip_len - hlen - off;
		else {
			mhip->ip_len = len;
			mhip->ip_off |= IP_MF;
		}
		mhip->ip_len += sizeof (struct ip);
#if vax
		mhip->ip_len = htons((u_short)mhip->ip_len);
#endif
		mh->m_next = m_copy(m, off, len);
		if (mh->m_next == 0) {
			(void) m_free(mh);
			error = ENOBUFS;	/* ??? */
			goto bad;
		}
#if vax
		mhip->ip_off = htons((u_short)mhip->ip_off);
#endif
		mhip->ip_sum = 0;
		mhip->ip_sum = in_cksum(mh, hlen);
		if (error = (*ifp->if_output)(ifp, mh, dst))
			break;
	}
	m_freem(m);
	goto done;

unreachable:
	if (ipnorouteprint)
		printf("no route to %x (from %x, len %d)\n",
		    ip->ip_dst.s_addr, ip->ip_src.s_addr, ip->ip_len);
	error = ENETUNREACH;
bad:
	m_freem(m);
done:
	if (ro == &iproute && ro->ro_rt)
		RTFREE(ro->ro_rt);
	return (error);
}

/*
 * Copy options from ip to jp.
 * If off is 0 all options are copied
 * otherwise copy selectively.
 */
ip_optcopy(ip, jp, off)
	struct ip *ip, *jp;
	int off;
{
	register u_char *cp, *dp;
	int opt, optlen, cnt;

COUNT(IP_OPTCOPY);
	cp = (u_char *)(ip + 1);
	dp = (u_char *)(jp + 1);
	cnt = (ip->ip_hl << 2) - sizeof (struct ip);
	for (; cnt > 0; cnt -= optlen, cp += optlen) {
		opt = cp[0];
		if (opt == IPOPT_EOL)
			break;
		if (opt == IPOPT_NOP)
			optlen = 1;
		else
			optlen = cp[1];
		if (optlen > cnt)			/* XXX */
			optlen = cnt;			/* XXX */
		if (off == 0 || IPOPT_COPIED(opt)) {
			bcopy((caddr_t)cp, (caddr_t)dp, (unsigned)optlen);
			dp += optlen;
		}
	}
	for (optlen = dp - (u_char *)(jp+1); optlen & 0x3; optlen++)
		*dp++ = IPOPT_EOL;
	return (optlen);
}
