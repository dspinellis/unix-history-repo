/*	ip_output.c	1.30	82/03/30	*/

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

ip_output(m, opt, ro, allowbroadcast)
	struct mbuf *m;
	struct mbuf *opt;
	struct route *ro;
	int allowbroadcast;
{
	register struct ip *ip = mtod(m, struct ip *);
	register struct ifnet *ifp;
	int len, hlen = sizeof (struct ip), off;
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
	 * Find interface for this packet in the routing
	 * table.  Note each interface has placed itself
	 * in there at boot time, so calls to rtalloc
	 * degenerate to if_ifonnetof(ip->ip_dst.s_net).
	 */
	if (ro == 0) {
		ro = &iproute;
		bzero((caddr_t)ro, sizeof (*ro));
	}
	if (ro->ro_rt == 0) {
		ro->ro_dst.sa_family = AF_INET;
		((struct sockaddr_in *)&ro->ro_dst)->sin_addr = ip->ip_dst;
		rtalloc(ro);
		if (ro != &iproute)
			ro->ro_rt->rt_refcnt++;
	}
	if (ro->ro_rt == 0 || (ifp = ro->ro_rt->rt_ifp) == 0) {
printf("no route to %x\n", ip->ip_dst.s_addr);
		goto bad;
}
	dst = ro->ro_rt->rt_flags&RTF_DIRECT ?
	    (struct sockaddr *)&ro->ro_dst : &ro->ro_rt->rt_gateway;
	if (!allowbroadcast && (ifp->if_flags & IFF_BROADCAST)) {
		struct sockaddr_in *sin;

		sin = (struct sockaddr_in *)&ifp->if_broadaddr;
		if (sin->sin_addr.s_addr == ip->ip_dst.s_addr)
			goto bad;
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
		ro->ro_rt->rt_use++;
		return ((*ifp->if_output)(ifp, m, dst));
	}

	/*
	 * Too large for interface; fragment if possible.
	 * Must be able to put at least 8 bytes per fragment.
	 */
	if (ip->ip_off & IP_DF)
		goto bad;
	len = (ifp->if_mtu - hlen) &~ 7;
	if (len < 8)
		goto bad;

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

		if (mh == 0)
			goto bad;
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
			goto bad;
		}
#if vax
		mhip->ip_off = htons((u_short)mhip->ip_off);
#endif
		mhip->ip_sum = 0;
		mhip->ip_sum = in_cksum(mh, hlen);
		ro->ro_rt->rt_use++;
		if ((*ifp->if_output)(ifp, mh, dst) == 0)
			goto bad;
	}
	m_freem(m);
	return (1);
bad:
	m_freem(m);
	return (0);
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
