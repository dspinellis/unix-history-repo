/*	ip_output.c	1.21	81/12/09	*/

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

ip_output(m, opt)
	struct mbuf *m;
	struct mbuf *opt;
{
	register struct ip *ip = mtod(m, struct ip *);
	register struct ifnet *ifp;
	int len, hlen = sizeof (struct ip), off;

COUNT(IP_OUTPUT);
	if (opt)				/* XXX */
		m_free(opt);			/* XXX */
	/*
	 * Fill in IP header.
	 */
	ip->ip_v = IPVERSION;
	ip->ip_hl = hlen >> 2;
	ip->ip_off &= IP_DF;
	ip->ip_id = htons(ip_id++);

	/*
	 * Find interface for this packet.
	 */
	ifp = if_ifonnetof(ip->ip_dst);
	if (ifp == 0) {
		ifp = if_gatewayfor(ip->ip_dst);
		if (ifp == 0)
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
		return ((*ifp->if_output)(ifp, m, PF_INET));
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
	for (off = 0; off < ip->ip_len; off += len) {
		struct mbuf *mh = m_get(0);
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
		mhip->ip_off = off;
		if (off + len >= ip->ip_len)
			mhip->ip_len = ip->ip_len - off;
		else {
			mhip->ip_len = len;
			mhip->ip_off |= IP_MF;
		}
		mhip->ip_len = htons((u_short)(mhip->ip_len + sizeof (struct ip)));
		mh->m_next = m_copy(m, off, len);
		if (mh->m_next == 0) {
			(void) m_free(mh);
			goto bad;
		}
		ip->ip_off = htons((u_short)ip->ip_off);
		ip->ip_sum = 0;
		ip->ip_sum = in_cksum(m, hlen);
		if ((*ifp->if_output)(ifp, mh, PF_INET) == 0)
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
