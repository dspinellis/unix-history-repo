/*	ip_output.c	1.15	81/11/20	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/mtpr.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/ip.h"
#include "../net/ip_var.h"

ip_output(m)
	struct mbuf *m;
{
	register struct ip *ip = mtod(m, struct ip *);
	int len, hlen = ip->ip_hl << 2, off;

COUNT(IP_OUTPUT);
	/*
	 * Fill in IP header.
	 */
	ip->ip_v = IPVERSION;
	ip->ip_hl = hlen >> 2;
	ip->ip_off &= IP_DF;
	ip->ip_ttl = MAXTTL;
	ip->ip_id = ip_id++;

	/*
	 * If small enough for interface, can just send directly.
	 */
	if (ip->ip_len <= MTU) {
		ip_send(ip);
		return;
	}

	/*
	 * Too large for interface; fragment if possible.
	 * Must be able to put at least 8 bytes per fragment.
	 */
	if (ip->ip_off & IP_DF)
		goto bad;
	len = (MTU-hlen) &~ 7;
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
		mh->m_next = m_copy(m, off, len);
		if (mh->m_next == 0) {
			m_free(mh);
			goto bad;
		}
		ip_send(mhip);
	}
bad:
	m_freem(m);
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

/* REST OF CODE HERE IS GARBAGE */

ip_send(ip)
	register struct ip *ip;
{
	register struct mbuf *m;
	register struct imp *l;
	int hlen = ip->ip_hl << 2;
	int s;
COUNT(IP_SEND);

	m = dtom(ip);
	l = (struct imp *)(mtod(m, caddr_t) - L1822);
	l->i_shost = ip->ip_src.s_host;
	l->i_dhost = ip->ip_dst.s_host;
	l->i_type = IPTYPE;
	ip->ip_sum = 0;
	ip->ip_len = htons((u_short)ip->ip_len);
	ip->ip_id = htons(ip->ip_id);
	ip->ip_off = htons((u_short)ip->ip_off);
	ip->ip_sum = inet_cksum(m, hlen);
	m->m_off -= L1822;
	m->m_len += L1822;
	m->m_act = NULL;
#ifndef IMPLOOP
	s = splimp();
	if (imp_stat.outq_head != NULL)
		imp_stat.outq_tail->m_act = m;
	else
		imp_stat.outq_head = m;
	imp_stat.outq_tail = m;
	splx(s);
	if (!imp_stat.outactive)
		enstart(0);
#else
	if (imp_stat.inq_head != NULL)
		imp_stat.inq_tail->m_act = m;
	else
		imp_stat.inq_head = m;
	imp_stat.inq_tail = m;
	setsoftnet();
#endif IMPLOOP
}

/* END GARBAGE */
