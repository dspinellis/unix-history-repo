/* ip_output.c 1.13 81/11/16 */

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/mtpr.h"
#include "../h/socket.h"
#include "../net/inet_cksum.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/inet_host.h"
#include "../net/ip.h"
#include "../net/ip_var.h"

ip_output(mp)
	struct mbuf *mp;
{
	register i, rnd;
	register struct mbuf *m, *n;
	register struct ip *p;
	struct mbuf *mm;
	int hlen, adj, max, len, off;

COUNT(IP_OUTPUT);
	p = mtod(mp, struct ip *);
	hlen = sizeof (struct ip);

	/*
	 * Fill in and byte swap ip header.
	 */
	p->ip_v = IPVERSION;
	p->ip_hl = hlen >> 2;
	p->ip_off = 0 | (p->ip_off & IP_DF);
	p->ip_ttl = MAXTTL;
	p->ip_id = ip_id++;

	if (p->ip_len <= MTU) {
		ip_send(p);
		return;
	}
	if (p->ip_off & IP_DF)
		return;
	max = MTU - hlen;
	len = p->ip_len - hlen;
	off = 0;
	m = mp;
	while (len > 0) {
		p->ip_off |= off >> 3;
		i = -hlen;
		while (m != NULL) {
			i += m->m_len;
			if (i > max)
				break;
			n = m;
			m = m->m_next;
		}
		if (i < max || m == NULL) {
			p->ip_off = p->ip_off &~ IP_MF;
			p->ip_len = i + hlen;
			ip_send(p);
			return;
		}
		if ((mm = m_get(1)) == NULL)    /* no more bufs */
			return;
		p->ip_off |= IP_MF;
		i -= m->m_len;
		rnd = i & ~7;
		adj = i - rnd;
		p->ip_len = rnd + hlen;
		n->m_next = NULL;
		mm->m_next = m;
		m = mm;
		m->m_off = MMAXOFF - hlen - adj;
		m->m_len = hlen + adj;
		bcopy((caddr_t)p, mtod(m, caddr_t), (unsigned)hlen);
		if (adj) {
			n->m_len -= adj;
			bcopy(mtod(n, caddr_t) + n->m_len,
			    mtod(m, caddr_t) + hlen, (unsigned) adj);
		}
		ip_send(p);
		p = (struct ip *)((int)m + m->m_off);
		len -= rnd;
		off += rnd;
	}
	return;
}

ip_send(ip)
	register struct ip *ip;		/* known to be r11 */
{
	register struct mbuf *m;
	register struct imp *l;
	register int hlen = ip->ip_hl << 2;
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
	CKSUM_IPSET(m, ip, r11, hlen);
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
