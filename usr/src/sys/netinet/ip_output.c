/* ip_output.c 1.7 81/10/31 */

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../inet/inet.h"
#include "../inet/inet_systm.h"
#include "../inet/imp.h"
#include "../inet/inet_host.h"
#include "../inet/ip.h"
#include "../inet/tcp.h"

ip_output(mp)
	struct mbuf *mp;
{
	register i, rnd;
	register struct mbuf *m, *n;
	register struct ip *p;
	struct mbuf *mm;
	int hlen, adj, max, len, off;

COUNT(IP_OUTPUT);
	p = (struct ip *)((int)mp + mp->m_off); /* -> ip header */
	hlen = sizeof (struct ip);               /* header length */

	/*
	 * Fill in and byte swap ip header.
	 */
	p->ip_v = IPVERSION;
	p->ip_hl = hlen >> 2;
	p->ip_off = 0 | (p->ip_off & IP_DF);
	p->ip_ttl = MAXTTL;
	p->ip_id = ip_id++;

	if (p->ip_len <= MTU)
		return (ip_send(p));
	if (p->ip_off & IP_DF)
		return (0);
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
			return (ip_send(p));
		}
		if ((mm = m_get(1)) == NULL)    /* no more bufs */
			return(0);
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
		bcopy(p, (caddr_t)((int)m + m->m_off), hlen);
		if (adj) {
			n->m_len -= adj;
			bcopy((caddr_t)((int)n + n->m_len + n->m_off),
			      (caddr_t)((int)m + m->m_off + hlen), adj);
		}
		ip_send(p);
		p = (struct ip *)((int)m + m->m_off);
		len -= rnd;
		off += rnd;
	}
}

ip_send(p)
	struct ip *p;
{
	register struct mbuf *m;
	register struct imp *l;
	int s;
COUNT(IP_SEND);

	m = dtom(p);
	l = (struct imp *)((int)m + m->m_off - L1822);
	if ((l->i_shost = p->ip_src.s_host) == 0)
		l->i_shost = 253;
	if ((l->i_dhost = p->ip_dst.s_host) == 0)
		l->i_dhost = 253;
	l->i_type = IPTYPE;
	p->ip_sum = 0;
	p->ip_len = htons(p->ip_len);
	p->ip_id = htons(p->ip_id);
	p->ip_off = htons(p->ip_off);
	p->ip_sum = cksum(m, sizeof(struct ip));
	m->m_off -= L1822;              /* -> 1822 leader */
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
#endif IMPLOOP
	return (1);
}
