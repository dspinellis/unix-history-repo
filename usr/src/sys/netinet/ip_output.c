/* ip_output.c 1.5 81/10/29 */

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

	if (p->ip_len > MTU) {          /* must fragment */
		if (p->ip_off & IP_DF)
			return (0);
		max = MTU - hlen;       /* maximum data length in fragment */
		len = p->ip_len - hlen; /* data length */
		off = 0;                /* fragment offset */
		m = mp;

		while (len > 0) {

			/* correct the header */

			p->ip_off |= off >> 3;

			/* find the end of the fragment */

			i = -hlen;
			while (m != NULL) {
				i += m->m_len;
				if (i > max)
					break;
				n = m;
				m = m->m_next;
			}

			if (i < max || m == NULL) {     /* last fragment */
				p->ip_off = p->ip_off &~ IP_MF;
				p->ip_len = i + hlen;
				break;

			} else {                        /* more fragments */

				/* allocate header mbuf for next fragment */

				if ((mm = m_get(1)) == NULL)    /* no more bufs */
					return(0);

				p->ip_off |= IP_MF;

				/* terminate fragment at 8 byte boundary (round down) */

				i -= m->m_len;
        			rnd = i & ~7;           /* fragment length */
				adj = i - rnd;          /* leftover in mbuf */
				p->ip_len = rnd + hlen;

				/* setup header for next fragment and
				   append remaining fragment data */

				n->m_next = NULL;
				mm->m_next = m;
				m = mm;
				m->m_off = MMAXOFF - hlen - adj;
				m->m_len = hlen + adj;

				/* copy old header to new */

				bcopy(p, (caddr_t)((int)m + m->m_off), hlen);

				/* copy leftover data from previous frag */

				if (adj) {
					n->m_len -= adj;
					bcopy((caddr_t)((int)n + n->m_len + n->m_off),
					      (caddr_t)((int)m + m->m_off + hlen), adj);
				}
			}

			ip_send(p);             /* pass frag to local net level */

			p = (struct ip *)((int)m + m->m_off);   /* -> new hdr */
			len -= rnd;
			off += rnd;
		}
	}

	return(ip_send(p));     /* pass datagram to local net level */
}

ip_send(p)      /* format header and send message to 1822 level */
struct ip *p;
{
	register struct mbuf *m;
	register struct imp *l;
	int s;
COUNT(IP_SEND);

	m = dtom(p);                    /* ->header mbuf */

	/* set up 1822 leader fields for transmit */

	l = (struct imp *)((int)m + m->m_off - L1822);
/*
	l->i_hst = p->ip_dst.s_host;
	l->i_impno = p->ip_dst.s_imp;
	l->i_mlen = p->ip_len + L1822;
	l->i_link = IPLINK;
	l->i_type = 0;
	l->i_htype = 0;
	l->i_stype = 0;
*/
	if ((l->i_shost = p->ip_src.s_host) == 0)
		l->i_shost = 253;
	if ((l->i_dhost = p->ip_dst.s_host) == 0)
		l->i_dhost = 253;
	l->i_type = IPTYPE;

	/* finish ip leader by calculating checksum and doing
	   necessary byte-swapping  */

	p->ip_sum = 0;
	p->ip_len = htons(p->ip_len);
	p->ip_id = htons(p->ip_id);
	p->ip_off = htons(p->ip_off);
	p->ip_sum = cksum(m, sizeof(struct ip));

	m->m_off -= L1822;              /* -> 1822 leader */
	m->m_len += L1822;

	m->m_act = NULL;

#ifndef IMPLOOP

	/* put output message on queue */

	s = splimp();
	if (imp_stat.outq_head != NULL)
		imp_stat.outq_tail->m_act = m;
	else
		imp_stat.outq_head = m;
	imp_stat.outq_tail = m;
	splx(s);

	/* if no outstanding output, start some */

	if (!imp_stat.outactive)
		imp_output(0);

#else
	/* software looping: put msg chain on input queue */

	if (imp_stat.inq_head != NULL)
		imp_stat.inq_tail->m_act = m;
	else
		imp_stat.inq_head = m;
	imp_stat.inq_tail = m;

#endif IMPLOOP
	return (1);
}

ip_setup(up, m, len)            /* setup an ip header for raw write */
register struct ucb *up;
register struct mbuf *m;
int len;
{
	register struct ip *ip;
COUNT(IP_SETUP);

	m->m_off = MMAXOFF - sizeof(struct ip);
	m->m_len = sizeof(struct ip);

	ip = (struct ip *)((int)m + m->m_off);

	ip->ip_tos = 0;
	ip->ip_id = 0;
	ip->ip_off = 0;
	ip->ip_p = up->uc_lolink;
	ip->ip_len = len + sizeof(struct ip);

	ip->ip_src.s_addr = n_lhost.s_addr;
        ip->ip_dst.s_addr = up->uc_host->h_addr.s_addr;
}
