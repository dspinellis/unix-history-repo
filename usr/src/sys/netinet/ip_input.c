/* ip_input.c 1.4 81/10/18 */
#include "../h/param.h"
#include "../bbnnet/net.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/ucb.h"
#include "../h/systm.h"

int nosum = 0;

ip_input(mp)
struct mbuf *mp;
{
	register struct ip *ip, *q;
	register struct ipq *fp;
	register struct mbuf *m;
	register i;
	struct mbuf *n;
	int hlen;
	struct ip *p, *savq;
	struct ipq *ip_findf();

COUNT(IP_INPUT);
	ip = (struct ip *)((int)mp + mp->m_off);        /* ->ip hdr */

	/* make sure header does not overflow mbuf */

	if ((hlen = ip->ip_hl << 2) > mp->m_len) {
		printf("ip header overflow\n");
		m_freem(mp);
		return;
	}

	i = (unsigned short)ip->ip_sum;
	ip->ip_sum = 0;

	if (i != (unsigned short)cksum(mp, hlen)) {     /* verify checksum */
		netstat.ip_badsum++;
		if (!nosum) {
        	  	m_freem(mp);
        		return;
		}
	}
	ip_bswap(ip);
	fp = netcb.n_ip_head ? ip_findf(ip) : 0;

	/*
	 * adjust message length to remove any padding
	 */
	for (i=0, m=mp; m != NULL; m = m->m_next) {
		i += m->m_len;
		n = m;
	}
	i -= ip->ip_len;

	if (i != 0)
        	if (i > (int)n->m_len)
        		m_adj(mp, -i);
        	else
        		n->m_len -= i;

	ip->ip_len -= hlen;     /* length of data */
  	ip->ip_mff = ((ip->ip_off & ip_mf) ? TRUE : FALSE);
	ip->ip_off <<= 3;
	if (ip->ip_mff || ip->ip_off)
		goto fragged;
	if (fp != NULL) {
		q = fp->iqx.ip_next;
		while (q != (struct ip *)fp) {
			m_freem(dtom(q));
			q = q->ip_next;
		}
		ip_freef(fp);           /* free header */
	}
	if (hlen > sizeof (struct ip))
		ip_opt(ip, hlen);
	switch (ip->ip_p) {

	case TCPROTO:
		tcp_input(mp);
		break;

	default:
		raw_input(mp, ip->ip_p, UIP);
		break;
	}
	return;

fragged:
	/* -> msg buf beyond ip hdr if not first fragment */

	if (ip->ip_off != 0) {
		mp->m_off += hlen;
		mp->m_len -= hlen;
	}

	if (fp == NULL) {               /* first fragment of datagram in */

	/* set up reass.q header: enq it, set up as head of frag
	   chain, set a timer value, and move in ip header */

		if ((m = m_get(1)) == NULL) {   /* allocate an mbuf */
			m_freem(mp);
			return;
		}

		fp = (struct ipq *)((int)m + MHEAD);
		fp->iqx.ip_next = fp->iqx.ip_prev = (struct ip *)fp;
		bcopy(ip, &fp->iqh, min(MLEN-28, hlen));
		fp->iqh.ip_ttl = MAXTTL;
		fp->iq_next = NULL;
		fp->iq_prev = netcb.n_ip_tail;
		if (netcb.n_ip_head != NULL)
			netcb.n_ip_tail->iq_next = fp;
		else
			netcb.n_ip_head = fp;
		netcb.n_ip_tail = fp;
	}

	/***********************************************************
	*                                                          *
	*              merge fragment into reass.q                 *
	*    algorithm:   match  start  and  end  bytes  of new    *
	*    fragment  with  fragments  on  the  queue.   if   no  *
	*    overlaps  are  found,  add  new  frag. to the queue.  *
	*    otherwise, adjust start and end of new frag.  so  no  *
	*    overlap   and   add  remainder  to  queue.   if  any  *
	*    fragments are completely covered by the new one,  or  *
	*    if  the  new  one is completely duplicated, free the  *
	*    fragments.                                            *
	*                                                          *
	***********************************************************/

	q = fp->iqx.ip_next;    /* -> top of reass. chain */
	ip->ip_end = ip->ip_off + ip->ip_len - 1;

	/* skip frags which new doesn't overlap at end */

	while ((q != (struct ip *)fp) && (ip->ip_off > q->ip_end))
		q = q->ip_next;

	if (q == (struct ip *)fp)               /* frag at end of chain */
		ip_enq(ip, fp->iqx.ip_prev);
	
	else {
		if (ip->ip_end < q->ip_off)     /* frag doesn't overlap any on chain */
			ip_enq(ip, q->ip_prev);

		/* new overlaps beginning of next frag only */

		else if (ip->ip_end < q->ip_end) {
			if ((i = ip->ip_end - q->ip_off + 1) < ip->ip_len) {
				ip->ip_len -= i;
				ip->ip_end -= i;
				m_adj(mp, -i);
				ip_enq(ip, q->ip_prev);
			} else
				m_freem(mp);

		/* new overlaps end of previous frag */

		} else {

			savq = q;
			if (ip->ip_off <= q->ip_off) {  /* complete cover */
				savq = q->ip_prev;
				ip_deq(q);
				m_freem(dtom(q));
			
			} else {                        /* overlap */
				if ((i = q->ip_end - ip->ip_off + 1) < ip->ip_len) {
					ip->ip_off += i;
					ip->ip_len -= i;
					m_adj(mp, i);
				} else
					ip->ip_len = 0;
			}

		/* new overlaps at beginning of successor frags */

			q = savq->ip_next;
			while ((q != (struct ip *)fp) && (ip->ip_len != 0) &&
				(q->ip_off < ip->ip_end))

				/* complete cover */

				if (q->ip_end <= ip->ip_end) {
					p = q->ip_next;
					ip_deq(q);
					m_freem(dtom(q));
					q = p;

				} else {        /* overlap */

					if ((i = ip->ip_end - q->ip_off + 1) < ip->ip_len) {
						ip->ip_len -= i;
						ip->ip_end -= i;
						m_adj(mp, -i);
					} else
						ip->ip_len = 0;
					break;
				}

		/* enqueue whatever is left of new before successors */

			if (ip->ip_len != 0)
				ip_enq(ip, savq);
			else
				m_freem(mp);
		}
	}

	/* check for completed fragment reassembly */

	if ((i = ip_done(fp)) == 0)
		return;

	p = fp->iqx.ip_next;            /* -> top mbuf */
	m = dtom(p);
	p->ip_len = i;                  /* total data length */
	ip_opt(p, p->ip_hl << 2);       /* option processing */
	ip_mergef(fp);                  /* cleanup frag chain */

	/* copy src/dst internet address to header mbuf */

	bcopy(&fp->iqh.ip_src, &p->ip_src, 2*sizeof(struct socket));
	ip_freef(fp);                   /* dequeue header */
	i = p->ip_p;
	if (i == TCPROTO)
		tcp_input(m);
	else
		raw_input(m, i, UIP);
}

ip_done(p)
	register struct ip *p;
{
	register struct ip *q;
	register next;

COUNT(IP_DONE);
	q = p->ip_next;

	if (q->ip_off != 0)
		return(0);
	do {
		next = q->ip_end + 1;
		q = q->ip_next;
	} while ((q != p) && (q->ip_off == next));

	if ((q == p) && !(q->ip_prev->ip_mff))        /* all fragments in */
		return(next);                         /* total data length */
	else
		return(0);
}

ip_mergef(p)    /* merge mbufs of fragments of completed datagram */
register struct ip *p;
{
	register struct mbuf *m, *n;
	register struct ip *q;
	int dummy;

COUNT(IP_MERGEF);
	q = p->ip_next;                         /* -> bottom of reass chain */
	n = (struct mbuf *)&dummy;              /* dummy for init assignment */

	while (q != p) {        /* through chain */

	        n->m_next = m = dtom(q);
		while (m != NULL)
			if (m->m_len != 0) {
				n = m;
				m = m->m_next;
			} else                  /* free null mbufs */
				n->m_next = m = m_free(m);
		q = q->ip_next;
	}
}


ip_freef(fp)	        /* deq and free reass.q header */
register struct ipq *fp;
{
COUNT(IP_FREEF);
	if (fp->iq_prev != NULL)
		(fp->iq_prev)->iq_next = fp->iq_next;
	else
		netcb.n_ip_head = fp->iq_next;
	if (fp->iq_next != NULL)
		(fp->iq_next)->iq_prev = fp->iq_prev;
	else
		netcb.n_ip_tail = fp->iq_prev;
	m_free(dtom(fp));
}

struct ipq *
ip_findf(p)         /* does fragment reass chain w/this hdr exist? */
register struct ip *p;
{
	register struct ipq *fp;

COUNT(IP_FINDF);
	for (fp = netcb.n_ip_head; (fp != NULL && (
			p->ip_src.s_addr != fp->iqh.ip_src.s_addr ||
			p->ip_dst.s_addr != fp->iqh.ip_dst.s_addr ||
			p->ip_id != fp->iqh.ip_id ||
			p->ip_p != fp->iqh.ip_p)); fp = fp->iq_next);
	return(fp);
}

ip_opt(ip, hlen)        /* process ip options */
struct ip *ip;
int hlen;
{
	register char *p, *q;
	register i, len;
	register struct mbuf *m;

COUNT(IP_OPT);
	p = q = (char *)((int)ip + sizeof(struct ip));  /* -> at options */
	
	if ((i = hlen - sizeof(struct ip)) > 0) {       /* any options */

/*      *** IP OPTION PROCESSING ***

		while (i > 0)

  			switch (*q++) {
			case 0:
			case 1:
				i--;
				break;

			default:
				i -= *q;
				q += *q;
			}
*/              q += i;
		m = dtom(q);
		len = (int)m + m->m_off + m->m_len - (int)q;
		bcopy((caddr_t)q, (caddr_t)p, len);    /* remove options */
		m->m_len -= i;
	}
}

ip_enq(p, prev)
register struct ip *p;
register struct ip *prev;
{
COUNT(IP_ENQ);
	p->ip_prev = prev;
	p->ip_next = prev->ip_next;
	prev->ip_next->ip_prev = p;
	prev->ip_next = p;
}

ip_deq(p)
register struct ip *p;
{
COUNT(IP_DEQ);
	p->ip_prev->ip_next = p->ip_next;
	p->ip_next->ip_prev = p->ip_prev;
}

ip_timeo()      /* frag reass.q timeout routine */
{
	register struct ip *q;
	register struct ipq *fp;
	int s = splnet();

COUNT(IP_TIMEO);
	timeout(ip_timeo, 0, hz);       /* reschedule every second */

	/* search through reass.q */

	for (fp = netcb.n_ip_head; fp != NULL; fp = fp->iq_next)

		if (--(fp->iqx.ip_ttl) == 0) {  /* time to die */

			q = fp->iqx.ip_next;    /* free mbufs assoc. w/chain */
			while (q != (struct ip *)fp) {
				m_freem(dtom(q));
				q = q->ip_next;
			}
			ip_freef(fp);           /* free header */
		}
	splx(s);
}
