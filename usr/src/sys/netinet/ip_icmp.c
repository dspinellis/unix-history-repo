/*	ip_icmp.c	4.10	81/12/03	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/clock.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/ip.h"
#include "../net/ip_icmp.h"

/*
 * ICMP routines: error generation, receive packet processing, and
 * routines to turnaround packets back to the originator, and
 * host table maintenance routines.
 */

/*
 * Generate an error packet of type error in response to bad packet ip.
 */
icmp_error(oip, type, code)
	struct ip *oip;
	int type;
{
	unsigned oiplen = oip->ip_hl << 2;
	struct icmp *icp = (struct icmp *)((int)oip + oiplen);
	struct mbuf *m;
	struct ip *nip;
COUNT(ICMP_ERROR);

	/*
	 * Make sure that the old IP packet had 8 bytes of data to return;
	 * if not, don't bother.  Also don't EVER error if the old
	 * packet protocol was ICMP.
	 */
	if (oip->ip_len - oiplen < 8 || oip->ip_p == IPPROTO_ICMP)
		goto free;

	/*
	 * Get a new mbuf, and fill in a ICMP header at the bottom
	 * of the mbuf, followed by the old IP header and 8 bytes
	 * of its data.
	 */
	m = m_get(0);
	if (m == 0)
		goto free;
	m->m_off = MMAXOFF - (oiplen + 8);
	icp->icmp_type = type;
	if (type == ICMP_PARAMPROB) {
		icp->icmp_code = 0;
		icp->icmp_pptr = code;
	} else
		icp->icmp_code = code;
	bcopy((caddr_t)oip, (caddr_t)&icp->icmp_ip, oiplen + 8);

	/*
	 * Now prepend an IP header and reflect this packet back to
	 * the source.
	 */
	m->m_off -= sizeof (struct ip);
	m->m_len += sizeof (struct ip);
	nip = (struct ip *)mtod(m, struct ip *);
	*nip = *oip;
	icmp_reflect(nip);
	return;

	/*
	 * Discard mbufs of original datagram
	 */
free:
	m_freem(dtom(oip));
}

/*
 * Process a received ICMP message.
 */
icmp_input(m)
	struct mbuf *m;
{
	register struct icmp *icp;
	register struct ip *ip = mtod(m, struct ip *);
	int hlen = ip->ip_hl << 2;
	int icmplen = ip->ip_len - hlen;
	int i;
	extern u_char ip_protox[];
COUNT(ICMP_INPUT);

	/*
	 * Locate icmp structure in mbuf, and check
	 * that not corrupted and of at least minimum length.
	 */
	m->m_len -= hlen;
	m->m_off += hlen;
	/* need routine to make sure header is in this mbuf here */
	icp = (struct icmp *)mtod(m, struct icmp *);
	i = icp->icmp_cksum;
	icp->icmp_cksum = 0;
	if (i != in_cksum(m, icmplen) || icmplen < ICMP_MINLEN)
		goto free;

	/*
	 * Message type specific processing.
	 */
	switch (icp->icmp_type) {

	case ICMP_UNREACH:
	case ICMP_TIMXCEED:
	case ICMP_PARAMPROB:
	case ICMP_SOURCEQUENCH:
	case ICMP_REDIRECT:
		/*
		 * Problem with previous datagram; advise
		 * higher level routines.
		 */
		if (icmplen < ICMP_ADVLENMIN || icmplen < ICMP_ADVLEN(icp))
			goto free;
		(*protosw[ip_protox[ip->ip_p]].pr_ctlinput)(m);
		goto free;

	case ICMP_ECHO:
		icp->icmp_type = ICMP_ECHOREPLY;
		goto reflect;

	case ICMP_TSTAMP:
		if (icmplen < ICMP_TSLEN)
			goto free;
		icp->icmp_type = ICMP_TSTAMPREPLY;
		icp->icmp_rtime = iptime();
		icp->icmp_ttime = icp->icmp_rtime;	/* bogus, do later! */
		goto reflect;
		
	case ICMP_IREQ:
		/* fill in source address zero fields! */
		goto reflect;

	case ICMP_ECHOREPLY:
	case ICMP_TSTAMPREPLY:
	case ICMP_IREQREPLY:
		if (icmplen < ICMP_ADVLENMIN || icmplen < ICMP_ADVLEN(icp))
			goto free;
		icmp_gotreply(icp);
		goto free;

	default:
		goto free;
	}
reflect:
	icmp_reflect(ip);
free:
	m_freem(dtom(ip));
}

/*
 * Reflect the ip packet back to the source
 */
icmp_reflect(ip)
	struct ip *ip;
{
	struct in_addr t;
COUNT(ICMP_REFLECT);

	t = ip->ip_src; ip->ip_dst = ip->ip_src; ip->ip_src = t;
	/*
	 * This is a little naive... do we have to munge the options
	 * to reverse source routing?
	 */
	icmp_send(ip);
}

/*
 * Send an icmp packet back to the ip level, after
 * supplying a checksum.
 */
icmp_send(ip)
	struct ip *ip;
{
	struct icmp *icp = 0;					/* XXX */
COUNT(ICMP_SEND);

	icp->icmp_cksum = 0;
	icp->icmp_cksum = in_cksum(dtom(ip), 0);		/* XXX */
	ip->ip_ttl = MAXTTL;
	(void) ip_output(dtom(ip), (struct mbuf *)0);
}

icmp_ctlinput(m)
	struct mbuf *m;
{
COUNT(ICMP_CTLINPUT);

	m_freem(m);
}

/*
 * Got a reply, e.g. to an echo message or a timestamp
 * message; nothing is done with these yet.
 */
/*ARGSUSED*/
icmp_gotreply(icp)
	struct icmp *icp;
{

COUNT(ICMP_GOTREPLY);
}

icmp_drain()
{

COUNT(ICMP_DRAIN);
}

n_time
iptime()
{
	int s = spl6();
	u_long t;

COUNT(IPTIME);
	t = (time % SECDAY) * 1000 + lbolt * hz;
	splx(s);
	return (htonl(t));
}
