/* ip_icmp.c 4.1 81/11/07 */

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../inet/inet.h"
#include "../inet/inet_systm.h"
#include "../inet/ip.h"

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
	int oiplen = oip->ip_hl << 2;
	struct icmp *icp = (struct icp *)((int)oip + oiplen);
	struct mbuf *m;
	struct ip *nip;

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
	bcopy((caddr_t)oip, (caddr_t)&icp->icmp_ip.icmp_ip, oiplen + 8);

	/*
	 * Now prepend an IP header and reflect this packet back to
	 * the source.
	 */
	m->m_off -= sizeof (struct ip);
	m->m_len += sizeof (struct ip);
	nip = (struct ip *)mtod(m);
	*nip = *ip;
	icmp_reflect(ip);

	/*
	 * Discard mbufs of original datagram
	 */
free:
	m_freem(dtom(oip));
}

/*
 * Processor a received ICMP message.
 */
icmp_input(m)
	struct mbuf *m;
{
	int hlen = ip->ip_hl << 2;
	register struct ip *ip = (struct ip *)mtod(m);
	register struct icmp *icp;
	int icmplen = ip->ip_len - hlen;

	/*
	 * Locate icmp structure in mbuf, and check
	 * that not corrupted and of at least minimum length.
	 */
	m->m_len -= hlen;
	m->m_off += hlen;
	/* need routine to make sure header is in this mbuf here */
	icp = (struct icmp *)mtod(m);
	i = icp->icmp_cksum;
	icp->icmp_cksum = 0;
	if (i != cksum(m, icmplen) || icmplen < ICMP_MINLEN)
		goto bad;

	/*
	 * Message type specific processing.
	 */
	switch (ipc->icmp_type) {

	case ICMP_UNREACH:
	case ICMP_TIMXCEED:
	case ICMP_PARAMPROB:
	case ICMP_SOURCEQUENCH:
	case ICMP_REDIRECT:
		/*
		 * Problem with previous datagram; advise
		 * higher level routines.
		 */
		if (icmplen < ICMP_MINADVLEN || icmplen < ICMP_ADVLEN(ipc))
			goto drop;
		icmp_advise(ip, ipc);
		goto free;

	case ICMP_ECHO:
		icp->icmp_type = ICMP_ECHOREPLY;
		goto reflect;

	case ICMP_TSTAMP:
		if (icmplen < ICMP_MINLEN + sizeof ())
			goto bad;
		icp->icmp_type = ICMP_TSTAMPREPLY;
		millitime(&icp->icmp_rtime);
		icp->icmp_ttime = icp->icmp_rtime;	/* bogus, do later! */
		goto reflect;
		
	case ICMP_IREQ:
		/* fill in source address zero fields! */
		goto reflect;

	case ICMP_ECHOREPLY:
	case ICMP_TSTAMPREPLY:
	case ICMP_IREQREPLY:
		if (icmplen < ICMP_MINADVLEN || icmplen < ICMP_ADVLEN(ipc))
			goto drop;
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
	struct inet_addr t;

	/*
	 * This is a little naive... do we have to munge the options
	 * to reverse source routing?
	 */
	t = ip->ip_src.s_addr;
	ip->ip_dst.s_addr = ip->ip_src.d_addr;
	ip->ip_src.d_addr = t;
	icmp_send(ip);
}

/*
 * Send an icmp packet back to the ip level, after
 * supplying a checksum.
 */
icmp_send(ip, ipc)
	struct ip *ip;
	struct icmp *ipc;
{

	ipc->ipc_cksum = 0;
	ipc->ipc_cksum = cksum(m, impclen);
	/* what about ttl? */
	ip_output(ip);
}

/*
 * Advise a higher level protocol of a problem reported by
 * a gateway or another host.
 */
icmp_advise(ip, icp)
	struct ip *ip;
	struct icmp *icp;
{
	int (*f)(), tcp_advise(), udp_advise(), raw_advise();

	switch (ip->ip_p) {
	case IPPROTO_TCP:
		f = tcp_advise;
		break;

	case IPPROTO_UDP:
		f = udp_advise;
		break;

	default:
		f = raw_advise;
		break;
	}
	(*f)(ip, icp);
}

/*
 * Got a reply, e.g. to an echo message or a timestamp
 * message; nothing is done with these yet.
 */
/*ARGSUSED*/
icmp_gotreply(icp)
	struct icmp *icp;
{

}
