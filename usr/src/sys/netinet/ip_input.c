/* ip_input.c 1.25 81/12/11 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/clock.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/ip.h"			/* belongs before in.h */
#include "../net/ip_var.h"
#include "../net/ip_icmp.h"
#include "../net/tcp.h"

u_char	ip_protox[IPPROTO_MAX];

/*
 * IP initialization: fill in IP protocol switch table.
 * All protocols not implemented in kernel go to raw IP protocol handler.
 */
ip_init()
{
	register struct protosw *pr;
	register int i;

COUNT(IP_INIT);
	pr = pffindproto(PF_INET, IPPROTO_RAW);
	if (pr == 0)
		panic("ip_init");
	for (i = 0; i < IPPROTO_MAX; i++)
		ip_protox[i] = pr - protosw;
	for (pr = protosw; pr <= protoswLAST; pr++)
		if (pr->pr_family == PF_INET &&
		    pr->pr_protocol && pr->pr_protocol != IPPROTO_RAW)
			ip_protox[pr->pr_protocol] = pr - protosw;
	ipq.next = ipq.prev = &ipq;
	ip_id = time & 0xffff;
}

u_char	ipcksum = 1;
struct	ip *ip_reass();

/*
 * Ip input routine.  Checksum and byte swap header.  If fragmented
 * try to reassamble.  If complete and fragment queue exists, discard.
 * Process options.  Pass to next level.
 */
ipintr()
{
	register struct ip *ip;
	register struct mbuf *m;
	struct mbuf *m0, *mopt;
	register int i;
	register struct ipq *fp;
	int hlen, s;

COUNT(IPINTR);
next:
	/*
	 * Get next datagram off input queue and get IP header
	 * in first mbuf.
	 */
	s = splimp();
	IF_DEQUEUE(&ipintrq, m);
	splx(s);
	if (m == 0)
		return;
	if (m->m_len < sizeof (struct ip) &&
	    m_pullup(m, sizeof (struct ip)) == 0)
		goto bad;
	ip = mtod(m, struct ip *);
	if ((hlen = ip->ip_hl << 2) > m->m_len) {
		if (m_pullup(m, hlen) == 0)
			goto bad;
		ip = mtod(m, struct ip *);
	}
	if (ipcksum)
		if (ip->ip_sum = in_cksum(m, hlen)) {
			printf("ip_sum %x\n", ip->ip_sum);	/* XXX */
			ipstat.ips_badsum++;
			goto bad;
		}

#if vax
	/*
	 * Convert fields to host representation.
	 */
	ip->ip_len = ntohs((u_short)ip->ip_len);
	ip->ip_id = ntohs(ip->ip_id);
	ip->ip_off = ntohs((u_short)ip->ip_off);
#endif

	/*
	 * Check that the amount of data in the buffers
	 * is as at least much as the IP header would have us expect.
	 * Trim mbufs if longer than we expect.
	 * Drop packet if shorter than we expect.
	 */
	i = 0;
	m0 = m;
	for (; m != NULL; m = m->m_next)
		i += m->m_len;
	m = m0;
	if (i != ip->ip_len) {
		if (i < ip->ip_len) {
			ipstat.ips_tooshort++;
			goto bad;
		}
		m_adj(m, ip->ip_len - i);
	}

	/*
	 * Process options and, if not destined for us,
	 * ship it on.
	 */
	if (hlen > sizeof (struct ip))
		ip_dooptions(ip);
	if (ifnet && ip->ip_dst.s_addr != ifnet->if_addr.s_addr &&
	    if_ifwithaddr(ip->ip_dst) == 0) {
		if (--ip->ip_ttl == 0) {
			icmp_error(ip, ICMP_TIMXCEED, 0);
			goto next;
		}
		mopt = m_get(M_DONTWAIT);
		if (mopt == 0)
			goto bad;
		ip_stripoptions(ip, mopt);
		(void) ip_output(m0, mopt);
		goto next;
	}

	/*
	 * Look for queue of fragments
	 * of this datagram.
	 */
	for (fp = ipq.next; fp != &ipq; fp = fp->next)
		if (ip->ip_id == fp->ipq_id &&
		    ip->ip_src.s_addr == fp->ipq_src.s_addr &&
		    ip->ip_dst.s_addr == fp->ipq_dst.s_addr &&
		    ip->ip_p == fp->ipq_p)
			goto found;
	fp = 0;
found:

	/*
	 * Adjust ip_len to not reflect header,
	 * set ip_mff if more fragments are expected,
	 * convert offset of this to bytes.
	 */
	ip->ip_len -= hlen;
	((struct ipasfrag *)ip)->ipf_mff = 0;
	if (ip->ip_off & IP_MF)
		((struct ipasfrag *)ip)->ipf_mff = 1;
	ip->ip_off <<= 3;

	/*
	 * If datagram marked as having more fragments
	 * or if this is not the first fragment,
	 * attempt reassembly; if it succeeds, proceed.
	 */
	if (((struct ipasfrag *)ip)->ipf_mff || ip->ip_off) {
		ip = ip_reass((struct ipasfrag *)ip, fp);
		if (ip == 0)
			goto next;
		hlen = ip->ip_hl << 2;
		m = dtom(ip);
	} else
		if (fp)
			(void) ip_freef(fp);

	/*
	 * Switch out to protocol's input routine.
	 */
	(*protosw[ip_protox[ip->ip_p]].pr_input)(m);
	goto next;
bad:
printf("ip_input bad!\n");
	m_freem(m);
	goto next;
}

/*
 * Take incoming datagram fragment and try to
 * reassemble it into whole datagram.  If a chain for
 * reassembly of this datagram already exists, then it
 * is given as fp; otherwise have to make a chain.
 */
struct ip *
ip_reass(ip, fp)
	register struct ipasfrag *ip;
	register struct ipq *fp;
{
	register struct mbuf *m = dtom(ip);
	register struct ipasfrag *q;
	struct mbuf *t;
	int hlen = ip->ip_hl << 2;
	int i, next;
COUNT(IP_REASS);

	/*
	 * Presence of header sizes in mbufs
	 * would confuse code below.
	 */
	m->m_off += hlen;
	m->m_len -= hlen;

	/*
	 * If first fragment to arrive, create a reassembly queue.
	 */
	if (fp == 0) {
		if ((t = m_get(1)) == NULL)
			goto dropfrag;
		t->m_off = MMINOFF;
		fp = mtod(t, struct ipq *);
		insque(fp, &ipq);
		fp->ipq_ttl = IPFRAGTTL;
		fp->ipq_p = ip->ip_p;
		fp->ipq_id = ip->ip_id;
		fp->ipq_next = fp->ipq_prev = (struct ipasfrag *)fp;
		fp->ipq_src = ((struct ip *)ip)->ip_src;
		fp->ipq_dst = ((struct ip *)ip)->ip_dst;
		q = (struct ipasfrag *)fp;
		goto insert;
	}

	/*
	 * Find a segment which begins after this one does.
	 */
	for (q = fp->ipq_next; q != (struct ipasfrag *)fp; q = q->ipf_next)
		if (q->ip_off > ip->ip_off)
			break;

	/*
	 * If there is a preceding segment, it may provide some of
	 * our data already.  If so, drop the data from the incoming
	 * segment.  If it provides all of our data, drop us.
	 */
	if (q->ipf_prev != (struct ipasfrag *)fp) {
		i = q->ipf_prev->ip_off + q->ipf_prev->ip_len - ip->ip_off;
		if (i > 0) {
			if (i >= ip->ip_len)
				goto dropfrag;
			m_adj(dtom(ip), i);
			ip->ip_off += i;
			ip->ip_len -= i;
		}
	}

	/*
	 * While we overlap succeeding segments trim them or,
	 * if they are completely covered, dequeue them.
	 */
	while (q != (struct ipasfrag *)fp && ip->ip_off + ip->ip_len > q->ip_off) {
		i = (ip->ip_off + ip->ip_len) - q->ip_off;
		if (i < q->ip_len) {
			q->ip_len -= i;
			m_adj(dtom(q), i);
			break;
		}
		q = q->ipf_next;
		m_freem(dtom(q->ipf_prev));
		ip_deq(q->ipf_prev);
	}

insert:
	/*
	 * Stick new segment in its place;
	 * check for complete reassembly.
	 */
	ip_enq(ip, q->ipf_prev);
	next = 0;
	for (q = fp->ipq_next; q != (struct ipasfrag *)fp; q = q->ipf_next) {
		if (q->ip_off != next)
			return (0);
		next += q->ip_len;
	}
	if (q->ipf_prev->ipf_mff)
		return (0);

	/*
	 * Reassembly is complete; concatenate fragments.
	 */
	q = fp->ipq_next;
	m = dtom(q);
	t = m->m_next;
	m->m_next = 0;
	m_cat(m, t);
	while ((q = q->ipf_next) != (struct ipasfrag *)fp)
		m_cat(m, dtom(q));

	/*
	 * Create header for new ip packet by
	 * modifying header of first packet;
	 * dequeue and discard fragment reassembly header.
	 * Make header visible.
	 */
	ip = fp->ipq_next;
	ip->ip_len = next;
	((struct ip *)ip)->ip_src = fp->ipq_src;
	((struct ip *)ip)->ip_dst = fp->ipq_dst;
	remque(fp);
	(void) m_free(dtom(fp));
	m = dtom(ip);
	m->m_len += sizeof (struct ipasfrag);
	m->m_off -= sizeof (struct ipasfrag);
	return ((struct ip *)ip);

dropfrag:
	m_freem(m);
	return (0);
}

/*
 * Free a fragment reassembly header and all
 * associated datagrams.
 */
struct ipq *
ip_freef(fp)
	struct ipq *fp;
{
	register struct ipasfrag *q;
	struct mbuf *m;
COUNT(IP_FREEF);

	for (q = fp->ipq_next; q != (struct ipasfrag *)fp; q = q->ipf_next)
		m_freem(dtom(q));
	m = dtom(fp);
	fp = fp->next;
	remque(fp->prev);
	(void) m_free(m);
	return (fp);
}

/*
 * Put an ip fragment on a reassembly chain.
 * Like insque, but pointers in middle of structure.
 */
ip_enq(p, prev)
	register struct ipasfrag *p, *prev;
{

COUNT(IP_ENQ);
	p->ipf_prev = prev;
	p->ipf_next = prev->ipf_next;
	prev->ipf_next->ipf_prev = p;
	prev->ipf_next = p;
}

/*
 * To ip_enq as remque is to insque.
 */
ip_deq(p)
	register struct ipasfrag *p;
{

COUNT(IP_DEQ);
	p->ipf_prev->ipf_next = p->ipf_next;
	p->ipf_next->ipf_prev = p->ipf_prev;
}

/*
 * IP timer processing;
 * if a timer expires on a reassembly
 * queue, discard it.
 */
ip_slowtimo()
{
	register struct ipq *fp;
	int s = splnet();

COUNT(IP_SLOWTIMO);
	for (fp = ipq.next; fp != &ipq; )
		if (--fp->ipq_ttl == 0)
			fp = ip_freef(fp);
		else
			fp = fp->next;
	splx(s);
}

/*
 * Drain off all datagram fragments.
 */
ip_drain()
{

COUNT(IP_DRAIN);
	while (ipq.next != &ipq)
		(void) ip_freef(ipq.next);
}

/*
 * Do option processing on a datagram,
 * possibly discarding it if bad options
 * are encountered.
 */
ip_dooptions(ip)
	struct ip *ip;
{
	register u_char *cp;
	int opt, optlen, cnt;
	struct in_addr *sin;
	register struct ip_timestamp *ipt;
	register struct ifnet *ifp;
	struct in_addr t;

COUNT(IP_DOOPTIONS);
	cp = (u_char *)(ip + 1);
	cnt = (ip->ip_hl << 2) - sizeof (struct ip);
	for (; cnt > 0; cnt -= optlen, cp += optlen) {
		opt = cp[0];
		if (opt == IPOPT_EOL)
			break;
		if (opt == IPOPT_NOP)
			optlen = 1;
		else
			optlen = cp[1];
		switch (opt) {

		default:
			break;

		/*
		 * Source routing with record.
		 * Find interface with current destination address.
		 * If none on this machine then drop if strictly routed,
		 * or do nothing if loosely routed.
		 * Record interface address and bring up next address
		 * component.  If strictly routed make sure next
		 * address on directly accessible net.
		 */
		case IPOPT_LSRR:
			if (cp[2] < 4 || cp[2] > optlen - (sizeof (long) - 1))
				break;
			sin = (struct in_addr *)(cp + cp[2]);
			ifp = if_ifwithaddr(*sin);
			if (ifp == 0) {
				if (opt == IPOPT_SSRR)
					goto bad;
				break;
			}
			t = ip->ip_dst; ip->ip_dst = *sin; *sin = t;
			cp[2] += 4;
			if (cp[2] > optlen - (sizeof (long) - 1))
				break;
			ip->ip_dst = sin[1];
			if (opt == IPOPT_SSRR && if_ifonnetof(ip->ip_dst)==0)
				goto bad;
			break;

		case IPOPT_TS:
			ipt = (struct ip_timestamp *)cp;
			if (ipt->ipt_len < 5)
				goto bad;
			if (ipt->ipt_ptr > ipt->ipt_len - sizeof (long)) {
				if (++ipt->ipt_oflw == 0)
					goto bad;
				break;
			}
			sin = (struct in_addr *)(cp+cp[2]);
			switch (ipt->ipt_flg) {

			case IPOPT_TS_TSONLY:
				break;

			case IPOPT_TS_TSANDADDR:
				if (ipt->ipt_ptr + 8 > ipt->ipt_len)
					goto bad;
				/* stamp with ``first'' interface address */
				*sin++ = ifnet->if_addr;
				break;

			case IPOPT_TS_PRESPEC:
				if (if_ifwithaddr(*sin) == 0)
					continue;
				if (ipt->ipt_ptr + 8 > ipt->ipt_len)
					goto bad;
				ipt->ipt_ptr += 4;
				break;

			default:
				goto bad;
			}
			*(n_time *)sin = iptime();
			ipt->ipt_ptr += 4;
		}
	}
	return;
bad:
	/* SHOULD FORCE ICMP MESSAGE */
	return;
}

/*
 * Strip out IP options, at higher
 * level protocol in the kernel.
 * Second argument is buffer to which options
 * will be moved, and return value is their length.
 */
ip_stripoptions(ip, mopt)
	struct ip *ip;
	struct mbuf *mopt;
{
	register int i;
	register struct mbuf *m;
	int olen;
COUNT(IP_STRIPOPTIONS);

	olen = (ip->ip_hl<<2) - sizeof (struct ip);
	m = dtom(ip);
	ip++;
	if (mopt) {
		mopt->m_len = olen;
		mopt->m_off = MMINOFF;
		bcopy((caddr_t)ip, mtod(m, caddr_t), (unsigned)olen);
	}
	i = m->m_len - (sizeof (struct ip) + olen);
	bcopy((caddr_t)ip+olen, (caddr_t)ip, (unsigned)i);
	m->m_len -= i;
}
