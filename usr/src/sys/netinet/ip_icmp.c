/*	ip_icmp.c	6.11	85/05/27	*/

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "time.h"
#include "kernel.h"

#include "../net/route.h"
#include "../net/if.h"

#include "in.h"
#include "in_systm.h"
#include "in_var.h"
#include "ip.h"
#include "ip_icmp.h"
#include "icmp_var.h"

#ifdef ICMPPRINTFS
/*
 * ICMP routines: error generation, receive packet processing, and
 * routines to turnaround packets back to the originator, and
 * host table maintenance routines.
 */
int	icmpprintfs = 0;
#endif

/*
 * Generate an error packet of type error
 * in response to bad packet ip.
 */
icmp_error(oip, type, code)
	struct ip *oip;
	int type, code;
{
	register unsigned oiplen = oip->ip_hl << 2;
	register struct icmp *icp;
	struct mbuf *m;
	struct ip *nip;

#ifdef ICMPPRINTFS
	if (icmpprintfs)
		printf("icmp_error(%x, %d, %d)\n", oip, type, code);
#endif
	icmpstat.icps_error++;
	/*
	 * Make sure that the old IP packet had 8 bytes of data to return;
	 * if not, don't bother.  Also don't EVER error if the old
	 * packet protocol was ICMP.
	 */
	if (oip->ip_len < 8) {
		icmpstat.icps_oldshort++;
		goto free;
	}
	if (oip->ip_p == IPPROTO_ICMP) {
		icmpstat.icps_oldicmp++;
		goto free;
	}

	/*
	 * First, formulate icmp message
	 */
	m = m_get(M_DONTWAIT, MT_HEADER);
	if (m == NULL)
		goto free;
	m->m_len = oiplen + 8 + ICMP_MINLEN;
	m->m_off = MMAXOFF - m->m_len;
	icp = mtod(m, struct icmp *);
	if ((u_int)type > ICMP_IREQREPLY)
		panic("icmp_error");
	icmpstat.icps_outhist[type]++;
	icp->icmp_type = type;
	icp->icmp_void = 0;
	if (type == ICMP_PARAMPROB) {
		icp->icmp_pptr = code;
		code = 0;
	}
	icp->icmp_code = code;
	bcopy((caddr_t)oip, (caddr_t)&icp->icmp_ip, oiplen + 8);
	nip = &icp->icmp_ip;
	nip->ip_len += oiplen;
	nip->ip_len = htons((u_short)nip->ip_len);

	/*
	 * Now, copy old ip header in front of icmp
	 * message.  This allows us to reuse any source
	 * routing info present.
	 */
	m->m_off -= oiplen;
	nip = mtod(m, struct ip *);
	bcopy((caddr_t)oip, (caddr_t)nip, oiplen);
	nip->ip_len = m->m_len + oiplen;
	nip->ip_p = IPPROTO_ICMP;
	/* icmp_send adds ip header to m_off and m_len, so we deduct here */
	m->m_off += oiplen;
	icmp_reflect(nip);

free:
	m_freem(dtom(oip));
}

static struct sockproto icmproto = { AF_INET, IPPROTO_ICMP };
static struct sockaddr_in icmpsrc = { AF_INET };
static struct sockaddr_in icmpdst = { AF_INET };

/*
 * Process a received ICMP message.
 */
icmp_input(m)
	struct mbuf *m;
{
	register struct icmp *icp;
	register struct ip *ip = mtod(m, struct ip *);
	int icmplen = ip->ip_len, hlen = ip->ip_hl << 2;
	register int i;
	int (*ctlfunc)(), code;
	extern u_char ip_protox[];
	extern struct in_addr in_makeaddr();

	/*
	 * Locate icmp structure in mbuf, and check
	 * that not corrupted and of at least minimum length.
	 */
#ifdef ICMPPRINTFS
	if (icmpprintfs)
		printf("icmp_input from %x, len %d\n", ip->ip_src, icmplen);
#endif
	if (icmplen < ICMP_MINLEN) {
		icmpstat.icps_tooshort++;
		goto free;
	}
	/* THIS LENGTH CHECK STILL MISSES ANY IP OPTIONS IN ICMP_IP */
	i = MIN(icmplen, ICMP_ADVLENMIN + hlen);
 	if ((m->m_off > MMAXOFF || m->m_len < i) &&
 		(m = m_pullup(m, i)) == 0)  {
		icmpstat.icps_tooshort++;
		return;
	}
 	ip = mtod(m, struct ip *);
	m->m_len -= hlen;
	m->m_off += hlen;
	icp = mtod(m, struct icmp *);
	if (in_cksum(m, icmplen)) {
		icmpstat.icps_checksum++;
		goto free;
	}

#ifdef ICMPPRINTFS
	/*
	 * Message type specific processing.
	 */
	if (icmpprintfs)
		printf("icmp_input, type %d code %d\n", icp->icmp_type,
		    icp->icmp_code);
#endif
	if (icp->icmp_type > ICMP_IREQREPLY)
		goto free;
	icmpstat.icps_inhist[icp->icmp_type]++;
	code = icp->icmp_code;
	switch (icp->icmp_type) {

	case ICMP_UNREACH:
		if (code > 5)
			goto badcode;
		code += PRC_UNREACH_NET;
		goto deliver;

	case ICMP_TIMXCEED:
		if (code > 1)
			goto badcode;
		code += PRC_TIMXCEED_INTRANS;
		goto deliver;

	case ICMP_PARAMPROB:
		if (code)
			goto badcode;
		code = PRC_PARAMPROB;
		goto deliver;

	case ICMP_SOURCEQUENCH:
		if (code)
			goto badcode;
		code = PRC_QUENCH;
	deliver:
		/*
		 * Problem with datagram; advise higher level routines.
		 */
		icp->icmp_ip.ip_len = ntohs((u_short)icp->icmp_ip.ip_len);
		if (icmplen < ICMP_ADVLENMIN || icmplen < ICMP_ADVLEN(icp)) {
			icmpstat.icps_badlen++;
			goto free;
		}
#ifdef ICMPPRINTFS
		if (icmpprintfs)
			printf("deliver to protocol %d\n", icp->icmp_ip.ip_p);
#endif
		if (ctlfunc = inetsw[ip_protox[icp->icmp_ip.ip_p]].pr_ctlinput)
			(*ctlfunc)(code, (caddr_t)icp);
		goto free;

	badcode:
		icmpstat.icps_badcode++;
		goto free;

	case ICMP_ECHO:
		icp->icmp_type = ICMP_ECHOREPLY;
		goto reflect;

	case ICMP_TSTAMP:
		if (icmplen < ICMP_TSLEN) {
			icmpstat.icps_badlen++;
			goto free;
		}
		icp->icmp_type = ICMP_TSTAMPREPLY;
		icp->icmp_rtime = iptime();
		icp->icmp_ttime = icp->icmp_rtime;	/* bogus, do later! */
		goto reflect;
		
	case ICMP_IREQ:
#define	satosin(sa)	((struct sockaddr_in *)(sa))
		if (in_netof(ip->ip_src) == 0)
			ip->ip_src = in_makeaddr(
			    in_netof(satosin(&in_ifaddr->ia_addr)->sin_addr),
			    in_lnaof(ip->ip_src));
		icp->icmp_type = ICMP_IREQREPLY;
		goto reflect;

	case ICMP_REDIRECT:
		if (icmplen < ICMP_ADVLENMIN || icmplen < ICMP_ADVLEN(icp)) {
			icmpstat.icps_badlen++;
			goto free;
		}
		/*
		 * Short circuit routing redirects to force
		 * immediate change in the kernel's routing
		 * tables.  The message is also handed to anyone
		 * listening on a raw socket (e.g. the routing
		 * daemon for use in updating its tables).
		 */
		icmpdst.sin_addr = icp->icmp_gwaddr;
		if (code == ICMP_REDIRECT_NET || code == ICMP_REDIRECT_TOSNET) {
			icmpsrc.sin_addr =
			 in_makeaddr(in_netof(icp->icmp_ip.ip_dst), INADDR_ANY);
			rtredirect((struct sockaddr *)&icmpsrc,
			  (struct sockaddr *)&icmpdst, RTF_GATEWAY);
			pfctlinput(PRC_REDIRECT_NET, (caddr_t)icp);
		} else {
			icmpsrc.sin_addr = icp->icmp_ip.ip_dst;
			rtredirect((struct sockaddr *)&icmpsrc,
			  (struct sockaddr *)&icmpdst, RTF_GATEWAY | RTF_HOST);
			pfctlinput(PRC_REDIRECT_HOST, (caddr_t)icp);
		}
		/* FALL THROUGH */

	case ICMP_ECHOREPLY:
	case ICMP_TSTAMPREPLY:
	case ICMP_IREQREPLY:
		icmpsrc.sin_addr = ip->ip_src;
		icmpdst.sin_addr = ip->ip_dst;
		raw_input(dtom(icp), &icmproto, (struct sockaddr *)&icmpsrc,
		  (struct sockaddr *)&icmpdst);
		return;

	default:
		goto free;
	}
reflect:
	ip->ip_len += hlen;		/* since ip_input deducts this */
	icmpstat.icps_reflect++;
	icmpstat.icps_outhist[icp->icmp_type]++;
	icmp_reflect(ip);
	return;
free:
	m_freem(dtom(ip));
}

/*
 * Reflect the ip packet back to the source
 * TODO: rearrange ip source routing options.
 */
icmp_reflect(ip)
	struct ip *ip;
{
	register struct in_addr t;
	register struct in_ifaddr *ia;

	t = ip->ip_dst;
	if (t.s_addr == INADDR_ANY)
		t = IA_SIN(in_ifaddr)->sin_addr;
	else for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if (t.s_addr == satosin(&ia->ia_broadaddr)->sin_addr.s_addr &&
		    (ia->ia_ifp->if_flags & IFF_BROADCAST)) {
			t = IA_SIN(ia)->sin_addr;
			break;
		}
	ip->ip_dst = ip->ip_src;
	ip->ip_src = t;
	icmp_send(ip);
}

/*
 * Send an icmp packet back to the ip level,
 * after supplying a checksum.
 */
icmp_send(ip)
	struct ip *ip;
{
	register int hlen;
	register struct icmp *icp;
	register struct mbuf *m;

	m = dtom(ip);
	hlen = ip->ip_hl << 2;
	icp = mtod(m, struct icmp *);
	icp->icmp_cksum = 0;
	icp->icmp_cksum = in_cksum(m, ip->ip_len - hlen);
	m->m_off -= hlen;
	m->m_len += hlen;
#ifdef ICMPPRINTFS
	if (icmpprintfs)
		printf("icmp_send dst %x src %x\n", ip->ip_dst, ip->ip_src);
#endif
	(void) ip_output(m, (struct mbuf *)0, (struct route *)0, 0);
}

n_time
iptime()
{
	int s = spl6();
	u_long t;

	t = (time.tv_sec % (24*60*60)) * 1000 + time.tv_usec / 1000;
	splx(s);
	return (htonl(t));
}
