#ifdef RCSIDENT
static char rcsident[] = "$Header: udp.c,v 1.18 85/07/31 09:44:10 walsh Exp $";
#endif


#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/udp.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/icmp.h"
#ifdef HMPTRAPS
#include "../bbnnet/hmp_traps.h"
#endif

#ifdef RCSIDENT
static char rcsudphdr[] = RCSUDPHDR;
#endif

extern int nosum;

struct inpcb udp;
struct udp_stat udpstat;

/*
 *  Process incoming udp messages.  Called directly from ip_input.
 *  User sees udp header with pseudo-header which overlays ip header
 *  (defined in udp.h).
 */
udp_input(mp)
register struct mbuf *mp;
{
    register struct udp *p;
    register struct inpcb *inp;
    register u_short i, j, ulen;

    udpstat.u_total ++;
    /*
     * see ip_input()
     */
    if ((mp->m_off > MMAXOFF) || (mp->m_len < sizeof(struct udp)))
    {
	if ((mp = m_pullup(mp, sizeof(struct udp))) == NULL)
	{
	    udpstat.u_tooshort ++;
	    return;
	}
    }

    p = mtod(mp, struct udp *);
    ulen = ((struct ip *) p) ->ip_len; /* ip_input() set to amt IP data */
    mp->m_off += sizeof p->u_x;
    mp->m_len -= sizeof p->u_x;
    p->u_x1 = 0;

    if (ntohs(p->u_len) != ulen) 
    {
	/*
	 * u_ilen overlays IP checksum, which is now zero.
	 * ulen is the actual number of bytes we got on input
	 * from IP; u_len is what UDP says we should have
	 * (sizeof(udp_specific) + datalen)
	 */
	log(LOG_WARNING, "UDP len %d, but got %d\n", ntohs(p->u_len), ulen);
	netlog (mp);
	return;
    }
    p->u_ilen = p->u_len;

    /*
     * Do checksum calculation.  Assumes pseudo-header passed up from
     * IP level and finished above.
     * Zero checksum on send means no checksum was generated.
     */
    if ((i = p->u_sum) != 0) 
    {
	p->u_sum = 0;
	j = (u_short) in_cksum(mp, (int) (ulen + UDPCKSIZE));
	/*
	 * Remember that zero is special, and compensate for this.
	 */
	if (j == 0)
	    j = (~j);
	if (i != j) 
	{
	    udpstat.u_badsum++;
	    if (! nosum)
	    {
#ifdef HMPTRAPS
		/* hmp_trap(T_UDP_CKSUM, (caddr_t)0,0); */
#endif
		inet_cksum_err ("udp", (struct ip *) p, (u_long) i, (u_long) j);
		netlog(mp);
		return;
	    }
	}
    }

    inp = in_pcblookup(&udp, p->u_s.s_addr, (u_short)0,
			     p->u_d.s_addr, p->u_dst, TRUE);

    /* if a user is found, queue the data, otherwise drop it */

    if (inp != NULL)
    {
	struct sockaddr_in udpsock;
	struct sockbuf *sorcv;

	/*
	 * throw away entire IP and UDP leaders.
	 * user gets address separately.
	 */
	mp->m_off += sizeof (struct udp) - sizeof (p->u_x);
	mp->m_len -= sizeof (struct udp) - sizeof (p->u_x);

	udpsock.sin_family = AF_INET;
	udpsock.sin_port = p->u_src;
	udpsock.sin_addr = p->u_s;
	udpsock.sin_zero[0] = udpsock.sin_zero[1] = 0;

	sorcv = &inp->inp_socket->so_rcv;

	if (! sbappendaddr(sorcv, (struct sockaddr *)&udpsock, mp,
	    (struct mbuf *) NULL)) 
	{
	    m_freem(mp);
	    if ((ulen - UDPSIZE + sizeof(struct sockaddr)) > sbspace(sorcv))
		udpstat.u_sonospace ++;
	    else
		udpstat.u_nobuf ++;
	}
	else
	    sorwakeup(inp->inp_socket);
    }
    else 
    {
	/*
	 * No one wants this packet.
	 */
	if (!in_broadcast(p->u_s) && !in_broadcast(p->u_d))
	    /*
	     * Don't bother everyone on the net.  Someone else may
	     * provide the service (port).
	     */
	    ic_errmsg (icmp_addr((struct ip *) p), p->u_s,
		ICMP_UNRCH, ICMP_UNRCH_PORT, 0,
		sizeof(struct ip) + ICMP_ERRLEN, (char *) p);

	udpstat.u_drops++;
	m_freem(mp);
    }
}

/*
 * Output a udp message.  Called from udp_usrreq().
 */
udp_output(inp, mp)
struct inpcb *inp;
register struct mbuf *mp;
{
    register struct udp *p;
    register struct mbuf *m;
    register int len;

    len = 0;
    for (m = mp; m; m = m->m_next)
	len += m->m_len;

    /*
     * find a place to put the IP/UDP headers.
     */
    m = m_get(M_WAIT, MT_HEADER);
    if (m == 0) 
    {
	m_freem(mp);
	return (ENOBUFS);
    }
    m->m_next = mp;
    mp = m;

    /*
     * Compose header in first mbuf.  Get addresses and ports
     * from ucb, add in pseudo-header fields for checksum.
     * Ensure header is aligned for memory access speed...
     */
    mp->m_off = (MMAXOFF - sizeof(struct udp)) & ~(sizeof(long) -1);
    mp->m_len = sizeof(struct udp);
    p = mtod(mp, struct udp *);

    /* stuff UDP fields */
    p->u_src = inp->inp_lport;
    p->u_dst = inp->inp_fport;
    p->u_len = htons((u_short)len+UDPSIZE);

    /* and "IP" fields */
    ((struct ip *) p)->ip_tos = 0;	/* for ip_send() */
    p->u_x1 = 0;
    p->u_pr = IPPROTO_UDP;
    p->u_ilen = p->u_len;
    p->u_s = inp->inp_laddr;
    p->u_d = inp->inp_faddr;

    /* Do checksum.  Include pseudo header. */
    mp->m_off += sizeof p->u_x;
    mp->m_len -= sizeof p->u_x;
    p->u_sum = 0;
    p->u_sum = in_cksum(mp, len + sizeof(struct udp) - sizeof p->u_x);
    if (p->u_sum == 0)
	/* Zero is reserved for unsummed packets */
	p->u_sum = (~ p->u_sum);
    mp->m_off -= sizeof p->u_x;
    mp->m_len += sizeof p->u_x;

    /*
     * Now send the packet via IP.
     */
    return(ip_send(inp, mp, len+UDPSIZE, FALSE));
}
