/*	raw_ip.c	4.13	82/06/20	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../net/if.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/raw_cb.h"
#include "../net/route.h"
#include <errno.h>

/*
 * Raw interface to IP protocol.
 */

static struct sockaddr_in ripdst = { AF_INET };
static struct sockaddr_in ripsrc = { AF_INET };
static struct sockproto ripproto = { PF_INET };
/*
 * Setup generic address and protocol structures
 * for raw_input routine, then pass them along with
 * mbuf chain.
 */
rip_input(m)
	struct mbuf *m;
{
	register struct ip *ip = mtod(m, struct ip *);

	ripproto.sp_protocol = ip->ip_p;
	ripdst.sin_addr = ip->ip_dst;
	ripsrc.sin_addr = ip->ip_src;
	raw_input(m, &ripproto, (struct sockaddr *)&ripsrc,
	  (struct sockaddr *)&ripdst);
}

/*
 * Generate IP header and pass packet to ip_output.
 * Tack on options user may have setup with control call.
 */
rip_output(m0, so)
	struct mbuf *m0;
	struct socket *so;
{
	register struct mbuf *m;
	register struct ip *ip;
	int len = 0, error;
	struct rawcb *rp = sotorawcb(so);
	struct ifnet *ifp;
	struct sockaddr_in *sin;

	/*
	 * Calculate data length and get an mbuf
	 * for IP header.
	 */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	m = m_get(M_DONTWAIT);
	if (m == 0) {
		error = ENOBUFS;
		goto bad;
	}
	
	/*
	 * Fill in IP header as needed.
	 */
	m->m_off = MMAXOFF - sizeof(struct ip);
	m->m_len = sizeof(struct ip);
	m->m_next = m0;
	ip = mtod(m, struct ip *);
	ip->ip_p = so->so_proto->pr_protocol;
	ip->ip_len = sizeof(struct ip) + len;
	if (rp->rcb_flags & RAW_LADDR) {
		sin = (struct sockaddr_in *)&rp->rcb_laddr;
		if (sin->sin_family != AF_INET) {
			error = EAFNOSUPPORT;
			goto bad;
		}
		ip->ip_src.s_addr = sin->sin_addr.s_addr;
	} else
		ip->ip_src.s_addr = 0;
	ip->ip_dst = ((struct sockaddr_in *)&rp->rcb_faddr)->sin_addr;
	ip->ip_ttl = MAXTTL;
	return (ip_output(m, (struct mbuf *)0, &routetoif, 1));
bad:
	m_freem(m);
	return (error);
}
