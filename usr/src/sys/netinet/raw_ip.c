/*	raw_ip.c	4.2	82/01/24	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/raw_cb.h"
#include "/usr/include/errno.h"

/*
 * Raw interface to IP protocol.
 */

static struct sockaddr_in ripaddr = { PF_INET };
static struct sockproto ripproto = { AF_INET };

/*
 * Setup generic address and protocol structures
 * for raw_input routine, then pass them along with
 * mbuf chain.
 */
rip_input(m)
	struct mbuf *m;
{
	register struct ip *ip = mtod(m, struct ip *);
	struct sockaddr_in sin;
	struct sockproto sp;

COUNT(RIP_INPUT);
	ripproto.sp_protocol = ip->ip_p;
	ripaddr.sin_addr = ip->ip_dst;
	raw_input(m, ripproto, ripaddr);
}

/*ARGSUSED*/
rip_ctlinput(m)
	struct mbuf *m;
{
COUNT(RIP_CTLINPUT);
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
	register int len = 0;
	register struct rawcb *rp = sotorawcb(so);

COUNT(RIP_OUTPUT);
	if (so->so_options & SO_DEBUG)
		printf("rip_output\n");
	/*
	 * Calculate data length and get an mbuf
	 * for IP header.
	 */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	m = m_get(M_DONTWAIT);
	if (m == 0) {
		(void) m_freem(m);
		return;
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
	ip->ip_dst =
		((struct sockaddr_in *)&rp->rcb_addr)->sin_addr;
	ip->ip_src =
		((struct sockaddr_in *)&so->so_addr)->sin_addr;
	ip->ip_ttl = MAXTTL;
printf("ip=<p=%d,len=%d,dst=%x,src=%x>\n",ip->ip_p,ip->ip_len,ip->ip_dst,ip->ip_src);
	return (ip_output(m, 0));
}

/*
 * Intercept control operations related to
 * handling of IP options.  Otherwise,
 * just pass things on to the raw_usrreq
 * routine for setup and tear down of
 * raw control block data structures.
 */
rip_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	register struct rawcb *rp = sotorawcb(so);

COUNT(RAW_USRREQ);
	if (rp == 0 && req != PRU_ATTACH)
		return (EINVAL);

	switch (req) {
	
	case PRU_CONTROL:
		return (EOPNOTSUPP);
	}
	return (raw_usrreq(so, req, m, addr));
}
