/*	raw_imp.c	4.9	82/03/28	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_imp.h"
#include "../net/raw_cb.h"
#include "../errno.h"

/*
 * Raw interface to IMP.
 */

struct	sockaddr_in rawimpaddr = { AF_IMPLINK };
/*
 * Generate IMP leader and pass packet to impoutput.
 * The user must create a skeletal leader in order to
 * communicate message type, message subtype, etc.
 * We fill in holes where needed and verify parameters
 * supplied by user.
 */
rimp_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	struct mbuf *n;
	int len;
	register struct imp_leader *ip;
	register struct sockaddr_in *sin;
	register struct rawcb *rp = sotorawcb(so);
	struct ifnet *ifp;
	struct control_leader *cp;

COUNT(RIMP_OUTPUT);
	/*
	 * Verify user has supplied necessary space
	 * for the leader and check parameters in it.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof(struct control_leader)) &&
	    (m = m_pullup(m, sizeof(struct control_leader))) == 0)
		return (0);
	cp = mtod(m, struct control_leader *);
	if (cp->dl_mtype == IMPTYPE_DATA)
		if (m->m_len < sizeof(struct imp_leader) &&
		    (m = m_pullup(m, sizeof(struct imp_leader))) == 0)
			return (0);
	ip = mtod(m, struct imp_leader *);
	if (ip->il_format != IMP_NFF)
		goto bad;
#ifdef notdef
	if (ip->il_link != IMPLINK_IP &&
	    (ip->il_link < IMPLINK_LOWEXPER || ip->il_link > IMPLINK_HIGHEXPER))
		goto bad;
#endif

	/*
	 * Fill in IMP leader -- impoutput refrains from rebuilding
	 * the leader when it sees the protocol family PF_IMPLINK.
	 * (message size calculated by walking through mbuf's)
	 */
	for (len = 0, n = m; n; n = n->m_next)
		len += n->m_len;
	ip->il_length = htons((u_short)(len << 3));
	sin = (struct sockaddr_in *)&rp->rcb_addr;
	ip->il_network = sin->sin_addr.s_net;
	ip->il_host = sin->sin_addr.s_host;
	ip->il_imp = sin->sin_addr.s_imp;
	/* no routing here */
	ifp = if_ifonnetof(ip->il_network);
	if (ifp == 0)
		goto bad;
	return (impoutput(ifp, m, (struct sockaddr *)&rawimpaddr));

bad:
	m_freem(m);
	return (0);
}
