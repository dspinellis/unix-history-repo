/*	raw_pup.c	6.2	84/08/29	*/

#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "protosw.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../netpup/pup.h"

/*
 * Raw PUP protocol interface.
 */

struct	sockaddr_pup pupsrc = { AF_PUP };
struct	sockaddr_pup pupdst = { AF_PUP };
struct	sockproto pupproto = { PF_PUP };
/*
 * Raw PUP input.
 */
rpup_input(m)
	struct mbuf *m;
{
	register struct pup_header *pup = mtod(m, struct pup_header *);

	pupproto.sp_protocol = pup->pup_type;
	bcopy((caddr_t)&pup->pup_dnet, (caddr_t)&pupdst.spup_net,
	    sizeof (struct pupport));
	bcopy((caddr_t)&pup->pup_snet, (caddr_t)&pupsrc.spup_net,
	    sizeof (struct pupport));
	raw_input(m, &pupproto, (struct sockaddr *)&pupsrc,
	  (struct sockaddr *)&pupdst);
}

/*
 * Encapsulate packet in PUP header which is supplied by the
 * user.  This is done to allow user to specify PUP identifier.
 */
rpup_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	register struct rawcb *rp = sotorawcb(so);
	register struct pup_header *pup;
	int len, error = 0;
	register struct mbuf *n, *last;
	struct sockaddr_pup *dst;
	struct ifnet *ifp;
	u_short *pc;

	/*
	 * Verify user has supplied necessary space
	 * for the header and check parameters in it.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof(struct pup_header)) &&
	    (m = m_pullup(m, sizeof(struct pup_header))) == 0) {
		error = EINVAL;
		goto bad;
	}
	pup = mtod(m, struct pup_header *);
	if (pup->pup_type == 0 || (pup->pup_tcontrol &~ PUP_TRACE)) {
		error = EINVAL;
		goto bad;
	}
	for (len = 0, n = last = m; n; last = n, n = n->m_next)
		len += n->m_len;
	/* assume user leaves space for checksum */
	if ((len & 1) || len < MINPUPSIZ || len > MAXPUPSIZ) {
		error = EMSGSIZE;
		goto bad;
	}
	pup->pup_length = htons((u_short)len);
	dst = (struct sockaddr_pup *)&rp->rcb_faddr;
	bcopy((caddr_t)&dst->spup_net, (caddr_t)&pup->pup_dnet,
	    sizeof (struct pupport));
	if (rp->rcb_route.ro_rt == 0)
		ifp = if_ifonnetof(dst->spup_net);
	else {
		rp->rcb_route.ro_rt->rt_use++;
		ifp = rp->rcb_route.ro_rt->rt_ifp;
	}
	if (ifp == 0) {
		error = ENETUNREACH;
		goto bad;
	}
	if (rp->rcb_flags & RAW_LADDR) {
		register struct sockaddr_pup *src;

		src = (struct sockaddr_pup *)&rp->rcb_laddr;
		bcopy((caddr_t)&src->spup_net, (caddr_t)&pup->pup_snet,
		    sizeof (struct pupport));
	} else {
		pup->pup_snet = ifp->if_net;
		pup->pup_shost = ifp->if_host[0];
		/* socket is specified by user */
	}
	/*
	 * Fill in checksum unless user indicates none should be specified.
	 */
	pc = (u_short *)(mtod(last, caddr_t) + last->m_len - sizeof (short));
	if (*pc != PUP_NOCKSUM)
		*pc = htons((u_short)pup_cksum(m, len - sizeof (short)));
	return ((*ifp->if_output)(ifp, m, (struct sockaddr *)dst));
bad:
	m_freem(m);
	return (error);
}
