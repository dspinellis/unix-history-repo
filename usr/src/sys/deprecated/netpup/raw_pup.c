/*	raw_pup.c	4.17	83/05/30	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"

#include "../netpup/pup.h"
#include "../net/raw_cb.h"

/*
 * Raw PUP protocol interface.
 */

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
	u_short *pchecksum;

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
	pup->pup_length = htons(len);
	dst = (struct sockaddr_pup *)&rp->rcb_faddr;
	bcopy((caddr_t)dst->spup_net, (caddr_t)pup->pup_dnet,
	    sizeof (struct pupport));
	ifp = if_ifonnetof((u_int)pup->pup_dnet);
	if (ifp == 0) {
		error = ENETUNREACH;
		goto bad;
	}
	if (rp->rcb_flags & RAW_LADDR) {
		register struct sockaddr_pup *src;

		src = (struct sockaddr_pup *)&rp->rcb_laddr;
		bcopy((caddr_t)src->spup_net, (caddr_t)pup->pup_snet,
		    sizeof (struct pupport));
	} else {
		pup->pup_snet = ifp->if_net;
		pup->pup_shost = ifp->if_host[0];
		/* socket is specified by user */
	}
	/*
	 * Fill in checksum unless user indicates none should be specified.
	 */
	pchecksum =
	    (u_short *)(mtod(last, caddr_t) + last->m_len - sizeof (short));
	if (*pchecksum != PUP_NOCKSUM)
		*pchecksum = pup_cksum(m, len - sizeof (short));
	return ((*ifp->if_output)(ifp, m, (struct sockaddr *)dst));
bad:
	m_freem(m);
	return (error);
}
