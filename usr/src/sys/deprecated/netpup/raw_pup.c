/*	raw_pup.c	4.3	82/02/15	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/pup.h"
#include "../net/raw_cb.h"
#include "/usr/include/errno.h"

/*
 * Raw PUP protocol interface.
 */

static struct sockaddr_pup pupsrc = { AF_PUP };
static struct sockaddr_pup pupdst = { AF_PUP };
static struct sockproto pupproto = { PF_PUP };

/*
 * Setup generic address and protocol structures
 * for raw_input routine, then pass them along with
 * mbuf chain.
 */
rpup_input(m)
	struct mbuf *m;
{
	register struct pup_header *pup = mtod(m, struct pup_header *);

COUNT(RAWPUP_INPUT);
	pupproto.sp_protocol = pup->pup_type;
	pupdst.spup_addr = pup->pup_daddr;
	pupsrc.spup_addr = pup->pup_saddr;
	raw_input(m, &pupproto, &pupdst, &pupsrc);
}

/*ARGSUSED*/
rpup_ctlinput(m)
	struct mbuf *m;
{
COUNT(RPUP_CTLINPUT);
}

/*
 * Encapsulate packet in PUP header which is supplied by the
 * user.  This is done to allow user to specify PUP identifier.
 */
rpup_output(m0, so)
	struct mbuf *m0;
	struct socket *so;
{
	register struct rawcb *rp = sotorawcb(so);
	register struct pup_header *pup;
	int len;
	struct mbuf *n;
	struct sockaddr_pup *spup;

COUNT(RPUP_OUTPUT);
	/*
	 * Verify user has supplied necessary space
	 * for the header and check parameters in it.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof(struct pup_header)) &&
	    (m = m_pullup(m, sizeof(struct pup_header)) == 0) {
		goto bad;
	pup = mtod(m, struct pup_header *);
	if (pup->pup_type == 0)
		goto bad;
	if (pup->pup_tcontrol && (pup->pup_tcontrol & ~PUP_TRACE))
		goto bad;
	for (len = 0, n = m; n; n = n->m_next)
		len += n->m_len;
	pup->pup_length = len;
	spup = (struct sockaddr_pup *)&rp->rcb_addr;
	pup->pup_dport = spup->spup_addr;

	/*
	 * Insure proper source address is included.
	 */
	spup = (struct sockadrr_pup *)rp->rcb_socket->so_addr;
	pup->pup_sport = spup->spup_addr;
	/* for now, assume user generates PUP checksum. */

	if (rp->rcb_pcb == 0)			/* XXX */
		panic("rawpup_output");
	return (enoutput((struct ifnet *)rp->rcb_pcb, m, PF_PUP));

bad:
	m_freem(m);
	return (0);
}

/*
 * Intercept connects and sends to verify interface
 * exists for destination address.  Disconnects are
 * also looked at to insure pointer is invalidated.
 */
rpup_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	register struct rawcb *rp = sotorawcb(so);

COUNT(RPUP_USRREQ);
	if (rp == 0 && req != PRU_ATTACH)
		return (EINVAL);

	switch (req) {

	/*
	 * Verify address has an interface to go with it
	 * and record information for use in output routine.
	 */
	case PRU_SEND:
	case PRU_CONNECT: {
		register struct sockaddr_pup *spup;
		register struct ifnet *ifp;

		spup = (struct sockaddr_pup *)addr;
		ifp = if_ifonnetof(spup->spup_addr);
		if (ifp == 0) {
			ifp = if_gatewayfor(spup->spup_addr);
			if (ifp == 0)
				return (EADDRNOTAVAIL);	/* XXX */
		}
		rp->rcb_pcb = (caddr_t)ifp;
		break;
		}

	case PRU_DISCONNECT:
		rp->rcb_pcb = 0;
		break;
	
	case PRU_CONTROL:
		return (EOPNOTSUPP);
	}
	return (raw_usrreq(so, req, m, addr));
}
