/*
 *	@(#)raw_hy.c	6.1	%G%
 *
 * 4.2 BSD Unix kernel - NSC HYPERchannel support
 * NEEDS WORK FOR 4.3
 *
 * $Header: raw_hy.c,v 3.1 84/02/15 04:27:44 steveg Exp $
 * $Locker:  $
 *
 * Copyright (c) 1984, Tektronix Inc.
 * All Rights Reserved
 *
 */


#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "protosw.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "if_hy.h"

/*
 * Raw interface to HYPERchannel.
 */

/*
 * Generate HYPERchannel leader and pass packet to hyoutput.
 * The user must create a skeletal leader in order to
 * communicate message type, message subtype, etc.
 * We don't really check the header supplied by the user.
 */
rhy_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	struct mbuf *n;
	int error = 0;
	register struct sockaddr_in *sin;
	register struct rawcb *rp = sotorawcb(so);
	struct ifnet *ifp;

	/*
	 * Verify user has supplied necessary space
	 * for the header.
	 */
	if ((m->m_off > MMAXOFF || m->m_len < sizeof(struct hym_hdr)) &&
	    (m = m_pullup(m, sizeof(struct hym_hdr))) == 0) {
		error = EMSGSIZE;	/* XXX */
		goto bad;
	}

	sin = (struct sockaddr_in *)&rp->rcb_faddr;
	/* no routing here */
	ifp = if_ifonnetof((int)sin->sin_addr.s_net);
	if (ifp)
		return (hyoutput(ifp, m, (struct sockaddr *)sin));
	error = ENETUNREACH;
bad:
	m_freem(m);
	return (error);
}
