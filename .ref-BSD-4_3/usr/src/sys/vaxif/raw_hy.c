/*
 *	@(#)raw_hy.c	7.1	6/5/86
 *
 * 4.3 BSD Unix kernel - NSC HYPERchannel support
 *
 * $Header: raw_hy.c,v 3.1 84/02/15 04:27:44 steveg Exp $
 * $Locker:  $
 *
 * Copyright (c) 1984, Tektronix Inc.
 * All Rights Reserved
 *
 */

#include "hy.h"
#if NHY > 0

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
#include "../netinet/in_var.h"
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
	int error = 0;
	register struct sockaddr_in *sin;
	register struct rawcb *rp = sotorawcb(so);
	struct in_ifaddr *ia;

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
	ia = in_iaonnetof(in_netof(sin->sin_addr));
	if (ia)
		return (hyoutput(ia->ia_ifp, m, (struct sockaddr *)sin));
	error = ENETUNREACH;
bad:
	m_freem(m);
	return (error);
}
#endif
