/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: rmp.c 1.3 89/06/07$
 *
 *	@(#)rmp.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../netrmp/rmp.h"
#include "../netrmp/rmp_var.h"

/*
**  rmp_output: route packet to proper network interface.
*/

rmp_output(m, so)
struct mbuf *m;
struct socket *so;
{
	struct ifnet *ifp;
	struct rawcb *rp = sotorawcb(so);
	struct rmp_packet *rmp;

	/*
	 *  Convert the mbuf back to an RMP packet so we can get the
	 *  address of the "ifnet struct" specifying the interface it
	 *  should go out on.
	 */
	rmp = mtod(m, struct rmp_packet *);
	ifp = rmp->ifp;

	/*
	 *  Strip off the "ifnet struct ptr" from the packet leaving
	 *  us with a complete IEEE 802.2 packet.
	 */
	m_adj(m, sizeof(struct ifnet *));

	/*
	 *  Send the packet.
	 */
	return ((*ifp->if_output) (ifp, m, &rp->rcb_faddr));
}
