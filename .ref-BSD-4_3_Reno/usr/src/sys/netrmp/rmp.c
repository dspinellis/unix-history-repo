/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: rmp.c 1.3 89/06/07$
 *
 *	@(#)rmp.c	7.1 (Berkeley) 5/8/90
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
