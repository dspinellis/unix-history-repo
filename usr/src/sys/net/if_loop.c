/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_loop.c	7.4 (Berkeley) %G%
 */

/*
 * Loopback interface driver for protocol testing and timing.
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "errno.h"
#include "ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../machine/mtpr.h"

#ifdef	INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#define	LOMTU	(1024+512)

struct	ifnet loif;
int	looutput(), loioctl();

loattach()
{
	register struct ifnet *ifp = &loif;

	ifp->if_name = "lo";
	ifp->if_mtu = LOMTU;
	ifp->if_flags = IFF_LOOPBACK;
	ifp->if_ioctl = loioctl;
	ifp->if_output = looutput;
	if_attach(ifp);
}

looutput(ifp, m0, dst)
	struct ifnet *ifp;
	register struct mbuf *m0;
	struct sockaddr *dst;
{
	int s;
	register struct ifqueue *ifq;
	struct mbuf *m;

	/*
	 * Place interface pointer before the data
	 * for the receiving protocol.
	 */
	if (m0->m_off <= MMAXOFF &&
	    m0->m_off >= MMINOFF + sizeof(struct ifnet *)) {
		m0->m_off -= sizeof(struct ifnet *);
		m0->m_len += sizeof(struct ifnet *);
	} else {
		MGET(m, M_DONTWAIT, MT_HEADER);
		if (m == (struct mbuf *)0)
			return (ENOBUFS);
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct ifnet *);
		m->m_next = m0;
		m0 = m;
	}
	*(mtod(m0, struct ifnet **)) = ifp;
	s = splimp();
	ifp->if_opackets++;
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		ifq = &ipintrq;
		if (IF_QFULL(ifq)) {
			IF_DROP(ifq);
			m_freem(m0);
			splx(s);
			return (ENOBUFS);
		}
		IF_ENQUEUE(ifq, m0);
		schednetisr(NETISR_IP);
		break;
#endif
#ifdef NS
	case AF_NS:
		ifq = &nsintrq;
		if (IF_QFULL(ifq)) {
			IF_DROP(ifq);
			m_freem(m0);
			splx(s);
			return (ENOBUFS);
		}
		IF_ENQUEUE(ifq, m0);
		schednetisr(NETISR_NS);
		break;
#endif
	default:
		splx(s);
		printf("lo%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		m_freem(m0);
		return (EAFNOSUPPORT);
	}
	ifp->if_ipackets++;
	splx(s);
	return (0);
}

/*
 * Process an ioctl request.
 */
/* ARGSUSED */
loioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	int error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		/*
		 * Everything else is done at a higher level.
		 */
		break;

	default:
		error = EINVAL;
	}
	return (error);
}
