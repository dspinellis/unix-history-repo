/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_usrreq.c	7.2 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#ifdef BSD4_3
#include "../net/if.h"
#include "ioctl.h"
#include "dir.h"
#include "user.h"
#include "stat.h"
#endif

#include "../netccitt/x25.h"
#include "../netccitt/pk.h"
#include "../netccitt/pk_var.h"

struct	x25_packet *pk_template ();

/*
 * 
 *  X.25 Packet level protocol interface to socket abstraction.
 *
 *  Process an X.25 user request on a logical channel.  If this is a send
 *  request then m is the mbuf chain of the send data. If this is a timer
 *  expiration (called from the software clock routine) them timertype is
 *  the particular timer.
 *
 */

pk_usrreq (so, req, m, nam, rights)
struct socket *so;
int req;
register struct mbuf *m, *nam;
struct mbuf *rights;
{
	register struct pklcd *lcp = (struct pklcd *) so -> so_pcb;
	register struct x25_packet *xp;
	register int s = splnet (), error = 0;

#ifdef BSD4_3
	if (req == PRU_CONTROL) {
		error = pk_control(so, (int)m, (caddr_t)nam,
			(struct ifnet *)rights);
		splx (s);
		return (error);
	}
#endif
	if (rights && rights -> m_len) {
		splx (s);
		return (EINVAL);
	}

/*
	pk_trace (pkcbhead, TR_USER, (struct pklcd *)0,
		req, (struct x25_packet *)0);
*/

	if (lcp == NULL && req != PRU_ATTACH) {
		splx (s);
		return (EINVAL);
	}

	switch (req) {
	/* 
	 *  X.25 attaches to socket via PRU_ATTACH and allocates a logical
	 *  channel descriptor.  If the socket is to  receive connections,
	 *  then the LISTEN state is entered.
	 */
	case PRU_ATTACH: 
		if (lcp) {
			error = EISCONN;
			/* Socket already connected. */
			break;
		}
		error = pk_attach (so);
		break;

	/* 
	 *  Detach a logical channel from the socket. If the state of the
	 *  channel is embryonic, simply discard it. Otherwise we have to 
	 *  initiate a PRU_DISCONNECT which will finish later.
	 */
	case PRU_DETACH: 
		pk_disconnect (lcp);
		break;

	/* 
	 *  Give the socket an address.
	 */
	case PRU_BIND: 
		if (nam -> m_len == sizeof (struct x25_sockaddr))
			old_to_new (nam);
		error = pk_bind (lcp, nam);
		break;

	/* 
	 *  Prepare to accept connections.
	 */
	case PRU_LISTEN: 
		if (lcp -> lcd_ceaddr == 0) {
			error = EDESTADDRREQ;
			break;
		}
		lcp -> lcd_state = LISTEN;
		lcp -> lcd_listen = pk_listenhead;
		pk_listenhead = lcp;
		break;

	/* 
	 *  Initiate a CALL REQUEST to peer entity. Enter state SENT_CALL
	 *  and mark the socket as connecting. Set timer waiting for 
	 *  CALL ACCEPT or CLEAR.
	 */
	case PRU_CONNECT: 
		if (nam -> m_len == sizeof (struct x25_sockaddr))
			old_to_new (nam);
		error = pk_connect (lcp, nam);
		break;

	/* 
	 *  Initiate a disconnect to peer entity via a CLEAR REQUEST packet.
	 *  The socket will be disconnected when we receive a confirmation
	 *  or a clear collision.
	 */
	case PRU_DISCONNECT: 
		pk_disconnect (lcp);
		break;

	/* 
	 *  Accept an INCOMING CALL. Most of the work has already been done
	 *  by pk_input. Just return the callers address to the user.
	 */
	case PRU_ACCEPT: 
		if (lcp -> lcd_craddr == NULL)
			break;
		bcopy ((caddr_t)lcp -> lcd_craddr, mtod (nam, caddr_t),
			sizeof (struct sockaddr_x25));
		nam -> m_len = sizeof (struct sockaddr_x25);
		if (lcp -> lcd_flags & X25_OLDSOCKADDR)
			new_to_old (nam);
		break;

	/* 
	 *  After a receive, we should send a RR.
	 */
	case PRU_RCVD: 
		lcp -> lcd_rxcnt++;
		lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_RR);
		pk_output (lcp);
		break;

	/* 
	 *  Do send by placing data on the socket output queue.
	 *  SHOULD WE USE m_cat HERE.
	 */
	case PRU_SEND: 
		error = pk_send (lcp, m);
		break;

	/* 
	 *  Abort a virtual circuit. For example all completed calls
	 *  waiting acceptance.
	 */
	case PRU_ABORT: 
		pk_disconnect (lcp);
		break;

	/* Begin unimplemented hooks. */

	case PRU_SHUTDOWN: 
		error = EOPNOTSUPP;
		break;

	case PRU_CONTROL: 
		error = EOPNOTSUPP;
		break;

	case PRU_SENSE: 
#ifdef BSD4_3
		((struct stat *)m) -> st_blksize = so -> so_snd.sb_hiwat;
#else
		error = EOPNOTSUPP;
#endif
		break;

	/* End unimplemented hooks. */

	case PRU_SOCKADDR: 
		if (lcp -> lcd_ceaddr == 0)
			return (EADDRNOTAVAIL);
		nam -> m_len = sizeof (struct sockaddr_x25);
		bcopy ((caddr_t)lcp -> lcd_ceaddr, mtod (nam, caddr_t),
			sizeof (struct sockaddr_x25));
		if (lcp -> lcd_flags & X25_OLDSOCKADDR)
			new_to_old (nam);
		break;

	case PRU_PEERADDR:
		if (lcp -> lcd_state != DATA_TRANSFER)
			return (ENOTCONN);
		nam -> m_len = sizeof (struct sockaddr_x25);
		bcopy (lcp -> lcd_craddr ? (caddr_t)lcp -> lcd_craddr :
			(caddr_t)lcp -> lcd_ceaddr,
			mtod (nam, caddr_t), sizeof (struct sockaddr_x25));
		if (lcp -> lcd_flags & X25_OLDSOCKADDR)
			new_to_old (nam);
		break;

	/* 
	 *  Receive INTERRUPT packet.
	 */
	case PRU_RCVOOB: 
		m -> m_len = 1;
		*mtod (m, char *) = lcp -> lcd_intrdata;
		break;

	/* 
	 *  Send INTERRUPT packet.
	 */
	case PRU_SENDOOB: 
		m_freem (m);
		if (lcp -> lcd_intrconf_pending) {
			error = ETOOMANYREFS;
			break;
		}
		lcp -> lcd_intrcnt++;
		xp = lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_INTERRUPT);
		xp -> packet_data = 0;
		(dtom (xp)) -> m_len++;
		pk_output (lcp);
		break;

	default: 
		panic ("pk_usrreq");
	}

	splx (s);
	return (error);
}

#ifdef BSD4_3
/*ARGSUSED*/
pk_control (so, cmd, data, ifp)
struct socket *so;
int cmd;
caddr_t data;
register struct ifnet *ifp;
{
	register struct ifreq *ifr = (struct ifreq *)data;
	register struct ifaddr *ifa = 0;
	register int error, s;
	struct sockaddr oldaddr;

	/*
	 * Find address for this interface, if it exists.
	 */
	if (ifp)
		for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next)
			if (ifa->ifa_addr.sa_family == AF_CCITT)
				break;

	switch (cmd) {
	case SIOCGIFADDR:
		if (ifa == 0)
			return (EADDRNOTAVAIL);
		ifr -> ifr_addr = ifa->ifa_addr;
		return (0);

	case SIOCSIFADDR:
		if (!suser())
			return (u.u_error);

		if (ifp == 0)
			panic("pk_control");
		if (ifa == (struct ifaddr *)0) {
			register struct ifaddr *ia;
			register struct mbuf *m;

			m = m_getclr(M_WAIT, MT_IFADDR);
			if (m == (struct mbuf *)NULL)
				return (ENOBUFS);
			ia = mtod(m, struct ifaddr *);
			if (ifa = ifp->if_addrlist) {
				for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
					;
				ifa->ifa_next = ia;
			} else
				ifp->if_addrlist = ia;
			ifa = ia;
			ifa->ifa_ifp = ifp;
		}
		oldaddr = ifa->ifa_addr;
		s = splimp();
		ifa->ifa_addr = ifr->ifr_addr;
		/*
		 * Give the interface a chance to initialize if this
		 * is its first address, and to validate the address.
		 */
		if (ifp->if_ioctl && (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ifa))) {
			splx(s);
			ifa->ifa_addr = oldaddr;
			return (error);
		}
		splx(s);
#ifndef WATERLOO
		(void) pk_accton ();
#endif
		return (0);

	default:
		if (ifp == 0 || ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
}
#endif

/*
 * Do an in-place conversion of an "old style"
 * socket address to the new style
 */

static
old_to_new (m)
register struct mbuf *m;
{
	register struct x25_sockaddr *oldp;
	register struct sockaddr_x25 *newp;
	register char *ocp, *ncp;
	struct sockaddr_x25 new;

	oldp = mtod (m, struct x25_sockaddr *);
	newp = &new;
	bzero ((caddr_t)newp, sizeof (*newp));

	newp -> x25_family = AF_CCITT;
	newp->x25_opts.op_flags = (oldp->xaddr_facilities & X25_REVERSE_CHARGE)
		| X25_MQBIT | X25_OLDSOCKADDR;
	if (oldp -> xaddr_facilities & XS_HIPRIO)	/* Datapac specific */
		newp -> x25_opts.op_psize = X25_PS128;
	bcopy ((caddr_t)oldp -> xaddr_addr, newp -> x25_addr,
		(unsigned)min (oldp -> xaddr_len, sizeof (newp -> x25_addr) - 1));
	bcopy ((caddr_t)oldp -> xaddr_proto, newp -> x25_udata, 4);
	newp -> x25_udlen = 4;

	ocp = (caddr_t)oldp -> xaddr_userdata;
	ncp = newp -> x25_udata + 4;
	while (*ocp && ocp < (caddr_t)oldp -> xaddr_userdata + 12) {
		*ncp++ = *ocp++;
		newp -> x25_udlen++;
	}

	bcopy ((caddr_t)newp, mtod (m, char *), sizeof (*newp));
	m->m_len = sizeof (*newp);
}

/*
 * Do an in-place conversion of a new style
 * socket address to the old style
 */

static
new_to_old (m)
register struct mbuf *m;
{
	register struct x25_sockaddr *oldp;
	register struct sockaddr_x25 *newp;
	register char *ocp, *ncp;
	struct x25_sockaddr old;

	oldp = &old;
	newp = mtod (m, struct sockaddr_x25 *);
	bzero ((caddr_t)oldp, sizeof (*oldp));

	oldp -> xaddr_facilities = newp -> x25_opts.op_flags & X25_REVERSE_CHARGE;
	if (newp -> x25_opts.op_psize == X25_PS128)
		oldp -> xaddr_facilities |= XS_HIPRIO;	/* Datapac specific */
	ocp = (char *)oldp -> xaddr_addr;
	ncp = newp -> x25_addr;
	while (*ncp) {
		*ocp++ = *ncp++;
		oldp -> xaddr_len++;
	}

	bcopy (newp -> x25_udata, (caddr_t)oldp -> xaddr_proto, 4);
	bcopy (newp -> x25_udata + 4, (caddr_t)oldp -> xaddr_userdata,
		(unsigned)(newp -> x25_udlen - 4));

	bcopy ((caddr_t)oldp, mtod (m, char *), sizeof (*oldp));
	m -> m_len = sizeof (*oldp);
}

pk_send (lcp, m)
register struct pklcd *lcp;
register struct mbuf *m;
{
	register struct x25_packet *xp;
	register struct mbuf *m0;
	register int len;

	m0 = dtom ((xp = pk_template (lcp -> lcd_lcn, X25_DATA)));
	m0 -> m_next = m;
	/*
	 * Application has elected (at call setup time) to prepend
	 * a control byte to each packet written indicating m-bit
	 * and q-bit status.  Examine and then discard this byte.
	 */
	if (lcp -> lcd_flags & X25_MQBIT) {
		register octet *cp;

		if (m -> m_len < 1) {
			m_freem (m0);
			return (EMSGSIZE);
		}
		cp = mtod (m, octet *);
		if (*cp & 0x80)					/* XXX */
			xp -> q_bit = 1;
		xp -> packet_type |= (*cp & 0x40) >> 2;		/* XXX */
		m -> m_len--;
		m -> m_off++;
	}
	len = m -> m_len;
	while (m -> m_next) {
		m = m -> m_next;
		len += m -> m_len;
	}
	if (len > (1 << lcp -> lcd_packetsize)) {
		m_freem (m0);
		return (EMSGSIZE);
	}

#ifdef BSD4_3
 	sbappendrecord (&lcp -> lcd_so -> so_snd, m0);
#else
	m -> m_act = (struct mbuf *) 1;
	sbappend (&lcp -> lcd_so -> so_snd, m0);
#endif
	lcp -> lcd_template = 0;
	lcp -> lcd_txcnt++;
	pk_output (lcp);
	return (0);
}
