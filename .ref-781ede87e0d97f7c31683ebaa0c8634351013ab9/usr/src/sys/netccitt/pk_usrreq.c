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
 *	@(#)pk_usrreq.c	7.9 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "protosw.h"
#include "socketvar.h"
#include "errno.h"
#include "ioctl.h"
#include "user.h"
#include "stat.h"

#include "../net/if.h"

#include "x25.h"
#include "pk.h"
#include "pk_var.h"

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

pk_usrreq (so, req, m, nam, control)
struct socket *so;
int req;
register struct mbuf *m, *nam;
struct mbuf *control;
{
	register struct pklcd *lcp = (struct pklcd *) so -> so_pcb;
	register struct x25_packet *xp;
	register int error = 0;

	if (req == PRU_CONTROL)
		return (pk_control(so, (int)m, (caddr_t)nam,
			(struct ifnet *)control));
	if (control && control->m_len) {
		error = EINVAL;
		goto release;
	}
	if (lcp == NULL && req != PRU_ATTACH) {
		error = EINVAL;
		goto release;
	}

/*
	pk_trace (pkcbhead, TR_USER, (struct pklcd *)0,
		req, (struct x25_packet *)0);
*/

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
		lcp = pk_attach (so);
		if (lcp == 0)
			error = ENOBUFS;
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
		error = pk_connect (lcp, nam, (struct sockaddr_25 *)0);
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
		if (lcp -> lcd_intrconf_pending) {
			error = ETOOMANYREFS;
			break;
		}
		lcp -> lcd_intrcnt++;
		xp = lcp -> lcd_template = pk_template (lcp -> lcd_lcn, X25_INTERRUPT);
		xp -> packet_data = 0;
		(dtom (xp)) -> m_len++;
		pk_output (lcp);
		m_freem (m);
		break;

	default: 
		panic ("pk_usrreq");
	}
release:
	if (control != NULL)
		m_freem(control);
	return (error);
}

/* 
 * If you want to use UBC X.25 level 3 in conjunction with some
 * other X.25 level 2 driver, have the ifp->if_ioctl routine
 * assign pk_start to pkp -> pk_start when called with SIOCSIFCONF_X25.
 */
/* ARGSUSED */
pk_start (lcp)
register struct pklcd *lcp;
{
	extern int pk_send();

	lcp -> lcd_send = pk_send;
	return (pk_output(lcp));
}

/*ARGSUSED*/
pk_control (so, cmd, data, ifp)
struct socket *so;
int cmd;
caddr_t data;
register struct ifnet *ifp;
{
	register struct ifreq_x25 *ifr = (struct ifreq_x25 *)data;
	register struct ifaddr *ifa = 0;
	register struct x25_ifaddr *ia = 0;
	struct pklcd *dev_lcp = 0;
	int error, s, old_maxlcn;
	unsigned n;

	/*
	 * Find address for this interface, if it exists.
	 */
	if (ifp)
		for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next)
			if (ifa->ifa_addr->sa_family == AF_CCITT)
				break;

	ia = (struct x25_ifaddr *)ifa;
	switch (cmd) {
	case SIOCGIFCONF_X25:
		if (ifa == 0)
			return (EADDRNOTAVAIL);
		ifr->ifr_xc = ia->ia_xc;
		return (0);

	case SIOCSIFCONF_X25:
		if (error = suser(u.u_cred, &u.u_acflag))
			return (error);
		if (ifp == 0)
			panic("pk_control");
		if (ifa == (struct ifaddr *)0) {
			register struct mbuf *m;

			MALLOC(ia, struct x25_ifaddr *, sizeof (*ia),
				M_IFADDR, M_WAITOK);
			if (ia == 0)
				return (ENOBUFS);
			bzero((caddr_t)ia, sizeof (*ia));
			if (ifa = ifp->if_addrlist) {
				for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
					;
				ifa->ifa_next = &ia->ia_ifa;
			} else
				ifp->if_addrlist = &ia->ia_ifa;
			ifa = &ia->ia_ifa;
			ifa->ifa_netmask = (struct sockaddr *)&ia->ia_sockmask;
			ifa->ifa_addr = (struct sockaddr *)&ia->ia_xc.xc_addr;
			ia->ia_xcp = &ia->ia_xc;
			ia->ia_ifp = ifp;
			ia->ia_pkcb.pk_ia = ia;
			ia->ia_pkcb.pk_next = pkcbhead;
			ia->ia_pkcb.pk_state = DTE_WAITING;
			ia->ia_pkcb.pk_start = pk_start;
			pkcbhead = &ia->ia_pkcb;
		}
		old_maxlcn = ia->ia_maxlcn;
		ia->ia_xc = ifr->ifr_xc;
		if (ia->ia_chan && (ia->ia_maxlcn != old_maxlcn)) {
			pk_restart(&ia->ia_pkcb, X25_RESTART_NETWORK_CONGESTION);
			dev_lcp = ia->ia_chan[0];
			free((caddr_t)ia->ia_chan, M_IFADDR);
			ia->ia_chan = 0;
		}
		if (ia->ia_chan == 0) {
			n = (ia->ia_maxlcn + 1) * sizeof(struct pklcd *);
			ia->ia_chan = (struct pklcd **) malloc(n, M_IFADDR, M_WAITOK);
			if (ia->ia_chan) {
				bzero((caddr_t)ia->ia_chan, n);
				if (dev_lcp == 0)
					dev_lcp = pk_attach((struct socket *)0);
				ia->ia_chan[0] = dev_lcp;
				dev_lcp->lcd_state = READY;
				dev_lcp->lcd_pkp = &ia->ia_pkcb;
			} else {
				if (dev_lcp)
					pk_close(dev_lcp);
				return (ENOBUFS);
			}
		}
		/*
		 * Give the interface a chance to initialize if this
		 * is its first address, and to validate the address.
		 */
		s = splimp();
		if (ifp->if_ioctl)
			error = (*ifp->if_ioctl)(ifp, SIOCSIFCONF_X25, ifa);
		if (error)
			ifp->if_flags &= ~IFF_UP;
		splx(s);
		return (error);

	default:
		if (ifp == 0 || ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
}

pk_ctloutput(cmd, so, level, optname, mp)
struct socket *so;
struct mbuf **mp;
int cmd, level, optname;
{
	register struct mbuf *m = *mp;
	int error;

	if (cmd == PRCO_SETOPT) switch (optname) {
	case PK_ACCTFILE:
		if (m == 0)
			return (EINVAL);
		if (m->m_len)
			error = pk_accton(mtod(m, char *));
		else
			error = pk_accton((char *)0);
		(void) m_freem(m);
		*mp = 0;
		return (error);
	}
	if (*mp) {
		(void) m_freem(*mp);
		*mp = 0;
	}
	return (EOPNOTSUPP);

}

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
struct pklcd *lcp;
register struct mbuf *m;
{
	int mqbit = 0, error;

	/*
	 * Application has elected (at call setup time) to prepend
	 * a control byte to each packet written indicating m-bit
	 * and q-bit status.  Examine and then discard this byte.
	 */
	if (lcp -> lcd_flags & X25_MQBIT) {
		if (m -> m_len < 1) {
			m_freem (m);
			return (EMSGSIZE);
		}
		mqbit = *(mtod(m, u_char *));
		m -> m_len--;
		m -> m_data++;
		m -> m_pkthdr.len--;
	}
	if ((error = pk_fragment(lcp, m, mqbit & 0x80, mqbit &0x40, 1)) == 0)
		error = pk_output (lcp);
	return (error);
}
