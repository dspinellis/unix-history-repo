/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
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
 *	@(#)raw_usrreq.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"

#include "if.h"
#include "route.h"
#include "netisr.h"
#include "raw_cb.h"

#include "machine/mtpr.h"

/*
 * Initialize raw connection block q.
 */
raw_init()
{

	rawcb.rcb_next = rawcb.rcb_prev = &rawcb;
	rawintrq.ifq_maxlen = IFQ_MAXLEN;
}


/*
 * Raw protocol input routine.  Find the socket
 * associated with the packet(s) and move them over.  If
 * nothing exists for this packet, drop it.
 */
/*
 * Raw protocol interface.
 */
raw_input(m0, proto, src, dst)
	struct mbuf *m0;
	struct sockproto *proto;
	struct sockaddr *src, *dst;
{
	register struct rawcb *rp;
	register struct mbuf *m = m0;
	struct socket *last;

	last = 0;
	for (rp = rawcb.rcb_next; rp != &rawcb; rp = rp->rcb_next) {
		if (rp->rcb_proto.sp_family != proto->sp_family)
			continue;
		if (rp->rcb_proto.sp_protocol  &&
		    rp->rcb_proto.sp_protocol != proto->sp_protocol)
			continue;
		/*
		 * We assume the lower level routines have
		 * placed the address in a canonical format
		 * suitable for a structure comparison.
		 *
		 * Note that if the lengths are not the same
		 * the comparison will fail at the first byte.
		 */
#define	equal(a1, a2) \
  (bcmp((caddr_t)(a1), (caddr_t)(a2), a1->sa_len) == 0)
		if (rp->rcb_laddr && !equal(rp->rcb_laddr, dst))
			continue;
		if (rp->rcb_faddr && !equal(rp->rcb_faddr, src))
			continue;
		if (last) {
			struct mbuf *n;
			if (n = m_copy(m, 0, (int)M_COPYALL)) {
				if (sbappendaddr(&last->so_rcv, src,
				    n, (struct mbuf *)0) == 0)
					/* should notify about lost packet */
					m_freem(n);
				else
					sorwakeup(last);
			}
		}
		last = rp->rcb_socket;
	}
	if (last) {
		if (sbappendaddr(&last->so_rcv, src,
		    m, (struct mbuf *)0) == 0)
			m_freem(m);
		else
			sorwakeup(last);
	} else
		m_freem(m);
}

/*ARGSUSED*/
raw_ctlinput(cmd, arg)
	int cmd;
	struct sockaddr *arg;
{

	if (cmd < 0 || cmd > PRC_NCMDS)
		return;
	/* INCOMPLETE */
}

/*ARGSUSED*/
raw_usrreq(so, req, m, nam, rights, control)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights, *control;
{
	register struct rawcb *rp = sotorawcb(so);
	register int error = 0;
	int len;

	if (req == PRU_CONTROL)
		return (EOPNOTSUPP);
	if (rights && rights->m_len) {
		error = EOPNOTSUPP;
		goto release;
	}
	if (rp == 0) {
		error = EINVAL;
		goto release;
	}
	switch (req) {

	/*
	 * Allocate a raw control block and fill in the
	 * necessary info to allow packets to be routed to
	 * the appropriate raw interface routine.
	 */
	case PRU_ATTACH:
		if ((so->so_state & SS_PRIV) == 0) {
			error = EACCES;
			break;
		}
		error = raw_attach(so, (int)nam);
		break;

	/*
	 * Destroy state just before socket deallocation.
	 * Flush data or not depending on the options.
	 */
	case PRU_DETACH:
		if (rp == 0) {
			error = ENOTCONN;
			break;
		}
		raw_detach(rp);
		break;

#ifdef notdef
	/*
	 * If a socket isn't bound to a single address,
	 * the raw input routine will hand it anything
	 * within that protocol family (assuming there's
	 * nothing else around it should go to). 
	 */
	case PRU_CONNECT:
		if (rp->rcb_faddr) {
			error = EISCONN;
			break;
		}
		nam = m_copym(nam, 0, M_COPYALL, M_WAIT);
		rp->rcb_faddr = mtod(nam, struct sockaddr *);
		soisconnected(so);
		break;

	case PRU_BIND:
		if (rp->rcb_laddr) {
			error = EINVAL;			/* XXX */
			break;
		}
		error = raw_bind(so, nam);
		break;
#endif

	case PRU_CONNECT2:
		error = EOPNOTSUPP;
		goto release;

	case PRU_DISCONNECT:
		if (rp->rcb_faddr == 0) {
			error = ENOTCONN;
			break;
		}
		raw_disconnect(rp);
		soisdisconnected(so);
		break;

	/*
	 * Mark the connection as being incapable of further input.
	 */
	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	/*
	 * Ship a packet out.  The appropriate raw output
	 * routine handles any massaging necessary.
	 */
	case PRU_SEND:
		if (nam) {
			if (rp->rcb_faddr) {
				error = EISCONN;
				break;
			}
			rp->rcb_faddr = mtod(nam, struct sockaddr *);
		} else if (rp->rcb_faddr == 0) {
			error = ENOTCONN;
			break;
		}
		error = (*so->so_proto->pr_output)(m, so);
		m = NULL;
		if (nam)
			rp->rcb_faddr = 0;
		break;

	case PRU_ABORT:
		raw_disconnect(rp);
		sofree(so);
		soisdisconnected(so);
		break;

	case PRU_SENSE:
		/*
		 * stat: don't bother with a blocksize.
		 */
		return (0);

	/*
	 * Not supported.
	 */
	case PRU_RCVOOB:
	case PRU_RCVD:
		return(EOPNOTSUPP);

	case PRU_LISTEN:
	case PRU_ACCEPT:
	case PRU_SENDOOB:
		error = EOPNOTSUPP;
		break;

	case PRU_SOCKADDR:
		if (rp->rcb_laddr == 0) {
			error = EINVAL;
			break;
		}
		len = rp->rcb_laddr->sa_len;
		bcopy((caddr_t)rp->rcb_laddr, mtod(nam, caddr_t), (unsigned)len);
		nam->m_len = len;
		break;

	case PRU_PEERADDR:
		if (rp->rcb_faddr == 0) {
			error = ENOTCONN;
			break;
		}
		len = rp->rcb_faddr->sa_len;
		bcopy((caddr_t)rp->rcb_faddr, mtod(nam, caddr_t), (unsigned)len);
		nam->m_len = len;
		break;

	default:
		panic("raw_usrreq");
	}
release:
	if (m != NULL)
		m_freem(m);
	return (error);
}

rawintr() {} /* XXX - referenced by locore.  will soon go away */
