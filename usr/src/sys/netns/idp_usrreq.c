/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)idp_usrreq.c	6.3 (Berkeley) %G%
 */

#include "param.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "stat.h"

#include "../net/if.h"
#include "../net/route.h"

#include "ns.h"
#include "ns_pcb.h"
#include "idp.h"
#include "idp_var.h"
#include "ns_error.h"

/*
 * IDP protocol implementation.
 */

struct	sockaddr_ns idp_ns = { AF_NS };

idp_input(m, nsp)
	struct mbuf *m;
	register struct nspcb *nsp;
{
	register struct idp *idp = mtod(m, struct idp *);
	if (nsp==0)
		panic("Impossible idp_input");

	/*
	 * Construct sockaddr format source address.
	 * Stuff source address and datagram in user buffer.
	 */
	idp_ns.sns_addr = idp->idp_sna;
	nsp->nsp_rpt = idp->idp_pt;
	if ( ! (nsp->nsp_flags & NSP_RAWIN) ) {
		m->m_len -= sizeof (struct idp);
		m->m_off += sizeof (struct idp);
	}
	if (sbappendaddr(&nsp->nsp_socket->so_rcv, (struct sockaddr *)&idp_ns,
	    m, (struct mbuf *)0) == 0)
		goto bad;
	sorwakeup(nsp->nsp_socket);
	return;
bad:
	m_freem(m);
}

idp_abort(nsp)
	struct nspcb *nsp;
{
	struct socket *so = nsp->nsp_socket;

	ns_pcbdisconnect(nsp);
	soisdisconnected(so);
}

idp_output(nsp, m0)
	struct nspcb *nsp;
	struct mbuf *m0;
{
	register struct mbuf *m;
	register struct idp *idp;
	register struct socket *so;
	register int len = 0;
	register struct route *ro;
	struct mbuf *mprev;
	extern int idpcksum;

	/*
	 * Calculate data length.
	 */
	for (m = m0; m; m = m->m_next) {
		mprev = m;
		len += m->m_len;
	}
	/*
	 * Make sure packet is actually of even length.
	 */
	
	if (len & 1) {
		m = mprev;
		if (m->m_len + m->m_off < MMAXOFF) {
			m->m_len++;
		} else {
			struct mbuf *m1 = m_get(M_DONTWAIT, MT_DATA);

			if (m1 == 0) {
				m_freem(m0);
				return (ENOBUFS);
			}
			m1->m_len = 1;
			m1->m_off = MMAXOFF - 1;
			* mtod(m1, char *) = 0;
			m->m_next = m1;
		}
	}

	/*
	 * Fill in mbuf with extended IDP header
	 * and addresses and length put into network format.
	 */
	if (nsp->nsp_flags & NSP_RAWOUT) {
		m = m0;
		idp = mtod(m, struct idp *);
	} else {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m_freem(m0);
			return (ENOBUFS);
		}
		m->m_off = MMAXOFF - sizeof (struct idp);
		m->m_len = sizeof (struct idp);
		m->m_next = m0;
		idp = mtod(m, struct idp *);
		idp->idp_tc = 0;
		idp->idp_pt = nsp->nsp_dpt;
		idp->idp_sna = nsp->nsp_laddr;
		idp->idp_dna = nsp->nsp_faddr;
		len += sizeof (struct idp);
	}

	idp->idp_len = htons((u_short)len);

	if (idpcksum) {
		idp->idp_sum = 0;
		len = ((len - 1) | 1) + 1;
		idp->idp_sum = ns_cksum(m, len);
	} else
		idp->idp_sum = 0xffff;

	/*
	 * Output datagram.
	 */
	so = nsp->nsp_socket;
	if (so->so_options & SO_DONTROUTE)
		return (ns_output(m, (struct route *)0,
		    (so->so_options & SO_BROADCAST) | NS_ROUTETOIF));
	/*
	 * Use cached route for previous datagram if
	 * this is also to the same destination. 
	 *
	 * NB: We don't handle broadcasts because that
	 *     would require 3 subroutine calls.
	 */
	ro = &nsp->nsp_route;
	if (ro->ro_rt &&
		((*(long *)&nsp->nsp_lastnet)!=ns_netof(idp->idp_dna)) &&
		!(ns_hosteq(satons_addr(ro->ro_dst), idp->idp_dna))) {
		RTFREE(ro->ro_rt);
		ro->ro_rt = (struct rtentry *)0;
		nsp->nsp_lastnet = idp->idp_dna.x_net;
	}
	return (ns_output(m, ro, so->so_options & SO_BROADCAST));
}
/*ARGSUSED*/
idp_ctloutput(req, so, level, name, value)
	int req, level;
	struct socket *so;
	int name;
	struct mbuf **value;
{
	register struct mbuf *m;
	struct nspcb *nsp = sotonspcb(so);
	int mask, error = 0;

	if (nsp == NULL) {
		error = EINVAL;
		goto release;
	}

	switch (req) {
	case PRCO_GETOPT:
		if (value==NULL) {
			error = EINVAL;
			goto release;
		}
		m = m_get(M_DONTWAIT, MT_DATA);
		switch (name) {
		case SO_HEADERS_ON_INPUT:
			mask = NSP_RAWIN;
			goto get_flags;
		case SO_HEADERS_ON_OUTPUT:
			mask = NSP_RAWOUT;
		get_flags:
			m->m_len = sizeof(short);
			m->m_off = MMAXOFF - sizeof(short);
			*mtod(m, short *) = nsp->nsp_flags & mask;
			break;
		case SO_DEFAULT_HEADERS:
			m->m_len = sizeof(struct idp);
			m->m_off = MMAXOFF - sizeof(struct idp);
			{
				register struct idp *idp = mtod(m, struct idp *);
				idp->idp_len = 0;
				idp->idp_sum = 0;
				idp->idp_tc = 0;
				idp->idp_pt = nsp->nsp_dpt;
				idp->idp_dna = nsp->nsp_faddr;
				idp->idp_sna = nsp->nsp_laddr;
			}
		}
		*value = m;
		break;
	case PRCO_SETOPT:
		switch (name) {
			int mask, *ok;

		case SO_HEADERS_ON_INPUT:
			mask = NSP_RAWIN;
			goto set_head;
		case SO_HEADERS_ON_OUTPUT:
			mask = NSP_RAWOUT;
		set_head:
			if (value && *value) {
				ok = mtod(*value, int *);
				if (*ok)
					nsp->nsp_flags |= mask;
				else
					nsp->nsp_flags &= ~mask;
			} else error = EINVAL;
			break;
		case SO_DEFAULT_HEADERS:
			{
				register struct idp *idp
				    = mtod(*value, struct idp *);
				nsp->nsp_dpt = idp->idp_pt;
			}
#ifdef NSIP
			break;
		case SO_NSIP_ROUTE:
			error = nsip_route(*value);
			break;
#endif NSIP
		}
		if (value && *value)
			m_freem(*value);
		break;
	}
	release:
		return(error);
}

/*ARGSUSED*/
idp_usrreq(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	struct nspcb *nsp = sotonspcb(so);
	int error = 0;

	if (req == PRU_CONTROL)
                return (ns_control(so, (int)m, (caddr_t)nam,
			(struct ifnet *)rights));
	if (rights && rights->m_len) {
		error = EINVAL;
		goto release;
	}
	if (nsp == NULL && req != PRU_ATTACH) {
		error = EINVAL;
		goto release;
	}
	switch (req) {

	case PRU_ATTACH:
		if (nsp != NULL) {
			error = EINVAL;
			break;
		}
		error = ns_pcballoc(so, &nspcb);
		if (error)
			break;
		error = soreserve(so, 2048, 2048);
		if (error)
			break;
		break;

	case PRU_DETACH:
		if (nsp == NULL) {
			error = ENOTCONN;
			break;
		}
		ns_pcbdetach(nsp);
		break;

	case PRU_BIND:
		error = ns_pcbbind(nsp, nam);
		break;

	case PRU_LISTEN:
		error = EOPNOTSUPP;
		break;

	case PRU_CONNECT:
		if (!ns_nullhost(nsp->nsp_faddr)) {
			error = EISCONN;
			break;
		}
		error = ns_pcbconnect(nsp, nam);
		if (error == 0)
			soisconnected(so);
		break;

	case PRU_CONNECT2:
		error = EOPNOTSUPP;
		break;

	case PRU_ACCEPT:
		error = EOPNOTSUPP;
		break;

	case PRU_DISCONNECT:
		if (ns_nullhost(nsp->nsp_faddr)) {
			error = ENOTCONN;
			break;
		}
		ns_pcbdisconnect(nsp);
		soisdisconnected(so);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	case PRU_SEND:
	{
		struct ns_addr laddr;
		int s;

		if (nam) {
			laddr = nsp->nsp_laddr;
			if (!ns_nullhost(nsp->nsp_faddr)) {
				error = EISCONN;
				break;
			}
			/*
			 * Must block input while temporarily connected.
			 */
			s = splnet();
			error = ns_pcbconnect(nsp, nam);
			if (error) {
				splx(s);
				break;
			}
		} else {
			if (ns_nullhost(nsp->nsp_faddr)) {
				error = ENOTCONN;
				break;
			}
		}
		error = idp_output(nsp, m);
		m = NULL;
		if (nam) {
			ns_pcbdisconnect(nsp);
			splx(s);
			nsp->nsp_laddr.x_host = laddr.x_host;
			nsp->nsp_laddr.x_port = laddr.x_port;
		}
	}
		break;

	case PRU_ABORT:
		ns_pcbdetach(nsp);
		sofree(so);
		soisdisconnected(so);
		break;

	case PRU_SOCKADDR:
		ns_setsockaddr(nsp, nam);
		break;

	case PRU_PEERADDR:
		ns_setpeeraddr(nsp, nam);
		break;

	case PRU_SENSE:
		/*
		 * stat: don't bother with a blocksize.
		 */
		return (0);

	case PRU_SENDOOB:
	case PRU_FASTTIMO:
	case PRU_SLOWTIMO:
	case PRU_PROTORCV:
	case PRU_PROTOSEND:
		error =  EOPNOTSUPP;
		break;

	case PRU_CONTROL:
	case PRU_RCVD:
	case PRU_RCVOOB:
		return (EOPNOTSUPP);	/* do not free mbuf's */

	default:
		panic("idp_usrreq");
	}
release:
	if (m != NULL)
		m_freem(m);
	return (error);
}
/*ARGSUSED*/
idp_raw_usrreq(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	int error = 0;
	struct nspcb *nsp = sotonspcb(so);
	extern struct nspcb nsrawpcb;

	switch (req) {

	case PRU_ATTACH:

		if (!suser() || (nsp != NULL)) {
			error = EINVAL;
			break;
		}
		error = ns_pcballoc(so, &nsrawpcb);
		if (error)
			break;
		error = soreserve(so, 2048, 2048);
		if (error)
			break;
		nsp = sotonspcb(so);
		nsp->nsp_faddr.x_host = ns_broadhost;
		nsp->nsp_flags = NSP_RAWIN | NSP_RAWOUT;
		break;
	default:
		error = idp_usrreq(so, req, m, nam, rights);
	}
	return(error);
}

