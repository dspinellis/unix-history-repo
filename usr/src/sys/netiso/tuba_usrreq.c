/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_usrreq.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"
#include "stat.h"

#include "net/if.h"
#include "net/route.h"

#include "in.h"
#include "in_systm.h"
#include "ip.h"
#include "in_pcb.h"
#include "ip_var.h"
#include "tcp.h"
#include "tcp_fsm.h"
#include "tcp_seq.h"
#include "tcp_timer.h"
#include "tcp_var.h"
#include "tcpip.h"
#include "tcp_debug.h"

#include "netiso/argo_debug.h"
#include "netiso/iso.h"
#include "netiso/clnp.h"
#include "netiso/iso_pcb.h"
#include "netiso/iso_var.h"
/*
 * TCP protocol interface to socket abstraction.
 */
extern	char *tcpstates[];
extern	struct inpcb tcb;
struct	isopcb tuba_isopcb;

/*
 * Process a TCP user request for TCP tb.  If this is a send request
 * then m is the mbuf chain of send data.  If this is a timer expiration
 * (called from the software clock routine), then timertype tells which timer.
 */
/*ARGSUSED*/
tuba_usrreq(so, req, m, nam, control)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *control;
{
	register struct inpcb *inp;
	register struct isopcb *isop;
	register struct tcpcb *tp;
	int s;
	int error = 0;
	int ostate;
	struct sockaddr_iso siso;

	if (req == PRU_CONTROL)
		return (iso_control(so, (int)m, (caddr_t)nam,
			(struct ifnet *)control));

	s = splnet();
	inp = sotoinpcb(so);
	/*
	 * When a TCP is attached to a socket, then there will be
	 * a (struct inpcb) pointed at by the socket, and this
	 * structure will point at a subsidary (struct tcpcb).
	 */
	if (inp == 0  && req != PRU_ATTACH) {
		splx(s);
		return (EINVAL);		/* XXX */
	}
	if (inp) {
		tp = inpcbtotcpcb(inp);
		if (tp == 0)
			panic("tuba_usrreq");
		ostate = tp->t_state;
		isop = tp->tp_tuba_pcb;
		if (isop == 0)
			panic("tuba_usrreq 2");
	} else
		ostate = 0;
	switch (req) {

	/*
	 * TCP attaches to socket via PRU_ATTACH, reserving space,
	 * and an internet control block.  We also need to
	 * allocate an isopcb and separate the control block from
	 * tcp/ip ones.
	 */
	case PRU_ATTACH:
		if (error = iso_pcballoc(so, &tuba_isopcb))
			break;
		isop = (struct isopcb *) tp->tp_tuba_pcb = so->so_pcb;
		if (error = tcp_userreq(so, req, m, nam, control)) {
			isop->isop_socket = 0;
			isop_detach(isop);
		}
		goto notrace;

	/*
	 * PRU_DETACH detaches the TCP protocol from the socket.
	 * If the protocol state is non-embryonic, then can't
	 * do this directly: have to initiate a PRU_DISCONNECT,
	 * which may finish later; embryonic TCB's can just
	 * be discarded here.
	 */
	case PRU_DETACH:
		if (tp->t_state > TCPS_LISTEN)
			tp = tcp_disconnect(tp);
		else
			tp = tcp_close(tp);
		if (tp == 0)
			tuba_pcbdetach(isop);
		break;

	/*
	 * Give the socket an address.
	 */
	case PRU_BIND:
		siso = mtod(nam, struct sockaddr_iso *);
		if (siso->siso_tlen && siso->siso_tlen != 2) {
			error = EINVAL;
			break;
		}
		if ((error = iso_pcbbind(isop, nam)) || 
		    (siso = isop->isop_laddr) == 0)
			break;
		bcopy(TSEL(siso), &inp->inp_lport, 2);
		if (siso->siso_nlen &&
		    !(inp->inp_laddr.s_addr = tuba_lookup(&siso->siso_addr)))
			error = ENOBUFS;
		break;

	/*
	 * Prepare to accept connections.
	 */
	case PRU_CONNECT:
	case PRU_LISTEN:
		if (inp->inp_lport == 0 &&
		    (error = iso_pcbbind(isop, (struct mbuf *)0)))
			break;
		bcopy(TSEL(isop->isop_laddr), &inp->inp_lport, 2);
		if (cmd == PRU_LISTEN) {
			tp->t_state = TCPS_LISTEN;
			break;
		}
	/*FALLTHROUGH*/
	/*
	 * Initiate connection to peer.
	 * Create a template for use in transmissions on this connection.
	 * Enter SYN_SENT state, and mark socket as connecting.
	 * Start keep-alive timer, and seed output sequence space.
	 * Send initial segment on connection.
	 */
	/* case PRU_CONNECT: */
		if (error = iso_pcbconnect(isop, nam))
			break;
		siso = mtod(nam, struct sockaddr_iso *);
		if (!(inp->inp_faddr.s_addr = tuba_lookup(&siso->siso_addr))) {
		unconnect:
			iso_pcbdisconnect(isop);
			error = ENOBUFS;
			break;
		}
		bcopy(TSEL(isop->isop_faddr), &inp->inp_fport, 2);
		if ((inp->inp_laddr.s_addr == 0 &&
		     (inp->inp_laddr.s_addr = 
			    tuba_lookup(&isop->isop_laddr->siso_addr)) == 0)
			goto unconnect;
		if ((tp->t_template = tcp_template(tp)) == 0)
			goto unconnect;
		soisconnecting(so);
		tcpstat.tcps_connattempt++;
		tp->t_state = TCPS_SYN_SENT;
		tp->t_timer[TCPT_KEEP] = TCPTV_KEEP_INIT;
		tp->iss = tcp_iss; tcp_iss += TCP_ISSINCR/2;
		tcp_sendseqinit(tp);
		error = tcp_output(tp);
		tuba_refcnt(isop, 1);
		break;

	/*
	 * Initiate disconnect from peer.
	 * If connection never passed embryonic stage, just drop;
	 * else if don't need to let data drain, then can just drop anyways,
	 * else have to begin TCP shutdown process: mark socket disconnecting,
	 * drain unread data, state switch to reflect user close, and
	 * send segment (e.g. FIN) to peer.  Socket will be really disconnected
	 * when peer sends FIN and acks ours.
	 *
	 * SHOULD IMPLEMENT LATER PRU_CONNECT VIA REALLOC TCPCB.
	 */
	case PRU_DISCONNECT:
		if ((tp = tcp_disconnect(tp)) == 0)
			tuba_pcbdetach(isop);
		break;

	/*
	 * Accept a connection.  Essentially all the work is
	 * done at higher levels; just return the address
	 * of the peer, storing through addr.
	 */
	case PRU_ACCEPT:
		bcopy((caddr_t)isop->isop_faddr, mtod(m, caddr_t),
			nam->m_len = isop->isop_faddr->siso_len);
		break;

	/*
	 * Mark the connection as being incapable of further output.
	 */
	case PRU_SHUTDOWN:
		socantsendmore(so);
		tp = tcp_usrclosed(tp);
		if (tp)
			error = tcp_output(tp);
		else
			tuba_pcbdetach(isop);
		break;
	/*
	 * Abort the TCP.
	 */
	case PRU_ABORT:
		if ((tp = tcp_drop(tp, ECONNABORTED)) == 0)
			tuba_pcbdetach(isop);
		break;


	case PRU_SOCKADDR:
		if (isop->isop_laddr)
			bcopy((caddr_t)isop->isop_laddr, mtod(m, caddr_t),
				nam->m_len = isop->isop_laddr->siso_len);
		break;

	case PRU_PEERADDR:
		if (isop->isop_faddr)
			bcopy((caddr_t)isop->isop_faddr, mtod(m, caddr_t),
				nam->m_len = isop->isop_faddr->siso_len);
		break;

	default:
		error = tcp_usrreq(so, req, m, nam, control);
		goto notrace;
	}
	if (tp && (so->so_options & SO_DEBUG))
		tcp_trace(TA_USER, ostate, tp, (struct tcpiphdr *)0, req);
notrace:
	splx(s);
	return(error);
}

tuba_ctloutput(op, so, level, optname, mp)
	int op;
	struct socket *so;
	int level, optname;
	struct mbuf **mp;
{
	int clnp_ctloutput(), tcp_ctloutput();

	return ((level != IPPROTO_TCP ? clnp_ctloutput : tcp_ctloutput)
		(clnp_ctloutput(op, so, level, optname, mp)));
}
