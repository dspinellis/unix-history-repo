/*	uipc_pipe.c	4.3	81/11/18	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet_systm.h"		/* XXX */

int	piusrreq();
#define	PIPSIZ	4096

/*
 * Code for pipes and other loopback protocols (single machine, that is.)
 */
struct	protosw pipeproto = {
	SOCK_STREAM,	PF_LOCAL,	0,		PR_CONNREQUIRED,
	0,		0,		0,		0,
	piusrreq,	0,		0,
	0,		0,		0,		0
};

/*
 * Connect a pipe from wso to rso.  The protocol control block
 * for a pipe is used to store a pointer to the matching socket.
 * Each half of the pipe gets half of the buffer space (half send
 * buffers, half receive buffers).
 */
piconnect(wso, rso)
	struct socket *wso, *rso;
{

COUNT(PICONNECT);
	if (m_reserve(PIPSIZ) == 0) {
		u.u_error = ENOBUFS;
		return (0);
	}
	wso->so_proto = rso->so_proto = &pipeproto;
	wso->so_pcb = (caddr_t)rso;
	rso->so_pcb = (caddr_t)wso;
	wso->so_snd.sb_hiwat = PIPSIZ/2;
	wso->so_snd.sb_mbcnt = PIPSIZ;
	wso->so_state |= SS_ISCONNECTED|SS_CANTRCVMORE;
	rso->so_rcv.sb_hiwat = PIPSIZ/2;
	rso->so_rcv.sb_mbcnt = PIPSIZ;
	rso->so_state |= SS_ISCONNECTED|SS_CANTSENDMORE;
	return (1);
}

/*
 * User requests on pipes and other internally implemented
 * structures.
 */
/*ARGSUSED*/
piusrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct socket *so2 = (struct socket *)so->so_pcb;

COUNT(PIUSRREQ);
	switch (req) {

	case PRU_ATTACH:
	case PRU_DETACH:
		break;

	case PRU_CONNECT:
	case PRU_ACCEPT:
		return (EOPNOTSUPP);

	case PRU_DISCONNECT:
		if (so2 == 0)
			return (ENOTCONN);
		so->so_pcb = 0;
		soisdisconnected(so);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		if (so2)
			socantrcvmore(so2);
		break;

	case PRU_RCVD:
		if (so->so_rcv.sb_cc == 0 && so2 && so2->so_snd.sb_cc) {
			so->so_rcv.sb_cc = so2->so_snd.sb_cc;
			so->so_rcv.sb_mbcnt = so2->so_snd.sb_mbcnt;
			so->so_rcv.sb_mb = so2->so_rcv.sb_mb;
			so2->so_snd.sb_cc = 0;
			so2->so_snd.sb_mbcnt = 0;
			so2->so_snd.sb_mb = 0;
			sorwakeup(so);
			sowwakeup(so2);
		}
		break;

	case PRU_SEND:
		sbappend(&so->so_snd, m);
		sorwakeup(so2);
		break;

	case PRU_ABORT:
		return (EOPNOTSUPP);

	case PRU_CONTROL:
		return (EOPNOTSUPP);

	default:
		panic("piusrreq");
	}
	return (0);
}
