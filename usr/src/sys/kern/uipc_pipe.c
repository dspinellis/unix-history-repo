/*	uipc_pipe.c	4.2	81/11/16	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protocol.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/inaddr.h"

int	pi_usrreq();
#define	PIPSIZ	4096

/*
 * Code for pipes and other loopback protocols (single machine, that is.)
 */
struct	protosw pipeproto = {
	SOCK_STREAM,	PF_LOCAL,	0,		PR_CONNREQUIRED,
	0,		0,		0,		0,
	pi_usrreq,	0,		0,
	0,		0,		0,		0
};

/*
 * Connect a pipe from wso to rso.  The protocol control block
 * for a pipe is used to store a pointer to the matching socket.
 * Each half of the pipe gets half of the buffer space (half send
 * buffers, half receive buffers).
 */
pi_connect(wso, rso)
	struct socket *wso, *rso;
{

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

pi_splice(pso, so)
	struct socket *pso, *so;
{

	if (pso->so_proto != &pipeproto) {
		struct socket *tso;
		tso = pso; pso = so; so = tso;
	}
	if (pso->so_proto != &pipeproto)
		return (EOPNOTSUPP);
	/* check types and buffer space */
	/* merge buffers */
	return (0);
}

/*
 * User requests on pipes and other internally implemented
 * structures.
 */
/*ARGSUSED*/
pi_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct socket *so2 = (struct socket *)so->so_pcb;

	switch (req) {

	case PRU_ATTACH:
	case PRU_DETACH:
		break;

	case PRU_CONNECT:
		return (EOPNOTSUPP);

	case PRU_DISCONNECT:
		if (so2 == 0)
			return (ENOTCONN);
		so->so_pcb = 0;
		soisdisconnected(so);
		break;

	case PRU_FLUSH:
		return (EOPNOTSUPP);

	case PRU_SHUTDOWN:
		so->so_state |= SS_CANTSENDMORE;
		sowwakeup(so);
		if (so2) {
			so2->so_state |= SS_CANTRCVMORE;
			sorwakeup(so2);
		}
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
		panic("pi_usrreq");
	}
	return (0);
}
