/*	uipc_pipe.c	4.10	81/12/02	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in_systm.h"		/* XXX */

int	piusrreq();
#define	PIPSIZ	4096

/*
 * Code for pipes and other loopback protocols (single machine, that is.)
 */
struct	protosw pipeproto = {
	SOCK_STREAM,	PF_UNIX,	0,		PR_CONNREQUIRED|PR_WANTRCVD,
	0,		0,		0,		0,
	piusrreq,
	0,		0,		0,		0
};

/*
 * Connect a pipe from wso to rso.  The protocol control block
 * for a pipe is used to store a pointer to the matching socket.
 */
piconnect(wso, rso)
	struct socket *wso, *rso;
{

COUNT(PICONNECT);
	if (m_reserve(PIPSIZ*2/MSIZE) == 0) {
		u.u_error = ENOBUFS;
		return (0);
	}
	wso->so_proto = rso->so_proto = &pipeproto;
	wso->so_pcb = (caddr_t)rso;
	rso->so_pcb = (caddr_t)wso;
	wso->so_snd.sb_hiwat = PIPSIZ;
	wso->so_snd.sb_mbmax = 2*PIPSIZ;
	wso->so_state |= SS_ISCONNECTED|SS_CANTRCVMORE;
	rso->so_rcv.sb_hiwat = 0;
	rso->so_rcv.sb_mbmax = 0;
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
		break;

	case PRU_DETACH:
		so->so_pcb = 0;
		break;

	case PRU_CONNECT:
	case PRU_ACCEPT:
		return (EOPNOTSUPP);

	case PRU_DISCONNECT:
		if (so2 == 0)
			return (ENOTCONN);
		so->so_pcb = 0;
		so2->so_pcb = 0;
		soisdisconnected(so);
		soisdisconnected(so2);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		if (so2)
			socantrcvmore(so2);
		break;

	case PRU_RCVD:
		if (so2 == 0)
			break;
#define	rcv (&so->so_rcv)
#define snd (&so2->so_snd)
		/*
		 * Transfer resources back to send port
		 * and wakeup any waiting to write.
		 */
		snd->sb_mbmax += rcv->sb_mbmax - rcv->sb_mbcnt;
		rcv->sb_mbmax = rcv->sb_mbcnt;
		snd->sb_hiwat += rcv->sb_hiwat - rcv->sb_cc;
		rcv->sb_hiwat = rcv->sb_cc;
		sbwakeup(snd);
#undef snd
#undef rcv
		break;

	case PRU_SEND:
#define	rcv (&so2->so_rcv)
#define	snd (&so->so_snd)
		if (so2 == 0)
			panic("pipe send");
		/*
		 * Send to paired receive port, and then
		 * give it enough resources to hold what it already has.
		 * Wake up readers.
		 */
		sbappend(rcv, m);
		snd->sb_mbmax -= rcv->sb_mbcnt - rcv->sb_mbmax;
		rcv->sb_mbmax = rcv->sb_mbcnt;
		snd->sb_hiwat -= rcv->sb_cc - rcv->sb_hiwat;
		rcv->sb_hiwat = rcv->sb_cc;
		sbwakeup(rcv);
#undef snd
#undef rcv
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

psndrcv(snd, rcv)
	struct sockbuf *snd, *rcv;
{

	printf("snd: (cc,hiwat,mbcnt,mbmax) (%d,%d,%d,%d) ",
	    snd->sb_cc, snd->sb_hiwat, snd->sb_mbcnt, snd->sb_mbmax);
	printf("m %x, m->m_len %d\n", snd->sb_mb, snd->sb_mb ? snd->sb_mb->m_len : 0);
	printf("rcv: (cc,hiwat,mbcnt,mbmax) (%d,%d,%d,%d) ",
	    rcv->sb_cc, rcv->sb_hiwat, rcv->sb_mbcnt, rcv->sb_mbmax);
	printf("m %x, m->m_len %d\n", rcv->sb_mb, rcv->sb_mb ? rcv->sb_mb->m_len : 0);
}
