/*	uipc_usrreq.c	1.6	83/01/04	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/unpcb.h"
#include "../h/un.h"
#include "../h/inode.h"
#include "../h/nami.h"

/*
 * Unix communications domain.
 */

/*ARGSUSED*/
uipc_usrreq(so, req, m, nam, opt)
	struct socket *so;
	int req;
	struct mbuf *m, *nam;
	struct socketopt *opt;
{
	struct unpcb *unp = sotounpcb(so);
	register struct socket *so2;
	int error = 0;

	if (unp == 0 && req != PRU_ATTACH)
		return (EINVAL);			/* XXX */
	switch (req) {

	case PRU_ATTACH:
		if (unp) {
			error = EISCONN;
			break;
		}
		error = unp_attach(so);
		break;

	case PRU_DETACH:
		unp_detach(unp);
		break;

	case PRU_BIND:
		error = unp_bind(unp, nam);
		break;

	case PRU_LISTEN:
		if (unp->unp_inode == 0)
			error = EINVAL;
		break;

	case PRU_CONNECT:
		error = unp_connect(so, nam);
		break;

	case PRU_DISCONNECT:
		unp_disconnect(unp);
		break;

	case PRU_ACCEPT:
		nam->m_len = unp->unp_remaddr->m_len;
		bcopy(mtod(unp->unp_remaddr, caddr_t),
		    mtod(nam, caddr_t), (unsigned)nam->m_len);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		unp_usrclosed(unp);
		break;

	case PRU_RCVD:
		switch (so->so_type) {

		case SOCK_DGRAM:
			panic("uipc 1");
			/*NOTREACHED*/

		case SOCK_STREAM:
#define	rcv (&so->so_rcv)
#define snd (&so2->so_snd)
			if (unp->unp_conn == 0)
				break;
			so2 = unp->unp_conn->unp_socket;
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

		default:
			panic("uipc 2");
		}
		break;

	case PRU_SEND:
		switch (so->so_type) {

		case SOCK_DGRAM:
			if (nam) {
				if (unp->unp_conn) {
					error = EISCONN;
					break;
				}
				error = unp_connect(so, nam);
				if (error)
					break;
			} else {
				if (unp->unp_conn == 0) {
					error = ENOTCONN;
					break;
				}
			}
			so2 = unp->unp_conn->unp_socket;
			/* BEGIN XXX */
			if (sbspace(&so2->so_rcv) > 0)
				(void) sbappendaddr(&so2->so_rcv,
					mtod(nam, struct sockaddr *), m);
			/* END XXX */
			if (nam)
				unp_disconnect(unp);
			break;

		case SOCK_STREAM:
#define	rcv (&so2->so_rcv)
#define	snd (&so->so_snd)
			if (unp->unp_conn == 0)
				panic("uipc 3");
			so2 = unp->unp_conn->unp_socket;
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

		default:
			panic("uipc 4");
		}
		break;

	case PRU_ABORT:
		unp_drop(unp, ECONNABORTED);
		break;

/* SOME AS YET UNIMPLEMENTED HOOKS */
	case PRU_CONTROL:
		error = EOPNOTSUPP;
		break;

	case PRU_SENSE:
		error = EOPNOTSUPP;
		break;
/* END UNIMPLEMENTED HOOKS */

	case PRU_RCVOOB:
		break;

	case PRU_SENDOOB:
		break;

	case PRU_SOCKADDR:
		break;

	case PRU_SLOWTIMO:
		break;

	default:
		panic("piusrreq");
	}
	return (0);
}

int	unp_sendspace = 1024*2;
int	unp_recvspace = 1024*2;

unp_attach(so)
	struct socket *so;
{
	register struct mbuf *m;
	register struct unpcb *unp;
	int error;
	
	error = soreserve(so, unp_sendspace, unp_recvspace);
	if (error)
		return (error);
	m = m_getclr(M_DONTWAIT, MT_PCB);
	if (m == NULL)
		return (ENOBUFS);
	unp = mtod(m, struct unpcb *);
	so->so_pcb = (caddr_t)unp;
	unp->unp_socket = so;
	return (0);
}

unp_detach(unp)
	register struct unpcb *unp;
{
	
	if (unp->unp_inode) {
		irele(unp->unp_inode);
		unp->unp_inode = 0;
	}
	if (unp->unp_conn)
		unp_disconnect(unp);
	while (unp->unp_refs)
		unp_drop(unp->unp_refs, ECONNRESET);
	soisdisconnected(unp->unp_socket);
	unp->unp_socket->so_pcb = 0;
	m_freem(unp->unp_remaddr);
	(void) m_free(dtom(unp));
}

unp_bind(unp, nam)
	struct unpcb *unp;
	struct mbuf *nam;
{
	struct sockaddr_un *soun = mtod(nam, struct sockaddr_un *);
	register struct inode *ip;
	extern schar();
	int error;

	u.u_dirp = soun->sun_path;
	soun->sun_path[sizeof(soun->sun_path)-1] = 0;
	ip = namei(schar, CREATE, 1);
	if (ip) {
		iput(ip);
		return (EADDRINUSE);
	}
	ip = maknode(IFSOCK | 0777);
	if (ip == NULL) {
		error = u.u_error;		/* XXX */
		u.u_error = 0;			/* XXX */
		return (error);
	}
	ip->i_socket = unp->unp_socket;
	unp->unp_inode = ip;
	iunlock(ip);			/* but keep reference */
	return (0);
}

unp_connect(so, nam)
	struct socket *so;
	struct mbuf *nam;
{
	register struct sockaddr_un *soun = mtod(nam, struct sockaddr_un *);
	struct unpcb *unp = sotounpcb(so);
	register struct inode *ip;
	int error;
	struct socket *so2;
	struct unpcb *unp2;

	u.u_dirp = soun->sun_path;
	soun->sun_path[sizeof(soun->sun_path)-1] = 0;
	ip = namei(schar, LOOKUP, 1);
	if (ip == 0) {
		error = u.u_error;
		u.u_error = 0;
		return (error);		/* XXX */
	}
	if ((ip->i_mode&IFMT) != IFSOCK) {
		error = ENOTSOCK;
		goto bad;
	}
	so2 = ip->i_socket;
	if (so2 == 0) {
		error = ECONNREFUSED;
		goto bad;
	}
	if (so2->so_type != so->so_type) {
		error = EPROTOTYPE;
		goto bad;
	}
	switch (so->so_type) {

	case SOCK_DGRAM:
		unp->unp_conn = sotounpcb(so2);
		unp2 = sotounpcb(so2);
		unp->unp_nextref = unp2->unp_refs;
		unp2->unp_refs = unp;
		break;

	case SOCK_STREAM:
		if ((so2->so_options&SO_ACCEPTCONN) == 0 ||
		    (so2 = sonewconn(so2)) == 0) {
			error = ECONNREFUSED;
			goto bad;
		}
		unp2 = sotounpcb(so2);
		unp->unp_conn = unp2;
		unp2->unp_conn = unp;
		unp2->unp_remaddr = m_copy(nam, 0, (int)M_COPYALL);
		break;

	default:
		panic("uipc connip");
	}
	soisconnected(so2);
	soisconnected(so);
	iput(ip);
	return (0);
bad:
	iput(ip);
	return (error);
}

unp_disconnect(unp)
	struct unpcb *unp;
{
	register struct unpcb *unp2 = unp->unp_conn;

	if (unp2 == 0)
		return;
	unp->unp_conn = 0;
	soisdisconnected(unp->unp_socket);
	switch (unp->unp_socket->so_type) {

	case SOCK_DGRAM:
		if (unp2->unp_refs == unp)
			unp2->unp_refs = unp->unp_nextref;
		else {
			unp2 = unp2->unp_refs;
			for (;;) {
				if (unp2 == 0)
					panic("unp_disconnect");
				if (unp2->unp_nextref == unp)
					break;
				unp2 = unp2->unp_nextref;
			}
			unp2->unp_nextref = unp->unp_nextref;
		}
		unp->unp_nextref = 0;
		break;

	case SOCK_STREAM:
		unp2->unp_conn = 0;
		soisdisconnected(unp2->unp_socket);
		break;
	}
}

unp_abort(unp)
	struct unpcb *unp;
{

	unp_detach(unp);
}

/*ARGSUSED*/
unp_usrclosed(unp)
	struct unpcb *unp;
{

}

unp_drop(unp, errno)
	struct unpcb *unp;
	int errno;
{

	unp->unp_socket->so_error = errno;
	unp_disconnect(unp);
}

unp_drain()
{

}
