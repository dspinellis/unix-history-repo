/*	uipc_usrreq.c	1.1	82/10/28	*/

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

/*
 * Unix communications domain.
 */

/*ARGSUSED*/
uipc_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct unpcb *unp = sotounpcb(so);
	register struct socket *so2;
	int error = 0;

	if (unp == 0 && req != PRU_ATTACH)
		return (EINVAL);			/* XXX */
	switch (req) {

	case PRU_ATTACH:
		if (unp) {
			error = EINVAL;
			break;
		}
		error = unp_attach(so, (struct sockaddr *)addr);
		break;

	case PRU_DETACH:
		unp_detach(unp);
		break;

	case PRU_CONNECT:
		error = unp_connect(so, (struct sockaddr_un *)addr);
		break;

	case PRU_DISCONNECT:
		unp_disconnect(unp);
		break;

/* BEGIN QUESTIONABLE */
	case PRU_ACCEPT: {
		struct sockaddr_un *soun = (struct sockaddr_un *)addr;

		if (soun) {
			bzero((caddr_t)soun, sizeof (*soun));
			soun->sun_family = AF_UNIX;
			/* XXX */
		}
		}
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		unp_usrclosed(unp);
		break;
/* END QUESTIONABLE */

	case PRU_RCVD:
		switch (so->so_type) {

		case SOCK_DGRAM:
			panic("uipc 1");

		case SOCK_STREAM: {
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
			}
			break;

		default:
			panic("uipc 2");
		}
		break;

	case PRU_SEND:
		switch (so->so_type) {

		case SOCK_DGRAM:
			if (addr) {
				if (unp->unp_conn) {
					error = EISCONN;
					break;
				}
				error = unp_connect(so, addr);
				if (error)
					break;
			} else {
				if (unp->unp_conn == 0) {
					error = ENOTCONN;
					break;
				}
			}
			so2 = unp->unp_conn->unp_socket;
			if (sbspace(&so2->so_rcv) > 0)		/* XXX */
				sbappendaddr(so2, m, addr);	/* XXX */
			if (addr)
				unp_disconnect(so);
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

unp_attach(so, soun)
	struct socket *so;
	struct sockaddr_un *soun;
{
	register struct unpcb *unp;
	struct mbuf *m;
	int error;
	
	error = soreserve(so, unp_sendspace, unp_recvspace);
	if (error)
		goto bad;
	m = m_getclr(M_DONTWAIT);
	if (m == 0) {
		error = ENOBUFS;
		goto bad;
	}
	unp = mtod(m, struct unpcb *);
	so->so_pcb = (caddr_t)unp;
	unp->unp_socket = so;
	if (soun) {
		error = unp_bind(unp, soun);
		if (error) {
			unp_detach(unp);
			goto bad;
		}
	}
	return (0);
bad:
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
		unp_drop(unp2, ECONNRESET);
		break;
	}
}

unp_abort(unp)
	struct unpcb *unp;
{

	unp_detach(unp);
}

unp_detach(unp)
	struct unpcb *unp;
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
	m_free(dtom(unp));
}

unp_usrclosed(unp)
	struct unpcb *unp;
{
	register struct socket *so = unp->unp_socket;

#ifdef sometimes /* ??? */
	soisdisconnected(unp->unp_socket);
#endif
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

unp_bind(unp, soun)
	struct unpcb *unp;
	struct sockaddr_un *soun;
{
	register struct inode *ip;
	int error;
	extern schar();

	u.u_dirp = soun->sun_path;
	soun->sun_path[sizeof(soun->sun_path)-1] = 0;
	ip = namei(schar, 1, 1);
	if (ip) {
		iput(ip);
		return (EEXIST);
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

unp_connect(so, soun)
	struct socket *so;
	struct sockaddr_un *soun;
{
	struct inode *ip;
	int error;

	u.u_dirp = soun->sun_path;
	soun->sun_path[sizeof(soun->sun_path)-1] = 0;
	ip = namei(schar, 0, 1);
	if (ip == 0) {
		error = u.u_error;
		u.u_error = 0;
		return (ENOENT);
	}
	error = unp_connectip(so, ip);
	return (error);
}

unp_connectip(so, ip)
	struct socket *so;
	struct inode *ip;
{
	struct unpcb *unp = sotounpcb(so);
	struct socket *so2, *so3;
	int error;
	struct unpcb *unp2;

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
		    (so3 = sonewconn(so2)) == 0) {
			error = ECONNREFUSED;
			goto bad;
		}
		unp->unp_conn = sotounpcb(so3);
		break;

	default:
		panic("uipc connip");
	}
	soisconnected(unp->unp_conn->unp_socket);
	soisconnected(so);
	iput(ip);
	return (0);
bad:
	iput(ip);
	return (error);
}
