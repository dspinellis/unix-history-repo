/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *	@(#)uipc_usrreq.c	7.7 (Berkeley) 6/29/88
 */

#include "param.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "unpcb.h"
#include "un.h"
#include "inode.h"
#include "file.h"
#include "stat.h"

/*
 * Unix communications domain.
 *
 * TODO:
 *	SEQPACKET, RDM
 *	rethink name space problems
 *	need a proper out-of-band
 */
struct	sockaddr sun_noname = { AF_UNIX };
ino_t	unp_ino;			/* prototype for fake inode numbers */

/*ARGSUSED*/
uipc_usrreq(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	struct unpcb *unp = sotounpcb(so);
	register struct socket *so2;
	int error = 0;

	if (req == PRU_CONTROL)
		return (EOPNOTSUPP);
	if (req != PRU_SEND && rights && rights->m_len) {
		error = EOPNOTSUPP;
		goto release;
	}
	if (unp == 0 && req != PRU_ATTACH) {
		error = EINVAL;
		goto release;
	}
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

	case PRU_CONNECT2:
		error = unp_connect2(so, (struct socket *)nam);
		break;

	case PRU_DISCONNECT:
		unp_disconnect(unp);
		break;

	case PRU_ACCEPT:
		/*
		 * Pass back name of connected socket,
		 * if it was bound and we are still connected
		 * (our peer may have closed already!).
		 */
		if (unp->unp_conn && unp->unp_conn->unp_addr) {
			nam->m_len = unp->unp_conn->unp_addr->m_len;
			bcopy(mtod(unp->unp_conn->unp_addr, caddr_t),
			    mtod(nam, caddr_t), (unsigned)nam->m_len);
		} else {
			nam->m_len = sizeof(sun_noname);
			*(mtod(nam, struct sockaddr *)) = sun_noname;
		}
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
			 * Adjust backpressure on sender
			 * and wakeup any waiting to write.
			 */
			snd->sb_mbmax += unp->unp_mbcnt - rcv->sb_mbcnt;
			unp->unp_mbcnt = rcv->sb_mbcnt;
			snd->sb_hiwat += unp->unp_cc - rcv->sb_cc;
			unp->unp_cc = rcv->sb_cc;
			sowwakeup(so2);
#undef snd
#undef rcv
			break;

		default:
			panic("uipc 2");
		}
		break;

	case PRU_SEND:
		if (rights) {
			error = unp_internalize(rights);
			if (error)
				break;
		}
		switch (so->so_type) {

		case SOCK_DGRAM: {
			struct sockaddr *from;

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
			if (unp->unp_addr)
				from = mtod(unp->unp_addr, struct sockaddr *);
			else
				from = &sun_noname;
			if (sbspace(&so2->so_rcv) > 0 &&
			    sbappendaddr(&so2->so_rcv, from, m, rights)) {
				sorwakeup(so2);
				m = 0;
			} else
				error = ENOBUFS;
			if (nam)
				unp_disconnect(unp);
			break;
		}

		case SOCK_STREAM:
#define	rcv (&so2->so_rcv)
#define	snd (&so->so_snd)
			if (so->so_state & SS_CANTSENDMORE) {
				error = EPIPE;
				break;
			}
			if (unp->unp_conn == 0)
				panic("uipc 3");
			so2 = unp->unp_conn->unp_socket;
			/*
			 * Send to paired receive port, and then reduce
			 * send buffer hiwater marks to maintain backpressure.
			 * Wake up readers.
			 */
			if (rights)
				(void)sbappendrights(rcv, m, rights);
			else
				sbappend(rcv, m);
			snd->sb_mbmax -=
			    rcv->sb_mbcnt - unp->unp_conn->unp_mbcnt;
			unp->unp_conn->unp_mbcnt = rcv->sb_mbcnt;
			snd->sb_hiwat -= rcv->sb_cc - unp->unp_conn->unp_cc;
			unp->unp_conn->unp_cc = rcv->sb_cc;
			sorwakeup(so2);
			m = 0;
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

	case PRU_SENSE:
		((struct stat *) m)->st_blksize = so->so_snd.sb_hiwat;
		if (so->so_type == SOCK_STREAM && unp->unp_conn != 0) {
			so2 = unp->unp_conn->unp_socket;
			((struct stat *) m)->st_blksize += so2->so_rcv.sb_cc;
		}
		((struct stat *) m)->st_dev = NODEV;
		if (unp->unp_ino == 0)
			unp->unp_ino = unp_ino++;
		((struct stat *) m)->st_ino = unp->unp_ino;
		return (0);

	case PRU_RCVOOB:
		return (EOPNOTSUPP);

	case PRU_SENDOOB:
		error = EOPNOTSUPP;
		break;

	case PRU_SOCKADDR:
		if (unp->unp_addr) {
			nam->m_len = unp->unp_addr->m_len;
			bcopy(mtod(unp->unp_addr, caddr_t),
			    mtod(nam, caddr_t), (unsigned)nam->m_len);
		} else
			nam->m_len = 0;
		break;

	case PRU_PEERADDR:
		if (unp->unp_conn && unp->unp_conn->unp_addr) {
			nam->m_len = unp->unp_conn->unp_addr->m_len;
			bcopy(mtod(unp->unp_conn->unp_addr, caddr_t),
			    mtod(nam, caddr_t), (unsigned)nam->m_len);
		} else
			nam->m_len = 0;
		break;

	case PRU_SLOWTIMO:
		break;

	default:
		panic("piusrreq");
	}
release:
	if (m)
		m_freem(m);
	return (error);
}

/*
 * Both send and receive buffers are allocated PIPSIZ bytes of buffering
 * for stream sockets, although the total for sender and receiver is
 * actually only PIPSIZ.
 * Datagram sockets really use the sendspace as the maximum datagram size,
 * and don't really want to reserve the sendspace.  Their recvspace should
 * be large enough for at least one max-size datagram plus address.
 */
#define	PIPSIZ	4096
u_long	unpst_sendspace = PIPSIZ;
u_long	unpst_recvspace = PIPSIZ;
u_long	unpdg_sendspace = 2*1024;	/* really max datagram size */
u_long	unpdg_recvspace = 4*1024;

int	unp_rights;			/* file descriptors in flight */

unp_attach(so)
	struct socket *so;
{
	register struct mbuf *m;
	register struct unpcb *unp;
	int error;
	
	if (so->so_snd.sb_hiwat == 0 || so->so_rcv.sb_hiwat == 0) {
		switch (so->so_type) {

		case SOCK_STREAM:
			error = soreserve(so, unpst_sendspace, unpst_recvspace);
			break;

		case SOCK_DGRAM:
			error = soreserve(so, unpdg_sendspace, unpdg_recvspace);
			break;
		}
		if (error)
			return (error);
	}
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
		unp->unp_inode->i_socket = 0;
		irele(unp->unp_inode);
		unp->unp_inode = 0;
	}
	if (unp->unp_conn)
		unp_disconnect(unp);
	while (unp->unp_refs)
		unp_drop(unp->unp_refs, ECONNRESET);
	soisdisconnected(unp->unp_socket);
	unp->unp_socket->so_pcb = 0;
	m_freem(unp->unp_addr);
	(void) m_free(dtom(unp));
	if (unp_rights)
		unp_gc();
}

unp_bind(unp, nam)
	struct unpcb *unp;
	struct mbuf *nam;
{
	struct sockaddr_un *soun = mtod(nam, struct sockaddr_un *);
	register struct inode *ip;
	register struct nameidata *ndp = &u.u_nd;
	int error;

	ndp->ni_dirp = soun->sun_path;
	if (unp->unp_inode != NULL || nam->m_len == MLEN)
		return (EINVAL);
	*(mtod(nam, caddr_t) + nam->m_len) = 0;
/* SHOULD BE ABLE TO ADOPT EXISTING AND wakeup() ALA FIFO's */
	ndp->ni_nameiop = CREATE | FOLLOW;
	ndp->ni_segflg = UIO_SYSSPACE;
	ip = namei(ndp);
	if (ip) {
		iput(ip);
		return (EADDRINUSE);
	}
	if (error = u.u_error) {
		u.u_error = 0;			/* XXX */
		return (error);
	}
	ip = maknode(IFSOCK | 0777, ndp);
	if (ip == NULL) {
		error = u.u_error;		/* XXX */
		u.u_error = 0;			/* XXX */
		return (error);
	}
	ip->i_socket = unp->unp_socket;
	unp->unp_inode = ip;
	unp->unp_addr = m_copy(nam, 0, (int)M_COPYALL);
	iunlock(ip);			/* but keep reference */
	return (0);
}

unp_connect(so, nam)
	struct socket *so;
	struct mbuf *nam;
{
	register struct sockaddr_un *soun = mtod(nam, struct sockaddr_un *);
	register struct inode *ip;
	register struct socket *so2, *so3;
	register struct nameidata *ndp = &u.u_nd;
	struct unpcb *unp2, *unp3;
	int error;

	ndp->ni_dirp = soun->sun_path;
	if (nam->m_len + (nam->m_off - MMINOFF) == MLEN)
		return (EMSGSIZE);
	*(mtod(nam, caddr_t) + nam->m_len) = 0;
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_SYSSPACE;
	ip = namei(ndp);
	if (ip == 0) {
		error = u.u_error;
		u.u_error = 0;
		return (error);		/* XXX */
	}
	if (access(ip, IWRITE)) {
		error = u.u_error;
		u.u_error = 0; 		/* XXX */
		goto bad;
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
	if (so->so_type != so2->so_type) {
		error = EPROTOTYPE;
		goto bad;
	}
	if (so->so_proto->pr_flags & PR_CONNREQUIRED) {
		if ((so2->so_options & SO_ACCEPTCONN) == 0 ||
		    (so3 = sonewconn(so2)) == 0) {
			error = ECONNREFUSED;
			goto bad;
		}
		unp2 = sotounpcb(so2);
		unp3 = sotounpcb(so3);
		if (unp2->unp_addr)
			unp3->unp_addr = m_copy(unp2->unp_addr, 0, M_COPYALL);
		so2 = so3;
	}
	error = unp_connect2(so, so2);
bad:
	iput(ip);
	return (error);
}

unp_connect2(so, so2)
	register struct socket *so;
	register struct socket *so2;
{
	register struct unpcb *unp = sotounpcb(so);
	register struct unpcb *unp2;

	if (so2->so_type != so->so_type)
		return (EPROTOTYPE);
	unp2 = sotounpcb(so2);
	unp->unp_conn = unp2;
	switch (so->so_type) {

	case SOCK_DGRAM:
		unp->unp_nextref = unp2->unp_refs;
		unp2->unp_refs = unp;
		soisconnected(so);
		break;

	case SOCK_STREAM:
		unp2->unp_conn = unp;
		soisconnected(so2);
		soisconnected(so);
		break;

	default:
		panic("unp_connect2");
	}
	return (0);
}

unp_disconnect(unp)
	struct unpcb *unp;
{
	register struct unpcb *unp2 = unp->unp_conn;

	if (unp2 == 0)
		return;
	unp->unp_conn = 0;
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
		unp->unp_socket->so_state &= ~SS_ISCONNECTED;
		break;

	case SOCK_STREAM:
		soisdisconnected(unp->unp_socket);
		unp2->unp_conn = 0;
		soisdisconnected(unp2->unp_socket);
		break;
	}
}

#ifdef notdef
unp_abort(unp)
	struct unpcb *unp;
{

	unp_detach(unp);
}
#endif

/*ARGSUSED*/
unp_usrclosed(unp)
	struct unpcb *unp;
{

}

unp_drop(unp, errno)
	struct unpcb *unp;
	int errno;
{
	struct socket *so = unp->unp_socket;

	so->so_error = errno;
	unp_disconnect(unp);
	if (so->so_head) {
		so->so_pcb = (caddr_t) 0;
		m_freem(unp->unp_addr);
		(void) m_free(dtom(unp));
		sofree(so);
	}
}

#ifdef notdef
unp_drain()
{

}
#endif

unp_externalize(rights)
	struct mbuf *rights;
{
	int newfds = rights->m_len / sizeof (int);
	register int i;
	register struct file **rp = mtod(rights, struct file **);
	register struct file *fp;
	int f;

	if (newfds > ufavail()) {
		for (i = 0; i < newfds; i++) {
			fp = *rp;
			unp_discard(fp);
			*rp++ = 0;
		}
		return (EMSGSIZE);
	}
	for (i = 0; i < newfds; i++) {
		f = ufalloc(0);
		if (f < 0)
			panic("unp_externalize");
		fp = *rp;
		u.u_ofile[f] = fp;
		fp->f_msgcount--;
		unp_rights--;
		*(int *)rp++ = f;
	}
	return (0);
}

unp_internalize(rights)
	struct mbuf *rights;
{
	register struct file **rp;
	int oldfds = rights->m_len / sizeof (int);
	register int i;
	register struct file *fp;

	rp = mtod(rights, struct file **);
	for (i = 0; i < oldfds; i++)
		if (getf(*(int *)rp++) == 0)
			return (EBADF);
	rp = mtod(rights, struct file **);
	for (i = 0; i < oldfds; i++) {
		fp = getf(*(int *)rp);
		*rp++ = fp;
		fp->f_count++;
		fp->f_msgcount++;
		unp_rights++;
	}
	return (0);
}

int	unp_defer, unp_gcing;
int	unp_mark();
extern	struct domain unixdomain;

unp_gc()
{
	register struct file *fp;
	register struct socket *so;

	if (unp_gcing)
		return;
	unp_gcing = 1;
restart:
	unp_defer = 0;
	for (fp = file; fp < fileNFILE; fp++)
		fp->f_flag &= ~(FMARK|FDEFER);
	do {
		for (fp = file; fp < fileNFILE; fp++) {
			if (fp->f_count == 0)
				continue;
			if (fp->f_flag & FDEFER) {
				fp->f_flag &= ~FDEFER;
				unp_defer--;
			} else {
				if (fp->f_flag & FMARK)
					continue;
				if (fp->f_count == fp->f_msgcount)
					continue;
				fp->f_flag |= FMARK;
			}
			if (fp->f_type != DTYPE_SOCKET ||
			    (so = (struct socket *)fp->f_data) == 0)
				continue;
			if (so->so_proto->pr_domain != &unixdomain ||
			    (so->so_proto->pr_flags&PR_RIGHTS) == 0)
				continue;
			if (so->so_rcv.sb_flags & SB_LOCK) {
				sbwait(&so->so_rcv);
				goto restart;
			}
			unp_scan(so->so_rcv.sb_mb, unp_mark);
		}
	} while (unp_defer);
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0)
			continue;
		if (fp->f_count == fp->f_msgcount && (fp->f_flag & FMARK) == 0)
			while (fp->f_msgcount)
				unp_discard(fp);
	}
	unp_gcing = 0;
}

unp_dispose(m)
	struct mbuf *m;
{
	int unp_discard();

	if (m)
		unp_scan(m, unp_discard);
}

unp_scan(m0, op)
	register struct mbuf *m0;
	int (*op)();
{
	register struct mbuf *m;
	register struct file **rp;
	register int i;
	int qfds;

	while (m0) {
		for (m = m0; m; m = m->m_next)
			if (m->m_type == MT_RIGHTS && m->m_len) {
				qfds = m->m_len / sizeof (struct file *);
				rp = mtod(m, struct file **);
				for (i = 0; i < qfds; i++)
					(*op)(*rp++);
				break;		/* XXX, but saves time */
			}
		m0 = m0->m_act;
	}
}

unp_mark(fp)
	struct file *fp;
{

	if (fp->f_flag & FMARK)
		return;
	unp_defer++;
	fp->f_flag |= (FMARK|FDEFER);
}

unp_discard(fp)
	struct file *fp;
{

	fp->f_msgcount--;
	unp_rights--;
	closef(fp);
}
