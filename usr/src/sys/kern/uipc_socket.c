/*	uipc_socket.c	4.50	82/10/03	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/stat.h"
#include "../h/ioctl.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/route.h"
#include "../h/uio.h"

/*
 * Socket operation routines.
 * These routines are called by the routines in
 * sys_socket.c or from a system process, and
 * implement the semantics of socket operations by
 * switching out to the protocol specific routines.
 */

socreate(dom, aso, type, proto, opt)
	struct socket **aso;
	int type, proto;
	struct socketopt *opt;
{
	register struct protosw *prp;
	register struct socket *so;
	struct mbuf *m;
	int pf, error;

	pf = dom ? PF_UNIX : PF_INET;		/* should be u.u_protof */
	if (proto)
		prp = pffindproto(pf, proto);
	else
		prp = pffindtype(pf, type);
	if (prp == 0)
		return (EPROTONOSUPPORT);
	if (prp->pr_type != type)
		return (EPROTOTYPE);
	m = m_getclr(M_WAIT);
	if (m == 0)
		return (ENOBUFS);
	so = mtod(m, struct socket *);
	so->so_options = 0;
	so->so_state = 0;
	if (u.u_uid == 0)
		so->so_state = SS_PRIV;
	so->so_proto = prp;
	error = (*prp->pr_usrreq)(so, PRU_ATTACH,
	    (struct mbuf *)0, (struct mbuf *)0, (struct socketopt *)0);
	if (error) {
		so->so_state |= SS_NOFDREF;
		sofree(so);
		return (error);
	}
	*aso = so;
	return (0);
}

sobind(so, nam, opt)
	struct socket *so;
	struct mbuf *nam;
	struct socketopt *opt;
{
	int s = splnet();
	int error;

	error =
	    (*so->so_proto->pr_usrreq)(so, PRU_BIND,
		(struct mbuf *)0, nam, opt);
	splx(s);
	return (error);
}

solisten(so, backlog)
	struct socket *so;
	int backlog;
{
	int s = splnet();
	int error;

	error = (*so->so_proto->pr_usrreq)(so, PRU_LISTEN,
	    (struct mbuf *)0, (struct mbuf *)0, (struct socketopt *)0);
	if (error) {
		splx(s);
		return (error);
	}
	if (so->so_q == 0) {
		so->so_q = so;
		so->so_q0 = so;
		so->so_options |= SO_ACCEPTCONN;
	}
	if (backlog < 0)
		backlog = 0;
	so->so_qlimit = backlog < 5 ? backlog : 5;
	so->so_options |= SO_NEWFDONCONN;
	return (0);
}

sofree(so)
	struct socket *so;
{

	if (so->so_head) {
		if (!soqremque(so, 0) && !soqremque(so, 1))
			panic("sofree dq");
		so->so_head = 0;
	}
	if (so->so_pcb || (so->so_state & SS_NOFDREF) == 0)
		return;
	sbrelease(&so->so_snd);
	sbrelease(&so->so_rcv);
	(void) m_free(dtom(so));
}

/*
 * Close a socket on last file table reference removal.
 * Initiate disconnect if connected.
 * Free socket when disconnect complete.
 */
soclose(so, exiting)
	register struct socket *so;
	int exiting;
{
	int s = splnet();		/* conservative */

	if (so->so_options & SO_ACCEPTCONN) {
		while (so->so_q0 != so)
			soclose(so->so_q0, 1);
		while (so->so_q != so)
			soclose(so->so_q, 1);
	}
	if (so->so_pcb == 0)
		goto discard;
	if (exiting)
		so->so_options |= SO_KEEPALIVE;
	if (so->so_state & SS_ISCONNECTED) {
		if ((so->so_state & SS_ISDISCONNECTING) == 0) {
			u.u_error = sodisconnect(so, (struct sockaddr *)0);
			if (u.u_error) {
				if (exiting)
					goto drop;
				splx(s);
				return;
			}
		}
		if ((so->so_options & SO_DONTLINGER) == 0) {
			if ((so->so_state & SS_ISDISCONNECTING) &&
			    (so->so_state & SS_NBIO) &&
			    exiting == 0) {
				u.u_error = EINPROGRESS;
				splx(s);
				return;
			}
			/* should use tsleep here, for at most linger */
			while (so->so_state & SS_ISCONNECTED)
				sleep((caddr_t)&so->so_timeo, PZERO+1);
		}
	}
drop:
	if (so->so_pcb) {
		u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_DETACH,
		    (struct mbuf *)0, (struct mbuf *)0, (struct socketopt *)0);
		if (exiting == 0 && u.u_error) {
			splx(s);
			return;
		}
	}
discard:
	so->so_state |= SS_NOFDREF;
	sofree(so);
	splx(s);
}

/*ARGSUSED*/
sostat(so, sb)
	struct socket *so;
	struct stat *sb;
{

	bzero((caddr_t)sb, sizeof (*sb));		/* XXX */
	return (0);					/* XXX */
}

soaccept(so, nam, opt)
	struct socket *so;
	struct mbuf *nam;
	struct socketopt *opt;
{
	int s = splnet();
	int error;

	error = (*so->so_proto->pr_usrreq)(so, PRU_ACCEPT,
	    (struct mbuf *)0, nam, opt);
	splx(s);
	return (error);
}

soconnect(so, nam, opt)
	struct socket *so;
	struct mbuf *nam;
	struct socketopt *opt;
{
	int s = splnet();
	int error;

	if (so->so_state & (SS_ISCONNECTED|SS_ISCONNECTING)) {
		error = EISCONN;
		goto bad;
	}
	error = (*so->so_proto->pr_usrreq)(so, PRU_CONNECT,
	    (struct mbuf *)0, nam, opt);
bad:
	splx(s);
	return (error);
}

sodisconnect(so, nam)
	struct socket *so;
	struct mbuf *nam;
{
	int s = splnet();
	int error;

	if ((so->so_state & SS_ISCONNECTED) == 0) {
		error = ENOTCONN;
		goto bad;
	}
	if (so->so_state & SS_ISDISCONNECTING) {
		error = EALREADY;
		goto bad;
	}
	error = (*so->so_proto->pr_usrreq)(so, PRU_DISCONNECT,
	    (struct mbuf *)0, nam, (struct socketopt *)0);
bad:
	splx(s);
	return (error);
}

/*
 * Send on a socket.
 * If send must go all at once and message is larger than
 * send buffering, then hard error.
 * Lock against other senders.
 * If must go all at once and not enough room now, then
 * inform user that this would block and do nothing.
 */
sosend(so, nam, uio)
	register struct socket *so;
	struct mbuf *nam;
	struct uio *uio;
{
	struct mbuf *top = 0;
	register struct mbuf *m, **mp = &top;
	register u_int len;
	int error = 0, space, s;

	if (sosendallatonce(so) && uio->uio_resid > so->so_snd.sb_hiwat)
		return (EMSGSIZE);
#ifdef notdef
	/* NEED TO PREVENT BUSY WAITING IN SELECT FOR WRITING */
	if ((so->so_snd.sb_flags & SB_LOCK) && (so->so_state & SS_NBIO))
		return (EWOULDBLOCK);
#endif
restart:
	sblock(&so->so_snd);
#define	snderr(errno)	{ error = errno; splx(s); goto release; }

	u.u_ru.ru_msgsnd++;
again:
	s = splnet();
	if (so->so_state & SS_CANTSENDMORE) {
		psignal(u.u_procp, SIGPIPE);
		snderr(EPIPE);
	}
	if (so->so_error) {
		error = so->so_error;
		so->so_error = 0;				/* ??? */
		splx(s);
		goto release;
	}
	if ((so->so_state & SS_ISCONNECTED) == 0) {
		if (so->so_proto->pr_flags & PR_CONNREQUIRED)
			snderr(ENOTCONN);
		if (nam == 0)
			snderr(EDESTADDRREQ);
	}
	if (top) {
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND,
		    top, (caddr_t)nam, (struct socketopt *)0);
		top = 0;
		if (error) {
			splx(s);
			goto release;
		}
		mp = &top;
	}
	if (uio->uio_resid == 0) {
		splx(s);
		goto release;
	}
	space = sbspace(&so->so_snd);
	if (space <= 0 || sosendallatonce(so) && space < uio->uio_resid) {
		if (so->so_state & SS_NBIO)
			snderr(EWOULDBLOCK);
		sbunlock(&so->so_snd);
		sbwait(&so->so_snd);
		splx(s);
		goto restart;
	}
	splx(s);
	while (uio->uio_resid > 0 && space > 0) {
		register struct iovec *iov = uio->uio_iov;

		if (iov->iov_len == 0) {
			uio->uio_iov++;
			uio->uio_iovcnt--;
			if (uio->uio_iovcnt < 0)
				panic("sosend");
			continue;
		}
		MGET(m, 1);
		if (m == NULL) {
			error = ENOBUFS;			/* SIGPIPE? */
			goto release;
		}
		if (iov->iov_len >= CLBYTES && space >= CLBYTES) {
			register struct mbuf *p;
			MCLGET(p, 1);
			if (p == 0)
				goto nopages;
			m->m_off = (int)p - (int)m;
			len = CLBYTES;
		} else {
nopages:
			m->m_off = MMINOFF;
			len = MIN(MLEN, iov->iov_len);
		}
		uiomove(mtod(m, caddr_t), len, UIO_WRITE, uio);
		m->m_len = len;
		*mp = m;
		mp = &m->m_next;
		space = sbspace(&so->so_snd);
	}
	goto again;

release:
	sbunlock(&so->so_snd);
	if (top)
		m_freem(top);
	return (error);
}

soreceive(so, aname, uio)
	register struct socket *so;
	struct mbuf **aname;
	struct uio *uio;
{
	register struct iovec *iov;
	register struct mbuf *m, *n;
	u_int len;
	int eor, s, error = 0;

restart:
	sblock(&so->so_rcv);
	s = splnet();

#define	rcverr(errno)	{ error = errno; splx(s); goto release; }
	if (so->so_rcv.sb_cc == 0) {
		if (so->so_error) {
			error = so->so_error;
			so->so_error = 0;
			splx(s);
			goto release;
		}
		if (so->so_state & SS_CANTRCVMORE) {
			splx(s);
			goto release;
		}
		if ((so->so_state & SS_ISCONNECTED) == 0 &&
		    (so->so_proto->pr_flags & PR_CONNREQUIRED))
			rcverr(ENOTCONN);
		if (so->so_state & SS_NBIO)
			rcverr(EWOULDBLOCK);
		sbunlock(&so->so_rcv);
		sbwait(&so->so_rcv);
		splx(s);
		goto restart;
	}
	u.u_ru.ru_msgrcv++;
	m = so->so_rcv.sb_mb;
	if (m == 0)
		panic("receive");
	if (so->so_proto->pr_flags & PR_ADDR) {
		so->so_rcv.sb_cc -= m->m_len;
		so->so_rcv.sb_mbcnt -= MSIZE;
		if (aname) {
			*aname = m;
			m = m->m_next;
			(*aname)->m_next = 0;
		} else
			m = m_free(m);
		if (m == 0)
			panic("receive 2");
		so->so_rcv.sb_mb = m;
	}
	eor = 0;
	do {
		if (uio->uio_resid <= 0)
			break;
		len = uio->uio_resid;
		so->so_state &= ~SS_RCVATMARK;
		if (so->so_oobmark && len > so->so_oobmark)
			len = so->so_oobmark;
		if (len > m->m_len)
			len = m->m_len;
		splx(s);
		uiomove(mtod(m, caddr_t), (int)len, UIO_READ, uio);
		s = splnet();
		if (len == m->m_len) {
			eor = (int)m->m_act;
			sbfree(&so->so_rcv, m);
			so->so_rcv.sb_mb = m->m_next;
			MFREE(m, n);
		} else {
			m->m_off += len;
			m->m_len -= len;
			so->so_rcv.sb_cc -= len;
		}
		if (so->so_oobmark) {
			so->so_oobmark -= len;
			if (so->so_oobmark == 0) {
				so->so_state |= SS_RCVATMARK;
				break;
			}
		}
	} while ((m = so->so_rcv.sb_mb) && !eor);
	if ((so->so_proto->pr_flags & PR_ATOMIC) && eor == 0)
		do {
			if (m == 0)
				panic("receive 3");
			sbfree(&so->so_rcv, m);
			eor = (int)m->m_act;
			so->so_rcv.sb_mb = m->m_next;
			MFREE(m, n);
			m = n;
		} while (eor == 0);
	if ((so->so_proto->pr_flags & PR_WANTRCVD) && so->so_pcb)
		(*so->so_proto->pr_usrreq)(so, PRU_RCVD,
		    (struct mbuf *)0, (struct mbuf *)0, (struct socketopt *)0);
release:
	sbunlock(&so->so_rcv);
	splx(s);
	return (error);
}

sohasoutofband(so)
	struct socket *so;
{

	if (so->so_pgrp == 0)
		return;
	if (so->so_pgrp > 0)
		gsignal(so->so_pgrp, SIGURG);
	else {
		struct proc *p = pfind(-so->so_pgrp);

		if (p)
			psignal(p, SIGURG);
	}
}

/*ARGSUSED*/
soioctl(so, cmd, data)
	register struct socket *so;
	int cmd;
	register char *data;
{

	switch (cmd) {

	case FIONBIO:
		if (*(int *)data)
			so->so_state |= SS_NBIO;
		else
			so->so_state &= ~SS_NBIO;
		return;

	case FIOASYNC:
		if (*(int *)data)
			so->so_state |= SS_ASYNC;
		else
			so->so_state &= ~SS_ASYNC;
		return;

	case SIOCSKEEP:
		if (*(int *)data)
			so->so_options &= ~SO_KEEPALIVE;
		else
			so->so_options |= SO_KEEPALIVE;
		return;

	case SIOCGKEEP:
		*(int *)data = (so->so_options & SO_KEEPALIVE) != 0;
		return;

	case SIOCSLINGER:
		so->so_linger = *(int *)data;
		if (so->so_linger)
			so->so_options &= ~SO_DONTLINGER;
		else
			so->so_options |= SO_DONTLINGER;
		return;

	case SIOCGLINGER:
		*(int *)data = so->so_linger;
		return;

	case SIOCSPGRP:
		so->so_pgrp = *(int *)data;
		return;

	case SIOCGPGRP:
		*(int *)data = so->so_pgrp;
		return;

	case SIOCDONE: {
		int flags = *(int *)data;

		flags++;
		if (flags & FREAD) {
			int s = splimp();
			socantrcvmore(so);
			sbflush(&so->so_rcv);
			splx(s);
		}
		if (flags & FWRITE)
			u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_SHUTDOWN,
			    (struct mbuf *)0, (struct mbuf *)0,
			    (struct socketopt *)0);
		return;
	}

	case SIOCSENDOOB: {
		char oob = *(char *)data;
		struct mbuf *m;

		m = m_get(M_DONTWAIT);
		if (m == 0) {
			u.u_error = ENOBUFS;
			return;
		}
		m->m_off = MMINOFF;
		m->m_len = sizeof (char);
		*mtod(m, char *) = oob;
		(*so->so_proto->pr_usrreq)(so, PRU_SENDOOB,
		    m, (struct mbuf *)0, (struct socketopt *)0);
		return;
	}

	case SIOCRCVOOB: {
		struct mbuf *m = m_get(M_DONTWAIT);

		if (m == 0) {
			u.u_error = ENOBUFS;
			return;
		}
		m->m_off = MMINOFF; *mtod(m, caddr_t) = 0;
		(*so->so_proto->pr_usrreq)(so, PRU_RCVOOB,
		    m, (struct mbuf *)0, (struct socketopt *)0);
		*(char *)data = *mtod(m, char *);
		(void) m_free(m);
		return;
	}

	case SIOCATMARK:
		*(int *)data = (so->so_state&SS_RCVATMARK) != 0;
		return;

	/* routing table update calls */
	case SIOCADDRT:
	case SIOCDELRT:
		if (!suser())
			return;
		u.u_error = rtrequest(cmd, (struct rtentry *)data);
		return;

	/* type/protocol specific ioctls */
	}
	u.u_error = EOPNOTSUPP;
}
