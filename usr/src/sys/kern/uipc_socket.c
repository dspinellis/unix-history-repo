/*	uipc_socket.c	4.47	82/08/14	*/

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
 * Socket support routines.
 *
 * DEAL WITH INTERRUPT NOTIFICATION.
 */

/*
 * Create a socket.
 */
socreate(aso, type, asp, asa, options)
	struct socket **aso;
	int type;
	struct sockproto *asp;
	struct sockaddr *asa;
	int options;
{
	register struct protosw *prp;
	register struct socket *so;
	struct mbuf *m;
	int pf, proto, error;

	/*
	 * Use process standard protocol/protocol family if none
	 * specified by address argument.
	 */
	if (asp == 0) {
		pf = PF_INET;		/* should be u.u_protof */
		proto = 0;
	} else {
		pf = asp->sp_family;
		proto = asp->sp_protocol;
	}

	/*
	 * If protocol specified, look for it, otherwise
	 * for a protocol of the correct type in the right family.
	 */
	if (proto)
		prp = pffindproto(pf, proto);
	else
		prp = pffindtype(pf, type);
	if (prp == 0)
		return (EPROTONOSUPPORT);

	/*
	 * Get a socket structure.
	 */
	m = m_getclr(M_WAIT);
	if (m == 0)
		return (ENOBUFS);
	so = mtod(m, struct socket *);
	so->so_options = options;
	if (options & SO_ACCEPTCONN) {
		so->so_q = so;
		so->so_q0 = so;
		so->so_qlimit = (so->so_options & SO_NEWFDONCONN) ? 5 : 1;
	}
	so->so_state = 0;
	if (u.u_uid == 0)
		so->so_state = SS_PRIV;

	/*
	 * Attach protocol to socket, initializing
	 * and reserving resources.
	 */
	so->so_proto = prp;
	error = (*prp->pr_usrreq)(so, PRU_ATTACH, 0, asa);
	if (error) {
		so->so_state |= SS_NOFDREF;
		sofree(so);
		return (error);
	}
	*aso = so;
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
	register struct socket *so2;

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
		u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_DETACH, 0, 0);
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

/*
 * Accept connection on a socket.
 */
soaccept(so, asa)
	struct socket *so;
	struct sockaddr *asa;
{
	int s = splnet();
	int error;

	error = (*so->so_proto->pr_usrreq)(so, PRU_ACCEPT, 0, (caddr_t)asa);
	splx(s);
	return (error);
}

/*
 * Connect socket to a specified address.
 * If already connected or connecting, then avoid
 * the protocol entry, to keep its job simpler.
 */
soconnect(so, asa)
	struct socket *so;
	struct sockaddr *asa;
{
	int s = splnet();
	int error;

	if (so->so_state & (SS_ISCONNECTED|SS_ISCONNECTING)) {
		error = EISCONN;
		goto bad;
	}
	error = (*so->so_proto->pr_usrreq)(so, PRU_CONNECT, 0, (caddr_t)asa);
bad:
	splx(s);
	return (error);
}

/*
 * Disconnect from a socket.
 * Address parameter is from system call for later multicast
 * protocols.  Check to make sure that connected and no disconnect
 * in progress (for protocol's sake), and then invoke protocol.
 */
sodisconnect(so, asa)
	struct socket *so;
	struct sockaddr *asa;
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
	error = (*so->so_proto->pr_usrreq)(so, PRU_DISCONNECT, 0, asa);
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
sosend(so, asa)
	register struct socket *so;
	struct sockaddr *asa;
{
	struct mbuf *top = 0;
	register struct mbuf *m, **mp = &top;
	register u_int len;
	int error = 0, space, s;

	if (sosendallatonce(so) && u.u_count > so->so_snd.sb_hiwat)
		return (EMSGSIZE);
#ifdef notdef
	/* NEED TO PREVENT BUSY WAITING IN SELECT FOR WRITING */
	if ((so->so_snd.sb_flags & SB_LOCK) && (so->so_state & SS_NBIO))
		return (EWOULDBLOCK);
#endif
restart:
	sblock(&so->so_snd);
#define	snderr(errno)	{ error = errno; splx(s); goto release; }

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
		if (asa == 0)
			snderr(EDESTADDRREQ);
	}
	if (top) {
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND, top, asa);
		top = 0;
		if (error) {
			splx(s);
			goto release;
		}
		mp = &top;
	}
	if (u.u_count == 0) {
		splx(s);
		goto release;
	}
	space = sbspace(&so->so_snd);
	if (space <= 0 || sosendallatonce(so) && space < u.u_count) {
		if (so->so_state & SS_NBIO)
			snderr(EWOULDBLOCK);
		sbunlock(&so->so_snd);
		sbwait(&so->so_snd);
		splx(s);
		goto restart;
	}
	splx(s);
	while (u.u_count && space > 0) {
		MGET(m, 1);
		if (m == NULL) {
			error = ENOBUFS;			/* SIGPIPE? */
			goto release;
		}
		if (u.u_count >= CLBYTES && space >= CLBYTES) {
			register struct mbuf *p;
			MCLGET(p, 1);
			if (p == 0)
				goto nopages;
			m->m_off = (int)p - (int)m;
			len = CLBYTES;
		} else {
nopages:
			m->m_off = MMINOFF;
			len = MIN(MLEN, u.u_count);
		}
		iomove(mtod(m, caddr_t), len, B_WRITE);
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

soreceive(so, asa, uio)
	register struct socket *so;
	struct sockaddr *asa;
	struct uio *uio;
{
	register struct iovec *iov;
	register struct mbuf *m, *n;
	u_int len;
	int eor, s, error = 0, resid = uio->uio_resid;
	int cnt;

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
	m = so->so_rcv.sb_mb;
	if (m == 0)
		panic("receive");
	if (so->so_proto->pr_flags & PR_ADDR) {
		if (m->m_len != sizeof (struct sockaddr))
			panic("soreceive addr");
		if (asa)
			bcopy(mtod(m, caddr_t), (caddr_t)asa, sizeof (*asa));
		so->so_rcv.sb_cc -= m->m_len;
		so->so_rcv.sb_mbcnt -= MSIZE;
		m = m_free(m);
		if (m == 0)
			panic("receive 2");
		so->so_rcv.sb_mb = m;
	}
	eor = 0;
	do {
		if (uio->uio_iovcnt == 0)
			break;
		iov = uio->uio_iov;
		len = iov->iov_len;
		so->so_state &= ~SS_RCVATMARK;
		if (so->so_oobmark && len > so->so_oobmark)
			len = so->so_oobmark;
		if (len > m->m_len)
			len = m->m_len;
		splx(s);
		uiomove(mtod(m, caddr_t), len, UIO_WRITETO, uio);
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
		(*so->so_proto->pr_usrreq)(so, PRU_RCVD, 0, 0);
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
			u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_SHUTDOWN, (struct mbuf *)0, 0);
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
		(*so->so_proto->pr_usrreq)(so, PRU_SENDOOB, m, 0);
		return;
	}

	case SIOCRCVOOB: {
		struct mbuf *m = m_get(M_DONTWAIT);

		if (m == 0) {
			u.u_error = ENOBUFS;
			return;
		}
		m->m_off = MMINOFF; *mtod(m, caddr_t) = 0;
		(*so->so_proto->pr_usrreq)(so, PRU_RCVOOB, m, 0);
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
