/*	uipc_socket.c	4.8	81/11/20	*/

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
#include "../net/inet.h"
#include "../net/inet_systm.h"

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
COUNT(SOCREATE);

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

	/*
	 * Attach protocol to socket, initializing
	 * and reserving resources.
	 */
	so->so_proto = prp;
	(*prp->pr_usrreq)(so, PRU_ATTACH, 0, asa);
	if (so->so_error) {
		error = so->so_error;
		(void) m_free(dtom(so));
		return (error);
	}
	*aso = so;
	return (0);
}

sofree(so)
	struct socket *so;
{

COUNT(SOFREE);
	if (so->so_pcb || (so->so_state & SS_USERGONE) == 0)
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
soclose(so)
	register struct socket *so;
{
	int s = splnet();		/* conservative */

COUNT(SOCLOSE);
	if (so->so_pcb == 0)
		goto discard;
	if (so->so_state & SS_ISCONNECTED) {
		if ((so->so_state & SS_ISDISCONNECTING) == 0) {
			u.u_error = sodisconnect(so, (struct sockaddr *)0);
			if (u.u_error) {
				splx(s);
				return;
			}
		}
		if ((so->so_state & SS_ISDISCONNECTING) &&
		    (so->so_options & SO_NBIO)) {
			u.u_error = EINPROGRESS;
			splx(s);
			return;
		}
		while (so->so_state & SS_ISCONNECTED)
			sleep((caddr_t)&so->so_timeo, PZERO+1);
	}
	u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_DETACH, 0, 0);
discard:
	so->so_state |= SS_USERGONE;
	sofree(so);
	splx(s);
}

sosplice(pso, so)
	struct socket *pso, *so;
{

COUNT(SOSPLICE);
	if (pso->so_proto->pr_family != PF_LOCAL) {
		struct socket *tso;
		tso = pso; pso = so; so = tso;
	}
	if (pso->so_proto->pr_family != PF_LOCAL)
		return (EOPNOTSUPP);
	/* check types and buffer space */
	/* merge buffers */
	return (0);
}

/*ARGSUSED*/
sostat(so, sb)
	struct socket *so;
	struct stat *sb;
{

COUNT(SOSTAT);
	return (EOPNOTSUPP);
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

COUNT(SOACCEPT);
	if (so->so_state & (SS_ISCONNECTED|SS_ISCONNECTING)) {
		error = EISCONN;
		goto bad;
	}
	if ((so->so_options & SO_ACCEPTCONN) == 0) {
		error = EINVAL;			/* XXX */
		goto bad;
	}
	error = (*so->so_proto->pr_usrreq)(so, PRU_ACCEPT, 0, (caddr_t)asa);
bad:
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

COUNT(SOCONNECT);
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

COUNT(SODISCONNECT);
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

COUNT(SOSEND);
	if (so->so_state & SS_CANTSENDMORE)
		return (EPIPE);
	if (sosendallatonce(so) && u.u_count > so->so_snd.sb_hiwat)
		return (EMSGSIZE);
	if ((so->so_snd.sb_flags & SB_LOCK) && (so->so_options & SO_NBIO))
		return (EWOULDBLOCK);
	sblock(&so->so_snd);
#define	snderr(errno)	{ error = errno; splx(s); goto release; }

	s = splnet();
again:
	if ((so->so_state & SS_ISCONNECTED) == 0) {
		if (so->so_proto->pr_flags & PR_CONNREQUIRED)
			snderr(ENOTCONN);
		if (asa == 0)
			snderr(EDESTADDRREQ);
	}
	if (so->so_error)
		snderr(so->so_error);
	if (top) {
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND, top, asa);
		if (error) {
			splx(s);
			goto release;
		}
		top = 0;
		mp = &top;
	}
	if (sosendallatonce(so) && sbspace(&so->so_snd) < u.u_count) {
		if (so->so_options & SO_NBIO)
			snderr(EWOULDBLOCK);
		sbunlock(&so->so_snd);
		sbwait(&so->so_snd);
		splx(s);
		goto again;
	}
	splx(s);
	while (u.u_count && (space = sbspace(&so->so_snd)) > 0) {
		MGET(m, 1);
		if (m == NULL) {
			error = ENOBUFS;
			m_freem(top);
			goto release;
		}
		if (u.u_count >= PGSIZE && space >= NMBPG) {
			register struct mbuf *p;
			MPGET(p, 1);
			if (p == 0)
				goto nopages;
			m->m_off = (int)p - (int)m;
			len = PGSIZE;
		} else {
nopages:
			m->m_off = MMINOFF;
			len = MIN(MLEN, u.u_count);
		}
		iomove(mtod(m, caddr_t), len, B_WRITE);
		m->m_len = len;
		*mp = m;
		mp = &m->m_next;
	}
	s = splnet();
	goto again;

release:
	sbunlock(&so->so_snd);
	return (error);
}

soreceive(so, asa)
	register struct socket *so;
	struct sockaddr *asa;
{
	register struct mbuf *m, *n;
	u_int len;
	int eor, s, error = 0;

COUNT(SORECEIVE);
restart:
	sblock(&so->so_rcv);
	s = splnet();

#define	rcverr(errno)	{ error = errno; splx(s); goto release; }
	if (so->so_rcv.sb_cc == 0) {
		if ((so->so_state & SS_ISCONNECTED) == 0 &&
		    (so->so_proto->pr_flags & PR_CONNREQUIRED))
			rcverr(ENOTCONN);
		if (so->so_state & SS_CANTRCVMORE) {
			splx(s);
			goto release;
		}
		if (so->so_options & SO_NBIO)
			rcverr (EWOULDBLOCK);
		sbunlock(&so->so_rcv);
		sbwait(&so->so_rcv);
		goto restart;
	}
	m = so->so_rcv.sb_mb;
	if (m == 0)
		panic("receive");
	if ((so->so_proto->pr_flags & PR_ADDR)) {
		so->so_rcv.sb_mb = m->m_next;
		if (asa) {
			so->so_rcv.sb_cc -= m->m_len;
			len = MIN(m->m_len, sizeof (struct sockaddr));
			bcopy(mtod(m, caddr_t), (caddr_t)asa, len);
		} else
			bzero((caddr_t)asa, sizeof (*asa));
		m = so->so_rcv.sb_mb;
		if (m == 0)
			panic("receive 2");
	}
	eor = 0;
	do {
		len = MIN(m->m_len, u.u_count);
		if (len == m->m_len) {
			eor = (int)m->m_act;
			sbfree(&so->so_rcv, m);
		}
		splx(s);
		iomove(mtod(m, caddr_t), len, B_READ);
		s = splnet();
		if (len == m->m_len) {
			MFREE(m, n);
		} else {
			m->m_off += len;
			m->m_len -= len;
			so->so_rcv.sb_cc -= len;
		}
	} while ((m = so->so_rcv.sb_mb) && u.u_count && !eor);
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

/*ARGSUSED*/
soioctl(so, cmd, cmdp)
	register struct socket *so;
	int cmd;
	register caddr_t cmdp;
{

COUNT(SOIOCTL);
	switch (cmdp) {

	}
	switch (so->so_type) {

	case SOCK_STREAM:
		break;

	case SOCK_DGRAM:
		break;

	case SOCK_RDM:
		break;

	case SOCK_RAW:
		break;

	}
}
