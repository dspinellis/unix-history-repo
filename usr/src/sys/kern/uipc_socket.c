/*	uipc_socket.c	4.3	81/11/14	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/mbuf.h"
#include "../h/protocol.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/inaddr.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"

/*
 * Socket support routines.
 *
 * DEAL WITH INTERRUPT NOTIFICATION.
 * DO NEWFD STUFF
 */

/*
 * Create a socket.
 */
socket(aso, type, iap, options)
	struct socket **aso;
	int type;
	register struct in_addr *iap;
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
	if (iap == 0) {
		pf = PF_INET;		/* should be u.u_protof */
		proto = 0;
	} else {
		pf = iap->ia_pf;
		proto = iap->ia_proto;
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
	(*prp->pr_usrreq)(so, PRU_ATTACH, 0, 0);
	if (so->so_error) {
/*###80 [cc] operands of = have incompatible types %%%*/
/*###80 [cc] zerosocket undefined %%%*/
		error = so->so_error;
		m_free(dtom(so));
		return (error);
	}
	*aso = so;
	return (0);
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

	if (so->so_pcb == 0)
		goto discard;
	if (so->so_state & SS_ISCONNECTED) {
		if ((so->so_state & SS_ISDISCONNECTING) == 0) {
			u.u_error = disconnect(so, 0);
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
	if (so->so_pcb == 0)
		sofree(so);
	splx(s);
}

sostat(so, sb)
	struct socket *so;
	struct stat *sb;
{

	return (EOPNOTSUPP);
}

/*
 * Connect socket to a specified address.
 * If already connected or connecting, then avoid
 * the protocol entry, to keep its job simpler.
 */
connect(so, iap)
	struct socket *so;
	struct in_addr *iap;
{
	int s = splnet();
	int error;

	if (so->so_state & (SS_ISCONNECTED|SS_ISCONNECTING)) {
		error = EISCONN;
		goto bad;
	}
	error = (*so->so_proto->pr_usrreq)(so, PRU_CONNECT, 0, (caddr_t)iap);
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
disconnect(so, iap)
	struct socket *so;
	struct in_addr *iap;
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
	error = (*so->so_proto->pr_usrreq)(so, PRU_DISCONNECT, 0, 0);
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
send(so, iap)
	register struct socket *so;
	struct in_addr *iap;
{
	struct mbuf *top = 0;
	register struct mbuf *m, **mp = &top;
	register int bufs;
	register int len;
	int error = 0;
	int s;

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
		if (iap == 0)
			snderr(EDESTADDRREQ);
	}
	if (so->so_error)
		snderr(so->so_error);
	if (top) {
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND, top, iap);
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
	while (u.u_count > 0 && sbspace(&so->so_snd) > 0) {
		MGET(m, 1);
		if (m == NULL) {
			error = ENOBUFS;
			m_freem(top);
			goto release;
		}
		if (u.u_count >= PGSIZE && bufs >= NMBPG) {
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

receive(so, iap)
	register struct socket *so;
	struct in_addr *iap;
{
	register struct mbuf *m, *n;
	register int len;
	int eor, s, error = 0;

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
			rcverr(EWOULDBLOCK);
		sbunlock(&so->so_rcv);
		sleep((caddr_t)&so->so_rcv.sb_cc, PZERO+1);
		goto restart;
	}
	m = so->so_rcv.sb_mb;
	if (m == 0)
		panic("receive");
	
	/*
	 * Pull address off receive chain, if protocol
	 * put one there.
	 */
	if ((so->so_proto->pr_flags & PR_ADDR)) {
		so->so_rcv.sb_mb = m->m_next;
		if (iap) {
			so->so_rcv.sb_cc -= m->m_len;
			len = MIN(m->m_len, sizeof (struct in_addr));
			bcopy(mtod(m, caddr_t), (caddr_t)iap, len);
		} else
			*iap = zeroin_addr;
		m = so->so_rcv.sb_mb;
		if (m == 0)
			panic("receive 2");
	}

	/*
	 * Next pull data off the chain.
	 * Stop at eor or when run out of space in user buffer.
	 */
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

	/*
	 * If atomic protocol discard rest of record.
	 */
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

	/*
	 * If protocol cares, inform it that
	 * there is more space in the receive buffer.
	 */
	if ((so->so_proto->pr_flags & PR_WANTRCVD) && so->so_pcb)
		(*so->so_proto->pr_usrreq)(so, PRU_RCVD, 0, 0);

release:
	sounlock(&so->so_rcv);
	splx(s);
}

skioctl(so, cmd, cmdp)
	register struct socket *so;
	int cmd;
	register caddr_t cmdp;
{

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
/*###417 [cc] operands of = have incompatible types %%%*/
/*###417 [cc] zeroin_addr undefined %%%*/
