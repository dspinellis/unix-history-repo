/*	uipc_socket.c	4.1	81/11/07	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "file.h"
#include "../h/inode.h"
#include "../h/mbuf.h"
#include "protocol.h"
#include "protocolsw.h"
#include "socket.h"
#include "socketvar.h"

struct	socket zerosocket;
struct	in_addr zeroin_addr;

/*
 * Socket system call interface.  Copy in arguments
 * set up file descriptor and call internal socket
 * creation routine.
 */
ssocket()
{
	register struct a {
		int type;
		in_addr *ain;
		int flags;
	} *uap = (struct a *)u.u_ap;
	in_addr in;
	struct socket *so0;
	register struct socket *so;
	register struct file *fp;

	if ((fp = falloc()) == NULL)
		return;
	fp->f_flag = FSOCKET|FREAD|FWRITE;
	if (copyin((caddr_t)uap->ain, &in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = socket(&so0, a->type, &in, a->flags);
	if (u.u_error)
		goto bad;
	fp->f_socket = so;
	return;
bad:
	u.u_ofile[u.u_r.r_val1] = 0;
	fp->f_count = 0;
}

/*
 * Create a socket.
 */
socket(aso, type, iap, flags)
	struct socket **aso;
	int type;
	register struct in_addr *iap;
	int flags;
{
	register struct protosw *prp;
	register struct socket *so;
	struct mbuf *m;
	int pf, proto;

	/*
	 * Pin down protocol if possible.
	 * If no address specified, use a generic protocol.
	 */
	if (iap == 0) {
		pf = PF_GENERIC;
		proto = 0;
	} else {
		pf = iap->ia_pf;
		proto = iap->ia_proto;
	}
	if (proto) {
		/*
		 * A specific protocol was requested.  Look
		 * for the protocol.  If not found, then we
		 * don't support it.
		 */
		prp = pf_findproto(pf, proto);
		if (prp == 0)
			return (EPROTONOTSUPP);
	} else {
		/*
		 * No specific protocol was requested.  Look
		 * in the specified (or generic) protocol set
		 * for a protocol of this type.
		 */
		prp = pf_findtype(pf, type);
		if (prp == 0)
			return (ESOCKTYPENOTSUPP);
	}

	/*
	 * Get a socket structure.
	 */
	m = m_get(M_WAIT);
	if (m == 0)
		return (ENOBUFS);
	m->m_off = MMINOFF;
	so = mtod(m, struct socket *);
	*so = zerosocket;
	so->so_flags = flags;

	/*
	 * An early call to protocol initialization.  If protocol
	 * actually hasn't been decided on yet (till we know
	 * peer), then the generic protocol allocated so far can
	 * just make sure a reasonable amount of resources will
	 * be available to it (say by allocating liberally now
	 * and returning some of the resources later).
	 */
	so->so_proto = prp;
	(*prp->pr_usrreq)(so, PRU_ATTACH, 0, 0);
	if (so->so_error) {
		m_free(dtom(so));
		return (u.u_error);
	}
	*aso = so;
	return (0);
}

/*
 * Connect socket to foreign peer; system call
 * interface.  Copy in arguments and call internal routine.
 */
sconnect()
{
	register struct a {
		int fdes;
		in_addr *a;
	} *uap = (struct a *)u.u_ap;
	in_addr in;

	if (copyin((caddr_t)uap->aaddr, &in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	u.u_error = connect(fp->f_socket, &in);
	if (u.u_error)
		return;
	s = splnet();
	for (;;) {
		/* should use tsleep here */
		if ((*so->so_proto->pr_usrreq)(so, PRU_ISCONN, 0, &in) == 0)
			break;
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	}
	splx(s);
}

connect(so, iap)
	struct socket *so;
	struct in_addr *iap;
{

	return ((*so->so_proto->pr_usrreq)(so, PRU_CONNECT, 0, &in));
}

/*
 * Disconnect socket from foreign peer; system call
 * interface.  Copy in arguments and call internal routine.
 */
sdisconnect()
{
	register struct a {
		int fdes;
		in_addr *a;
	} *uap = (struct a *)u.u_ap;
	in_addr in;

	if (uap->a && copyin((caddr_t)uap->aaddr, &in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	disconnect(fp->f_socket, uap->a ? &in : 0);
}

disconnect(so, iap)
	struct socket *so;
	struct in_addr *iap;
{

	u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_DISCONNECT, 0, &in);
}

/*
 * Send data on socket.
 */
ssend()
{
	register struct a {
		int fdes;
		in_addr *ain;
		caddr_t cbuf;
		int count;
	} *uap = (struct a *)u.u_ap;

	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	if (uap->count < 0) {
		u.u_error = EINVAL;
		return;
	}
	u.u_base = uap->buf;
	u.u_count = uap->len;
	u.u_segflg = 0;
	if (useracc(u.u_base, u.u_count, B_READ) == 0 ||
	    uap->ain && copyin((caddr_t)uap->ain, (caddr_t)&in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = send(fp->f_socket, uap->ain ? &in : 0);
}

send(so, iap)
	register struct socket *so;
	struct in_addr *iap;
{
	register struct mbuf *m, **mp;
	struct mbuf *top;
	int error = 0;

	if (so->so_proto->pr_flags & PR_ATOMIC) {
		if (u.u_count > so->so_snd.sb_hiwat) {
			error = EMSGSIZE;
			goto release;
		}
	}
again:
	while (so->so_snd.sb_flags & SB_LOCK) {
		so->so_snd.sb_flags |= SB_WANT;
		sleep((caddr_t)&so->so_snd.sb_flags, PZERO+1);
	}
	if (so->so_snd.sb_hiwat - so->so_snd.sb_cc < u.u_count) {
		so->so_snd.sb_flags |= SB_WAIT;
		sleep((caddr_t)&so->so_snd.sb_cc, PZERO+1);
		goto again;
	}
	so->so_snd.sb_flags |= SB_LOCK;
	while (u.u_count > 0) {
		bufs = so->so_snd.sb_mbmax - so->so_snd.sb_mbcnt;
		while (bufs == 0) {
			so->so_snd.sb_flags |= SB_WAIT;
			sleep((caddr_t)&so->so_snd.sb_cc, PZERO+1);
		}
		mp = &top;
		top = 0;
		while (--bufs >= 0 && u.u_count > 0) {
			MGET(m, 1);
			if (m == NULL) {
				error = ENOBUFS;
				m_freem(top);
				goto release;
			}
			if (u.u_count >= PGSIZE && bufs >= NMBPG) {
				MPGET(p, 1);
				if (p == 0)
					goto nopage;
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
		error = (*so->so_proto->pr_usrreq)(so, PRU_SEND, top, iap);
		splx(s);
		if (error)
			break;
	}
release:
	so->so_snd.sb_flags &= ~SB_LOCK;
	if (so->so_snd.sb_flags & SB_WANT)
		wakeup((caddr_t)&so->so_sb.sb_flags);
	return (error);
}

/*
 * Receive data on socket.
 */
sreceive()
{
	register struct a {
		int fdes;
		in_addr *ain;
		caddr_t cbuf;
		int count;
	} *uap = (struct a *)u.u_ap;

	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	if (uap->count < 0) {
		u.u_error = EINVAL;
		return;
	}
	u.u_base = uap->buf;
	u.u_count = uap->len;
	u.u_segflg = 0;
	if (useracc(u.u_base, u.u_count, B_WRITE) == 0 ||
	    uap->ain && copyin((caddr_t)uap->ain, (caddr_t)&in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	receive(fp->f_socket, uap->ain ? &in : 0);
}

receive(so, iap)
	register struct socket *so;
	struct in_addr *iap;
{
	register struct mbuf *m, *n;
	register int eor, len, s;

again:
	while (so->so_rcv.sb_flags & SB_LOCK) {
		so->so_rcv.sb_flags |= SB_WANT;
		sleep((caddr_t)&so->so_rcv.sb_flags, PZERO+1);
	}
	if (so->so_rcv.sb_cc == 0) {
		if ((so->so_proto->pr_usrreq)(so, PR_ISDISCONN, 0, 0) == 0)
			return;
		so->so_rcv.sb_flags |= SB_WAIT;
		sleep((caddr_t)&so->so_rcv.sb_cc, PZERO+1);
		goto again;
	}
	so->so_rcv.sb_flags |= SB_LOCK;
	m = up->uc_rcv.sb_mb;
	if (m == 0)
		panic("receive");
	eor = 0;
	do {
		len = MIN(m->m_len, u.u_count);
		if (len == m->m_len) {
			eor = (int)m->m_act;
			up->uc_rcv.sb_mb = m->m_next;
			up->uc_rcv.sb_cc -= len;
			if (up->uc_rcv.sb_cc < 0)
				panic("receive 2");
		}
		splx(s);
		iomove(mtod(m, caddr_t), len, B_READ);
		s = splnet();
		if (len == m->m_len) {
			MFREE(m, n);
		} else {
			m->m_off += len;
			m->m_len -= len;
			up->uc_rcv.sb_cc -= len;
			if (up->uc_rcv.sb_cc < 0)
				panic("receive 3");
		}
	} while ((m = up->uc_rcv.sb_mb) && u.u_count && !eor);
	if ((so->so_proto->pr_flags & PR_ATOMIC) && eor == 0)
		do {
			m = so->so_rcv.sb_mb;
			if (m == 0)
				panic("receive 4");
			up->uc_rcv.sb_cc -= m->m_len;
			if (up->uc_rcv.sb_cc < 0)
				panic("receive 5");
			eor = (int)m->m_act;
			so->so_rcv.sb_mb = m->m_next;
			MFREE(m, n);
		} while (eor == 0);
	if (iap)
		if ((so->so_proto->pr_flags & PR_PROVIDEADDR)) {
			m = up->uc_rcv.sb_mb;
			if (m == 0)
				panic("receive 6");
			up->uc_rcv.sb_mb = m->m_next;
			up->uc_rcv.sb_cc -= m->m_len;
			len = MIN(m->m_len, sizeof (struct in_addr));
			bcopy(mtod(m, caddr_t), (caddr_t)iap, len);
		} else
			*iap = zeroin_addr;
	(*so->so_proto->pr_usrreq)(up, PRU_RCVD, m, 0);
}

skioctl()
{

	/* switch out based on socket type */
}
