/*	uipc_syscalls.c	4.31	82/10/17	*/

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
#include "../h/descrip.h"
#include "../h/uio.h"

/*
 * System call interface to the socket abstraction.
 */

socket()
{
	register struct a {
		int	domain;
		int	type;
		int	protocol;
		struct	socketopt *opt;
	} *uap = (struct a *)u.u_ap;
	struct socket *so;
	register struct file *fp;
	struct socketopt aopt;

	u.u_error = sockopt(&aopt, uap->opt);
	if (u.u_error)
		return;
	if ((fp = falloc()) == NULL)
		goto freeopt;
	fp->f_flag = FREAD|FWRITE;
	fp->f_type = DTYPE_SOCKET;
	u.u_error = socreate(0, &so, uap->type, uap->protocol, &aopt);
	if (u.u_error)
		goto bad;
	fp->f_socket = so;
freeopt:
	if (uap->opt)
		m_free(dtom(aopt.so_optdata));
	return;
bad:
	u.u_ofile[u.u_r.r_val1] = 0;
	fp->f_count = 0;
	goto freeopt;
}

bind()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	namelen;
		struct	socketopt *opt;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct mbuf *nam;
	struct socketopt aopt;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	u.u_error = sockname(&nam, uap->name, uap->namelen);
	if (u.u_error)
		return;
	u.u_error = sockopt(&aopt, uap->opt);
	if (u.u_error) {
		m_freem(nam);
		goto freeopt;
	}
	u.u_error = sobind(fp->f_socket, nam, &aopt);
	m_freem(nam);
freeopt:
	if (uap->opt)
		m_free(dtom(aopt.so_optdata));
}

listen()
{
	register struct a {
		int	s;
		int	backlog;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	u.u_error = solisten(fp->f_socket, uap->backlog);
}

accept()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	*anamelen;
		struct	socketopt *opt;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct mbuf *nam;
	struct socketopt aopt;
	int namelen;
	int s;
	register struct socket *so;

	if (uap->name == 0)
		goto noname;
	if (copyin((caddr_t)uap->anamelen, (caddr_t)&namelen, sizeof (namelen))) {
		u.u_error = EFAULT;
		return;
	}
	if (useracc((caddr_t)uap->name, namelen, B_WRITE) == 0) {
		u.u_error = EFAULT;
		return;
	}
noname:
	u.u_error = sockopt(&aopt, uap->opt);
	if (u.u_error)
		return;
	fp = getf(uap->s);
	if (fp == 0)
		goto bad;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		goto bad;
	}
	s = splnet();
	so = fp->f_socket;
	if ((so->so_options & SO_ACCEPTCONN) == 0) {
		u.u_error = EINVAL;
		splx(s);
		goto bad;
	}
	if ((so->so_state & SS_NBIO) && so->so_qlen == 0) {
		u.u_error = EWOULDBLOCK;
		splx(s);
		goto bad;
	}
	while (so->so_qlen == 0 && so->so_error == 0) {
		if (so->so_state & SS_CANTRCVMORE) {
			so->so_error = ECONNABORTED;
			break;
		}
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	}
	if (so->so_error) {
		u.u_error = so->so_error;
		splx(s);
		goto bad;
	}
	if ((so->so_options & SO_NEWFDONCONN) == 0) {
		struct socket *nso = so->so_q;
		(void) soqremque(nso, 1);
		soclose(so, 1);
		fp->f_socket = nso;
		nso->so_q = 0;
		so = nso;
		goto ret;
	}
	if (ufalloc() < 0) {
		splx(s);
		goto bad;
	}
	fp = falloc();
	if (fp == 0) {
		u.u_ofile[u.u_r.r_val1] = 0;
		splx(s);
		goto bad;
	}
	{ struct socket *aso = so->so_q;
	  if (soqremque(aso, 1) == 0)
		panic("accept");
	  so = aso;
	}
	fp->f_type = DTYPE_SOCKET;
	fp->f_flag = FREAD|FWRITE;
	fp->f_socket = so;
ret:
	nam = m_get(M_WAIT);
	soaccept(so, nam, &aopt);
	if (uap->name) {
		if (namelen > nam->m_len)
			namelen = nam->m_len;
		/* SHOULD COPY OUT A CHAIN HERE */
		(void) copyout(mtod(nam, caddr_t), uap->name, namelen);
		(void) copyout((caddr_t)&namelen, uap->anamelen,
		    sizeof (*uap->anamelen));
	}
	m_freem(nam);
	splx(s);
bad:
	if (uap->opt)
		m_free(dtom(aopt.so_optdata));
}

connect()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	namelen;
		struct	socketopt *opt;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register struct socket *so;
	struct mbuf *nam;
	struct socketopt aopt;
	int s;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	so = fp->f_socket;
	u.u_error = sockname(&nam, uap->name, uap->namelen);
	if (u.u_error)
		return;
	u.u_error = sockopt(&aopt, uap->opt);
	if (u.u_error) {
		m_freem(nam);
		return;
	}
	u.u_error = soconnect(so, nam, &aopt);
	if (u.u_error)
		goto bad;
	s = splnet();
	if ((so->so_state & SS_NBIO) &&
	    (so->so_state & SS_ISCONNECTING)) {
		u.u_error = EINPROGRESS;
		splx(s);
		goto bad;
	}
	while ((so->so_state & SS_ISCONNECTING) && so->so_error == 0)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
	so->so_error = 0;
	splx(s);
bad:
	m_freem(nam);
	if (uap->opt)
		m_free(dtom(aopt.so_optdata));
	return;
}

socketpair()
{

	u.u_error = ENOENT;
}

sendto()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
		caddr_t	to;
		int	tolen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;
	struct mbuf *to;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	auio.uio_resid = uap->len;
	auio.uio_segflg = 0;
	auio.uio_offset = 0;	/* XXX */
	if (useracc(uap->buf, uap->len, B_READ) == 0) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = sockname(&to, uap->to, uap->tolen);
	if (u.u_error)
		goto bad;
	u.u_error = sosend(fp->f_socket, to, &auio, uap->flags);
bad:
	m_freem(to);
}

send()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	auio.uio_resid = uap->len;
	auio.uio_segflg = 0;
	auio.uio_offset = 0;	/* XXX */
	if (useracc(uap->buf, uap->len, B_READ) == 0) {
		u.u_error = EFAULT;
		return;
	}
	if (u.u_error)
		return;
	u.u_error = sosend(fp->f_socket, (struct mbuf *)0, &auio, uap->flags);
}

recvfrom()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
		caddr_t	from;
		int	*fromlenaddr;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;
	struct mbuf *from;
	int fromlen;

	if (copyin((caddr_t)uap->fromlenaddr, (caddr_t)&fromlen, sizeof (fromlen))) {
		u.u_error = EFAULT;
		return;
	}
	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	auio.uio_resid = uap->len;
	auio.uio_segflg = 0;
	auio.uio_offset = 0;	/* XXX */
	if (useracc(uap->buf, uap->len, B_WRITE) == 0)  {
		u.u_error = EFAULT;
		return;
	}
	from = 0;
	u.u_error = soreceive(fp->f_socket, &from, &auio, uap->flags);
	if (u.u_error)
		goto bad;
	if (from == 0)
		fromlen = 0;
	else {
		if (fromlen > from->m_len)
			fromlen = from->m_len;
		if (copyout(mtod(from, caddr_t), uap->from, fromlen)) {
			u.u_error = EFAULT;
			goto bad;
		}
	}
	if (copyout((caddr_t)&fromlen, (caddr_t)uap->fromlenaddr,
	    sizeof (fromlen))) {
		u.u_error = EFAULT;
		goto bad;
	}
	u.u_r.r_val1 = uap->len - auio.uio_resid;
bad:
	if (from)
		m_freem(from);
	return;
}

recv()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;

	fp = getf(uap->s);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	auio.uio_resid = uap->len;
	auio.uio_segflg = 0;
	auio.uio_offset = 0;	/* XXX */
	if (useracc(uap->buf, uap->len, B_WRITE) == 0)  {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = soreceive(fp->f_socket, (struct mbuf *)0, &auio, uap->flags);
	u.u_r.r_val1 = uap->len - auio.uio_resid;
}

sendmsg()
{

	u.u_error = EINVAL;
}

recvmsg()
{

	u.u_error = EINVAL;
}

shutdown()
{

	u.u_error = EINVAL;
}

pipe()
{
	register struct file *rf, *wf;
	struct socket *rso, *wso;
	int r;

	u.u_error = socreate(1, &rso, SOCK_STREAM, 0, 0);
	if (u.u_error)
		return;
	u.u_error = socreate(1, &wso, SOCK_STREAM, 0, 0);
	if (u.u_error)
		goto free;
	rf = falloc();
	if (rf == NULL)
		goto free2;
	r = u.u_r.r_val1;
	rf->f_flag = FREAD;
	rf->f_type = DTYPE_SOCKET;
	rf->f_socket = rso;
	wf = falloc();
	if (wf == NULL)
		goto free3;
	wf->f_flag = FWRITE;
	wf->f_type = DTYPE_SOCKET;
	wf->f_socket = wso;
	u.u_r.r_val2 = u.u_r.r_val1;
	u.u_r.r_val1 = r;
	if (piconnect(wso, rso) == 0)
		goto free4;
	return;
free4:
	wf->f_count = 0;
	u.u_ofile[u.u_r.r_val2] = 0;
free3:
	rf->f_count = 0;
	u.u_ofile[r] = 0;
free2:
	wso->so_state |= SS_NOFDREF;
	sofree(wso);
free:
	rso->so_state |= SS_NOFDREF;
	sofree(rso);
}

/*
 * Get socket address.
 */
ssocketaddr()
{
	register struct a {
		int	fdes;
		struct	sockaddr *asa;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register struct socket *so;
	struct mbuf *m;

	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return;
	}
	so = fp->f_socket;
	m = m_getclr(M_WAIT);
	u.u_error =
		(*so->so_proto->pr_usrreq)(so, PRU_SOCKADDR, 0, m, 0);
	if (u.u_error)
		goto bad;
	if (copyout(mtod(m, caddr_t), (caddr_t)uap->asa, sizeof (struct sockaddr)))
		u.u_error = EFAULT;
bad:
	m_freem(m);
}

sockname(aname, name, namelen)
	struct mbuf **aname;
	caddr_t name;
	int namelen;
{
	register struct mbuf *m;

	if (namelen > MLEN)
		return (EINVAL);
	m = m_get(M_WAIT);
	m->m_len = namelen;
	if (copyin(name, mtod(m, caddr_t), namelen)) {
		(void) m_free(m);
		return (EFAULT);
	}
	*aname = m;
	return (0);
}

sockopt(so, opt)
	register struct socketopt *so;
	caddr_t opt;
{
	register struct mbuf *m;

	if (opt == 0) {
		so->so_optlen = 0;
		so->so_optdata = 0;
		return (0);
	}
	if (copyin((caddr_t)opt, (caddr_t)so, sizeof (struct socketopt)))
		return (EFAULT);
	if (so->so_optlen < 0 || so->so_optlen > MLEN)
		return (EINVAL);
	m = m_get(M_WAIT);
	m->m_len = so->so_optlen;
	if (copyin(so->so_optdata, mtod(m, caddr_t), m->m_len)) {
		(void) m_free(m);
		return (EFAULT);
	}
	so->so_optdata = mtod(m, caddr_t);
	return (0);
}
