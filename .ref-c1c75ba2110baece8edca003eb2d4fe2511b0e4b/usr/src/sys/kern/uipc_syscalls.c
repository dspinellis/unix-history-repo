/*
 * Copyright (c) 1982, 1986, 1988 The Regents of the University of California.
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
 *	@(#)uipc_syscalls.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "file.h"
#include "buf.h"
#include "malloc.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"

/*
 * System call interface to the socket abstraction.
 */

struct	file *getsock();
extern	struct fileops socketops;

socket()
{
	register struct a {
		int	domain;
		int	type;
		int	protocol;
	} *uap = (struct a *)u.u_ap;
	struct socket *so;
	register struct file *fp;

	if ((fp = falloc()) == NULL)
		return;
	fp->f_flag = FREAD|FWRITE;
	fp->f_type = DTYPE_SOCKET;
	fp->f_ops = &socketops;
	u.u_error = socreate(uap->domain, &so, uap->type, uap->protocol);
	if (u.u_error)
		goto bad;
	fp->f_data = (caddr_t)so;
	return;
bad:
	u.u_ofile[u.u_r.r_val1] = 0;
	fp->f_count = 0;
}

bind()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	namelen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct mbuf *nam;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	u.u_error = sockargs(&nam, uap->name, uap->namelen, MT_SONAME);
	if (u.u_error)
		return;
	u.u_error = sobind((struct socket *)fp->f_data, nam);
	m_freem(nam);
}

listen()
{
	register struct a {
		int	s;
		int	backlog;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	u.u_error = solisten((struct socket *)fp->f_data, uap->backlog);
}

accept()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	*anamelen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct mbuf *nam;
	int namelen;
	int s;
	register struct socket *so;

	if (uap->name == 0)
		goto noname;
	u.u_error = copyin((caddr_t)uap->anamelen, (caddr_t)&namelen,
		sizeof (namelen));
	if (u.u_error)
		return;
	if (useracc((caddr_t)uap->name, (u_int)namelen, B_WRITE) == 0) {
		u.u_error = EFAULT;
		return;
	}
noname:
	fp = getsock(uap->s);
	if (fp == 0)
		return;
	s = splnet();
	so = (struct socket *)fp->f_data;
	if ((so->so_options & SO_ACCEPTCONN) == 0) {
		u.u_error = EINVAL;
		splx(s);
		return;
	}
	if ((so->so_state & SS_NBIO) && so->so_qlen == 0) {
		u.u_error = EWOULDBLOCK;
		splx(s);
		return;
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
		so->so_error = 0;
		splx(s);
		return;
	}
	if (ufalloc(0) < 0) {
		splx(s);
		return;
	}
	fp = falloc();
	if (fp == 0) {
		u.u_ofile[u.u_r.r_val1] = 0;
		splx(s);
		return;
	}
	{ struct socket *aso = so->so_q;
	  if (soqremque(aso, 1) == 0)
		panic("accept");
	  so = aso;
	}
	fp->f_type = DTYPE_SOCKET;
	fp->f_flag = FREAD|FWRITE;
	fp->f_ops = &socketops;
	fp->f_data = (caddr_t)so;
	nam = m_get(M_WAIT, MT_SONAME);
	(void) soaccept(so, nam);
	if (uap->name) {
		if (namelen > nam->m_len)
			namelen = nam->m_len;
		/* SHOULD COPY OUT A CHAIN HERE */
		(void) copyout(mtod(nam, caddr_t), (caddr_t)uap->name,
		    (u_int)namelen);
		(void) copyout((caddr_t)&namelen, (caddr_t)uap->anamelen,
		    sizeof (*uap->anamelen));
	}
	m_freem(nam);
	splx(s);
}

#ifdef COMPAT_43
oaccept()
{
	return (accept());
}
#endif

connect()
{
	register struct a {
		int	s;
		caddr_t	name;
		int	namelen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register struct socket *so;
	struct mbuf *nam;
	int s;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	so = (struct socket *)fp->f_data;
	if ((so->so_state & SS_NBIO) &&
	    (so->so_state & SS_ISCONNECTING)) {
		u.u_error = EALREADY;
		return;
	}
	u.u_error = sockargs(&nam, uap->name, uap->namelen, MT_SONAME);
	if (u.u_error)
		return;
	u.u_error = soconnect(so, nam);
	if (u.u_error)
		goto bad;
	if ((so->so_state & SS_NBIO) &&
	    (so->so_state & SS_ISCONNECTING)) {
		u.u_error = EINPROGRESS;
		m_freem(nam);
		return;
	}
	s = splnet();
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0)
			u.u_error = EINTR;
		goto bad2;
	}
	while ((so->so_state & SS_ISCONNECTING) && so->so_error == 0)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
	so->so_error = 0;
bad2:
	splx(s);
bad:
	so->so_state &= ~SS_ISCONNECTING;
	m_freem(nam);
}

socketpair()
{
	register struct a {
		int	domain;
		int	type;
		int	protocol;
		int	*rsv;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp1, *fp2;
	struct socket *so1, *so2;
	int sv[2];

	if (useracc((caddr_t)uap->rsv, 2 * sizeof (int), B_WRITE) == 0) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = socreate(uap->domain, &so1, uap->type, uap->protocol);
	if (u.u_error)
		return;
	u.u_error = socreate(uap->domain, &so2, uap->type, uap->protocol);
	if (u.u_error)
		goto free;
	fp1 = falloc();
	if (fp1 == NULL)
		goto free2;
	sv[0] = u.u_r.r_val1;
	fp1->f_flag = FREAD|FWRITE;
	fp1->f_type = DTYPE_SOCKET;
	fp1->f_ops = &socketops;
	fp1->f_data = (caddr_t)so1;
	fp2 = falloc();
	if (fp2 == NULL)
		goto free3;
	fp2->f_flag = FREAD|FWRITE;
	fp2->f_type = DTYPE_SOCKET;
	fp2->f_ops = &socketops;
	fp2->f_data = (caddr_t)so2;
	sv[1] = u.u_r.r_val1;
	u.u_error = soconnect2(so1, so2);
	if (u.u_error)
		goto free4;
	if (uap->type == SOCK_DGRAM) {
		/*
		 * Datagram socket connection is asymmetric.
		 */
		 u.u_error = soconnect2(so2, so1);
		 if (u.u_error)
			goto free4;
	}
	u.u_r.r_val1 = 0;
	(void) copyout((caddr_t)sv, (caddr_t)uap->rsv, 2 * sizeof (int));
	return;
free4:
	fp2->f_count = 0;
	u.u_ofile[sv[1]] = 0;
free3:
	fp1->f_count = 0;
	u.u_ofile[sv[0]] = 0;
free2:
	(void)soclose(so2);
free:
	(void)soclose(so1);
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
	struct msghdr msg;
	struct iovec aiov;

	msg.msg_name = uap->to;
	msg.msg_namelen = uap->tolen;
	msg.msg_iov = &aiov;
	msg.msg_iovlen = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	msg.msg_accrights = 0;
	msg.msg_control = 0;
	sendit(uap->s, &msg, uap->flags);
}

#ifdef COMPAT_43
osend()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov;

	msg.msg_name = 0;
	msg.msg_namelen = 0;
	msg.msg_iov = &aiov;
	msg.msg_iovlen = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	msg.msg_accrights = 0;
	msg.msg_control = 0;
	sendit(uap->s, &msg, uap->flags);
}

osendmsg()
{
	register struct a {
		int	s;
		caddr_t	msg;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov[MSG_MAXIOVLEN];

	u.u_error = copyin(uap->msg, (caddr_t)&msg, sizeof (struct omsghdr));
	if (u.u_error)
		return;
	if ((u_int)msg.msg_iovlen >= sizeof (aiov) / sizeof (aiov[0])) {
		u.u_error = EMSGSIZE;
		return;
	}
	u.u_error = copyin((caddr_t)msg.msg_iov, (caddr_t)aiov,
		(unsigned)(msg.msg_iovlen * sizeof (aiov[0])));
	if (u.u_error)
		return;
	msg.msg_control = 0;
	msg.msg_controllen = 0;
	sendit(uap->s, &msg, uap->flags);
}
#endif

sendmsg()
{
	register struct a {
		int	s;
		caddr_t	msg;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov[MSG_MAXIOVLEN];

	u.u_error = copyin(uap->msg, (caddr_t)&msg, sizeof (msg));
	if (u.u_error)
		return;
	if ((u_int)msg.msg_iovlen >= sizeof (aiov) / sizeof (aiov[0])) {
		u.u_error = EMSGSIZE;
		return;
	}
	u.u_error = copyin((caddr_t)msg.msg_iov, (caddr_t)aiov,
		(unsigned)(msg.msg_iovlen * sizeof (aiov[0])));
	if (u.u_error)
		return;
	msg.msg_iov = aiov;
	sendit(uap->s, &msg, uap->flags);
}

sendit(s, mp, flags)
	int s;
	register struct msghdr *mp;
	int flags;
{
	register struct file *fp;
	struct uio auio;
	register struct iovec *iov;
	register int i;
	struct mbuf *to, *rights, *control;
	int len;
	
	fp = getsock(s);
	if (fp == 0)
		return;
	auio.uio_iov = mp->msg_iov;
	auio.uio_iovcnt = mp->msg_iovlen;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_offset = 0;			/* XXX */
	auio.uio_resid = 0;
	iov = mp->msg_iov;
	for (i = 0; i < mp->msg_iovlen; i++, iov++) {
		if (iov->iov_len < 0) {
			u.u_error = EINVAL;
			return;
		}
		if (iov->iov_len == 0)
			continue;
		if (useracc(iov->iov_base, (u_int)iov->iov_len, B_READ) == 0) {
			u.u_error = EFAULT;
			return;
		}
		auio.uio_resid += iov->iov_len;
	}
	if (mp->msg_name) {
		u.u_error =
		    sockargs(&to, mp->msg_name, mp->msg_namelen, MT_SONAME);
		if (u.u_error)
			return;
	} else
		to = 0;
	if (mp->msg_accrights) {
		u.u_error =
		    sockargs(&rights, mp->msg_accrights, mp->msg_accrightslen,
		    MT_RIGHTS);
		if (u.u_error)
			goto bad;
	} else
		rights = 0;
	if (mp->msg_control) {
		u.u_error =
		    sockargs(&control, mp->msg_control, mp->msg_controllen,
		    MT_CONTROL);
		if (u.u_error)
			goto bad;
	} else
		control = 0;
	len = auio.uio_resid;
	if (setjmp(&u.u_qsave)) {			/* XXX */
		if (auio.uio_resid == len) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				u.u_error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		u.u_error = sosend((struct socket *)fp->f_data, to, &auio,
		    flags, rights, control);
	u.u_r.r_val1 = len - auio.uio_resid;
bad:
	if (rights)
		m_freem(rights);
	if (to)
		m_freem(to);
	if (control)
		m_freem(control);
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
	struct msghdr msg;
	struct iovec aiov;
	int len;

	if (uap->fromlenaddr) {
		u.u_error = copyin((caddr_t)uap->fromlenaddr, (caddr_t)&len,
		   sizeof (len));
		if (u.u_error)
			return;
	} else
		len = 0;
	msg.msg_name = uap->from;
	msg.msg_namelen = len;
	msg.msg_iov = &aiov;
	msg.msg_iovlen = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	msg.msg_accrights = 0;
	msg.msg_control = 0;
	msg.msg_flags = uap->flags;
	recvit(uap->s, &msg, (caddr_t)uap->fromlenaddr, (caddr_t)0, (caddr_t)0);
}

#ifdef COMPAT_43
orecvfrom()
{
	return (recvfrom());
}

orecv()
{
	register struct a {
		int	s;
		caddr_t	buf;
		int	len;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov;

	msg.msg_name = 0;
	msg.msg_namelen = 0;
	msg.msg_iov = &aiov;
	msg.msg_iovlen = 1;
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->len;
	msg.msg_accrights = 0;
	msg.msg_control = 0;
	msg.msg_flags = uap->flags;
	recvit(uap->s, &msg, (caddr_t)0, (caddr_t)0, (caddr_t)0);
}

orecvmsg()
{
	register struct a {
		int	s;
		struct	omsghdr *msg;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov[MSG_MAXIOVLEN];

	u.u_error = copyin((caddr_t)uap->msg, (caddr_t)&msg,
	    sizeof (struct omsghdr));
	if (u.u_error)
		return;
	if ((u_int)msg.msg_iovlen >= sizeof (aiov) / sizeof (aiov[0])) {
		u.u_error = EMSGSIZE;
		return;
	}
	msg.msg_control = 0;
	msg.msg_flags = uap->flags;
	u.u_error = copyin((caddr_t)msg.msg_iov, (caddr_t)aiov,
		(unsigned)(msg.msg_iovlen * sizeof (aiov[0])));
	if (u.u_error)
		return;
	msg.msg_iov = aiov;
	if (msg.msg_accrights)
		if (useracc((caddr_t)msg.msg_accrights,
		    (unsigned)msg.msg_accrightslen, B_WRITE) == 0) {
			u.u_error = EFAULT;
			return;
		}
	    
	recvit(uap->s, &msg, (caddr_t)&uap->msg->msg_namelen,
	    (caddr_t)&uap->msg->msg_accrightslen, (caddr_t)0);
}
#endif

recvmsg()
{
	register struct a {
		int	s;
		struct	msghdr *msg;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct msghdr msg;
	struct iovec aiov[MSG_MAXIOVLEN];

	u.u_error = copyin((caddr_t)uap->msg, (caddr_t)&msg, sizeof (msg));
	if (u.u_error)
		return;
	if ((u_int)msg.msg_iovlen >= sizeof (aiov) / sizeof (aiov[0])) {
		u.u_error = EMSGSIZE;
		return;
	}
	msg.msg_flags = uap->flags;
	u.u_error = copyin((caddr_t)msg.msg_iov, (caddr_t)aiov,
		(unsigned)(msg.msg_iovlen * sizeof (aiov[0])));
	if (u.u_error)
		return;
	msg.msg_iov = aiov;
	if (msg.msg_accrights)
		if (useracc((caddr_t)msg.msg_accrights,
		    (unsigned)msg.msg_accrightslen, B_WRITE) == 0) {
			u.u_error = EFAULT;
			return;
		}
	if (msg.msg_control)
		if (useracc((caddr_t)msg.msg_control,
		    (unsigned)msg.msg_controllen, B_WRITE) == 0) {
			u.u_error = EFAULT;
			return;
		}
	recvit(uap->s, &msg, (caddr_t)0, (caddr_t)0, (caddr_t)0);
	u.u_error = copyout((caddr_t)&msg, (caddr_t)uap->msg, sizeof (msg));
}

recvit(s, mp, namelenp, rightslenp, controllenp)
	int s;
	register struct msghdr *mp;
	caddr_t namelenp, rightslenp, controllenp;
{
	register struct file *fp;
	struct uio auio;
	register struct iovec *iov;
	register int i;
	int len;
	struct mbuf *from = 0, *rights = 0, *control = 0;
	
	fp = getsock(s);
	if (fp == 0)
		return;
	auio.uio_iov = mp->msg_iov;
	auio.uio_iovcnt = mp->msg_iovlen;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_offset = 0;			/* XXX */
	auio.uio_resid = 0;
	iov = mp->msg_iov;
	for (i = 0; i < mp->msg_iovlen; i++, iov++) {
		if (iov->iov_len < 0) {
			u.u_error = EINVAL;
			return;
		}
		if (iov->iov_len == 0)
			continue;
		if (useracc(iov->iov_base, (u_int)iov->iov_len, B_WRITE) == 0) {
			u.u_error = EFAULT;
			return;
		}
		auio.uio_resid += iov->iov_len;
	}
	len = auio.uio_resid;
	if (setjmp(&u.u_qsave)) {			/* XXX */
		if (auio.uio_resid == len) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				u.u_error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		u.u_error = soreceive((struct socket *)fp->f_data, &from, &auio,
		    &mp->msg_flags, &rights, &control);
	u.u_r.r_val1 = len - auio.uio_resid;
	if (mp->msg_name) {
		len = mp->msg_namelen;
		if (len <= 0 || from == 0)
			len = 0;
		else {
			if (len > from->m_len)
				len = from->m_len;
			(void) copyout(mtod(from, caddr_t),
			    (caddr_t)mp->msg_name, (unsigned)len);
		}
		if (namelenp)
			(void) copyout((caddr_t)&len, namelenp, sizeof (int));
	}
	if (mp->msg_accrights) {
		len = mp->msg_accrightslen;
		if (len <= 0 || rights == 0)
			len = 0;
		else {
			if (len > rights->m_len)
				len = rights->m_len;
			(void) copyout((caddr_t)mtod(rights, caddr_t),
			    (caddr_t)mp->msg_accrights, (unsigned)len);
		}
		if (rightslenp)
			(void) copyout((caddr_t)&len, rightslenp, sizeof (int));
	}
	if (mp->msg_control) {
		len = mp->msg_controllen;
		if (len <= 0 || control == 0)
			len = 0;
		else {
			if (len >= control->m_len)
				len = control->m_len;
			else
				mp->msg_flags |= MSG_CTRUNC;
			(void) copyout((caddr_t)mtod(control, caddr_t),
			    (caddr_t)mp->msg_control, (unsigned)len);
		}
		if (controllenp)
			(void) copyout((caddr_t)&len, controllenp, sizeof(int));
	}
	if (rights)
		m_freem(rights);
	if (from)
		m_freem(from);
	if (control)
		m_freem(control);
}

shutdown()
{
	struct a {
		int	s;
		int	how;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	u.u_error = soshutdown((struct socket *)fp->f_data, uap->how);
}

setsockopt()
{
	struct a {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	valsize;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct mbuf *m = NULL;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	if (uap->valsize > MLEN) {
		u.u_error = EINVAL;
		return;
	}
	if (uap->val) {
		m = m_get(M_WAIT, MT_SOOPTS);
		if (m == NULL) {
			u.u_error = ENOBUFS;
			return;
		}
		u.u_error =
		    copyin(uap->val, mtod(m, caddr_t), (u_int)uap->valsize);
		if (u.u_error) {
			(void) m_free(m);
			return;
		}
		m->m_len = uap->valsize;
	}
	u.u_error =
	    sosetopt((struct socket *)fp->f_data, uap->level, uap->name, m);
}

getsockopt()
{
	struct a {
		int	s;
		int	level;
		int	name;
		caddr_t	val;
		int	*avalsize;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct mbuf *m = NULL;
	int valsize;

	fp = getsock(uap->s);
	if (fp == 0)
		return;
	if (uap->val) {
		u.u_error = copyin((caddr_t)uap->avalsize, (caddr_t)&valsize,
			sizeof (valsize));
		if (u.u_error)
			return;
	} else
		valsize = 0;
	u.u_error =
	    sogetopt((struct socket *)fp->f_data, uap->level, uap->name, &m);
	if (u.u_error)
		goto bad;
	if (uap->val && valsize && m != NULL) {
		if (valsize > m->m_len)
			valsize = m->m_len;
		u.u_error = copyout(mtod(m, caddr_t), uap->val, (u_int)valsize);
		if (u.u_error)
			goto bad;
		u.u_error = copyout((caddr_t)&valsize, (caddr_t)uap->avalsize,
		    sizeof (valsize));
	}
bad:
	if (m != NULL)
		(void) m_free(m);
}

pipe()
{
	register struct file *rf, *wf;
	struct socket *rso, *wso;
	int r;

	u.u_error = socreate(AF_UNIX, &rso, SOCK_STREAM, 0);
	if (u.u_error)
		return;
	u.u_error = socreate(AF_UNIX, &wso, SOCK_STREAM, 0);
	if (u.u_error)
		goto free;
	rf = falloc();
	if (rf == NULL)
		goto free2;
	r = u.u_r.r_val1;
	rf->f_flag = FREAD;
	rf->f_type = DTYPE_SOCKET;
	rf->f_ops = &socketops;
	rf->f_data = (caddr_t)rso;
	wf = falloc();
	if (wf == NULL)
		goto free3;
	wf->f_flag = FWRITE;
	wf->f_type = DTYPE_SOCKET;
	wf->f_ops = &socketops;
	wf->f_data = (caddr_t)wso;
	u.u_r.r_val2 = u.u_r.r_val1;
	u.u_r.r_val1 = r;
	if (u.u_error = unp_connect2(wso, rso))
		goto free4;
	wso->so_state |= SS_CANTRCVMORE;
	rso->so_state |= SS_CANTSENDMORE;
	return;
free4:
	wf->f_count = 0;
	u.u_ofile[u.u_r.r_val2] = 0;
free3:
	rf->f_count = 0;
	u.u_ofile[r] = 0;
free2:
	(void)soclose(wso);
free:
	(void)soclose(rso);
}

/*
 * Get socket name.
 */
getsockname()
{
	register struct a {
		int	fdes;
		caddr_t	asa;
		int	*alen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register struct socket *so;
	struct mbuf *m;
	int len;

	fp = getsock(uap->fdes);
	if (fp == 0)
		return;
	u.u_error = copyin((caddr_t)uap->alen, (caddr_t)&len, sizeof (len));
	if (u.u_error)
		return;
	so = (struct socket *)fp->f_data;
	m = m_getclr(M_WAIT, MT_SONAME);
	if (m == NULL) {
		u.u_error = ENOBUFS;
		return;
	}
	u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_SOCKADDR, 0, m, 0);
	if (u.u_error)
		goto bad;
	if (len > m->m_len)
		len = m->m_len;
	u.u_error = copyout(mtod(m, caddr_t), (caddr_t)uap->asa, (u_int)len);
	if (u.u_error)
		goto bad;
	u.u_error = copyout((caddr_t)&len, (caddr_t)uap->alen, sizeof (len));
bad:
	m_freem(m);
}

#ifdef COMPAT_43
ogetsockname()
{
	return (getsockname());
}
#endif

/*
 * Get name of peer for connected socket.
 */
getpeername()
{
	register struct a {
		int	fdes;
		caddr_t	asa;
		int	*alen;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register struct socket *so;
	struct mbuf *m;
	int len;

	fp = getsock(uap->fdes);
	if (fp == 0)
		return;
	so = (struct socket *)fp->f_data;
	if ((so->so_state & SS_ISCONNECTED) == 0) {
		u.u_error = ENOTCONN;
		return;
	}
	m = m_getclr(M_WAIT, MT_SONAME);
	if (m == NULL) {
		u.u_error = ENOBUFS;
		return;
	}
	u.u_error = copyin((caddr_t)uap->alen, (caddr_t)&len, sizeof (len));
	if (u.u_error)
		return;
	u.u_error = (*so->so_proto->pr_usrreq)(so, PRU_PEERADDR, 0, m, 0);
	if (u.u_error)
		goto bad;
	if (len > m->m_len)
		len = m->m_len;
	u.u_error = copyout(mtod(m, caddr_t), (caddr_t)uap->asa, (u_int)len);
	if (u.u_error)
		goto bad;
	u.u_error = copyout((caddr_t)&len, (caddr_t)uap->alen, sizeof (len));
bad:
	m_freem(m);
}

#ifdef COMPAT_43
ogetpeername()
{
	return (getpeername());
}
#endif

sockargs(aname, name, namelen, type)
	struct mbuf **aname;
	caddr_t name;
	int namelen, type;
{
	register struct mbuf *m;
	int error;

	if ((u_int)namelen > MLEN)
		return (EINVAL);
	m = m_get(M_WAIT, type);
	if (m == NULL)
		return (ENOBUFS);
	m->m_len = namelen;
	error = copyin(name, mtod(m, caddr_t), (u_int)namelen);
	if (error)
		(void) m_free(m);
	else
		*aname = m;
	return (error);
}

struct file *
getsock(fdes)
	int fdes;
{
	register struct file *fp;

	fp = getf(fdes);
	if (fp == NULL)
		return (0);
	if (fp->f_type != DTYPE_SOCKET) {
		u.u_error = ENOTSOCK;
		return (0);
	}
	return (fp);
}
