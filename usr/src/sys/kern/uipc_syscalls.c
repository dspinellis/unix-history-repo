/*	uipc_syscalls.c	4.5	81/11/20	*/

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
#include "../net/inet.h"
#include "../net/inet_systm.h"

/*
 * Socket system call interface.
 *
 * These routines interface the socket routines to UNIX,
 * isolating the system interface from the socket-protocol interface.
 *
 * TODO:
 *	SO_INTNOTIFY
 */

static	struct sockproto localproto = { PF_LOCAL, 0 };
/*
 * Pipe system call interface.
 */
spipe()
{
	register struct file *rf, *wf;
	struct socket *rso, *wso;
	int r;
COUNT(SPIPE);
	
	u.u_error = socreate(&rso, SOCK_STREAM,
	    &localproto, (struct sockaddr *)0, 0);
	if (u.u_error)
		return;
	u.u_error = socreate(&wso, SOCK_STREAM,
	    &localproto, (struct sockaddr *)0, 0);
	if (u.u_error)
		goto free;
	rf = falloc();
	if (rf == NULL)
		goto free2;
	r = u.u_r.r_val1;
	rf->f_flag = FREAD|FSOCKET;
	rf->f_socket = rso;
	wf = falloc();
	if (wf == NULL)
		goto free3;
	wf->f_flag = FWRITE|FSOCKET;
	wf->f_socket = wso;
	u.u_r.r_val2 = u.u_r.r_val1;
	u.u_r.r_val1 = r;
	if (piconnect(rso, wso) == 0)
		goto free4;
	return;
free4:
	wf->f_count = 0;
	u.u_ofile[u.u_r.r_val1] = 0;
free3:
	rf->f_count = 0;
	u.u_ofile[r] = 0;
free2:
	wso->so_state |= SS_USERGONE;
	sofree(wso);
free:
	rso->so_state |= SS_USERGONE;
	sofree(rso);
}

/*
 * Splice system call interface.
 */
ssplice()
{
	register struct a {
		int	fd1;
		int	fd2;
	} *ap = (struct a *)u.u_ap;
	struct file *f1, *f2;
COUNT(SSPLICE);

	f1 = getf(ap->fd1);
	if (f1 == NULL)
		return;
	f2 = getf(ap->fd2);
	if (f2 == NULL)
		return;
	if (f1 == f2) {
		u.u_error = EINVAL;
		return;
	}
	if ((f1->f_flag & FSOCKET) == 0 || (f2->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	if (f1->f_count > 1 || f2->f_count > 1) {
		u.u_error = ETOOMANYREFS;
		return;
	}
	u.u_error = sosplice(f1->f_socket, f2->f_socket);
	if (u.u_error)
		return;
	u.u_ofile[ap->fd1] = 0;
	u.u_ofile[ap->fd2] = 0;
	f1->f_count = 0;
	f2->f_count = 0;
}

/*
 * Socket system call interface.  Copy sa arguments
 * set up file descriptor and call internal socket
 * creation routine.
 */
ssocket()
{
	register struct a {
		int	type;
		struct	sockproto *asp;
		struct	sockaddr *asa;
		int	options;
	} *uap = (struct a *)u.u_ap;
	struct sockproto sp;
	struct sockaddr sa;
	struct socket *so;
	register struct file *fp;
COUNT(SSOCKET);

	if ((fp = falloc()) == NULL)
		return;
	fp->f_flag = FSOCKET|FREAD|FWRITE;
	if (uap->asp && copyin((caddr_t)uap->asp, (caddr_t)&sp, sizeof (sp)) ||
	    uap->asa && copyin((caddr_t)uap->asa, (caddr_t)&sa, sizeof (sa))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = socreate(&so, uap->type,
	    uap->asp ? &sp : 0, uap->asa ? &sa : 0, uap->options);
	if (u.u_error)
		goto bad;
	fp->f_socket = so;
	return;
bad:
	u.u_ofile[u.u_r.r_val1] = 0;
	fp->f_count = 0;
}

/*
 * Accept system call interface.
 */
saccept()
{
	register struct a {
		int	fdes;
		struct	sockaddr *asa;
	} *uap = (struct a *)u.u_ap;
	struct sockaddr sa;
	register struct file *fp;
	struct socket *so;
	int s;
COUNT(SACCEPT);

	if (uap->asa && useracc((caddr_t)uap->asa, sizeof (sa), B_WRITE)==0) {
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
	s = splnet();
	so = fp->f_socket;
	if ((so->so_options & SO_NBIO) &&
	    (so->so_state & SS_CONNAWAITING) == 0) {
		u.u_error = EWOULDBLOCK;
		splx(s);
		return;
	}
	u.u_error = soaccept(so, &sa);
	if (u.u_error) {
		splx(s);
		return;
	}
	/* deal with new file descriptor case */
	/* u.u_r.r_val1 = ... */
	splx(s);
}

/*
 * Connect socket to foreign peer; system call
 * interface.  Copy sa arguments and call internal routine.
 */
sconnect()
{
	register struct a {
		int	fdes;
		struct	sockaddr *a;
	} *uap = (struct a *)u.u_ap;
	struct sockaddr sa;
	register struct file *fp;
	register struct socket *so;
	int s;
COUNT(SCONNECT);

	if (copyin((caddr_t)uap->a, (caddr_t)&sa, sizeof (sa))) {
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
	so = fp->f_socket;
	u.u_error = soconnect(so, &sa);
	if (u.u_error)
		return;
	s = splnet();
	if ((so->so_options & SO_NBIO) &&
	    (so->so_state & SS_ISCONNECTING)) {
		u.u_error = EINPROGRESS;
		splx(s);
		return;
	}
	while ((so->so_state & SS_ISCONNECTING) && so->so_error == 0)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
	so->so_error = 0;
	splx(s);
}

/*
 * Disconnect socket from foreign peer; system call
 * interface.  Copy sa arguments and call internal routine.
 */
sdisconnect()
{
	register struct a {
		int	fdes;
		struct	sockaddr *asa;
	} *uap = (struct a *)u.u_ap;
	struct sockaddr sa;
	register struct file *fp;
	register struct socket *so;
	int s;
COUNT(SDISCONNECT);

	if (uap->asa &&
	    copyin((caddr_t)uap->asa, (caddr_t)&sa, sizeof (sa))) {
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
	so = fp->f_socket;
	u.u_error = sodisconnect(so, uap->asa ? &sa : 0);
	if (u.u_error)
		return;
	s = splnet();
	if ((so->so_options&SO_NBIO) && (so->so_state&SS_ISDISCONNECTING)) {
		u.u_error = EINPROGRESS;
		splx(s);
		return;
	}
	while (so->so_state & SS_ISDISCONNECTING)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
	so->so_error = 0;
	splx(s);
}

/*
 * Send data on socket.
 */
ssend()
{
	register struct a {
		int	fdes;
		struct	sockaddr *asa;
		caddr_t	cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct sockaddr sa;
COUNT(SSEND);

	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	u.u_count = uap->count;
	u.u_segflg = 0;
	if (useracc(uap->cbuf, uap->count, B_READ) == 0 ||
	    uap->asa && copyin((caddr_t)uap->asa, (caddr_t)&sa, sizeof (sa))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = sosend(fp->f_socket, uap->asa ? &sa : 0);
}

/*
 * Receive data on socket.
 */
sreceive()
{
	register struct a {
		int	fdes;
		struct	sockaddr *asa;
		caddr_t	cbuf;
		u_int	count;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct sockaddr sa;
COUNT(SRECEIVE);

	fp = getf(uap->fdes);
	if (fp == 0)
		return;
	if ((fp->f_flag & FSOCKET) == 0) {
		u.u_error = ENOTSOCK;
		return;
	}
	u.u_count = uap->count;
	u.u_segflg = 0;
	if (useracc(uap->cbuf, uap->count, B_WRITE) == 0 ||
	    uap->asa && copyin((caddr_t)uap->asa, (caddr_t)&sa, sizeof (sa))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = soreceive(fp->f_socket, uap->asa ? &sa : 0);
	if (u.u_error)
		return;
	if (uap->asa)
		(void) copyout((caddr_t)&sa, (caddr_t)uap->asa, sizeof (sa));
}
