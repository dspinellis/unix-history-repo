/*	sys_socket.c	4.3	83/06/12	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/ioctl.h"
#include "../h/uio.h"
#include "../h/stat.h"

#include "../net/if.h"
#include "../net/route.h"

int	soo_rw(), soo_ioctl(), soo_select(), soo_close();
struct	fileops socketops =
    { soo_rw, soo_ioctl, soo_select, soo_close };

soo_rw(fp, rw, uio)
	struct file *fp;
	enum uio_rw rw;
	struct uio *uio;
{
	int soreceive(), sosend();

	return (
	    (*(rw==UIO_READ?soreceive:sosend))
	      ((struct socket *)fp->f_data, 0, uio, 0, 0));
}

soo_ioctl(fp, cmd, data)
	struct file *fp;
	int cmd;
	register caddr_t data;
{
	register struct socket *so = (struct socket *)fp->f_data;

	switch (cmd) {

	case FIONBIO:
		if (*(int *)data)
			so->so_state |= SS_NBIO;
		else
			so->so_state &= ~SS_NBIO;
		break;

	case FIOASYNC:
		if (*(int *)data)
			so->so_state |= SS_ASYNC;
		else
			so->so_state &= ~SS_ASYNC;
		break;

	case FIONREAD:
		*(int *)data = so->so_rcv.sb_cc;
		break;

	case SIOCSPGRP:
		so->so_pgrp = *(int *)data;
		break;

	case SIOCGPGRP:
		*(int *)data = so->so_pgrp;
		break;

	case SIOCATMARK:
		*(int *)data = (so->so_state&SS_RCVATMARK) != 0;
		break;

	/* routing table update calls */
	case SIOCADDRT:
	case SIOCDELRT:
		if (!suser())
			return (u.u_error);
		return (rtrequest(cmd, (struct rtentry *)data));

	/* interface parameter requests */
	case SIOCSIFADDR:
	case SIOCSIFFLAGS:
	case SIOCSIFDSTADDR:
		if (!suser())
			return (u.u_error);
		return (ifrequest(cmd, data));

	case SIOCGIFADDR:
	case SIOCGIFFLAGS:
	case SIOCGIFDSTADDR:
		return (ifrequest(cmd, data));

	case SIOCGIFCONF:
		return (ifconf(cmd, data));

	/* type/protocol specific ioctls */
	default:
		return (ENOTTY);		/* XXX */
	}
	return (0);
}

soo_select(fp, which)
	struct file *fp;
	int which;
{
	register struct socket *so = (struct socket *)fp->f_data;
	register int s = splnet();

	switch (which) {

	case FREAD:
		if (soreadable(so)) {
			splx(s);
			return (1);
		}
		sbselqueue(&so->so_rcv);
		break;

	case FWRITE:
		if (sowriteable(so)) {
			splx(s);
			return (1);
		}
		sbselqueue(&so->so_snd);
		break;
	}
	splx(s);
	return (0);
}

soo_stat(so, ub)
	register struct socket *so;
	register struct stat *ub;
{

	bzero((caddr_t)ub, sizeof (*ub));
#ifdef notdef
	return ((*so->so_proto->pr_usrreq)(so, PRU_SENSE,
	    (struct mbuf *)ub, (struct mbuf *)0, 
	    (struct mbuf *)0));
#endif
	return (0);
}

soo_close(fp)
	struct file *fp;
{
	int error = soclose((struct socket *)fp->f_data);

	fp->f_data = 0;
	return (error);
}

/*ARGSUSED*/
soo_lock(so, pf, how)
	struct socket *so;
	u_char *pf;
	int how;
{

	return (EOPNOTSUPP);
}

/*ARGSUSED*/
soo_unlock(so, flags)
	struct socket *so;
	int flags;
{

	panic("soo_unlock");
}
