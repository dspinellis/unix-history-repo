/*	uipc_syscalls.c	4.1	81/11/10	*/

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
 * Socket system call interface.
 *
 * These routines interface the socket routines to UNIX,
 * isolating the system interface from the socket-protocol interface.
 *
 * DO SPLICE STUFF
 * DO PIPE STUFF
 * DO PORTALS
 * DO ASSOCIATIONS
 * DO NEWFD STUFF
 */

/*
 * Socket system call interface.  Copy in arguments
 * set up file descriptor and call internal socket
 * creation routine.
 */
ssocket()
{
	register struct a {
		int	type;
		struct	in_addr *ain;
		int	options;
	} *uap = (struct a *)u.u_ap;
	struct in_addr in;
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
	u.u_error = socket(&so0, uap->type, &in, uap->options);
	if (u.u_error)
		goto bad;
	fp->f_socket = so;
	return;
bad:
	u.u_ofile[u.u_r.r_val1] = 0;
	fp->f_count = 0;
}

/*
 * Pipe system call interface.
 */
spipe()
{

}

static	struct in_addr portalproto = { PF_PORTAL, /* rest don't care */ };
/*
 * Portal system call interface.
 *
 * This call creates a portal.
 * All the difficulty here is in dealing with errors.
 * A long sequence of steps is necessary:
 *	1. a socket must be allocated
 *	2. the server name length must be determined
 *	3. the protal must be entered into the file system
 *	4. the portal type and server must be entered into the portals' file
 *	5. a file descriptor referencing the socket+inode must be returned
 * If any errors occur in this process we must back it all out.
 */
sportal()
{
	register struct a {
		caddr_t	name;
		int	mode;
		caddr_t server;
		int	kind;
	} *ap = (struct a *)u.u_ap;
	struct socket *so;
	struct inode *ip;
	struct file *fp;
	int err, len;
	char ch;

	/*
	 * Allocate the socket for the portal.
	 */
	u.u_error = socket(&so, SOCK_STREAM, &portalproto, SO_NEWFDONCONN);
	if (u.u_error)
		return;

	/*
	 * Check that server name fis in a file system buffer.
	 * This to simplify the creation of the portal service process.
	 */
	if (ap->server) {
		u.u_dirp = ap->server;
		for (len = 0; len < BSIZE-2; len++) {
			register c = uchar();
			if (c < 0)
				goto bad;
			if (c == 0)
				break;
		}
		if (len == BSIZE - 2) {
			u.u_error = EINVAL;
			goto bad;
		}
	}

	/*
	 * Make sure that nothing with the portal's name exists.
	 */
	u.u_dirp = ap->name;
	ip = namei(uchar, 1);
	if (ip != NULL) {
		iput(ip);
		u.u_error = EEXIST;
	}
	if (u.u_error)
		goto bad;

	/*
	 * Make a node in the file system for the portal.
	 */
	ip = maknode((ap->mode & 0x7777) | IFPORTAL);
	if (ip == NULL)
		goto bad;

	/*
	 * Make the first character of the contents of the
	 * portal be the portal type and the rest of the portal be
	 * the pathname of the server (if one was given).
	 */
	ch = (char)ap->kind;
	u.u_base = (caddr_t)&ch;
	u.u_count = 1;
	u.u_offset = 0;
	u.u_segflg = 1;
	writei(ip);
	if (ap->server) {
		u.u_base = ap->server;
		u.u_count = len;
		u.u_segflg = 0;
		writei(ip);
	}
	if (u.u_error)
		goto bad2;
	
	/*
	 * Allocate a file descriptor and make it reference both
	 * the inode representing the portal and the call director
	 * socket for the portal.
	 */
	fp = falloc();
	if (fp == NULL)
		goto bad2;
	fp->f_flags = FPORTAL|FSOCKET;
	fp->f_inode = ip;
	fp->f_socket = s;

	/*
	 * Make the in-core inode reference the socket.
	 */
	ip->i_socket = s;
	prele(ip);
	return;
bad2:
	err = u.u_error;
	iput(ip);
	u.u_dirp = ap->name;
	unlink();
	u.u_error = err;
bad:
	sofree(s);
}

/*
 * Close a socket on last file table reference removal.
 * Initiate disconnect if connected.
 * Free socket when disconnect complete.
 */
skclose(so)
	register struct socket *so;
{
	int s = splnet();		/* conservative */

	if (so->so_pcb == 0)
		goto discard;
	if (so->so_state & SS_ISCONNECTED) {
		u.u_error = disconnect(so, 0);
		if (u.u_error) {
			splx(s);
			return;
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
	sofree(so);
	splx(s);
}

/*
 * Select a socket.
 */
soselect(so, flag)
	register struct socket *so;
	int flag;
{
	register struct proc *p;

	if (soreadable(so))
		return (1);
	if ((p = so->so_rcv.sb_sel) && p->p_wchan == (caddr_t)select)
		so->so_rcv.sb_flags |= SB_COLL;
	else
		so->so_rcv.sb_sel = u.u_procp;
	return (0);
}

/*
 * Wakeup read sleep/select'ers.
 */
sowakeup(so)
	struct socket *so;
{

	if (so->so_rcv.sb_sel && soreadable(so)) {
		selwakeup(so->so_rcv.sb_sel, so->so_rcv.sb_flags & SB_COLL);
		so->so_rcv.sb_sel = 0;
		so->so_rcv.sb_flags &= ~SB_COLL;
	}
	if (so->so_rcv.sb_flags & SB_WAIT) {
		so->so_rcv.sb_flags &= ~SB_WAIT;
		wakeup((caddr_t)&so->so_rcv.sb_cc);
	}
}

/*
 * Connect socket to foreign peer; system call
 * interface.  Copy in arguments and call internal routine.
 */
sconnect()
{
	register struct a {
		int fdes;
		struct in_addr *a;
	} *uap = (struct a *)u.u_ap;
	in_addr in;
	register struct file *fp;
	register struct socket *so;
	int s;

	if (copyin((caddr_t)uap->a, &in, sizeof (in))) {
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
	u.u_error = connect(so, &in);
	if (u.u_error)
		return;
	s = splnet();
	if ((so->so_options & SO_NBIO) && (so->so_state & SS_ISCONN) == 0) {
		u.u_error = EINPROGRESS;
		return;
	}
	while ((so->so_state & (SS_ISCONN|SS_ISCONNING)) == SS_ISCONNING)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
}

/*
 * Disconnect socket from foreign peer; system call
 * interface.  Copy in arguments and call internal routine.
 */
sdisconnect()
{
	register struct a {
		int	fdes;
		in_addr	 *addr;
	} *uap = (struct a *)u.u_ap;
	in_addr in;
	register struct file *fp;
	int s;

	if (uap->addr &&
	    copyin((caddr_t)uap->addr, (caddr_t)&in, sizeof (in))) {
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
	u.u_error = disconnect(fp->f_socket, uap->addr ? &in : 0);
	if (u.u_error)
		return;
	s = splnet();
	if ((so->so_options&SO_NBIO) && (so->so_state&SS_ISCONNECTED))
		return (EINPROGRESS);
	while ((so)->so_state & (SS_ISCONNECTED|SS_ISDISCONNECTING) == SS_ISDISCONNECTING)
		sleep((caddr_t)&so->so_timeo, PZERO+1);
	u.u_error = so->so_error;
}

/*
 * Send data on socket.
 */
ssend()
{
	register struct a {
		int	fdes;
		in_addr	*ain;
		caddr_t	cbuf;
		int	count;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct in_addr in;

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
	u.u_base = uap->cbuf;
	u.u_count = uap->count;
	u.u_segflg = 0;
	if (useracc(u.u_base, u.u_count, B_READ) == 0 ||
	    uap->ain && copyin((caddr_t)uap->ain, (caddr_t)&in, sizeof (in))) {
		u.u_error = EFAULT;
		return;
	}
	u.u_error = send(fp->f_socket, uap->ain ? &in : 0);
}

