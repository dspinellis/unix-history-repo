/*	kern_descrip.c	5.7	82/09/06	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/conf.h"
#include "../h/file.h"
#include "../h/inline.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/mount.h"

#include "../h/descrip.h"

/*
 * Descriptor management.
 */

/*
 * TODO:
 *	getf should be renamed
 *	ufalloc side effects are gross
 */

/*
 * System calls on descriptors.
 */
getdtablesize()
{

	u.u_r.r_val1 = NOFILE;
}

getdprop()
{
	register struct a {
		int	d;
		struct	dtype *dtypeb;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct dtype adtype;

	fp = getf(uap->d);
	if (fp == 0)
		return;
	adtype.dt_type = 0;		/* XXX */
	adtype.dt_protocol = 0;		/* XXX */
	if (copyout((caddr_t)&adtype, (caddr_t)uap->dtypeb,
	    sizeof (struct dtype)) < 0) {
		u.u_error = EFAULT;
		return;
	}
}

getdopt()
{

}

setdopt()
{

}

dup()
{
	register struct a {
		int	i;
	} *uap = (struct a *) u.u_ap;
	struct file *fp;
	int j;

	if (uap->i &~ 077) { uap->i &= 077; dup2(); return; }	/* XXX */

	fp = getf(uap->i);
	if (fp == 0)
		return;
	j = ufalloc();
	if (j < 0)
		return;
	dupit(j, fp, u.u_pofile[uap->i] & (RDLOCK|WRLOCK));
}

dup2()
{
	register struct a {
		int	i, j;
	} *uap = (struct a *) u.u_ap;
	register struct file *fp;

	fp = getf(uap->i);
	if (fp == 0)
		return;
	if (uap->j < 0 || uap->j >= NOFILE) {
		u.u_error = EBADF;
		return;
	}
	u.u_r.r_val1 = uap->j;
	if (uap->i == uap->j)
		return;
	if (u.u_ofile[uap->j]) {
		closef(u.u_ofile[uap->j], 0, u.u_pofile[uap->j]);
		if (u.u_error)
			return;
		/* u.u_ofile[uap->j] = 0; */
		/* u.u_pofile[uap->j] = 0; */
	}
	dupit(uap->j, fp, u.u_pofile[uap->i] & (RDLOCK|WRLOCK));
}

dupit(fd, fp, lockflags)
	int fd;
	register struct file *fp;
	register int lockflags;
{

	u.u_ofile[fd] = fp;
	u.u_pofile[fd] = lockflags;
	fp->f_count++;
	if (lockflags&RDLOCK)
		fp->f_inode->i_rdlockc++;
	if (lockflags&WRLOCK)
		fp->f_inode->i_wrlockc++;
}

close()
{
	register struct a {
		int	i;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	fp = getf(uap->i);
	if (fp == 0)
		return;
	closef(fp, 0, u.u_pofile[uap->i]);
	/* WHAT IF u.u_error ? */
	u.u_ofile[uap->i] = NULL;
	u.u_pofile[uap->i] = 0;
}

wrap()
{
	register struct a {
		int	d;
		struct	dtype *dtypeb;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct dtype adtype;

	fp = getf(uap->d);
	if (fp == 0)
		return;
	if (copyin((caddr_t)uap->dtypeb, (caddr_t)&adtype,
	    sizeof (struct dtype)) < 0) {
		u.u_error = EFAULT;
		return;
	}
	/* DO WRAP */
}

int	nselcoll;
/*
 * Select system call.
 */
select()
{
	register struct uap  {
		fd_set	*rp, *wp, *ep;
		struct	timeval *tv;
	} *uap = (struct uap *)u.u_ap;
	fd_set rd, wr;
	int nfds = 0, readable = 0, writeable = 0;
	struct timeval atv, origin, now;
	int s, tsel, ncoll, rem;

	if (uap->tv) {
		if (copyin((caddr_t)uap->tv, (caddr_t)&atv, sizeof (atv))) {
			u.u_error = EFAULT;
			return;
		}
	} else
		timerclear(&atv);
	if (uap->rp && copyin((caddr_t)uap->rp,(caddr_t)&rd,sizeof(fd_set)))
		return;
	if (uap->wp && copyin((caddr_t)uap->wp,(caddr_t)&wr,sizeof(fd_set)))
		return;
retry:
	s = spl7(); now = time; splx(s);
	ncoll = nselcoll;
	u.u_procp->p_flag |= SSEL;
	if (uap->rp)
		readable = selscan(rd, &nfds, FREAD);
	if (uap->wp)
		writeable = selscan(wr, &nfds, FWRITE);
	if (u.u_error)
		goto done;
	if (readable || writeable)
		goto done;
	if (!timerisset(&atv))
		goto done;
	s = spl6();
	if ((u.u_procp->p_flag & SSEL) == 0 || nselcoll != ncoll) {
		u.u_procp->p_flag &= ~SSEL;
		splx(s);
		goto retry;
	}
	u.u_procp->p_flag &= ~SSEL;
	tsel = tsleep((caddr_t)&selwait, PZERO+1, &atv);
	splx(s);
	switch (tsel) {

	case TS_OK:
		now = time;
		timevalsub(&now, &origin);
		timevalsub(&atv, now);
		if (atv.tv_sec < 0 || atv.tv_usec < 0)
			timerclear(&atv);
		goto retry;

	case TS_SIG:
		u.u_error = EINTR;
		return;

	case TS_TIME:
		break;
	}
done:
	rd.fds_bits[0] = readable;
	wr.fds_bits[0] = writeable;
	s = sizeof (fd_set);
	u.u_r.r_val1 = nfds;
	if (uap->rp)
		(void) copyout((caddr_t)&rd, (caddr_t)uap->rp, sizeof(fd_set));
	if (uap->wp)
		(void) copyout((caddr_t)&wr, (caddr_t)uap->wp, sizeof(fd_set));
}

selscan(fds, nfdp, flag)
	fd_set fds;
	int *nfdp, flag;
{
	struct file *fp;
	struct inode *ip;
	register int bits;
	int i, able, res = 0;
		
	bits = fds.fds_bits[0];
	while (i = ffs(bits)) {
		bits &= ~(1<<(i-1));
		fp = u.u_ofile[i-1];
		if (fp == NULL) {
			u.u_error = EBADF;
			return (0);
		}
		if (fp->f_type == DTYPE_SOCKET)
			able = soselect(fp->f_socket, flag);
		else {
			ip = fp->f_inode;
			switch (ip->i_mode & IFMT) {

			case IFCHR:
				able =
				    (*cdevsw[major(ip->i_rdev)].d_select)
					(ip->i_rdev, flag);
				break;

			case IFBLK:
			case IFREG:
			case IFDIR:
				able = 1;
				break;
			}

		}
		if (able) {
			res |= (1<<(i-1));
			(*nfdp)++;
		}
	}
	return (res);
}

/*ARGSUSED*/
seltrue(dev, flag)
	dev_t dev;
	int flag;
{

	return (1);
}

selwakeup(p, coll)
	register struct proc *p;
	int coll;
{
	int s;

	if (coll) {
		nselcoll++;
		wakeup((caddr_t)&selwait);
	}
	if (p) {
		if (p->p_wchan == (caddr_t)&selwait)
			setrun(p);
		else {
			s = spl6();
			if (p->p_flag & SSEL)
				p->p_flag &= ~SSEL;
			splx(s);
		}
	}
}

revoke()
{

	/* XXX */
}

/*
 * Allocate a user file descriptor.
 */
ufalloc()
{
	register i;

	for (i=0; i<NOFILE; i++)
		if (u.u_ofile[i] == NULL) {
			u.u_r.r_val1 = i;
			u.u_pofile[i] = 0;
			return (i);
		}
	u.u_error = EMFILE;
	return (-1);
}

struct	file *lastf;
/*
 * Allocate a user file descriptor
 * and a file structure.
 * Initialize the descriptor
 * to point at the file structure.
 */
struct file *
falloc()
{
	register struct file *fp;
	register i;

	i = ufalloc();
	if (i < 0)
		return (NULL);
	if (lastf == 0)
		lastf = file;
	for (fp = lastf; fp < fileNFILE; fp++)
		if (fp->f_count == 0)
			goto slot;
	for (fp = file; fp < lastf; fp++)
		if (fp->f_count == 0)
			goto slot;
	tablefull("file");
	u.u_error = ENFILE;
	return (NULL);
slot:
	u.u_ofile[i] = fp;
	fp->f_count++;
	fp->f_offset = 0;
	fp->f_inode = 0;
	lastf = fp + 1;
	return (fp);
}
/*
 * Convert a user supplied file descriptor into a pointer
 * to a file structure.  Only task is to check range of the descriptor.
 * Critical paths should use the GETF macro, defined in inline.h.
 */
struct file *
getf(f)
	register int f;
{
	register struct file *fp;

	if ((unsigned)f >= NOFILE || (fp = u.u_ofile[f]) == NULL) {
		u.u_error = EBADF;
		return (NULL);
	}
	return (fp);
}

/*
 * Internal form of close.
 * Decrement reference count on
 * file structure.
 * Also make sure the pipe protocol
 * does not constipate.
 *
 * Decrement reference count on the inode following
 * removal to the referencing file structure.
 * Call device handler on last close.
 * Nouser indicates that the user isn't available to present
 * errors to.
 *
 * Handling locking at this level is RIDICULOUS.
 */
closef(fp, nouser, flags)
	register struct file *fp;
	int nouser, flags;
{
	register struct inode *ip;
	register struct mount *mp;
	int flag, mode;
	dev_t dev;
	register int (*cfunc)();

	if (fp == NULL)
		return;
	if (fp->f_count > 1) {
		fp->f_count--;
		return;
	}
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = 0;			/* XXX */
		soclose(fp->f_socket, nouser);
		if (nouser == 0 && u.u_error)
			return;
		fp->f_socket = 0;
		fp->f_count = 0;
		return;
	}
	flag = fp->f_flag;
	ip = fp->f_inode;
	dev = (dev_t)ip->i_rdev;
	mode = ip->i_mode & IFMT;
	flags &= RDLOCK|WRLOCK;			/* conservative */
	if (flags)
		funlocki(ip, flags);
	ilock(ip);
	iput(ip);
	fp->f_count = 0;

	switch (mode) {

	case IFCHR:
		cfunc = cdevsw[major(dev)].d_close;
		break;

	case IFBLK:
		/*
		 * We don't want to really close the device if it is mounted
		 */
		for (mp = mount; mp < &mount[NMOUNT]; mp++)
			if (mp->m_bufp != NULL && mp->m_dev == dev)
				return;
		cfunc = bdevsw[major(dev)].d_close;
		break;

	default:
		return;
	}
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_type == DTYPE_SOCKET)		/* XXX */
			continue;
		if (fp->f_count && (ip = fp->f_inode) &&
		    ip->i_rdev == dev && (ip->i_mode&IFMT) == mode)
			return;
	}
	if (mode == IFBLK) {
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks
		 */
		bflush(dev);
		binval(dev);
	}
	(*cfunc)(dev, flag, fp);
}
