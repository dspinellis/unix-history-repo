/*	kern_descrip.c	5.1	82/07/15	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/clock.h"
#include "../h/mtpr.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/file.h"
#include "../h/inline.h"
#include "../h/socket.h"
#include "../h/socketvar.h"

/*
 * the dup system call.
 */
dup()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	fdes2;
	} *uap;
	register i, m;

	uap = (struct a *)u.u_ap;
	m = uap->fdes & ~077;
	uap->fdes &= 077;
	fp = getf(uap->fdes);
	if (fp == NULL)
		return;
	if ((m&0100) == 0) {
		if ((i = ufalloc()) < 0)
			return;
	} else {
		i = uap->fdes2;
		if (i<0 || i>=NOFILE) {
			u.u_error = EBADF;
			return;
		}
		u.u_r.r_val1 = i;
	}
	if (i != uap->fdes) {
		if (u.u_ofile[i]!=NULL)
			closef(u.u_ofile[i], 0);
		if (u.u_error)
			return;
		u.u_ofile[i] = fp;
		fp->f_count++;
	}
}

int	nselcoll;
/*
 * Select system call.
 */
select()
{
	register struct uap  {
		int	nfd;
		fd_set	*rp, *wp;
		int	timo;
	} *ap = (struct uap *)u.u_ap;
	fd_set rd, wr;
	int nfds = 0, readable = 0, writeable = 0;
	time_t t = time;
	int s, tsel, ncoll, rem;

	if (ap->nfd > NOFILE)
		ap->nfd = NOFILE;
	if (ap->nfd < 0) {
		u.u_error = EBADF;
		return;
	}
	if (ap->rp && copyin((caddr_t)ap->rp,(caddr_t)&rd,sizeof(fd_set)))
		return;
	if (ap->wp && copyin((caddr_t)ap->wp,(caddr_t)&wr,sizeof(fd_set)))
		return;
retry:
	ncoll = nselcoll;
	u.u_procp->p_flag |= SSEL;
	if (ap->rp)
		readable = selscan(ap->nfd, rd, &nfds, FREAD);
	if (ap->wp)
		writeable = selscan(ap->nfd, wr, &nfds, FWRITE);
	if (u.u_error)
		goto done;
	if (readable || writeable)
		goto done;
	rem = (ap->timo+999)/1000 - (time - t);
	if (ap->timo == 0 || rem <= 0)
		goto done;
	s = spl6();
	if ((u.u_procp->p_flag & SSEL) == 0 || nselcoll != ncoll) {
		u.u_procp->p_flag &= ~SSEL;
		splx(s);
		goto retry;
	}
	u.u_procp->p_flag &= ~SSEL;
	tsel = tsleep((caddr_t)&selwait, PZERO+1, rem);
	splx(s);
	switch (tsel) {

	case TS_OK:
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
	if (s * NBBY > ap->nfd)
		s = (ap->nfd + NBBY - 1) / NBBY;
	u.u_r.r_val1 = nfds;
	if (ap->rp)
		(void) copyout((caddr_t)&rd, (caddr_t)ap->rp, sizeof(fd_set));
	if (ap->wp)
		(void) copyout((caddr_t)&wr, (caddr_t)ap->wp, sizeof(fd_set));
}

selscan(nfd, fds, nfdp, flag)
	int nfd;
	fd_set fds;
	int *nfdp, flag;
{
	struct file *fp;
	struct inode *ip;
	register int bits;
	int i, able, res = 0;
		
	bits = fds.fds_bits[0];
	while (i = ffs(bits)) {
		if (i > nfd)
			break;
		bits &= ~(1<<(i-1));
		fp = u.u_ofile[i-1];
		if (fp == NULL) {
			u.u_error = EBADF;
			return (0);
		}
		if (fp->f_flag & FSOCKET)
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

/*
 * Close system call
 */
close()
{
	register struct file *fp;
	register struct a {
		int	fdes;
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fdes);
	if (fp == NULL)
		return;
	if (u.u_vrpages[uap->fdes]) {
		u.u_error = ETXTBSY;
		return;
	}
	u.u_ofile[uap->fdes] = NULL;
	closef(fp, 0);
}
