/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sys_generic.c	6.11 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "file.h"
#include "proc.h"
#include "uio.h"
#include "kernel.h"
#include "stat.h"

/*
 * Read system call.
 */
read()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	rwuio(&auio, UIO_READ);
}

readv()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		int	iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	if (uap->iovcnt <= 0 || uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    (unsigned)(uap->iovcnt * sizeof (struct iovec)));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_READ);
}

/*
 * Write system call
 */
write()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		int	count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->cbuf;
	aiov.iov_len = uap->count;
	rwuio(&auio, UIO_WRITE);
}

writev()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		int	iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	if (uap->iovcnt <= 0 || uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    (unsigned)(uap->iovcnt * sizeof (struct iovec)));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_WRITE);
}

rwuio(uio, rw)
	register struct uio *uio;
	enum uio_rw rw;
{
	struct a {
		int	fdes;
	};
	register struct file *fp;
	register struct iovec *iov;
	int i, count;

	GETF(fp, ((struct a *)u.u_ap)->fdes);
	if ((fp->f_flag&(rw==UIO_READ ? FREAD : FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	uio->uio_resid = 0;
	uio->uio_segflg = UIO_USERSPACE;
	iov = uio->uio_iov;
	for (i = 0; i < uio->uio_iovcnt; i++) {
		if (iov->iov_len < 0) {
			u.u_error = EINVAL;
			return;
		}
		uio->uio_resid += iov->iov_len;
		if (uio->uio_resid < 0) {
			u.u_error = EINVAL;
			return;
		}
		iov++;
	}
	count = uio->uio_resid;
	uio->uio_offset = fp->f_offset;
	if (setjmp(&u.u_qsave)) {
		if (uio->uio_resid == count) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				u.u_error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		u.u_error = (*fp->f_ops->fo_rw)(fp, rw, uio);
	u.u_r.r_val1 = count - uio->uio_resid;
	fp->f_offset += u.u_r.r_val1;
}

/*
 * Ioctl system call
 */
ioctl()
{
	register struct file *fp;
	struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	register int com;
	register u_int size;
	char data[IOCPARM_MASK+1];

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	com = uap->cmd;

#if defined(vax) && defined(COMPAT)
	/*
	 * Map old style ioctl's into new for the
	 * sake of backwards compatibility (sigh).
	 */
	if ((com&~0xffff) == 0) {
		com = mapioctl(com);
		if (com == 0) {
			u.u_error = EINVAL;
			return;
		}
	}
#endif
	if (com == FIOCLEX) {
		u.u_pofile[uap->fdes] |= UF_EXCLOSE;
		return;
	}
	if (com == FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~UF_EXCLOSE;
		return;
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = (com &~ (IOC_INOUT|IOC_VOID)) >> 16;
	if (size > sizeof (data)) {
		u.u_error = EFAULT;
		return;
	}
	if (com&IOC_IN) {
		if (size) {
			u.u_error =
			    copyin(uap->cmarg, (caddr_t)data, (u_int)size);
			if (u.u_error)
				return;
		} else
			*(caddr_t *)data = uap->cmarg;
	} else if ((com&IOC_OUT) && size)
		/*
		 * Zero the buffer on the stack so the user
		 * always gets back something deterministic.
		 */
		bzero((caddr_t)data, size);
	else if (com&IOC_VOID)
		*(caddr_t *)data = uap->cmarg;

	switch (com) {

	case FIONBIO:
		u.u_error = fset(fp, FNDELAY, *(int *)data);
		return;

	case FIOASYNC:
		u.u_error = fset(fp, FASYNC, *(int *)data);
		return;

	case FIOSETOWN:
		u.u_error = fsetown(fp, *(int *)data);
		return;

	case FIOGETOWN:
		u.u_error = fgetown(fp, (int *)data);
		return;
	}
	u.u_error = (*fp->f_ops->fo_ioctl)(fp, com, data);
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
	if (u.u_error == 0 && (com&IOC_OUT) && size)
		u.u_error = copyout(data, uap->cmarg, (u_int)size);
}

int	unselect();
int	nselcoll;

/*
 * Select uses bit masks of file descriptors in ints.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 */
#define NBI		(sizeof(int) * NBBY)		/* bits per int */
#define	NI		howmany(NOFILE, NBI)
#define	tbit(p, n)	((p)[(n)/NBI] & (1 << ((n) % NBI)))
#define	sbit(p, n)	((p)[(n)/NBI] |= (1 << ((n) % NBI)))
#define	cbit(p, n)	((p)[(n)/NBI] &= ~(1 << ((n) % NBI)))

/*
 * Select system call.
 */
select()
{
	register struct uap  {
		int	nd;
		int	*in, *ou, *ex;
		struct	timeval *tv;
	} *uap = (struct uap *)u.u_ap;
	int ibits[3][NI], obits[3][NI];
	struct timeval atv;
	int s, ncoll, ni;
	label_t lqsave;

	bzero(ibits, sizeof(ibits));
	bzero(obits, sizeof(obits));
	if (uap->nd > NOFILE)
		uap->nd = NOFILE;	/* forgiving, if slightly wrong */
	ni = howmany(uap->nd, NBI);

#define	getbits(name, x) \
	if (uap->name) { \
		u.u_error = copyin((caddr_t)uap->name, (caddr_t)ibits[x], \
		    ni * sizeof(int)); \
		if (u.u_error) \
			goto done; \
	}
	getbits(in, 0);
	getbits(ou, 1);
	getbits(ex, 2);
#undef	getbits

	if (uap->tv) {
		u.u_error = copyin((caddr_t)uap->tv, (caddr_t)&atv,
			sizeof (atv));
		if (u.u_error)
			goto done;
		if (itimerfix(&atv)) {
			u.u_error = EINVAL;
			goto done;
		}
		s = splhigh(); timevaladd(&atv, &time); splx(s);
	}
retry:
	ncoll = nselcoll;
	u.u_procp->p_flag |= SSEL;
	u.u_r.r_val1 = selscan(ibits, obits, uap->nd);
	if (u.u_error || u.u_r.r_val1)
		goto done;
	s = splhigh();
	/* this should be timercmp(&time, &atv, >=) */
	if (uap->tv && (time.tv_sec > atv.tv_sec ||
	    time.tv_sec == atv.tv_sec && time.tv_usec >= atv.tv_usec)) {
		splx(s);
		goto done;
	}
	if ((u.u_procp->p_flag & SSEL) == 0 || nselcoll != ncoll) {
		u.u_procp->p_flag &= ~SSEL;
		splx(s);
		goto retry;
	}
	u.u_procp->p_flag &= ~SSEL;
	if (uap->tv) {
		lqsave = u.u_qsave;
		if (setjmp(&u.u_qsave)) {
			untimeout(unselect, (caddr_t)u.u_procp);
			u.u_error = EINTR;
			splx(s);
			goto done;
		}
		timeout(unselect, (caddr_t)u.u_procp, hzto(&atv));
	}
	sleep((caddr_t)&selwait, PZERO+1);
	if (uap->tv) {
		u.u_qsave = lqsave;
		untimeout(unselect, (caddr_t)u.u_procp);
	}
	splx(s);
	goto retry;
done:
#define	putbits(name, x) \
	if (uap->name) { \
		int error = copyout((caddr_t)obits[x], (caddr_t)uap->name, \
		    ni * sizeof(int)); \
		if (error) \
			u.u_error = error; \
	}
	if (u.u_error != EINTR) {
		putbits(in, 0);
		putbits(ou, 1);
		putbits(ex, 2);
#undef putbits
	}
}

unselect(p)
	register struct proc *p;
{
	register int s = splhigh();

	switch (p->p_stat) {

	case SSLEEP:
		setrun(p);
		break;

	case SSTOP:
		unsleep(p);
		break;
	}
	splx(s);
}

selscan(ibits, obits, nfd)
	int (*ibits)[NI], (*obits)[NI];
{
	register int which, bits, i, j;
	int flag;
	struct file *fp;
	int n = 0;

	for (which = 0; which < 3; which++) {
		switch (which) {

		case 0:
			flag = FREAD; break;

		case 1:
			flag = FWRITE; break;

		case 2:
			flag = 0; break;
		}
		for (i = 0; i < nfd; i += NBI) {
			bits = ibits[which][i/NBI];
			while ((j = ffs(bits)) && i + --j < nfd) {
				bits &= ~(1 << j);
				fp = u.u_ofile[i + j];
				if (fp == NULL) {
					u.u_error = EBADF;
					break;
				}
				if ((*fp->f_ops->fo_select)(fp, flag)) {
					sbit(obits[which], i + j);
					n++;
				}
			}
		}
	}
	return (n);
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

	if (coll) {
		nselcoll++;
		wakeup((caddr_t)&selwait);
	}
	if (p) {
		int s = splhigh();
		if (p->p_wchan == (caddr_t)&selwait) {
			if (p->p_stat == SSLEEP)
				setrun(p);
			else
				unsleep(p);
		} else if (p->p_flag & SSEL)
			p->p_flag &= ~SSEL;
		splx(s);
	}
}
