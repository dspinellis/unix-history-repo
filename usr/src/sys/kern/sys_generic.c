/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)sys_generic.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "syscontext.h"
#include "ioctl.h"
#include "file.h"
#include "proc.h"
#include "uio.h"
#include "kernel.h"
#include "stat.h"
#include "malloc.h"
#ifdef KTRACE
#include "ktrace.h"
#endif

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
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;
	long cnt, error = 0;
#ifdef KTRACE
	struct iovec ktriov;
#endif

	if (((unsigned)uap->fdes) >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL ||
	    (fp->f_flag & FREAD) == 0)
		RETURN (EBADF);
	if (uap->count < 0)
		RETURN (EINVAL);
	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_resid = uap->count;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_USERSPACE;
#ifdef KTRACE
	/*
	 * if tracing, save a copy of iovec
	 */
	if (KTRPOINT(u.u_procp, KTR_GENIO))
		ktriov = aiov;
#endif
	cnt = uap->count;
	if (setjmp(&u.u_qsave)) {
		if (auio.uio_resid == cnt) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		error = (*fp->f_ops->fo_read)(fp, &auio, u.u_cred);
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (KTRPOINT(u.u_procp, KTR_GENIO))
		ktrgenio(u.u_procp->p_tracep, uap->fdes, UIO_READ, ktriov, cnt);
#endif
	u.u_r.r_val1 = cnt;
	RETURN (error);
}

readv()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	register struct iovec *iov;
	struct iovec aiov[UIO_SMALLIOV];
	long i, cnt, error = 0;
#ifdef KTRACE
	struct iovec *ktriov = NULL;
#endif

	if (((unsigned)uap->fdes) >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL ||
	    (fp->f_flag & FREAD) == 0)
		RETURN (EBADF);
	if (uap->iovcnt > UIO_SMALLIOV) {
		if (uap->iovcnt > UIO_MAXIOV)
			RETURN (EINVAL);
		MALLOC(iov, struct iovec *, 
		      sizeof(struct iovec) * uap->iovcnt, M_IOV, M_WAITOK);
	} else
		iov = aiov;
	auio.uio_iov = iov;
	auio.uio_iovcnt = uap->iovcnt;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_USERSPACE;
	if (error = copyin((caddr_t)uap->iovp, (caddr_t)iov,
	    uap->iovcnt * sizeof (struct iovec)))
		goto done;
	auio.uio_resid = 0;
	for (i = 0; i < uap->iovcnt; i++) {
		if (iov->iov_len < 0) {
			error = EINVAL;
			goto done;
		}
		auio.uio_resid += iov->iov_len;
		if (auio.uio_resid < 0) {
			error = EINVAL;
			goto done;
		}
		iov++;
	}
#ifdef KTRACE
	/*
	 * if tracing, save a copy of iovec
	 */
	if (KTRPOINT(u.u_procp, KTR_GENIO))  {
		int iovlen = auio.uio_iovcnt * sizeof (struct iovec);

		MALLOC(ktriov, struct iovec *, iovlen, M_TEMP, M_WAITOK);
		bcopy((caddr_t)auio.uio_iov, (caddr_t)ktriov, iovlen);
	}
#endif
	cnt = auio.uio_resid;
	if (setjmp(&u.u_qsave)) {
		if (auio.uio_resid == cnt) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		error = (*fp->f_ops->fo_read)(fp, &auio, u.u_cred);
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (ktriov != NULL) {
		ktrgenio(u.u_procp->p_tracep, uap->fdes, UIO_READ, ktriov, cnt);
		FREE(ktriov, M_TEMP);
	}
#endif
	u.u_r.r_val1 = cnt;
done:
	if (uap->iovcnt > UIO_SMALLIOV)
		FREE(iov, M_IOV);
	RETURN (error);
}

/*
 * Write system call
 */
write()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	struct iovec aiov;
	long cnt, error = 0;
#ifdef KTRACE
	struct iovec ktriov;
#endif

	if (((unsigned)uap->fdes) >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL ||
	    (fp->f_flag & FWRITE) == 0)
		RETURN (EBADF);
	if (uap->count < 0)
		RETURN (EINVAL);
	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_resid = uap->count;
	auio.uio_rw = UIO_WRITE;
	auio.uio_segflg = UIO_USERSPACE;
#ifdef KTRACE
	/*
	 * if tracing, save a copy of iovec
	 */
	if (KTRPOINT(u.u_procp, KTR_GENIO))
		ktriov = aiov;
#endif
	cnt = uap->count;
	if (setjmp(&u.u_qsave)) {
		if (auio.uio_resid == cnt) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		error = (*fp->f_ops->fo_write)(fp, &auio, u.u_cred);
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (KTRPOINT(u.u_procp, KTR_GENIO))
		ktrgenio(u.u_procp->p_tracep, uap->fdes, UIO_WRITE,
		    ktriov, cnt);
#endif
	u.u_r.r_val1 = cnt;
	RETURN (error);
}

writev()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	struct uio auio;
	register struct iovec *iov;
	struct iovec aiov[UIO_SMALLIOV];
	long i, cnt, error = 0;
#ifdef KTRACE
	struct iovec *ktriov = NULL;
#endif

	if (((unsigned)uap->fdes) >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL ||
	    (fp->f_flag & FWRITE) == 0)
		RETURN (EBADF);
	if (uap->iovcnt > UIO_SMALLIOV) {
		if (uap->iovcnt > UIO_MAXIOV)
			RETURN (EINVAL);
		MALLOC(iov, struct iovec *, 
		      sizeof(struct iovec) * uap->iovcnt, M_IOV, M_WAITOK);
	} else
		iov = aiov;
	auio.uio_iov = iov;
	auio.uio_iovcnt = uap->iovcnt;
	auio.uio_rw = UIO_WRITE;
	auio.uio_segflg = UIO_USERSPACE;
	if (error = copyin((caddr_t)uap->iovp, (caddr_t)iov,
	    uap->iovcnt * sizeof (struct iovec)))
		goto done;
	auio.uio_resid = 0;
	for (i = 0; i < uap->iovcnt; i++) {
		if (iov->iov_len < 0) {
			error = EINVAL;
			goto done;
		}
		auio.uio_resid += iov->iov_len;
		if (auio.uio_resid < 0) {
			error = EINVAL;
			goto done;
		}
		iov++;
	}
#ifdef KTRACE
	/*
	 * if tracing, save a copy of iovec
	 */
	if (KTRPOINT(u.u_procp, KTR_GENIO))  {
		int iovlen = auio.uio_iovcnt * sizeof (struct iovec);

		MALLOC(ktriov, struct iovec *, iovlen, M_TEMP, M_WAITOK);
		bcopy((caddr_t)auio.uio_iov, (caddr_t)ktriov, iovlen);
	}
#endif
	cnt = auio.uio_resid;
	if (setjmp(&u.u_qsave)) {
		if (auio.uio_resid == cnt) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				error = EINTR;
			else
				u.u_eosys = RESTARTSYS;
		}
	} else
		error = (*fp->f_ops->fo_write)(fp, &auio, u.u_cred);
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (ktriov != NULL) {
		ktrgenio(u.u_procp->p_tracep, uap->fdes, UIO_WRITE,
		    ktriov, cnt);
		FREE(ktriov, M_TEMP);
	}
#endif
	u.u_r.r_val1 = cnt;
done:
	if (uap->iovcnt > UIO_SMALLIOV)
		FREE(iov, M_IOV);
	RETURN (error);
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
	} *uap = (struct a *)u.u_ap;
	register int com;
	register u_int size;
	caddr_t memp = 0;
#define STK_PARAMS	128
	char stkbuf[STK_PARAMS];
	caddr_t data = stkbuf;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	com = uap->cmd;

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
	size = IOCPARM_LEN(com);
	if (size > IOCPARM_MAX) {
		u.u_error = ENOTTY;
		return;
	}
	if (size > sizeof (stkbuf)) {
		memp = (caddr_t)malloc((u_long)IOCPARM_LEN(com), M_IOCTLOPS,
		    M_WAITOK);
		data = memp;
	}
	if (com&IOC_IN) {
		if (size) {
			u.u_error = copyin(uap->cmarg, data, (u_int)size);
			if (u.u_error) {
				if (memp)
					free(memp, M_IOCTLOPS);
				return;
			}
		} else
			*(caddr_t *)data = uap->cmarg;
	} else if ((com&IOC_OUT) && size)
		/*
		 * Zero the buffer so the user always
		 * gets back something deterministic.
		 */
		bzero(data, size);
	else if (com&IOC_VOID)
		*(caddr_t *)data = uap->cmarg;

	switch (com) {

	case FIONBIO:
		u.u_error = fset(fp, FNDELAY, *(int *)data);
		break;

	case FIOASYNC:
		u.u_error = fset(fp, FASYNC, *(int *)data);
		break;

	case FIOSETOWN:
		u.u_error = fsetown(fp, *(int *)data);
		break;

	case FIOGETOWN:
		u.u_error = fgetown(fp, (int *)data);
		break;
	default:
		if (setjmp(&u.u_qsave))
			u.u_error = EINTR;
		else
			u.u_error = (*fp->f_ops->fo_ioctl)(fp, com, data);
		/*
		 * Copy any data to user, size was
		 * already set and checked above.
		 */
		if (u.u_error == 0 && (com&IOC_OUT) && size)
			u.u_error = copyout(data, uap->cmarg, (u_int)size);
		break;
	}
	if (memp)
		free(memp, M_IOCTLOPS);
}

int	unselect();
int	nselcoll;

/*
 * Select system call.
 */
select()
{
	register struct uap  {
		int	nd;
		fd_set	*in, *ou, *ex;
		struct	timeval *tv;
	} *uap = (struct uap *)u.u_ap;
	fd_set ibits[3], obits[3];
	struct timeval atv;
	int s, ncoll, ni;
	label_t lqsave;

	bzero((caddr_t)ibits, sizeof(ibits));
	bzero((caddr_t)obits, sizeof(obits));
	if (uap->nd > NOFILE)
		uap->nd = NOFILE;	/* forgiving, if slightly wrong */
	ni = howmany(uap->nd, NFDBITS);

#define	getbits(name, x) \
	if (uap->name) { \
		u.u_error = copyin((caddr_t)uap->name, (caddr_t)&ibits[x], \
		    (unsigned)(ni * sizeof(fd_mask))); \
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
	u.u_procp->p_flag &= ~SSEL;
#define	putbits(name, x) \
	if (uap->name) { \
		int error = copyout((caddr_t)&obits[x], (caddr_t)uap->name, \
		    (unsigned)(ni * sizeof(fd_mask))); \
		if (error) \
			u.u_error = error; \
	}
	if (u.u_error == 0) {
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
	fd_set *ibits, *obits;
{
	register int which, i, j;
	register fd_mask bits;
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
		for (i = 0; i < nfd; i += NFDBITS) {
			bits = ibits[which].fds_bits[i/NFDBITS];
			while ((j = ffs(bits)) && i + --j < nfd) {
				bits &= ~(1 << j);
				fp = u.u_ofile[i + j];
				if (fp == NULL) {
					u.u_error = EBADF;
					break;
				}
				if ((*fp->f_ops->fo_select)(fp, flag)) {
					FD_SET(i + j, &obits[which]);
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
