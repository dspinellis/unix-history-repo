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
 *	@(#)sys_generic.c	7.20 (Berkeley) %G%
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
read(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap;
	int *retval;
{
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
	if (KTRPOINT(p, KTR_GENIO))
		ktriov = aiov;
#endif
	cnt = uap->count;
	if (error = (*fp->f_ops->fo_read)(fp, &auio, fp->f_cred))
		if (auio.uio_resid != cnt && (error == ERESTART ||
		    error == EINTR || error == EWOULDBLOCK))
			error = 0;
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_GENIO) && error == 0)
		ktrgenio(p->p_tracep, uap->fdes, UIO_READ, &ktriov, cnt, error);
#endif
	*retval = cnt;
	RETURN (error);
}

/*
 * Scatter read system call.
 */
readv(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap;
	int *retval;
{
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
	if (KTRPOINT(p, KTR_GENIO))  {
		unsigned iovlen = auio.uio_iovcnt * sizeof (struct iovec);

		MALLOC(ktriov, struct iovec *, iovlen, M_TEMP, M_WAITOK);
		bcopy((caddr_t)auio.uio_iov, (caddr_t)ktriov, iovlen);
	}
#endif
	cnt = auio.uio_resid;
	if (error = (*fp->f_ops->fo_read)(fp, &auio, fp->f_cred))
		if (auio.uio_resid != cnt && (error == ERESTART ||
		    error == EINTR || error == EWOULDBLOCK))
			error = 0;
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (ktriov != NULL) {
		if (error == 0)
			ktrgenio(p->p_tracep, uap->fdes, UIO_READ, ktriov,
			    cnt, error);
		FREE(ktriov, M_TEMP);
	}
#endif
	*retval = cnt;
done:
	if (uap->iovcnt > UIO_SMALLIOV)
		FREE(iov, M_IOV);
	RETURN (error);
}

/*
 * Write system call
 */
write(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap;
	int *retval;
{
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
	if (KTRPOINT(p, KTR_GENIO))
		ktriov = aiov;
#endif
	cnt = uap->count;
	if (error = (*fp->f_ops->fo_write)(fp, &auio, fp->f_cred)) {
		if (auio.uio_resid != cnt && (error == ERESTART ||
		    error == EINTR || error == EWOULDBLOCK))
			error = 0;
		if (error == EPIPE)
			psignal(p, SIGPIPE);
	}
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_GENIO) && error == 0)
		ktrgenio(p->p_tracep, uap->fdes, UIO_WRITE,
		    &ktriov, cnt, error);
#endif
	*retval = cnt;
	RETURN (error);
}

/*
 * Gather write system call
 */
writev(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		struct	iovec *iovp;
		unsigned iovcnt;
	} *uap;
	int *retval;
{
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
	if (KTRPOINT(p, KTR_GENIO))  {
		unsigned iovlen = auio.uio_iovcnt * sizeof (struct iovec);

		MALLOC(ktriov, struct iovec *, iovlen, M_TEMP, M_WAITOK);
		bcopy((caddr_t)auio.uio_iov, (caddr_t)ktriov, iovlen);
	}
#endif
	cnt = auio.uio_resid;
	if (error = (*fp->f_ops->fo_write)(fp, &auio, fp->f_cred)) {
		if (auio.uio_resid != cnt && (error == ERESTART ||
		    error == EINTR || error == EWOULDBLOCK))
			error = 0;
		if (error == EPIPE)
			psignal(p, SIGPIPE);
	}
	cnt -= auio.uio_resid;
#ifdef KTRACE
	if (ktriov != NULL) {
		if (error == 0)
			ktrgenio(p->p_tracep, uap->fdes, UIO_WRITE,
				ktriov, cnt, error);
		FREE(ktriov, M_TEMP);
	}
#endif
	*retval = cnt;
done:
	if (uap->iovcnt > UIO_SMALLIOV)
		FREE(iov, M_IOV);
	RETURN (error);
}

/*
 * Ioctl system call
 */
/* ARGSUSED */
ioctl(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{
	register struct file *fp;
	register int com, error;
	register u_int size;
	caddr_t memp = 0;
#define STK_PARAMS	128
	char stkbuf[STK_PARAMS];
	caddr_t data = stkbuf;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0)
		RETURN (EBADF);
	com = uap->cmd;

	if (com == FIOCLEX) {
		u.u_pofile[uap->fdes] |= UF_EXCLOSE;
		RETURN (0);
	}
	if (com == FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~UF_EXCLOSE;
		RETURN (0);
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = IOCPARM_LEN(com);
	if (size > IOCPARM_MAX)
		RETURN (ENOTTY);
	if (size > sizeof (stkbuf)) {
		memp = (caddr_t)malloc((u_long)size, M_IOCTLOPS, M_WAITOK);
		data = memp;
	}
	if (com&IOC_IN) {
		if (size) {
			error = copyin(uap->cmarg, data, (u_int)size);
			if (error) {
				if (memp)
					free(memp, M_IOCTLOPS);
				RETURN (error);
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
		error = fset(fp, FNDELAY, *(int *)data);
		break;

	case FIOASYNC:
		error = fset(fp, FASYNC, *(int *)data);
		break;

	case FIOSETOWN:
		error = fsetown(fp, *(int *)data);
		break;

	case FIOGETOWN:
		error = fgetown(fp, (int *)data);
		break;
	default:
		error = (*fp->f_ops->fo_ioctl)(fp, com, data);
		/*
		 * Copy any data to user, size was
		 * already set and checked above.
		 */
		if (error == 0 && (com&IOC_OUT) && size)
			error = copyout(data, uap->cmarg, (u_int)size);
		break;
	}
	if (memp)
		free(memp, M_IOCTLOPS);
	RETURN (error);
}

int	nselcoll;

/*
 * Select system call.
 */
select(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	nd;
		fd_set	*in, *ou, *ex;
		struct	timeval *tv;
	} *uap;
	int *retval;
{
	fd_set ibits[3], obits[3];
	struct timeval atv;
	int s, ncoll, ni, error = 0, timo;

	bzero((caddr_t)ibits, sizeof(ibits));
	bzero((caddr_t)obits, sizeof(obits));
	if (uap->nd > NOFILE)
		uap->nd = NOFILE;	/* forgiving, if slightly wrong */
	ni = howmany(uap->nd, NFDBITS);

#define	getbits(name, x) \
	if (uap->name) { \
		error = copyin((caddr_t)uap->name, (caddr_t)&ibits[x], \
		    (unsigned)(ni * sizeof(fd_mask))); \
		if (error) \
			goto done; \
	}
	getbits(in, 0);
	getbits(ou, 1);
	getbits(ex, 2);
#undef	getbits

	if (uap->tv) {
		error = copyin((caddr_t)uap->tv, (caddr_t)&atv,
			sizeof (atv));
		if (error)
			goto done;
		if (itimerfix(&atv)) {
			error = EINVAL;
			goto done;
		}
		s = splhigh(); timevaladd(&atv, &time); splx(s);
		timo = hzto(&atv);
	} else
		timo = 0;
retry:
	ncoll = nselcoll;
	p->p_flag |= SSEL;
	error = selscan(ibits, obits, uap->nd, retval);
	if (error || *retval)
		goto done;
	s = splhigh();
	/* this should be timercmp(&time, &atv, >=) */
	if (uap->tv && (time.tv_sec > atv.tv_sec ||
	    time.tv_sec == atv.tv_sec && time.tv_usec >= atv.tv_usec)) {
		splx(s);
		goto done;
	}
	if ((p->p_flag & SSEL) == 0 || nselcoll != ncoll) {
		splx(s);
		goto retry;
	}
	p->p_flag &= ~SSEL;
	error = tsleep((caddr_t)&selwait, PSOCK | PCATCH, "select", timo);
	splx(s);
	if (error == 0)
		goto retry;
done:
	p->p_flag &= ~SSEL;
	/* select is not restarted after signals... */
	if (error == ERESTART)
		error = EINTR;
	if (error == EWOULDBLOCK)
		error = 0;
#define	putbits(name, x) \
	if (uap->name) { \
		int error2 = copyout((caddr_t)&obits[x], (caddr_t)uap->name, \
		    (unsigned)(ni * sizeof(fd_mask))); \
		if (error2) \
			error = error2; \
	}
	if (error == 0) {
		putbits(in, 0);
		putbits(ou, 1);
		putbits(ex, 2);
#undef putbits
	}
	RETURN (error);
}

selscan(ibits, obits, nfd, retval)
	fd_set *ibits, *obits;
	int nfd, *retval;
{
	register int which, i, j;
	register fd_mask bits;
	int flag;
	struct file *fp;
	int error = 0, n = 0;

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
					error = EBADF;
					break;
				}
				if ((*fp->f_ops->fo_select)(fp, flag)) {
					FD_SET(i + j, &obits[which]);
					n++;
				}
			}
		}
	}
	*retval = n;
	return (error);
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
