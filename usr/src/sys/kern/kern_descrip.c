/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kern_descrip.c	7.19 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "filedesc.h"
#include "kernel.h"
#include "vnode.h"
#include "proc.h"
#include "file.h"
#include "socket.h"
#include "socketvar.h"
#include "stat.h"
#include "ioctl.h"
#include "fcntl.h"
#include "malloc.h"
#include "syslog.h"

/*
 * Descriptor management.
 */
int nofile = NOFILE;		/* per-process maximum open files */

/*
 * System calls on descriptors.
 */
/* ARGSUSED */
getdtablesize(p, uap, retval)
	struct proc *p;
	struct args *uap;
	int *retval;
{

	*retval = nofile;
	return (0);
}

/*
 * Duplicate a file descriptor.
 */
/* ARGSUSED */
dup(p, uap, retval)
	struct proc *p;
	struct args {
		int	i;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	struct file *fp;
	int fd, error;

	/*
	 * XXX Compatibility
	 */
	if (uap->i &~ 077) { uap->i &= 077; return (dup2(p, uap, retval)); }

	if ((unsigned)uap->i >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->i)) == NULL)
		return (EBADF);
	if (error = ufalloc(fdp, 0, &fd))
		return (error);
	OFILE(fdp, fd) = fp;
	OFILEFLAGS(fdp, fd) = OFILEFLAGS(fdp, uap->i) &~ UF_EXCLOSE;
	fp->f_count++;
	if (fd > fdp->fd_lastfile)
		fdp->fd_lastfile = fd;
	*retval = fd;
	return (0);
}

/*
 * Duplicate a file descriptor to a particular value.
 */
/* ARGSUSED */
dup2(p, uap, retval)
	struct proc *p;
	register struct args {
		int	i;
		int	j;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	int i, error;

	if ((unsigned)uap->i >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->i)) == NULL ||
	    (unsigned)uap->j >= nofile)
		return (EBADF);
	*retval = uap->j;
	if (uap->i == uap->j)
		return (0);
	if ((unsigned)uap->j >= fdp->fd_maxfiles) {
		if (error = ufalloc(fdp, uap->j, &i))
			return (error);
		if (uap->j != i)
			panic("dup2: ufalloc");
	} else if (OFILE(fdp, uap->j)) {
		if (OFILEFLAGS(fdp, uap->j) & UF_MAPPED)
			(void) munmapfd(p, uap->j);
		error = closef(OFILE(fdp, uap->j));
	}
	OFILE(fdp, uap->j) = fp;
	OFILEFLAGS(fdp, uap->j) = OFILEFLAGS(fdp, uap->i) &~ UF_EXCLOSE;
	fp->f_count++;
	if (uap->j > fdp->fd_lastfile)
		fdp->fd_lastfile = uap->j;
	/*
	 * dup2() must succeed even though the close had an error.
	 */
	error = 0;		/* XXX */
	return (error);
}

/*
 * The file control system call.
 */
/* ARGSUSED */
fcntl(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	register char *pop;
	struct vnode *vp;
	int i, error, flags = F_POSIX;
	struct flock fl;

	if ((unsigned)uap->fdes >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->fdes)) == NULL)
		return (EBADF);
	pop = &OFILEFLAGS(fdp, uap->fdes);
	switch(uap->cmd) {
	case F_DUPFD:
		if ((unsigned)uap->arg >= nofile)
			return (EINVAL);
		if (error = ufalloc(fdp, uap->arg, &i))
			return (error);
		OFILE(fdp, i) = fp;
		OFILEFLAGS(fdp, i) = *pop &~ UF_EXCLOSE;
		fp->f_count++;
		if (i > fdp->fd_lastfile)
			fdp->fd_lastfile = i;
		*retval = i;
		return (0);

	case F_GETFD:
		*retval = *pop & 1;
		return (0);

	case F_SETFD:
		*pop = (*pop &~ 1) | (uap->arg & 1);
		return (0);

	case F_GETFL:
		*retval = OFLAGS(fp->f_flag);
		return (0);

	case F_SETFL:
		fp->f_flag &= ~FCNTLFLAGS;
		fp->f_flag |= FFLAGS(uap->arg) & FCNTLFLAGS;
		if (error = fset(fp, FNDELAY, fp->f_flag & FNDELAY))
			return (error);
		if (error = fset(fp, FASYNC, fp->f_flag & FASYNC))
			(void) fset(fp, FNDELAY, 0);
		return (error);

	case F_GETOWN:
		return (fgetown(fp, retval));

	case F_SETOWN:
		return (fsetown(fp, uap->arg));

	case F_SETLKW:
		flags |= F_WAIT;
		/* Fall into F_SETLK */

	case F_SETLK:
		if (fp->f_type != DTYPE_VNODE)
			return (EBADF);
		vp = (struct vnode *)fp->f_data;
		/* Copy in the lock structure */
		error = copyin((caddr_t)uap->arg, (caddr_t)&fl, sizeof (fl));
		if (error)
			return (error);
		if (fl.l_whence == SEEK_CUR)
			fl.l_start += fp->f_offset;
		switch (fl.l_type) {

		case F_RDLCK:
			if ((fp->f_flag & FREAD) == 0)
				return (EBADF);
			return (VOP_ADVLOCK(vp, p, F_SETLK, &fl, flags));

		case F_WRLCK:
			if ((fp->f_flag & FWRITE) == 0)
				return (EBADF);
			return (VOP_ADVLOCK(vp, p, F_SETLK, &fl, flags));

		case F_UNLCK:
			return (VOP_ADVLOCK(vp, p, F_UNLCK, &fl, F_POSIX));

		default:
			return (EINVAL);
		}

	case F_GETLK:
		if (fp->f_type != DTYPE_VNODE)
			return (EBADF);
		vp = (struct vnode *)fp->f_data;
		/* Copy in the lock structure */
		error = copyin((caddr_t)uap->arg, (caddr_t)&fl, sizeof (fl));
		if (error)
			return (error);
		if (error = VOP_ADVLOCK(vp, p, F_GETLK, &fl, F_POSIX))
			return (error);
		return (copyout((caddr_t)&fl, (caddr_t)uap->arg, sizeof (fl)));

	default:
		return (EINVAL);
	}
	/* NOTREACHED */
}

fset(fp, bit, value)
	struct file *fp;
	int bit, value;
{

	if (value)
		fp->f_flag |= bit;
	else
		fp->f_flag &= ~bit;
	return (fioctl(fp, (int)(bit == FNDELAY ? FIONBIO : FIOASYNC),
	    (caddr_t)&value));
}

fgetown(fp, valuep)
	struct file *fp;
	int *valuep;
{
	int error;

	switch (fp->f_type) {

	case DTYPE_SOCKET:
		*valuep = ((struct socket *)fp->f_data)->so_pgid;
		return (0);

	default:
		error = fioctl(fp, (int)TIOCGPGRP, (caddr_t)valuep);
		*valuep = -*valuep;
		return (error);
	}
}

fsetown(fp, value)
	struct file *fp;
	int value;
{

	if (fp->f_type == DTYPE_SOCKET) {
		((struct socket *)fp->f_data)->so_pgid = value;
		return (0);
	}
	if (value > 0) {
		struct proc *p = pfind(value);
		if (p == 0)
			return (ESRCH);
		value = p->p_pgrp->pg_id;
	} else
		value = -value;
	return (fioctl(fp, (int)TIOCSPGRP, (caddr_t)&value));
}

fioctl(fp, cmd, value)
	struct file *fp;
	int cmd;
	caddr_t value;
{

	return ((*fp->f_ops->fo_ioctl)(fp, cmd, value));
}

/*
 * Close a file descriptor.
 */
/* ARGSUSED */
close(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	register u_char *pf;

	if ((unsigned)uap->fdes >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->fdes)) == NULL)
		return (EBADF);
	pf = (u_char *)&OFILEFLAGS(fdp, uap->fdes);
	if (*pf & UF_MAPPED)
		(void) munmapfd(p, uap->fdes);
	OFILE(fdp, uap->fdes) = NULL;
	while (fdp->fd_lastfile >= 0 && OFILE(fdp, fdp->fd_lastfile) == NULL)
		fdp->fd_lastfile--;
	*pf = 0;
	return (closef(fp));
}

/*
 * Return status information about a file descriptor.
 */
/* ARGSUSED */
fstat(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		struct	stat *sb;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	struct stat ub;
	int error;

	if ((unsigned)uap->fdes >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->fdes)) == NULL)
		return (EBADF);
	switch (fp->f_type) {

	case DTYPE_VNODE:
		error = vn_stat((struct vnode *)fp->f_data, &ub);
		break;

	case DTYPE_SOCKET:
		error = soo_stat((struct socket *)fp->f_data, &ub);
		break;

	default:
		panic("fstat");
		/*NOTREACHED*/
	}
	if (error == 0)
		error = copyout((caddr_t)&ub, (caddr_t)uap->sb, sizeof (ub));
	return (error);
}

/*
 * Allocate a user file descriptor.
 */
int fdexpand, fdreexpand;

ufalloc(fdp, want, result)
	register struct filedesc *fdp;
	register int want;
	int *result;
{
	int last, osize, ofiles, nfiles;
	struct file **newofile;
	char *newofileflags;

	for (;;) {
		last = (fdp->fd_maxfiles < nofile) ? fdp->fd_maxfiles : nofile;
		for (; want < last; want++) {
			if (OFILE(fdp, want) == NULL) {
				OFILEFLAGS(fdp, want) = 0;
				if (want > fdp->fd_lastfile)
					fdp->fd_lastfile = want;
				*result = want;
				return (0);
			}
		}
		if (fdp->fd_maxfiles >= nofile)
			return (EMFILE);
		if (fdp->fd_maxfiles == NDFILE) {
			fdp->fd_moreofiles = (struct file **)
			    malloc(NDEXTENT * OFILESIZE, M_FILE, M_WAITOK);
			fdp->fd_moreofileflags =
			    (char *)&fdp->fd_moreofiles[NDEXTENT];
			bzero((char *)fdp->fd_moreofiles, NDEXTENT * OFILESIZE);
			fdp->fd_maxfiles = NDFILE + NDEXTENT;
			fdexpand++;
			continue;
		}
		ofiles = fdp->fd_maxfiles - NDFILE;
		osize = ofiles * OFILESIZE;
		nfiles = (2 * osize) / OFILESIZE;
		newofile = (struct file **) malloc(2 * osize, M_FILE, M_WAITOK);
		newofileflags = (char *)&newofile[nfiles];
		bzero((char *)newofile, 2 * osize);
		bcopy((char *)fdp->fd_moreofiles, (char *)newofile,
			sizeof(struct file *) * ofiles);
		bcopy((char *)fdp->fd_moreofileflags, (char *)newofileflags,
			sizeof(char) * ofiles);
		free(fdp->fd_moreofiles, M_FILE);
		fdp->fd_moreofiles = newofile;
		fdp->fd_moreofileflags = newofileflags;
		fdp->fd_maxfiles = NDFILE + nfiles;
		fdreexpand++;
	}
}

/*
 * Check to see if any user file descriptors are available.
 */
ufavail(fdp)
	register struct filedesc *fdp;
{
	register int i, avail;

	avail = nofile - fdp->fd_maxfiles;
	for (i = 0; i < fdp->fd_maxfiles; i++)
		if (OFILE(fdp, i) == NULL)
			avail++;
	return (avail);
}

struct	file *lastf;
/*
 * Allocate a user file descriptor
 * and a file structure.
 * Initialize the descriptor
 * to point at the file structure.
 */
falloc(p, resultfp, resultfd)
	register struct proc *p;
	struct file **resultfp;
	int *resultfd;
{
	register struct file *fp;
	int error, i;

	if (error = ufalloc(p->p_fd, 0, &i))
		return (error);
	if (lastf == 0)
		lastf = file;
	for (fp = lastf; fp < fileNFILE; fp++)
		if (fp->f_count == 0)
			goto slot;
	for (fp = file; fp < lastf; fp++)
		if (fp->f_count == 0)
			goto slot;
	tablefull("file");
	return (ENFILE);
slot:
	OFILE(p->p_fd, i) = fp;
	fp->f_count = 1;
	fp->f_data = 0;
	fp->f_offset = 0;
	fp->f_cred = u.u_cred;
	crhold(fp->f_cred);
	lastf = fp + 1;
	if (resultfp)
		*resultfp = fp;
	if (resultfd)
		*resultfd = i;
	return (0);
}

/*
 * Duplicate a filedesc structure.
 */
struct filedesc *
fddup(fdp, justref)
	register struct filedesc *fdp;
	int justref;
{
	register struct filedesc *newfdp;
	register struct file *fp;
	register int i;
	int j, last;

	if (justref) {
		fdp->fd_refcnt++;
		return (fdp);
	}
	MALLOC(newfdp, struct filedesc *, sizeof(*fdp), M_FILE, M_WAITOK);
	bcopy((char *)fdp, (char *)newfdp, sizeof(*fdp));
	VREF(newfdp->fd_cdir);
	if (newfdp->fd_rdir)
		VREF(newfdp->fd_rdir);
	newfdp->fd_refcnt = 1;
	newfdp->fd_maxfiles = NDFILE;
	if (fdp->fd_lastfile > NDFILE &&
	    ufalloc(newfdp, fdp->fd_lastfile, &j)) {
		log(LOG_ERR, "fddup: lost file descriptor(s)");
		last = newfdp->fd_maxfiles;
	} else
		last = fdp->fd_lastfile;
	for (i = 0; i <= last; i++) {
		fp = OFILE(fdp, i);
		if (fp == NULL)
			continue;
		fp->f_count++;
		OFILE(newfdp, i) = fp;
		OFILEFLAGS(newfdp, i) = OFILEFLAGS(fdp, i);
	}
	return (newfdp);
}

/*
 * Release a filedesc structure.
 */
fdrele(fdp)
	register struct filedesc *fdp;
{
	struct file *f;
	register int i;

	if (fdp->fd_refcnt > 1) {
		fdp->fd_refcnt--;
		return;
	}
	for (i = 0; i <= fdp->fd_lastfile; i++) {
		if (f = OFILE(fdp, i)) {
			OFILE(fdp, i) = NULL;
			OFILEFLAGS(fdp, i) = 0;
			(void) closef(f);
		}
	}
	if (fdp->fd_maxfiles > NDFILE)
		FREE(fdp->fd_moreofiles, M_FILE);
	vrele(fdp->fd_cdir);
	if (fdp->fd_rdir)
		vrele(fdp->fd_rdir);
	FREE(fdp, M_FILE);
}

/*
 * Internal form of close.
 * Decrement reference count on file structure.
 */
closef(fp)
	register struct file *fp;
{
	struct proc *p = u.u_procp;		/* XXX */
	struct vnode *vp;
	struct flock lf;
	int error;

	if (fp == NULL)
		return (0);
	/*
	 * POSIX record locking dictates that any close releases ALL
	 * locks owned by this process.  This is handled by setting
	 * a flag in the unlock to free ONLY locks obeying POSIX
	 * semantics, and not to free BSD-style file locks.
	 */
	if (fp->f_type == DTYPE_VNODE) {
		lf.l_whence = SEEK_SET;
		lf.l_start = 0;
		lf.l_len = 0;
		lf.l_type = F_UNLCK;
		vp = (struct vnode *)fp->f_data;
		(void) VOP_ADVLOCK(vp, p, F_UNLCK, &lf, F_POSIX);
	}
	if (fp->f_count > 1) {
		fp->f_count--;
		return (0);
	}
	if (fp->f_count < 1)
		panic("closef: count < 1");
	if (fp->f_type == DTYPE_VNODE)
		(void) VOP_ADVLOCK(vp, fp, F_UNLCK, &lf, F_FLOCK);
	error = (*fp->f_ops->fo_close)(fp);
	crfree(fp->f_cred);
	fp->f_count = 0;
	return (error);
}

/*
 * Apply an advisory lock on a file descriptor.
 *
 * Just attempt to get a record lock of the requested type on
 * the entire file (l_whence = SEEK_SET, l_start = 0, l_len = 0).
 */

/* ARGSUSED */
flock(p, uap, retval)
	struct proc *p;
	register struct args {
		int	fdes;
		int	how;
	} *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	struct vnode *vp;
	struct flock lf;
	int error;

	if ((unsigned)uap->fdes >= fdp->fd_maxfiles ||
	    (fp = OFILE(fdp, uap->fdes)) == NULL)
		return (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return (EOPNOTSUPP);
	vp = (struct vnode *)fp->f_data;
	lf.l_whence = SEEK_SET;
	lf.l_start = 0;
	lf.l_len = 0;
	if (uap->how & LOCK_UN) {
		lf.l_type = F_UNLCK;
		return (VOP_ADVLOCK(vp, fp, F_UNLCK, &lf, F_FLOCK));
	}
	if (uap->how & LOCK_EX)
		lf.l_type = F_WRLCK;
	else if (uap->how & LOCK_SH)
		lf.l_type = F_RDLCK;
	else
		return (EBADF);
	if (uap->how & LOCK_NB)
		return (VOP_ADVLOCK(vp, fp, F_SETLK, &lf, F_FLOCK));
	return (VOP_ADVLOCK(vp, fp, F_SETLK, &lf, F_FLOCK|F_WAIT));
}

/*
 * File Descriptor pseudo-device driver (/dev/fd/).
 *
 * Opening minor device N dup()s the file (if any) connected to file
 * descriptor N belonging to the calling process.  Note that this driver
 * consists of only the ``open()'' routine, because all subsequent
 * references to this file will be direct to the other driver.
 */
/* ARGSUSED */
fdopen(dev, mode, type)
	dev_t dev;
	int mode, type;
{
	struct proc *p = u.u_procp;		/* XXX */

	/*
	 * XXX Kludge: set p->p_dupfd to contain the value of the
	 * the file descriptor being sought for duplication. The error 
	 * return ensures that the vnode for this device will be released
	 * by vn_open. Open will detect this special error and take the
	 * actions in dupfdopen below. Other callers of vn_open or VOP_OPEN
	 * will simply report the error.
	 */
	p->p_dupfd = minor(dev);
	return (ENODEV);
}

/*
 * Duplicate the specified descriptor to a free descriptor.
 */
dupfdopen(fdp, indx, dfd, mode)
	register struct filedesc *fdp;
	register int indx, dfd;
	int mode;
{
	register struct file *wfp;
	struct file *fp;
	
	/*
	 * If the to-be-dup'd fd number is greater than the allowed number
	 * of file descriptors, or the fd to be dup'd has already been
	 * closed, reject.  Note, check for new == old is necessary as
	 * falloc could allocate an already closed to-be-dup'd descriptor
	 * as the new descriptor.
	 */
	fp = OFILE(fdp, indx);
	if ((u_int)dfd >= fdp->fd_maxfiles || (wfp = OFILE(fdp, dfd)) == NULL ||
	    fp == wfp)
		return (EBADF);

	/*
	 * Check that the mode the file is being opened for is a subset 
	 * of the mode of the existing descriptor.
	 */
	if (((mode & (FREAD|FWRITE)) | wfp->f_flag) != wfp->f_flag)
		return (EACCES);
	OFILE(fdp, indx) = wfp;
	OFILEFLAGS(fdp, indx) = OFILEFLAGS(fdp, dfd);
	wfp->f_count++;
	if (indx > fdp->fd_lastfile)
		fdp->fd_lastfile = indx;
	return (0);
}

#if defined(vax) || defined(tahoe)
/*
 * Brain dead routines to compensate for limitations in PCC
 */
struct file **
ofilefunc(fdp, indx)
	struct filedesc *fdp;
	int indx;
{

	if (indx < NDFILE)
		return (&fdp->fd_ofile[indx]);
	return (&fdp->fd_moreofiles[indx - NDFILE]);
}

char *
ofileflagsfunc(fdp, indx)
	struct filedesc *fdp;
	int indx;
{

	if (indx < NDFILE)
		return (&fdp->fd_ofileflags[indx]);
	return (&fdp->fd_moreofileflags[indx - NDFILE]);
}
#endif
