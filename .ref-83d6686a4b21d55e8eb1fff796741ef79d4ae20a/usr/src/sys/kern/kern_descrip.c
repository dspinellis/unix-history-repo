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
 *	@(#)kern_descrip.c	7.4 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "syscontext.h"
#include "kernel.h"
#include "vnode.h"
#include "proc.h"
#include "file.h"
#include "socket.h"
#include "socketvar.h"
#include "mount.h"
#include "stat.h"

#include "ioctl.h"

/*
 * Descriptor management.
 */

/*
 * System calls on descriptors.
 */
getdtablesize()
{

	u.u_r.r_val1 = NOFILE;
}

dup()
{
	register struct a {
		int	i;
	} *uap = (struct a *) u.u_ap;
	struct file *fp;
	int j;

	if (uap->i &~ 077) { uap->i &= 077; dup2(); return; }	/* XXX */

	if ((unsigned)uap->i >= NOFILE || (fp = u.u_ofile[uap->i]) == NULL)
		RETURN (EBADF);
	if (u.u_error = ufalloc(0, &j))
		return;
	u.u_r.r_val1 = j;
	dupit(j, fp, u.u_pofile[uap->i] &~ UF_EXCLOSE);
}

dup2()
{
	register struct a {
		int	i, j;
	} *uap = (struct a *) u.u_ap;
	register struct file *fp;

	if ((unsigned)uap->i >= NOFILE || (fp = u.u_ofile[uap->i]) == NULL)
		RETURN (EBADF);
	if (uap->j < 0 || uap->j >= NOFILE) {
		u.u_error = EBADF;
		return;
	}
	u.u_r.r_val1 = uap->j;
	if (uap->i == uap->j)
		return;
	if (u.u_ofile[uap->j]) {
		if (u.u_pofile[uap->j] & UF_MAPPED)
			munmapfd(uap->j);
		closef(u.u_ofile[uap->j]);
		if (u.u_error)
			return;
	}
	dupit(uap->j, fp, u.u_pofile[uap->i] &~ UF_EXCLOSE);
}

dupit(fd, fp, flags)
	int fd;
	register struct file *fp;
	register int flags;
{

	u.u_ofile[fd] = fp;
	u.u_pofile[fd] = flags;
	fp->f_count++;
	if (fd > u.u_lastfile)
		u.u_lastfile = fd;
}

/*
 * The file control system call.
 */
fcntl()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		int	cmd;
		int	arg;
	} *uap = (struct a *)u.u_ap;
	register char *pop;
	int i;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	pop = &u.u_pofile[uap->fdes];
	switch(uap->cmd) {
	case F_DUPFD:
		if (uap->arg < 0 || uap->arg >= NOFILE) {
			u.u_error = EINVAL;
			return;
		}
		if (u.u_error = ufalloc(uap->arg, &i))
			return;
		u.u_r.r_val1 = i;
		dupit(i, fp, *pop &~ UF_EXCLOSE);
		break;

	case F_GETFD:
		u.u_r.r_val1 = *pop & 1;
		break;

	case F_SETFD:
		*pop = (*pop &~ 1) | (uap->arg & 1);
		break;

	case F_GETFL:
		u.u_r.r_val1 = fp->f_flag+FOPEN;
		break;

	case F_SETFL:
		fp->f_flag &= FCNTLCANT;
		fp->f_flag |= (uap->arg-FOPEN) &~ FCNTLCANT;
		u.u_error = fset(fp, FNDELAY, fp->f_flag & FNDELAY);
		if (u.u_error)
			break;
		u.u_error = fset(fp, FASYNC, fp->f_flag & FASYNC);
		if (u.u_error)
			(void) fset(fp, FNDELAY, 0);
		break;

	case F_GETOWN:
		u.u_error = fgetown(fp, &u.u_r.r_val1);
		break;

	case F_SETOWN:
		u.u_error = fsetown(fp, uap->arg);
		break;

	default:
		u.u_error = EINVAL;
	}
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

close()
{
	struct a {
		int	fdes;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;
	register u_char *pf;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	pf = (u_char *)&u.u_pofile[uap->fdes];
	if (*pf & UF_MAPPED)
		munmapfd(uap->fdes);
	u.u_ofile[uap->fdes] = NULL;
	while (u.u_lastfile >= 0 && u.u_ofile[u.u_lastfile] == NULL)
		u.u_lastfile--;
	*pf = 0;
	closef(fp);
	/* WHAT IF u.u_error ? */
}

fstat()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		struct	stat *sb;
	} *uap = (struct a *)u.u_ap;
	struct stat ub;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	switch (fp->f_type) {

	case DTYPE_VNODE:
		u.u_error = vn_stat((struct vnode *)fp->f_data, &ub);
		break;

	case DTYPE_SOCKET:
		u.u_error = soo_stat((struct socket *)fp->f_data, &ub);
		break;

	default:
		panic("fstat");
		/*NOTREACHED*/
	}
	if (u.u_error == 0)
		u.u_error = copyout((caddr_t)&ub, (caddr_t)uap->sb,
		    sizeof (ub));
}

/*
 * Allocate a user file descriptor.
 */
ufalloc(want, result)
	register int want;
	int *result;
{

	for (; want < NOFILE; want++)
		if (u.u_ofile[want] == NULL) {
			u.u_pofile[want] = 0;
			if (want > u.u_lastfile)
				u.u_lastfile = want;
			if (result)
				*result = want;
			return (0);
		}
	return (EMFILE);
}

ufavail()
{
	register int i, avail = 0;

	for (i = 0; i < NOFILE; i++)
		if (u.u_ofile[i] == NULL)
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
falloc(resultfp, resultfd)
	struct file **resultfp;
	int *resultfd;
{
	register struct file *fp;
	int error, i;

	if (error = ufalloc(0, &i))
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
	u.u_ofile[i] = fp;
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
 * Internal form of close.
 * Decrement reference count on file structure.
 */
closef(fp)
	register struct file *fp;
{

	if (fp == NULL)
		return;
	if (fp->f_count > 1) {
		fp->f_count--;
		return;
	}
	if (fp->f_count < 1)
		panic("closef: count < 1");
	(*fp->f_ops->fo_close)(fp);
	crfree(fp->f_cred);
	fp->f_count = 0;
}

/*
 * Apply an advisory lock on a file descriptor.
 */
flock()
{
	register struct a {
		int	fdes;
		int	how;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	if (fp->f_type != DTYPE_VNODE) {
		u.u_error = EOPNOTSUPP;
		return;
	}
	if (uap->how & LOCK_UN) {
		vn_unlock(fp, FSHLOCK|FEXLOCK);
		return;
	}
	if ((uap->how & (LOCK_SH | LOCK_EX)) == 0)
		return;					/* error? */
	if (uap->how & LOCK_EX)
		uap->how &= ~LOCK_SH;
	/* avoid work... */
	if ((fp->f_flag & FEXLOCK) && (uap->how & LOCK_EX) ||
	    (fp->f_flag & FSHLOCK) && (uap->how & LOCK_SH))
		return;
	u.u_error = vn_lock(fp, uap->how);
}

/*
 * File Descriptor pseudo-device driver (/dev/fd/).
 *
 * Fred Blonder - U of Maryland	11-Sep-1984
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
	struct file *fp, *wfp;
	int indx, dfd, rwmode;

	/*
	 * Note the horrid kludge here: u.u_r.r_val1 contains the value
	 * of the new file descriptor, which was set before the call to
	 * vn_open() by copen() in vfs_syscalls.c
	 */
	indx = u.u_r.r_val1;		/* XXX */
	if ((unsigned)indx >= NOFILE || (fp = u.u_ofile[indx]) == NULL)
		return (EBADF);
	dfd = minor(dev);
	if ((unsigned)dfd >= NOFILE || (wfp = u.u_ofile[dfd]) == NULL)
		return (EBADF);
	/*
	 * We must explicitly test for this case because ufalloc() may
	 * have allocated us the same file desriptor we are referring
	 * to, if the proccess referred to an invalid (closed) descriptor.
	 * Ordinarily this would be caught by the check for NULL above,
	 * but by the time we reach this routine u_pofile[minor(dev)]
	 * could already be set to point to our file struct.
	 */
	if (fp == wfp)
		return (EBADF);
	/*
	 * Fake a ``dup()'' sys call.
	 * Check that the mode the file is being opened
	 * for is consistent with the mode of the existing
	 * descriptor. This isn't as clean as it should be,
	 * but this entire driver is a real kludge anyway.
	 */
	rwmode = mode & (FREAD|FWRITE);
	if ((fp->f_flag & rwmode) != rwmode)
		return (EACCES);
	/*
	 * Delete references to this pseudo-device.
	 * Note that fp->f_count is guaranteed == 1, and
	 * that fp references the vnode for this driver.
	 */
	if (fp->f_count != 1 || fp->f_type != DTYPE_VNODE) 
		panic("fdopen");
	vrele((struct vnode *)fp->f_data);
	fp->f_count = 0;
	/* 
	 * Dup the file descriptor. 
	 */
	dupit(indx, wfp, u.u_pofile[dfd]);
	return (0);
}
