/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_descrip.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "inode.h"
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
 * TODO:
 *	eliminate u.u_error side effects
 */

/*
 * System calls on descriptors.
 */
getdtablesize()
{

	u.u_r.r_val1 = NOFILE;
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

	GETF(fp, uap->i);
	j = ufalloc(0);
	if (j < 0)
		return;
	dupit(j, fp, u.u_pofile[uap->i] &~ UF_EXCLOSE);
}

dup2()
{
	register struct a {
		int	i, j;
	} *uap = (struct a *) u.u_ap;
	register struct file *fp;

	GETF(fp, uap->i);
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
	} *uap;
	register i;
	register char *pop;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	pop = &u.u_pofile[uap->fdes];
	switch(uap->cmd) {
	case F_DUPFD:
		i = uap->arg;
		if (i < 0 || i >= NOFILE) {
			u.u_error = EINVAL;
			return;
		}
		if ((i = ufalloc(i)) < 0)
			return;
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
		int	i;
	} *uap = (struct a *)u.u_ap;
	register int i = uap->i;
	register struct file *fp;
	register u_char *pf;

	GETF(fp, i);
	pf = (u_char *)&u.u_pofile[i];
	if (*pf & UF_MAPPED)
		munmapfd(i);
	u.u_ofile[i] = NULL;
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
	} *uap;
	struct stat ub;

	uap = (struct a *)u.u_ap;
	GETF(fp, uap->fdes);
	switch (fp->f_type) {

	case DTYPE_INODE:
		u.u_error = ino_stat((struct inode *)fp->f_data, &ub);
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
ufalloc(i)
	register int i;
{

	for (; i < NOFILE; i++)
		if (u.u_ofile[i] == NULL) {
			u.u_r.r_val1 = i;
			u.u_pofile[i] = 0;
			if (i > u.u_lastfile)
				u.u_lastfile = i;
			return (i);
		}
	u.u_error = EMFILE;
	return (-1);
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
struct file *
falloc()
{
	register struct file *fp;
	register i;

	i = ufalloc(0);
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
	fp->f_count = 1;
	fp->f_data = 0;
	fp->f_offset = 0;
	lastf = fp + 1;
	return (fp);
}

/*
 * Convert a user supplied file descriptor into a pointer
 * to a file structure.  Only task is to check range of the descriptor.
 * Critical paths should use the GETF macro.
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
	(*fp->f_ops->fo_close)(fp);
	fp->f_count = 0;
}

/*
 * Apply an advisory lock on a file descriptor.
 */
flock()
{
	register struct a {
		int	fd;
		int	how;
	} *uap = (struct a *)u.u_ap;
	register struct file *fp;

	GETF(fp, uap->fd);
	if (fp->f_type != DTYPE_INODE) {
		u.u_error = EOPNOTSUPP;
		return;
	}
	if (uap->how & LOCK_UN) {
		ino_unlock(fp, FSHLOCK|FEXLOCK);
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
	u.u_error = ino_lock(fp, uap->how);
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
fdopen(dev, mode)
	dev_t dev;
	int mode;
{
	struct file *fp, *wfp;
	struct inode *ip, *wip;
	int rwmode;

	/*
	 * Note the horrid kludge here: u.u_r.r_val1 contains the value
	 * of the new file descriptor, which has not been disturbed since
	 * it was allocated.
	 */
	if ((fp = getf(u.u_r.r_val1)) == NULL)
		return (u.u_error);
	if ((wfp = getf(minor(dev))) == NULL)
		return (u.u_error);
	/*
	 * We must explicitly test for this case because ufalloc() may
	 * have allocated us the same file desriptor we are referring
	 * to, if the proccess referred to an invalid (closed) descriptor.
	 * Ordinarily this would be caught by getf(), but by the time we
	 * reach this routine u_pofile[minor(dev)] could already be set
	 * to point to our file struct.
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
	 * that fp references the inode for this driver.
	 */
	if (fp->f_count != 1 || fp->f_type != DTYPE_INODE) 
		panic("fdopen");
	irele((struct inode *)fp->f_data);
	fp->f_count = 0;
	/* 
	 * Dup the file descriptor. 
	 */
	dupit(u.u_r.r_val1, wfp, u.u_pofile[minor(dev)]);
	return (0);
}
