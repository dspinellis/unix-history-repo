/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vfs_vnops.c	7.23 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "buf.h"
#include "proc.h"
#include "uio.h"
#include "socket.h"
#include "socketvar.h"
#include "mount.h"
#include "vnode.h"
#include "ioctl.h"
#include "tty.h"

int	vn_read(), vn_write(), vn_ioctl(), vn_select(), vn_close();
struct 	fileops vnops =
	{ vn_read, vn_write, vn_ioctl, vn_select, vn_close };

/*
 * Common code for vnode open operations.
 * Check permissions, and call the VOP_OPEN or VOP_CREATE routine.
 */
vn_open(ndp, fmode, cmode)
	register struct nameidata *ndp;
	int fmode, cmode;
{
	register struct vnode *vp;
	struct vattr vat;
	struct vattr *vap = &vat;
	int error;

	if (fmode & FCREAT) {
		ndp->ni_nameiop = CREATE | LOCKPARENT | LOCKLEAF;
		if ((fmode & FEXCL) == 0)
			ndp->ni_nameiop |= FOLLOW;
		if (error = namei(ndp))
			return (error);
		if (ndp->ni_vp == NULL) {
			VATTR_NULL(vap);
			vap->va_type = VREG;
			vap->va_mode = cmode;
			if (error = VOP_CREATE(ndp, vap))
				return (error);
			fmode &= ~FTRUNC;
			vp = ndp->ni_vp;
		} else {
			if (ndp->ni_dvp == ndp->ni_vp)
				vrele(ndp->ni_dvp);
			else
				vput(ndp->ni_dvp);
			ndp->ni_dvp = NULL;
			vp = ndp->ni_vp;
			if (fmode & FEXCL) {
				error = EEXIST;
				goto bad;
			}
			fmode &= ~FCREAT;
		}
	} else {
		ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
		if (error = namei(ndp))
			return (error);
		vp = ndp->ni_vp;
	}
	if (vp->v_type == VSOCK) {
		error = EOPNOTSUPP;
		goto bad;
	}
	if ((fmode & FCREAT) == 0) {
		if (fmode & FREAD) {
			if (error = VOP_ACCESS(vp, VREAD, ndp->ni_cred))
				goto bad;
		}
		if (fmode & (FWRITE|FTRUNC)) {
			if (vp->v_type == VDIR) {
				error = EISDIR;
				goto bad;
			}
			if ((error = vn_writechk(vp)) ||
			    (error = VOP_ACCESS(vp, VWRITE, ndp->ni_cred)))
				goto bad;
		}
	}
	if (fmode & FTRUNC) {
		VATTR_NULL(vap);
		vap->va_size = 0;
		if (error = VOP_SETATTR(vp, vap, ndp->ni_cred))
			goto bad;
	}
	VOP_UNLOCK(vp);
	error = VOP_OPEN(vp, fmode, ndp->ni_cred);
	if (error)
		vrele(vp);
	return (error);

bad:
	vput(vp);
	return (error);
}

/*
 * Check for write permissions on the specified vnode.
 * The read-only status of the file system is checked.
 * Also, prototype text segments cannot be written.
 */
vn_writechk(vp)
	register struct vnode *vp;
{

	/*
	 * Disallow write attempts on read-only file systems;
	 * unless the file is a socket or a block or character
	 * device resident on the file system.
	 */
	if ((vp->v_mount->mnt_flag & MNT_RDONLY) && vp->v_type != VCHR &&
	    vp->v_type != VBLK && vp->v_type != VSOCK)
		return (EROFS);
	/*
	 * If there's shared text associated with
	 * the vnode, try to free it up once.  If
	 * we fail, we can't allow writing.
	 */
	if (vp->v_flag & VTEXT)
		xrele(vp);
	if (vp->v_flag & VTEXT)
		return (ETXTBSY);
	return (0);
}

/*
 * Vnode version of rdwri() for calls on file systems.
 */
vn_rdwr(rw, vp, base, len, offset, segflg, ioflg, cred, aresid)
	enum uio_rw rw;
	struct vnode *vp;
	caddr_t base;
	int len;
	off_t offset;
	enum uio_seg segflg;
	int ioflg;
	struct ucred *cred;
	int *aresid;
{
	struct uio auio;
	struct iovec aiov;
	int error;

	if ((ioflg & IO_NODELOCKED) == 0)
		VOP_LOCK(vp);
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = base;
	aiov.iov_len = len;
	auio.uio_resid = len;
	auio.uio_offset = offset;
	auio.uio_segflg = segflg;
	auio.uio_rw = rw;
	if (rw == UIO_READ)
		error = VOP_READ(vp, &auio, ioflg, cred);
	else
		error = VOP_WRITE(vp, &auio, ioflg, cred);
	if (aresid)
		*aresid = auio.uio_resid;
	else
		if (auio.uio_resid && error == 0)
			error = EIO;
	if ((ioflg & IO_NODELOCKED) == 0)
		VOP_UNLOCK(vp);
	return (error);
}

vn_read(fp, uio, cred)
	struct file *fp;
	struct uio *uio;
	struct ucred *cred;
{
	register struct vnode *vp = (struct vnode *)fp->f_data;
	int count, error;

	VOP_LOCK(vp);
	uio->uio_offset = fp->f_offset;
	count = uio->uio_resid;
	error = VOP_READ(vp, uio, (fp->f_flag & FNDELAY) ? IO_NDELAY : 0, cred);
	fp->f_offset += count - uio->uio_resid;
	VOP_UNLOCK(vp);
	return (error);
}

vn_write(fp, uio, cred)
	struct file *fp;
	struct uio *uio;
	struct ucred *cred;
{
	register struct vnode *vp = (struct vnode *)fp->f_data;
	int count, error, ioflag = 0;

	if (vp->v_type == VREG && (fp->f_flag & FAPPEND))
		ioflag |= IO_APPEND;
	if (fp->f_flag & FNDELAY)
		ioflag |= IO_NDELAY;
	VOP_LOCK(vp);
	uio->uio_offset = fp->f_offset;
	count = uio->uio_resid;
	error = VOP_WRITE(vp, uio, ioflag, cred);
	if (ioflag & IO_APPEND)
		fp->f_offset = uio->uio_offset;
	else
		fp->f_offset += count - uio->uio_resid;
	VOP_UNLOCK(vp);
	return (error);
}

/*
 * Get stat info for a vnode.
 */
vn_stat(vp, sb)
	struct vnode *vp;
	register struct stat *sb;
{
	struct vattr vattr;
	register struct vattr *vap;
	int error;
	u_short mode;

	vap = &vattr;
	error = VOP_GETATTR(vp, vap, u.u_cred);
	if (error)
		return (error);
	/*
	 * Copy from vattr table
	 */
	sb->st_dev = vap->va_fsid;
	sb->st_ino = vap->va_fileid;
	mode = vap->va_mode;
	switch (vp->v_type) {
	case VREG:
		mode |= S_IFREG;
		break;
	case VDIR:
		mode |= S_IFDIR;
		break;
	case VBLK:
		mode |= S_IFBLK;
		break;
	case VCHR:
		mode |= S_IFCHR;
		break;
	case VLNK:
		mode |= S_IFLNK;
		break;
	case VSOCK:
		mode |= S_IFSOCK;
		break;
	case VFIFO:
		mode |= S_IFIFO;
		break;
	default:
		return (EBADF);
	};
	sb->st_mode = mode;
	sb->st_nlink = vap->va_nlink;
	sb->st_uid = vap->va_uid;
	sb->st_gid = vap->va_gid;
	sb->st_rdev = vap->va_rdev;
	sb->st_size = vap->va_size;
	sb->st_atime = vap->va_atime.tv_sec;
	sb->st_spare1 = 0;
	sb->st_mtime = vap->va_mtime.tv_sec;
	sb->st_spare2 = 0;
	sb->st_ctime = vap->va_ctime.tv_sec;
	sb->st_spare3 = 0;
	sb->st_blksize = vap->va_blocksize;
	sb->st_flags = vap->va_flags;
	sb->st_gen = vap->va_gen;
	sb->st_blocks = vap->va_bytes / S_BLKSIZE;
	return (0);
}

/*
 * Vnode ioctl call
 */
vn_ioctl(fp, com, data)
	struct file *fp;
	int com;
	caddr_t data;
{
	register struct vnode *vp = ((struct vnode *)fp->f_data);
	struct vattr vattr;
	int error;

	switch (vp->v_type) {

	case VREG:
	case VDIR:
		if (com == FIONREAD) {
			if (error = VOP_GETATTR(vp, &vattr, u.u_cred))
				return (error);
			*(off_t *)data = vattr.va_size - fp->f_offset;
			return (0);
		}
		if (com == FIONBIO || com == FIOASYNC)	/* XXX */
			return (0);			/* XXX */
		/* fall into ... */

	default:
		return (ENOTTY);

	case VFIFO:
	case VCHR:
	case VBLK:
		error = VOP_IOCTL(vp, com, data, fp->f_flag, u.u_cred);
		if (error == 0 && com == TIOCSCTTY) {
			u.u_procp->p_session->s_ttyvp = vp;
			VREF(vp);
		}
		return (error);
	}
}

/*
 * Vnode select call
 */
vn_select(fp, which)
	struct file *fp;
	int which;
{
	return (VOP_SELECT(((struct vnode *)fp->f_data), which, fp->f_flag,
		u.u_cred));
}

/*
 * Vnode close call
 */
vn_close(fp)
	register struct file *fp;
{
	struct vnode *vp = ((struct vnode *)fp->f_data);
	int error;

	if (fp->f_flag & (FSHLOCK|FEXLOCK))
		vn_unlock(fp, FSHLOCK|FEXLOCK);
	/*
	 * Must delete vnode reference from this file entry
	 * before VOP_CLOSE, so that only other references
	 * will prevent close.
	 */
	fp->f_data = (caddr_t) 0;
	error = VOP_CLOSE(vp, fp->f_flag, u.u_cred);
	vrele(vp);
	return (error);
}

/*
 * Place an advisory lock on a vnode.
 * !! THIS IMPLIES THAT ALL STATEFUL FILE SERVERS WILL USE file table entries
 */
vn_lock(fp, cmd)
	register struct file *fp;
	int cmd;
{
	register int priority = PLOCK;
	register struct vnode *vp = (struct vnode *)fp->f_data;
	int error = 0;
	static char lockstr[] = "flock";

	if ((cmd & LOCK_EX) == 0)
		priority += 4;
	priority |= PCATCH;

	/*
	 * If there's a exclusive lock currently applied
	 * to the file, then we've gotta wait for the
	 * lock with everyone else.
	 */
again:
	while (vp->v_flag & VEXLOCK) {
		/*
		 * If we're holding an exclusive
		 * lock, then release it.
		 */
		if (fp->f_flag & FEXLOCK) {
			vn_unlock(fp, FEXLOCK);
			continue;
		}
		if (cmd & LOCK_NB)
			return (EWOULDBLOCK);
		vp->v_flag |= VLWAIT;
		if (error = tsleep((caddr_t)&vp->v_exlockc, priority,
		    lockstr, 0))
			return (error);
	}
	if (error = 0 && (cmd & LOCK_EX) && (vp->v_flag & VSHLOCK)) {
		/*
		 * Must wait for any shared locks to finish
		 * before we try to apply a exclusive lock.
		 *
		 * If we're holding a shared
		 * lock, then release it.
		 */
		if (fp->f_flag & FSHLOCK) {
			vn_unlock(fp, FSHLOCK);
			goto again;
		}
		if (cmd & LOCK_NB)
			return (EWOULDBLOCK);
		vp->v_flag |= VLWAIT;
		if (error = tsleep((caddr_t)&vp->v_shlockc, PLOCK | PCATCH,
		    lockstr, 0) == 0)
			return (error);
	}
	if (fp->f_flag & FEXLOCK)
		panic("vn_lock");
	if (cmd & LOCK_EX) {
		cmd &= ~LOCK_SH;
		vp->v_exlockc++;
		vp->v_flag |= VEXLOCK;
		fp->f_flag |= FEXLOCK;
	}
	if ((cmd & LOCK_SH) && (fp->f_flag & FSHLOCK) == 0) {
		vp->v_shlockc++;
		vp->v_flag |= VSHLOCK;
		fp->f_flag |= FSHLOCK;
	}
	return (0);
}

/*
 * Unlock a file.
 */
vn_unlock(fp, kind)
	register struct file *fp;
	int kind;
{
	register struct vnode *vp = (struct vnode *)fp->f_data;
	int flags;

	kind &= fp->f_flag;
	if (vp == NULL || kind == 0)
		return;
	flags = vp->v_flag;
	if (kind & FSHLOCK) {
		if ((flags & VSHLOCK) == 0)
			panic("vn_unlock: SHLOCK");
		if (--vp->v_shlockc == 0) {
			vp->v_flag &= ~VSHLOCK;
			if (flags & VLWAIT)
				wakeup((caddr_t)&vp->v_shlockc);
		}
		fp->f_flag &= ~FSHLOCK;
	}
	if (kind & FEXLOCK) {
		if ((flags & VEXLOCK) == 0)
			panic("vn_unlock: EXLOCK");
		if (--vp->v_exlockc == 0) {
			vp->v_flag &= ~(VEXLOCK|VLWAIT);
			if (flags & VLWAIT)
				wakeup((caddr_t)&vp->v_exlockc);
		}
		fp->f_flag &= ~FEXLOCK;
	}
}

/*
 * vn_fhtovp() - convert a fh to a vnode ptr (optionally locked)
 * 	- look up fsid in mount list (if not found ret error)
 *	- get vp by calling VFS_FHTOVP() macro
 *	- if lockflag lock it with VOP_LOCK()
 */
vn_fhtovp(fhp, lockflag, vpp)
	fhandle_t *fhp;
	int lockflag;
	struct vnode **vpp;
{
	register struct mount *mp;

	if ((mp = getvfs(&fhp->fh_fsid)) == NULL)
		return (ESTALE);
	if (VFS_FHTOVP(mp, &fhp->fh_fid, vpp))
		return (ESTALE);
	if (!lockflag)
		VOP_UNLOCK(*vpp);
	return (0);
}

/*
 * Noop
 */
vfs_noop()
{

	return (ENXIO);
}

/*
 * Null op
 */
vfs_nullop()
{

	return (0);
}
