/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_vnops.c	7.29 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "buf.h"
#include "proc.h"
#include "mount.h"
#include "namei.h"
#include "vnode.h"
#include "ioctl.h"
#include "tty.h"

struct 	fileops vnops =
	{ vn_read, vn_write, vn_ioctl, vn_select, vn_close };

/*
 * Common code for vnode open operations.
 * Check permissions, and call the VOP_OPEN or VOP_CREATE routine.
 */
vn_open(ndp, p, fmode, cmode)
	register struct nameidata *ndp;
	struct proc *p;
	int fmode, cmode;
{
	register struct vnode *vp;
	register struct ucred *cred = p->p_ucred;
	struct vattr vat;
	struct vattr *vap = &vat;
	int error;

	if (fmode & FCREAT) {
		ndp->ni_nameiop = CREATE | LOCKPARENT | LOCKLEAF;
		if ((fmode & FEXCL) == 0)
			ndp->ni_nameiop |= FOLLOW;
		if (error = namei(ndp, p))
			return (error);
		if (ndp->ni_vp == NULL) {
			VATTR_NULL(vap);
			vap->va_type = VREG;
			vap->va_mode = cmode;
			if (error = VOP_CREATE(ndp, vap, p))
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
		if (error = namei(ndp, p))
			return (error);
		vp = ndp->ni_vp;
	}
	if (vp->v_type == VSOCK) {
		error = EOPNOTSUPP;
		goto bad;
	}
	if ((fmode & FCREAT) == 0) {
		if (fmode & FREAD) {
			if (error = VOP_ACCESS(vp, VREAD, cred, p))
				goto bad;
		}
		if (fmode & (FWRITE|FTRUNC)) {
			if (vp->v_type == VDIR) {
				error = EISDIR;
				goto bad;
			}
			if ((error = vn_writechk(vp)) ||
			    (error = VOP_ACCESS(vp, VWRITE, cred, p)))
				goto bad;
		}
	}
	if (fmode & FTRUNC) {
		VATTR_NULL(vap);
		vap->va_size = 0;
		if (error = VOP_SETATTR(vp, vap, cred, p))
			goto bad;
	}
	VOP_UNLOCK(vp);
	error = VOP_OPEN(vp, fmode, cred, p);
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
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		switch (vp->v_type) {
		case VREG: case VDIR: case VLNK:
			return (EROFS);
		}
	}
	/*
	 * If there's shared text associated with
	 * the vnode, try to free it up once.  If
	 * we fail, we can't allow writing.
	 */
	if ((vp->v_flag & VTEXT) && !vnode_pager_uncache(vp))
		return (ETXTBSY);
	return (0);
}

/*
 * Vnode version of rdwri() for calls on file systems.
 */
vn_rdwr(rw, vp, base, len, offset, segflg, ioflg, cred, aresid, p)
	enum uio_rw rw;
	struct vnode *vp;
	caddr_t base;
	int len;
	off_t offset;
	enum uio_seg segflg;
	int ioflg;
	struct ucred *cred;
	int *aresid;
	struct proc *p;
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
	auio.uio_procp = p;
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
vn_stat(vp, sb, p)
	struct vnode *vp;
	register struct stat *sb;
	struct proc *p;
{
	struct vattr vattr;
	register struct vattr *vap;
	int error;
	u_short mode;

	vap = &vattr;
	error = VOP_GETATTR(vp, vap, p->p_ucred, p);
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
vn_ioctl(fp, com, data, p)
	struct file *fp;
	int com;
	caddr_t data;
	struct proc *p;
{
	register struct vnode *vp = ((struct vnode *)fp->f_data);
	struct vattr vattr;
	int error;

	switch (vp->v_type) {

	case VREG:
	case VDIR:
		if (com == FIONREAD) {
			if (error = VOP_GETATTR(vp, &vattr, p->p_ucred, p))
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
		error = VOP_IOCTL(vp, com, data, fp->f_flag, p->p_ucred, p);
		if (error == 0 && com == TIOCSCTTY) {
			p->p_session->s_ttyvp = vp;
			VREF(vp);
		}
		return (error);
	}
}

/*
 * Vnode select call
 */
vn_select(fp, which, p)
	struct file *fp;
	int which;
	struct proc *p;
{

	return (VOP_SELECT(((struct vnode *)fp->f_data), which, fp->f_flag,
		p->p_ucred, p));
}

/*
 * Vnode close call
 */
vn_close(fp, p)
	register struct file *fp;
	struct proc *p;
{
	struct vnode *vp = ((struct vnode *)fp->f_data);
	int error;

	/*
	 * Must delete vnode reference from this file entry
	 * before VOP_CLOSE, so that only other references
	 * will prevent close.
	 */
	fp->f_data = (caddr_t) 0;
	error = VOP_CLOSE(vp, fp->f_flag, fp->f_cred, p);
	vrele(vp);
	return (error);
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
