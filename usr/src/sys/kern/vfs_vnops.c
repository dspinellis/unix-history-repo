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
 *	@(#)vfs_vnops.c	7.4 (Berkeley) %G%
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
#include "../ufs/inode.h"
#include "../ufs/fs.h"
#include "../ufs/quota.h"
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
			vattr_null(vap);
			vap->va_type = VREG;
			vap->va_mode = cmode;
			if (error = VOP_CREATE(ndp, vap))
				return (error);
			fmode &= ~FTRUNC;
			vp = ndp->ni_vp;
		} else {
			vp = ndp->ni_vp;
			ndp->ni_vp = 0;
			VOP_ABORTOP(ndp);
			ndp->ni_vp = vp;
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
			if (error = vn_access(vp, VREAD, ndp->ni_cred))
				goto bad;
		}
		if (fmode & (FWRITE|FTRUNC)) {
			if (error = vn_access(vp, VWRITE, ndp->ni_cred))
				goto bad;
			if (vp->v_type == VDIR) {
				error = EISDIR;
				goto bad;
			}
		}
	}
	if (fmode & FTRUNC) {
		vattr_null(vap);
		vap->va_size = 0;
		if (error = VOP_SETATTR(vp, vap, ndp->ni_cred))
			goto bad;
	}
	VOP_UNLOCK(vp);
	if (setjmp(&u.u_qsave)) {
		if (error == 0)
			error = EINTR;
		return (error);
	}
	return (VOP_OPEN(vp, fmode, ndp->ni_cred));

bad:
	vput(vp);
	return(error);
}

/*
 * Check mode permission on vnode pointer. Mode is READ, WRITE or EXEC.
 * In the case of WRITE, the read-only status of the file system is
 * checked. Also in WRITE, prototype text segments cannot be written.
 */
vn_access(vp, mode, cred)
	register struct vnode *vp;
	int mode;
	struct ucred *cred;
{

	if (mode & VWRITE) {
		/*
		 * Disallow write attempts on read-only file systems;
		 * unless the file is a socket or a block or character
		 * device resident on the file system.
		 */
		if ((vp->v_mount->m_flag & M_RDONLY) &&
			vp->v_type != VCHR &&
			vp->v_type != VBLK &&
			vp->v_type != VSOCK)
				return (EROFS);
		/*
		 * If there's shared text associated with
		 * the inode, try to free it up once.  If
		 * we fail, we can't allow writing.
		 */
		if (vp->v_flag & VTEXT)
			xrele(vp);
		if (vp->v_flag & VTEXT)
			return (ETXTBSY);
	}
	return (VOP_ACCESS(vp, mode, cred));
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

	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = base;
	aiov.iov_len = len;
	auio.uio_resid = len;
	auio.uio_offset = offset;
	auio.uio_segflg = segflg;
	auio.uio_rw = rw;
	if (rw == UIO_READ)
		error = VOP_READ(vp, &auio, &offset, ioflg, cred);
	else
		error = VOP_WRITE(vp, &auio, &offset, ioflg, cred);
	if (aresid)
		*aresid = auio.uio_resid;
	else
		if (auio.uio_resid && error == 0)
			error = EIO;
	return (error);
}

vn_read(fp, uio, cred)
	struct file *fp;
	struct uio *uio;
	struct ucred *cred;
{

	return (VOP_READ((struct vnode *)fp->f_data, uio, &(fp->f_offset),
		(fp->f_flag & FNDELAY) ? IO_NDELAY : 0, cred));
}

vn_write(fp, uio, cred)
	struct file *fp;
	struct uio *uio;
	struct ucred *cred;
{
	register struct vnode *vp = (struct vnode *)fp->f_data;
	int ioflag = 0;

	if (vp->v_type == VREG && (fp->f_flag & FAPPEND))
		ioflag |= IO_APPEND;
	if (fp->f_flag & FNDELAY)
		ioflag |= IO_NDELAY;
	return (VOP_WRITE(vp, uio, &(fp->f_offset), ioflag, cred));
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
		mode |= IFREG;
		break;
	case VDIR:
		mode |= IFDIR;
		break;
	case VBLK:
		mode |= IFBLK;
		break;
	case VCHR:
		mode |= IFCHR;
		break;
	case VLNK:
		mode |= IFLNK;
		break;
	case VSOCK:
		mode |= IFSOCK;
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
	sb->st_spare4[0] = sb->st_spare4[1] = 0;
	/*
	 * XXX THIS IS NOT CORRECT!!, but be sure to change ufs_getattr()
	 * if you change it.
	 */
	sb->st_blocks = vap->va_bytes;
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

	case VCHR:
	case VBLK:
		u.u_r.r_val1 = 0;
		if (setjmp(&u.u_qsave)) {
			if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
				return(EINTR);
			u.u_eosys = RESTARTSYS;
			return (0);
		}
		return (VOP_IOCTL(vp, com, data, fp->f_flag, u.u_cred));
	}
}

/*
 * Vnode select call
 */
vn_select(fp, which)
	struct file *fp;
	int which;
{
	return(VOP_SELECT(((struct vnode *)fp->f_data), which, u.u_cred));
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

	if ((cmd & LOCK_EX) == 0)
		priority += 4;
	if (setjmp(&u.u_qsave)) {
		if ((u.u_sigintr & sigmask(u.u_procp->p_cursig)) != 0)
			return(EINTR);
		u.u_eosys = RESTARTSYS;
		return (0);
	}
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
		sleep((caddr_t)&vp->v_exlockc, priority);
	}
	if ((cmd & LOCK_EX) && (vp->v_flag & VSHLOCK)) {
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
		sleep((caddr_t)&vp->v_shlockc, PLOCK);
		goto again;
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
	int error;

	if ((mp = getvfs(&fhp->fh_fsid)) == NULL)
		return (ESTALE);
	if (error = VFS_FHTOVP(mp, &fhp->fh_fid, vpp))
		return (error);
	if (lockflag)
		VOP_LOCK(*vpp);
	return (0);
}

/*
 * Revoke access the current tty by all processes.
 * Used only by the super-user in init
 * to give ``clean'' terminals at login.
 */
vhangup()
{

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	if (u.u_ttyp == NULL)
		return;
	forceclose(u.u_ttyd);
	if ((u.u_ttyp->t_state) & TS_ISOPEN)
		gsignal(u.u_ttyp->t_pgid, SIGHUP);
	u.u_ttyp->t_session = 0;
	u.u_ttyp->t_pgid = 0;
}

forceclose(dev)
	dev_t dev;
{
	register struct file *fp;
	register struct vnode *vp;

	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0)
			continue;
		if (fp->f_type != DTYPE_VNODE)
			continue;
		vp = (struct vnode *)fp->f_data;
		if (vp == 0)
			continue;
		if (vp->v_type != VCHR)
			continue;
		if (vp->v_rdev != dev)
			continue;
		fp->f_flag &= ~(FREAD|FWRITE);
	}
}

/*
 * Vnode release, just decrement the count and call VOP_INACTIVE()
 */
void vrele(vp)
	register struct vnode *vp;
{

	if (vp == NULL)
		return;
	vp->v_count--;
	if (vp->v_count < 0)
		printf("inode %d, bad ref count %d\n",
			VTOI(vp)->i_number, vp->v_count);
	if (vp->v_count == 0)
		VOP_INACTIVE(vp);
}

/*
 * vput(), just unlock and vrele()
 */
vput(vp)
	register struct vnode *vp;
{
	VOP_UNLOCK(vp);
	vrele(vp);
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
