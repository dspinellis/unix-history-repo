/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 *	@(#)vfs_syscalls.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "syscontext.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "mount.h"
#include "proc.h"
#include "uio.h"
#include "malloc.h"

/*
 * Virtual File System System Calls
 */

/*
 * mount system call
 */
mount()
{
	register struct a {
		int	type;
		char	*dir;
		int	flags;
		caddr_t	data;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	struct vnode *vp;
	struct mount *mp;
	int error;

	/*
	 * Must be super user
	 */
	if (error = suser(u.u_cred, &u.u_acflag))
		RETURN (error);
	/*
	 * Get vnode to be covered
	 */
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->dir;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_count != 1) {
		vput(vp);
		RETURN (EBUSY);
	}
	if (vp->v_type != VDIR) {
		vput(vp);
		RETURN (ENOTDIR);
	}
	if (uap->type > MOUNT_MAXTYPE ||
	    vfssw[uap->type] == (struct vfsops *)0) {
		vput(vp);
		RETURN (ENODEV);
	}

	/*
	 * Mount the filesystem.
	 */
	mp = (struct mount *)malloc((u_long)sizeof(struct mount),
		M_MOUNT, M_WAITOK);
	mp->m_op = vfssw[uap->type];
	mp->m_flag = 0;
	mp->m_exroot = 0;
	error = vfs_add(vp, mp, uap->flags);
	if (!error)
		error = VFS_MOUNT(mp, uap->dir, uap->data, ndp);
	cache_purge(vp);
	VOP_UNLOCK(vp);
	if (!error) {
		vfs_unlock(mp);
	} else {
		vfs_remove(mp);
		free((caddr_t)mp, M_MOUNT);
		vrele(vp);
	}
	RETURN (error);
}

/*
 * Unmount system call.
 *
 * Note: unmount takes a path to the vnode mounted on as argument,
 * not special file (as before).
 */
unmount()
{
	struct a {
		char	*pathp;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	register struct vnode *vp;
	register struct mount *mp;
	register struct nameidata *ndp = &u.u_nd;
	struct vnode *coveredvp;
	int error;

	/*
	 * Must be super user
	 */
	if (error = suser(u.u_cred, &u.u_acflag))
		RETURN (error);

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->pathp;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	/*
	 * Must be the root of the filesystem
	 */
	if ((vp->v_flag & VROOT) == 0) {
		vput(vp);
		RETURN (EINVAL);
	}
	mp = vp->v_mount;
	vput(vp);
	/*
	 * Do the unmount.
	 */
	coveredvp = mp->m_vnodecovered;
	if (error = vfs_lock(mp))
		RETURN (error);

	xumount(mp);		/* remove unused sticky files from text table */
	cache_purgevfs(mp);	/* remove cache entries for this file sys */
	VFS_SYNC(mp, MNT_WAIT);

	error = VFS_UNMOUNT(mp, uap->flags);
	if (error) {
		vfs_unlock(mp);
	} else {
		vrele(coveredvp);
		vfs_remove(mp);
		free((caddr_t)mp, M_MOUNT);
	}
	RETURN (error);
}

/*
 * Sync system call.
 * Sync each mounted filesystem.
 */
sync()
{
	register struct mount *mp;

	mp = rootfs;
	do {
		if ((mp->m_flag & M_RDONLY) == 0)
			VFS_SYNC(mp, MNT_NOWAIT);
		mp = mp->m_next;
	} while (mp != rootfs);
}

/*
 * get filesystem statistics
 */
statfs()
{
	struct a {
		char *path;
		struct statfs *buf;
	} *uap = (struct a *)u.u_ap;
	register struct vnode *vp;
	register struct nameidata *ndp = &u.u_nd;
	struct statfs sb;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->path;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (error = VFS_STATFS(vp->v_mount, &sb))
		goto out;
	error = copyout((caddr_t)&sb, (caddr_t)uap->buf, sizeof(sb));
out:
	vput(vp);
	RETURN (error);
}

fstatfs()
{
	struct a {
		int fd;
		struct statfs *buf;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct statfs sb;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	if (error = VFS_STATFS(((struct vnode *)fp->f_data)->v_mount, &sb))
		RETURN (error);
	RETURN (copyout((caddr_t)&sb, (caddr_t)uap->buf, sizeof(sb)));
}

/*
 * Change current working directory to a given file descriptor.
 */
fchdir()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;
	register struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_type != VDIR)
		error = ENOTDIR;
	else
		error = vn_access(vp, VEXEC, u.u_cred);
	VOP_UNLOCK(vp);
	vrele(u.u_cdir);
	u.u_cdir = vp;
	RETURN (error);
}

/*
 * Change current working directory (``.'').
 */
chdir()
{
	struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = chdirec(ndp))
		RETURN (error);
	vrele(u.u_cdir);
	u.u_cdir = ndp->ni_vp;
	RETURN (0);
}

/*
 * Change notion of root (``/'') directory.
 */
chroot()
{
	struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		RETURN (error);
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = chdirec(ndp))
		RETURN (error);
	vrele(u.u_rdir);
	u.u_rdir = ndp->ni_vp;
	RETURN (0);
}

/*
 * Common routine for chroot and chdir.
 */
chdirec(ndp)
	register struct nameidata *ndp;
{
	struct vnode *vp;
	int error;

	if (error = namei(ndp))
		return (error);
	vp = ndp->ni_vp;
	if (vp->v_type != VDIR)
		error = ENOTDIR;
	else
		error = vn_access(vp, VEXEC, ndp->ni_cred);
	VOP_UNLOCK(vp);
	if (error)
		vrele(vp);
	return (error);
}

/*
 * Open system call.
 */
open()
{
	struct a {
		char	*fname;
		int	mode;
		int	crtmode;
	} *uap = (struct a *) u.u_ap;
	struct nameidata *ndp = &u.u_nd;

	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	RETURN (copen(uap->mode-FOPEN, uap->crtmode &~ u.u_cmask, ndp,
		&u.u_r.r_val1));
}

/*
 * Creat system call.
 */
creat()
{
	struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	struct nameidata *ndp = &u.u_nd;

	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	RETURN (copen(FWRITE|FCREAT|FTRUNC, uap->fmode &~ u.u_cmask, ndp,
		&u.u_r.r_val1));
}

/*
 * Common code for open and creat.
 * Check permissions, allocate an open file structure,
 * and call the device open routine if any.
 */
copen(fmode, cmode, ndp, resultfd)
	int fmode, cmode;
	struct nameidata *ndp;
	int *resultfd;
{
	register struct file *fp;
	struct file *nfp;
	int indx, error;
	extern struct fileops vnops;

	if (error = falloc(&nfp, &indx))
		return (error);
	fp = nfp;
	u.u_r.r_val1 = indx;	/* XXX for fdopen() */
	if (error = vn_open(ndp, fmode, (cmode & 07777) &~ ISVTX)) {
		u.u_ofile[indx] = NULL;
		crfree(fp->f_cred);
		fp->f_count--;
		return (error);
	}
	fp->f_flag = fmode & FMASK;
	fp->f_type = DTYPE_VNODE;
	fp->f_ops = &vnops;
	fp->f_data = (caddr_t)ndp->ni_vp;
	if (resultfd)
		*resultfd = indx;
	return (0);
}

/*
 * Mknod system call
 */
mknod()
{
	register struct a {
		char	*fname;
		int	fmode;
		int	dev;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		RETURN (error);
	ndp->ni_nameiop = CREATE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp != NULL) {
		error = EEXIST;
		goto out;
	}
	vattr_null(&vattr);
	switch (uap->fmode & IFMT) {

	case IFMT:	/* used by badsect to flag bad sectors */
		vattr.va_type = VBAD;
		break;
	case IFCHR:
		vattr.va_type = VCHR;
		break;
	case IFBLK:
		vattr.va_type = VBLK;
		break;
	default:
		error = EINVAL;
		goto out;
	}
	vattr.va_mode = (uap->fmode & 07777) &~ u.u_cmask;
	vattr.va_rdev = uap->dev;
out:
	if (error)
		VOP_ABORTOP(ndp);
	else
		error = VOP_MKNOD(ndp, &vattr, ndp->ni_cred);
	RETURN (error);
}

/*
 * link system call
 */
link()
{
	register struct a {
		char	*target;
		char	*linkname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp, *xp;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->target;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type == VDIR &&
	    (error = suser(u.u_cred, &u.u_acflag)))
		goto out1;
	ndp->ni_nameiop = CREATE | LOCKPARENT;
	ndp->ni_dirp = (caddr_t)uap->linkname;
	if (error = namei(ndp))
		goto out1;
	xp = ndp->ni_vp;
	if (xp != NULL) {
		error = EEXIST;
		goto out;
	}
	xp = ndp->ni_dvp;
	if (vp->v_mount != xp->v_mount)
		error = EXDEV;
out:
	if (error)
		VOP_ABORTOP(ndp);
	else
		error = VOP_LINK(vp, ndp);
out1:
	vrele(vp);
	RETURN (error);
}

/*
 * symlink -- make a symbolic link
 */
symlink()
{
	struct a {
		char	*target;
		char	*linkname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	char *target;
	int error;

	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->linkname;
	MALLOC(target, char *, MAXPATHLEN, M_NAMEI, M_WAITOK);
	if (error = copyinstr(uap->target, target, MAXPATHLEN, (u_int *)0))
		goto out1;
	ndp->ni_nameiop = CREATE | LOCKPARENT;
	if (error = namei(ndp))
		goto out1;
	vp = ndp->ni_vp;
	if (vp) {
		error = EEXIST;
		goto out;
	}
	vp = ndp->ni_dvp;
	vattr_null(&vattr);
	vattr.va_mode = 0777 &~ u.u_cmask;
out:
	if (error)
		VOP_ABORTOP(ndp);
	else
		error = VOP_SYMLINK(ndp, &vattr, target);
out1:
	FREE(target, M_NAMEI);
	RETURN (error);
}

/*
 * Unlink system call.
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
unlink()
{
	struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	int error;

	ndp->ni_nameiop = DELETE | LOCKPARENT | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type == VDIR &&
	    (error = suser(u.u_cred, &u.u_acflag)))
		goto out;
	/*
	 * Don't unlink a mounted file.
	 */
	if (vp->v_flag & VROOT) {
		error = EBUSY;
		goto out;
	}
	if (vp->v_flag & VTEXT)
		xrele(vp);	/* try once to free text */
out:
	if (error)
		VOP_ABORTOP(ndp);
	else
		error = VOP_REMOVE(ndp);
	RETURN (error);
}

/*
 * Seek system call
 */
lseek()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		off_t	off;
		int	sbase;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	int error;

	if ((unsigned)uap->fdes >= NOFILE ||
	    (fp = u.u_ofile[uap->fdes]) == NULL)
		RETURN (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		RETURN (ESPIPE);
	switch (uap->sbase) {

	case L_INCR:
		fp->f_offset += uap->off;
		break;

	case L_XTND:
		if (error = VOP_GETATTR((struct vnode *)fp->f_data,
		    &vattr, u.u_cred))
			RETURN (error);
		fp->f_offset = uap->off + vattr.va_size;
		break;

	case L_SET:
		fp->f_offset = uap->off;
		break;

	default:
		RETURN (EINVAL);
	}
	u.u_r.r_off = fp->f_offset;
	RETURN (0);
}

/*
 * Access system call
 */
saccess()
{
	register struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	int error, mode, svuid, svgid;

	svuid = u.u_uid;
	svgid = u.u_gid;
	u.u_uid = u.u_ruid;
	u.u_gid = u.u_rgid;
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		goto out1;
	vp = ndp->ni_vp;
	/*
	 * fmode == 0 means only check for exist
	 */
	if (uap->fmode) {
		mode = 0;
		if (uap->fmode & R_OK)
			mode |= VREAD;
		if (uap->fmode & W_OK)
			mode |= VWRITE;
		if (uap->fmode & X_OK)
			mode |= VEXEC;
		error = vn_access(vp, mode, u.u_cred);
	}
	vput(vp);
out1:
	u.u_uid = svuid;
	u.u_gid = svgid;
	RETURN (error);
}

/*
 * Stat system call.  This version follows links.
 */
stat()
{

	stat1(FOLLOW);
}

/*
 * Lstat system call.  This version does not follow links.
 */
lstat()
{

	stat1(NOFOLLOW);
}

stat1(follow)
	int follow;
{
	register struct a {
		char	*fname;
		struct stat *ub;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	struct stat sb;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | follow;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	error = vn_stat(ndp->ni_vp, &sb);
	vput(ndp->ni_vp);
	if (error)
		RETURN (error);
	error = copyout((caddr_t)&sb, (caddr_t)uap->ub, sizeof (sb));
	RETURN (error);
}

/*
 * Return target name of a symbolic link
 */
readlink()
{
	register struct a {
		char	*name;
		char	*buf;
		int	count;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct iovec aiov;
	struct uio auio;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type != VLNK) {
		error = EINVAL;
		goto out;
	}
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_offset = 0;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_resid = uap->count;
	error = VOP_READLINK(vp, &auio, ndp->ni_cred);
out:
	vput(vp);
	u.u_r.r_val1 = uap->count - auio.uio_resid;
	RETURN (error);
}

/*
 * Change flags of a file given path name.
 */
chflags()
{
	struct a {
		char	*fname;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_flags = uap->flags;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Change flags of a file given a file descriptor.
 */
fchflags()
{
	struct a {
		int	fd;
		int	flags;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	vattr_null(&vattr);
	vattr.va_flags = uap->flags;
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Change mode of a file given path name.
 */
chmod()
{
	struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Change mode of a file given a file descriptor.
 */
fchmod()
{
	struct a {
		int	fd;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	vattr_null(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Set ownership given a path name.
 */
chown()
{
	struct a {
		char	*fname;
		int	uid;
		int	gid;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | NOFOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Set ownership given a file descriptor.
 */
fchown()
{
	struct a {
		int	fd;
		int	uid;
		int	gid;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	vattr_null(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

utimes()
{
	register struct a {
		char	*fname;
		struct	timeval *tptr;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct timeval tv[2];
	struct vattr vattr;
	int error;

	if (error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv)))
		RETURN (error);
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_atime = tv[0];
	vattr.va_mtime = tv[1];
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->m_flag & M_RDONLY) {
		error = EROFS;
		goto out;
	}
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Truncate a file given its path name.
 */
truncate()
{
	struct a {
		char	*fname;
		off_t	length;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	vattr_null(&vattr);
	vattr.va_size = uap->length;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type == VDIR) {
		error = EISDIR;
		goto out;
	}
	if (error = vn_access(vp, VWRITE, ndp->ni_cred))
		goto out;
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Truncate a file given a file descriptor.
 */
ftruncate()
{
	struct a {
		int	fd;
		off_t	length;
	} *uap = (struct a *)u.u_ap;
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	if ((fp->f_flag & FWRITE) == 0)
		RETURN (EINVAL);
	vattr_null(&vattr);
	vattr.va_size = uap->length;
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_type == VDIR) {
		error = EISDIR;
		goto out;
	}
	if (error = vn_access(vp, VWRITE, fp->f_cred))
		goto out;
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Synch an open file.
 */
fsync()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	error = VOP_FSYNC((struct vnode *)fp->f_data, fp->f_flag, fp->f_cred);
	RETURN (error);
}

/*
 * Rename system call.
 *
 * Source and destination must either both be directories, or both
 * not be directories.  If target is a directory, it must be empty.
 */
rename()
{
	struct a {
		char	*from;
		char	*to;
	} *uap = (struct a *)u.u_ap;
	register struct vnode *tvp, *fvp, *tdvp;
	register struct nameidata *ndp = &u.u_nd;
	struct nameidata tond;
	int error;

	ndp->ni_nameiop = DELETE | WANTPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->from;
	if (error = namei(ndp))
		RETURN (error);
	fvp = ndp->ni_vp;
	bzero((caddr_t)&tond, sizeof(tond));
	tond.ni_nameiop = RENAME | LOCKPARENT | LOCKLEAF | NOCACHE;
	tond.ni_segflg = UIO_USERSPACE;
	tond.ni_dirp = uap->to;
	tond.ni_cdir = ndp->ni_cdir;
	tond.ni_cdir->v_count++;
	tond.ni_rdir = ndp->ni_rdir;
	if (tond.ni_rdir)
		tond.ni_rdir->v_count++;
	tond.ni_cred = ndp->ni_cred;
	crhold(tond.ni_cred);
	error = namei(&tond);
	tdvp = tond.ni_dvp;
	tvp = tond.ni_vp;
	if (tvp != NULL) {
		if (fvp->v_type == VDIR && tvp->v_type != VDIR) {
			error = EISDIR;
			goto out;
		} else if (fvp->v_type != VDIR && tvp->v_type == VDIR) {
			error = ENOTDIR;
			goto out;
		}
	}
	if (error) {
		VOP_ABORTOP(ndp);
		goto out1;
	}
	if (fvp->v_mount != tdvp->v_mount) {
		error = EXDEV;
		goto out;
	}
	if (fvp == tdvp || fvp == tvp)
		error = EINVAL;
out:
	if (error) {
		VOP_ABORTOP(&tond);
		VOP_ABORTOP(ndp);
	} else {
		error = VOP_RENAME(ndp, &tond);
	}
out1:
	vrele(tond.ni_cdir);
	if (tond.ni_rdir)
		vrele(tond.ni_rdir);
	crfree(tond.ni_cred);
	RETURN (error);
}

/*
 * Mkdir system call
 */
mkdir()
{
	struct a {
		char	*name;
		int	dmode;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = CREATE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp != NULL) {
		VOP_ABORTOP(ndp);
		RETURN (EEXIST);
	}
	vattr_null(&vattr);
	vattr.va_type = VDIR;
	vattr.va_mode = (uap->dmode & 0777) &~ u.u_cmask;
	error = VOP_MKDIR(ndp, &vattr);
	if (!error)
		vput(ndp->ni_vp);
	RETURN (error);
}

/*
 * Rmdir system call.
 */
rmdir()
{
	struct a {
		char	*name;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	int error;

	ndp->ni_nameiop = DELETE | LOCKPARENT | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type != VDIR) {
		error = ENOTDIR;
		goto out;
	}
	/*
	 * No rmdir "." please.
	 */
	if (ndp->ni_dvp == vp) {
		error = EINVAL;
		goto out;
	}
	/*
	 * Don't unlink a mounted file.
	 */
	if (vp->v_flag & VROOT)
		error = EBUSY;
out:
	if (error)
		VOP_ABORTOP(ndp);
	else
		error = VOP_RMDIR(ndp);
	RETURN (error);
}

/*
 * Read a block of directory entries in a file system independent format
 */
getdirentries()
{
	register struct a {
		int	fd;
		char	*buf;
		unsigned count;
		long	*basep;
	} *uap = (struct a *)u.u_ap;
	struct file *fp;
	struct uio auio;
	struct iovec aiov;
	off_t off;
	int error;

	if (error = getvnode(uap->fd, &fp))
		RETURN (error);
	if ((fp->f_flag & FREAD) == 0)
		RETURN (EBADF);
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_resid = uap->count;
	off = fp->f_offset;
	if (error = VOP_READDIR((struct vnode *)fp->f_data, &auio,
	    &(fp->f_offset), fp->f_cred))
		RETURN (error);
	error = copyout((caddr_t)&off, (caddr_t)uap->basep,
		sizeof(long));
	u.u_r.r_val1 = uap->count - auio.uio_resid;
	RETURN (error);
}

/*
 * mode mask for creation of files
 */
umask()
{
	register struct a {
		int	mask;
	} *uap = (struct a *)u.u_ap;

	u.u_r.r_val1 = u.u_cmask;
	u.u_cmask = uap->mask & 07777;
	RETURN (0);
}

getvnode(fdes, fpp)
	struct file **fpp;
	int fdes;
{
	struct file *fp;

	if ((unsigned)fdes >= NOFILE || (fp = u.u_ofile[fdes]) == NULL)
		return (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return (EINVAL);
	*fpp = fp;
	return (0);
}
