/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_syscalls.c	7.61 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "vnode.h"
#include "mount.h"
#include "proc.h"
#include "uio.h"
#include "malloc.h"

#define RETURN(val) {if (u.u_spare[0] != 0) panic("lock count"); return (val);}

/*
 * Virtual File System System Calls
 */

/*
 * mount system call
 */
/* ARGSUSED */
mount(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	type;
		char	*dir;
		int	flags;
		caddr_t	data;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	register struct mount *mp;
	int error, flag;

	/*
	 * Must be super user
	 */
	if (error = suser(ndp->ni_cred, &u.u_acflag))
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
	if (uap->flags & MNT_UPDATE) {
		if ((vp->v_flag & VROOT) == 0) {
			vput(vp);
			RETURN (EINVAL);
		}
		mp = vp->v_mount;
		/*
		 * We allow going from read-only to read-write,
		 * but not from read-write to read-only.
		 */
		if ((mp->mnt_flag & MNT_RDONLY) == 0 &&
		    (uap->flags & MNT_RDONLY) != 0) {
			vput(vp);
			RETURN (EOPNOTSUPP);	/* Needs translation */
		}
		flag = mp->mnt_flag;
		mp->mnt_flag |= MNT_UPDATE;
		VOP_UNLOCK(vp);
		goto update;
	}
	vinvalbuf(vp, 1);
	if (vp->v_usecount != 1) {
		vput(vp);
		RETURN (EBUSY);
	}
	if (vp->v_type != VDIR) {
		vput(vp);
		RETURN (ENOTDIR);
	}
	if ((unsigned long)uap->type > MOUNT_MAXTYPE ||
	    vfssw[uap->type] == (struct vfsops *)0) {
		vput(vp);
		RETURN (ENODEV);
	}

	/*
	 * Allocate and initialize the file system.
	 */
	mp = (struct mount *)malloc((u_long)sizeof(struct mount),
		M_MOUNT, M_WAITOK);
	mp->mnt_op = vfssw[uap->type];
	mp->mnt_flag = 0;
	mp->mnt_exroot = 0;
	mp->mnt_mounth = NULLVP;
	if (error = vfs_lock(mp)) {
		free((caddr_t)mp, M_MOUNT);
		vput(vp);
		RETURN (error);
	}
	if (vp->v_mountedhere != (struct mount *)0) {
		vfs_unlock(mp);
		free((caddr_t)mp, M_MOUNT);
		vput(vp);
		RETURN (EBUSY);
	}
	vp->v_mountedhere = mp;
	mp->mnt_vnodecovered = vp;
update:
	/*
	 * Set the mount level flags.
	 */
	if (uap->flags & MNT_RDONLY)
		mp->mnt_flag |= MNT_RDONLY;
	else
		mp->mnt_flag &= ~MNT_RDONLY;
	if (uap->flags & MNT_NOSUID)
		mp->mnt_flag |= MNT_NOSUID;
	else
		mp->mnt_flag &= ~MNT_NOSUID;
	if (uap->flags & MNT_NOEXEC)
		mp->mnt_flag |= MNT_NOEXEC;
	else
		mp->mnt_flag &= ~MNT_NOEXEC;
	if (uap->flags & MNT_NODEV)
		mp->mnt_flag |= MNT_NODEV;
	else
		mp->mnt_flag &= ~MNT_NODEV;
	if (uap->flags & MNT_SYNCHRONOUS)
		mp->mnt_flag |= MNT_SYNCHRONOUS;
	else
		mp->mnt_flag &= ~MNT_SYNCHRONOUS;
	/*
	 * Mount the filesystem.
	 */
	error = VFS_MOUNT(mp, uap->dir, uap->data, ndp);
	if (mp->mnt_flag & MNT_UPDATE) {
		mp->mnt_flag &= ~MNT_UPDATE;
		vrele(vp);
		if (error)
			mp->mnt_flag = flag;
		RETURN (error);
	}
	/*
	 * Put the new filesystem on the mount list after root.
	 */
	mp->mnt_next = rootfs->mnt_next;
	mp->mnt_prev = rootfs;
	rootfs->mnt_next = mp;
	mp->mnt_next->mnt_prev = mp;
	cache_purge(vp);
	if (!error) {
		VOP_UNLOCK(vp);
		vfs_unlock(mp);
		error = VFS_START(mp, 0);
	} else {
		vfs_remove(mp);
		free((caddr_t)mp, M_MOUNT);
		vput(vp);
	}
	RETURN (error);
}

/*
 * Unmount system call.
 *
 * Note: unmount takes a path to the vnode mounted on as argument,
 * not special file (as before).
 */
/* ARGSUSED */
unmount(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*pathp;
		int	flags;
	} *uap;
	int *retval;
{
	register struct vnode *vp;
	register struct nameidata *ndp = &u.u_nd;
	struct mount *mp;
	int error;

	/*
	 * Must be super user
	 */
	if (error = suser(ndp->ni_cred, &u.u_acflag))
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
	RETURN (dounmount(mp, uap->flags));
}

/*
 * Do an unmount.
 */
dounmount(mp, flags)
	register struct mount *mp;
	int flags;
{
	struct vnode *coveredvp;
	int error;

	coveredvp = mp->mnt_vnodecovered;
	if (vfs_busy(mp))
		return (EBUSY);
	mp->mnt_flag |= MNT_UNMOUNT;
	if (error = vfs_lock(mp))
		return (error);

	vnode_pager_umount(mp);	/* release cached vnodes */
	cache_purgevfs(mp);	/* remove cache entries for this file sys */
	if ((error = VFS_SYNC(mp, MNT_WAIT)) == 0 || (flags & MNT_FORCE))
		error = VFS_UNMOUNT(mp, flags);
	mp->mnt_flag &= ~MNT_UNMOUNT;
	vfs_unbusy(mp);
	if (error) {
		vfs_unlock(mp);
	} else {
		vrele(coveredvp);
		vfs_remove(mp);
		free((caddr_t)mp, M_MOUNT);
	}
	return (error);
}

/*
 * Sync system call.
 * Sync each mounted filesystem.
 */
/* ARGSUSED */
sync(p, uap, retval)
	register struct proc *p;
	struct args *uap;
	int *retval;
{
	register struct mount *mp;
	struct mount *omp;

	mp = rootfs;
	do {
		/*
		 * The lock check below is to avoid races with mount
		 * and unmount.
		 */
		if ((mp->mnt_flag & (MNT_MLOCK|MNT_RDONLY|MNT_MPBUSY)) == 0 &&
		    !vfs_busy(mp)) {
			VFS_SYNC(mp, MNT_NOWAIT);
			omp = mp;
			mp = mp->mnt_next;
			vfs_unbusy(omp);
		} else
			mp = mp->mnt_next;
	} while (mp != rootfs);
}

/*
 * operate on filesystem quotas
 */
/* ARGSUSED */
quotactl(p, uap, retval)
	register struct proc *p;
	register struct args {
		char *path;
		int cmd;
		int uid;
		caddr_t arg;
	} *uap;
	int *retval;
{
	register struct mount *mp;
	register struct nameidata *ndp = &u.u_nd;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->path;
	if (error = namei(ndp))
		RETURN (error);
	mp = ndp->ni_vp->v_mount;
	vrele(ndp->ni_vp);
	RETURN (VFS_QUOTACTL(mp, uap->cmd, uap->uid, uap->arg));
}

/*
 * get filesystem statistics
 */
/* ARGSUSED */
statfs(p, uap, retval)
	register struct proc *p;
	register struct args {
		char *path;
		struct statfs *buf;
	} *uap;
	int *retval;
{
	register struct mount *mp;
	register struct nameidata *ndp = &u.u_nd;
	register struct statfs *sp;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->path;
	if (error = namei(ndp))
		RETURN (error);
	mp = ndp->ni_vp->v_mount;
	sp = &mp->mnt_stat;
	vrele(ndp->ni_vp);
	if (error = VFS_STATFS(mp, sp))
		RETURN (error);
	sp->f_flags = mp->mnt_flag & MNT_VISFLAGMASK;
	RETURN (copyout((caddr_t)sp, (caddr_t)uap->buf, sizeof(*sp)));
}

/*
 * get filesystem statistics
 */
/* ARGSUSED */
fstatfs(p, uap, retval)
	register struct proc *p;
	register struct args {
		int fd;
		struct statfs *buf;
	} *uap;
	int *retval;
{
	struct file *fp;
	struct mount *mp;
	register struct statfs *sp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	mp = ((struct vnode *)fp->f_data)->v_mount;
	sp = &mp->mnt_stat;
	if (error = VFS_STATFS(mp, sp))
		RETURN (error);
	sp->f_flags = mp->mnt_flag & MNT_VISFLAGMASK;
	RETURN (copyout((caddr_t)sp, (caddr_t)uap->buf, sizeof(*sp)));
}

/*
 * get statistics on all filesystems
 */
getfsstat(p, uap, retval)
	register struct proc *p;
	register struct args {
		struct statfs *buf;
		long bufsize;
		int flags;
	} *uap;
	int *retval;
{
	register struct mount *mp;
	register struct statfs *sp;
	caddr_t sfsp;
	long count, maxcount, error;

	maxcount = uap->bufsize / sizeof(struct statfs);
	sfsp = (caddr_t)uap->buf;
	mp = rootfs;
	count = 0;
	do {
		if (sfsp && count < maxcount &&
		    ((mp->mnt_flag & MNT_MLOCK) == 0)) {
			sp = &mp->mnt_stat;
			/*
			 * If MNT_NOWAIT is specified, do not refresh the
			 * fsstat cache. MNT_WAIT overrides MNT_NOWAIT.
			 */
			if (((uap->flags & MNT_NOWAIT) == 0 ||
			    (uap->flags & MNT_WAIT)) &&
			    (error = VFS_STATFS(mp, sp))) {
				mp = mp->mnt_prev;
				continue;
			}
			sp->f_flags = mp->mnt_flag & MNT_VISFLAGMASK;
			if (error = copyout((caddr_t)sp, sfsp, sizeof(*sp)))
				RETURN (error);
			sfsp += sizeof(*sp);
		}
		count++;
		mp = mp->mnt_prev;
	} while (mp != rootfs);
	if (sfsp && count > maxcount)
		*retval = maxcount;
	else
		*retval = count;
	RETURN (0);
}

/*
 * Change current working directory to a given file descriptor.
 */
/* ARGSUSED */
fchdir(p, uap, retval)
	register struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_type != VDIR)
		error = ENOTDIR;
	else
		error = VOP_ACCESS(vp, VEXEC, ndp->ni_cred);
	VOP_UNLOCK(vp);
	if (error)
		RETURN (error);
	VREF(vp);
	vrele(ndp->ni_cdir);
	ndp->ni_cdir = vp;
	RETURN (0);
}

/*
 * Change current working directory (``.'').
 */
/* ARGSUSED */
chdir(p, uap, retval)
	register struct proc *p;
	struct args {
		char	*fname;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = chdirec(ndp))
		RETURN (error);
	vrele(ndp->ni_cdir);
	ndp->ni_cdir = ndp->ni_vp;
	RETURN (0);
}

/*
 * Change notion of root (``/'') directory.
 */
/* ARGSUSED */
chroot(p, uap, retval)
	register struct proc *p;
	struct args {
		char	*fname;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	int error;

	if (error = suser(ndp->ni_cred, &u.u_acflag))
		RETURN (error);
	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = chdirec(ndp))
		RETURN (error);
	if (ndp->ni_rdir != NULL)
		vrele(ndp->ni_rdir);
	ndp->ni_rdir = ndp->ni_vp;
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
		error = VOP_ACCESS(vp, VEXEC, ndp->ni_cred);
	VOP_UNLOCK(vp);
	if (error)
		vrele(vp);
	return (error);
}

/*
 * Open system call.
 * Check permissions, allocate an open file structure,
 * and call the device open routine if any.
 */
open(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	mode;
		int	crtmode;
	} *uap;
	int *retval;
{
	struct nameidata *ndp = &u.u_nd;
	register struct file *fp;
	int fmode, cmode;
	struct file *nfp;
	int indx, error;
	extern struct fileops vnops;

	if (error = falloc(&nfp, &indx))
		RETURN (error);
	fp = nfp;
	fmode = uap->mode - FOPEN;
	cmode = ((uap->crtmode &~ u.u_cmask) & 07777) &~ S_ISVTX;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	p->p_dupfd = -indx - 1;			/* XXX check for fdopen */
	if (error = vn_open(ndp, fmode, cmode)) {
		crfree(fp->f_cred);
		fp->f_count--;
		if (error == ENODEV &&		/* XXX from fdopen */
		    p->p_dupfd >= 0 &&
		    (error = dupfdopen(indx, p->p_dupfd, fmode)) == 0) {
			*retval = indx;
			RETURN (0);
		}
		if (error == ERESTART)
			error = EINTR;
		u.u_ofile[indx] = NULL;
		RETURN (error);
	}
	fp->f_flag = fmode & FMASK;
	fp->f_type = DTYPE_VNODE;
	fp->f_ops = &vnops;
	fp->f_data = (caddr_t)ndp->ni_vp;
	*retval = indx;
	RETURN (0);
}

#ifdef COMPAT_43
/*
 * Creat system call.
 */
ocreat(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*fname;
		int	fmode;
	} *uap;
	int *retval;
{
	struct args {
		char	*fname;
		int	mode;
		int	crtmode;
	} openuap;

	openuap.fname = uap->fname;
	openuap.crtmode = uap->fmode;
	openuap.mode = O_WRONLY | O_CREAT | O_TRUNC;
	RETURN (open(p, &openuap, retval));
}
#endif /* COMPAT_43 */

/*
 * Mknod system call
 */
/* ARGSUSED */
mknod(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	fmode;
		int	dev;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	if (error = suser(ndp->ni_cred, &u.u_acflag))
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
	VATTR_NULL(&vattr);
	switch (uap->fmode & S_IFMT) {

	case S_IFMT:	/* used by badsect to flag bad sectors */
		vattr.va_type = VBAD;
		break;
	case S_IFCHR:
		vattr.va_type = VCHR;
		break;
	case S_IFBLK:
		vattr.va_type = VBLK;
		break;
	default:
		error = EINVAL;
		goto out;
	}
	vattr.va_mode = (uap->fmode & 07777) &~ u.u_cmask;
	vattr.va_rdev = uap->dev;
out:
	if (!error) {
		error = VOP_MKNOD(ndp, &vattr, ndp->ni_cred);
	} else {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		if (vp)
			vrele(vp);
	}
	RETURN (error);
}

/*
 * Mkfifo system call
 */
/* ARGSUSED */
mkfifo(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	fmode;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	struct vattr vattr;
	int error;

#ifndef FIFO
	RETURN (EOPNOTSUPP);
#else
	ndp->ni_nameiop = CREATE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	if (ndp->ni_vp != NULL) {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == ndp->ni_vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		vrele(ndp->ni_vp);
		RETURN (EEXIST);
	}
	VATTR_NULL(&vattr);
	vattr.va_type = VFIFO;
	vattr.va_mode = (uap->fmode & 07777) &~ u.u_cmask;
	RETURN (VOP_MKNOD(ndp, &vattr, ndp->ni_cred));
#endif /* FIFO */
}

/*
 * link system call
 */
/* ARGSUSED */
link(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*target;
		char	*linkname;
	} *uap;
	int *retval;
{
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
	    (error = suser(ndp->ni_cred, &u.u_acflag)))
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
	if (!error) {
		error = VOP_LINK(vp, ndp);
	} else {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == ndp->ni_vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		if (ndp->ni_vp)
			vrele(ndp->ni_vp);
	}
out1:
	vrele(vp);
	RETURN (error);
}

/*
 * symlink -- make a symbolic link
 */
/* ARGSUSED */
symlink(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*target;
		char	*linkname;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	struct vattr vattr;
	char *target;
	int error;

	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->linkname;
	MALLOC(target, char *, MAXPATHLEN, M_NAMEI, M_WAITOK);
	if (error = copyinstr(uap->target, target, MAXPATHLEN, (u_int *)0))
		goto out;
	ndp->ni_nameiop = CREATE | LOCKPARENT;
	if (error = namei(ndp))
		goto out;
	if (ndp->ni_vp) {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == ndp->ni_vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		vrele(ndp->ni_vp);
		error = EEXIST;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_mode = 0777 &~ u.u_cmask;
	error = VOP_SYMLINK(ndp, &vattr, target);
out:
	FREE(target, M_NAMEI);
	RETURN (error);
}

/*
 * Unlink system call.
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
/* ARGSUSED */
unlink(p, uap, retval)
	register struct proc *p;
	struct args {
		char	*fname;
	} *uap;
	int *retval;
{
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
	    (error = suser(ndp->ni_cred, &u.u_acflag)))
		goto out;
	/*
	 * Don't unlink a mounted file.
	 */
	if (vp->v_flag & VROOT) {
		error = EBUSY;
		goto out;
	}
	(void) vnode_pager_uncache(vp);
out:
	if (!error) {
		error = VOP_REMOVE(ndp);
	} else {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		vput(vp);
	}
	RETURN (error);
}

/*
 * Seek system call
 */
lseek(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fdes;
		off_t	off;
		int	sbase;
	} *uap;
	off_t *retval;
{
	struct ucred *cred = u.u_nd.ni_cred;
	register struct file *fp;
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
		    &vattr, cred))
			RETURN (error);
		fp->f_offset = uap->off + vattr.va_size;
		break;

	case L_SET:
		fp->f_offset = uap->off;
		break;

	default:
		RETURN (EINVAL);
	}
	*retval = fp->f_offset;
	RETURN (0);
}

/*
 * Access system call
 */
/* ARGSUSED */
saccess(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	fmode;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct ucred *cred = ndp->ni_cred;
	register struct vnode *vp;
	int error, mode, svuid, svgid;

	svuid = cred->cr_uid;
	svgid = cred->cr_groups[0];
	cred->cr_uid = p->p_ruid;
	cred->cr_groups[0] = p->p_rgid;
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
		if ((mode & VWRITE) == 0 || (error = vn_writechk(vp)) == 0)
			error = VOP_ACCESS(vp, mode, ndp->ni_cred);
	}
	vput(vp);
out1:
	cred->cr_uid = svuid;
	cred->cr_groups[0] = svgid;
	RETURN (error);
}

/*
 * Stat system call.  This version follows links.
 */
/* ARGSUSED */
stat(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		struct stat *ub;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	struct stat sb;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
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
 * Lstat system call.  This version does not follow links.
 */
/* ARGSUSED */
lstat(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		struct stat *ub;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	struct stat sb;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | NOFOLLOW;
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
/* ARGSUSED */
readlink(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*name;
		char	*buf;
		int	count;
	} *uap;
	int *retval;
{
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
	*retval = uap->count - auio.uio_resid;
	RETURN (error);
}

/*
 * Change flags of a file given path name.
 */
/* ARGSUSED */
chflags(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	flags;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_flags = uap->flags;
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Change flags of a file given a file descriptor.
 */
/* ARGSUSED */
fchflags(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fd;
		int	flags;
	} *uap;
	int *retval;
{
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_flags = uap->flags;
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Change mode of a file given path name.
 */
/* ARGSUSED */
chmod(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	fmode;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Change mode of a file given a file descriptor.
 */
/* ARGSUSED */
fchmod(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fd;
		int	fmode;
	} *uap;
	int *retval;
{
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_mode = uap->fmode & 07777;
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Set ownership given a path name.
 */
/* ARGSUSED */
chown(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		int	uid;
		int	gid;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | NOFOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Set ownership given a file descriptor.
 */
/* ARGSUSED */
fchown(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fd;
		int	uid;
		int	gid;
	} *uap;
	int *retval;
{
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_uid = uap->uid;
	vattr.va_gid = uap->gid;
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Set the access and modification times of a file.
 */
/* ARGSUSED */
utimes(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		struct	timeval *tptr;
	} *uap;
	int *retval;
{
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
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		error = EROFS;
		goto out;
	}
	VATTR_NULL(&vattr);
	vattr.va_atime = tv[0];
	vattr.va_mtime = tv[1];
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Truncate a file given its path name.
 */
/* ARGSUSED */
truncate(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		off_t	length;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW | LOCKLEAF;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type == VDIR) {
		error = EISDIR;
		goto out;
	}
	if ((error = vn_writechk(vp)) ||
	    (error = VOP_ACCESS(vp, VWRITE, ndp->ni_cred)))
		goto out;
	VATTR_NULL(&vattr);
	vattr.va_size = uap->length;
	error = VOP_SETATTR(vp, &vattr, ndp->ni_cred);
out:
	vput(vp);
	RETURN (error);
}

/*
 * Truncate a file given a file descriptor.
 */
/* ARGSUSED */
ftruncate(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fd;
		off_t	length;
	} *uap;
	int *retval;
{
	struct vattr vattr;
	struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	if ((fp->f_flag & FWRITE) == 0)
		RETURN (EINVAL);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	if (vp->v_type == VDIR) {
		error = EISDIR;
		goto out;
	}
	if (error = vn_writechk(vp))
		goto out;
	VATTR_NULL(&vattr);
	vattr.va_size = uap->length;
	error = VOP_SETATTR(vp, &vattr, fp->f_cred);
out:
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Synch an open file.
 */
/* ARGSUSED */
fsync(p, uap, retval)
	register struct proc *p;
	struct args {
		int	fd;
	} *uap;
	int *retval;
{
	register struct vnode *vp;
	struct file *fp;
	int error;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	vp = (struct vnode *)fp->f_data;
	VOP_LOCK(vp);
	error = VOP_FSYNC(vp, fp->f_flag, fp->f_cred, MNT_WAIT);
	VOP_UNLOCK(vp);
	RETURN (error);
}

/*
 * Rename system call.
 *
 * Source and destination must either both be directories, or both
 * not be directories.  If target is a directory, it must be empty.
 */
/* ARGSUSED */
rename(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*from;
		char	*to;
	} *uap;
	int *retval;
{
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
	nddup(ndp, &tond);
	tond.ni_nameiop = RENAME | LOCKPARENT | LOCKLEAF | NOCACHE;
	tond.ni_segflg = UIO_USERSPACE;
	tond.ni_dirp = uap->to;
	if (error = namei(&tond)) {
		VOP_ABORTOP(ndp);
		vrele(ndp->ni_dvp);
		vrele(fvp);
		goto out1;
	}
	tdvp = tond.ni_dvp;
	tvp = tond.ni_vp;
	if (tvp != NULL) {
		if (fvp->v_type == VDIR && tvp->v_type != VDIR) {
			error = ENOTDIR;
			goto out;
		} else if (fvp->v_type != VDIR && tvp->v_type == VDIR) {
			error = EISDIR;
			goto out;
		}
		if (fvp->v_mount != tvp->v_mount) {
			error = EXDEV;
			goto out;
		}
	}
	if (fvp->v_mount != tdvp->v_mount) {
		error = EXDEV;
		goto out;
	}
	if (fvp == tdvp)
		error = EINVAL;
	/*
	 * If source is the same as the destination,
	 * then there is nothing to do.
	 */
	if (fvp == tvp)
		error = -1;
out:
	if (!error) {
		error = VOP_RENAME(ndp, &tond);
	} else {
		VOP_ABORTOP(&tond);
		if (tdvp == tvp)
			vrele(tdvp);
		else
			vput(tdvp);
		if (tvp)
			vput(tvp);
		VOP_ABORTOP(ndp);
		vrele(ndp->ni_dvp);
		vrele(fvp);
	}
out1:
	ndrele(&tond);
	if (error == -1)
		RETURN (0);
	RETURN (error);
}

/*
 * Mkdir system call
 */
/* ARGSUSED */
mkdir(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*name;
		int	dmode;
	} *uap;
	int *retval;
{
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
		if (ndp->ni_dvp == vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		vrele(vp);
		RETURN (EEXIST);
	}
	VATTR_NULL(&vattr);
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
/* ARGSUSED */
rmdir(p, uap, retval)
	register struct proc *p;
	struct args {
		char	*name;
	} *uap;
	int *retval;
{
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
	if (!error) {
		error = VOP_RMDIR(ndp);
	} else {
		VOP_ABORTOP(ndp);
		if (ndp->ni_dvp == vp)
			vrele(ndp->ni_dvp);
		else
			vput(ndp->ni_dvp);
		vput(vp);
	}
	RETURN (error);
}

/*
 * Read a block of directory entries in a file system independent format
 */
getdirentries(p, uap, retval)
	register struct proc *p;
	register struct args {
		int	fd;
		char	*buf;
		unsigned count;
		long	*basep;
	} *uap;
	int *retval;
{
	register struct vnode *vp;
	struct file *fp;
	struct uio auio;
	struct iovec aiov;
	off_t off;
	int error, eofflag;

	if (error = getvnode(u.u_ofile, uap->fd, &fp))
		RETURN (error);
	if ((fp->f_flag & FREAD) == 0)
		RETURN (EBADF);
	vp = (struct vnode *)fp->f_data;
	if (vp->v_type != VDIR)
		RETURN (EINVAL);
	aiov.iov_base = uap->buf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_rw = UIO_READ;
	auio.uio_segflg = UIO_USERSPACE;
	auio.uio_resid = uap->count;
	VOP_LOCK(vp);
	auio.uio_offset = off = fp->f_offset;
	error = VOP_READDIR(vp, &auio, fp->f_cred, &eofflag);
	fp->f_offset = auio.uio_offset;
	VOP_UNLOCK(vp);
	if (error)
		RETURN (error);
	error = copyout((caddr_t)&off, (caddr_t)uap->basep, sizeof(long));
	*retval = uap->count - auio.uio_resid;
	RETURN (error);
}

/*
 * mode mask for creation of files
 */
mode_t
umask(p, uap, retval)
	register struct proc *p;
	struct args {
		int	mask;
	} *uap;
	int *retval;
{

	*retval = u.u_cmask;
	u.u_cmask = uap->mask & 07777;
	RETURN (0);
}

/*
 * Void all references to file by ripping underlying filesystem
 * away from vnode.
 */
/* ARGSUSED */
revoke(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp = &u.u_nd;
	register struct vnode *vp;
	struct vattr vattr;
	int error;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (error = namei(ndp))
		RETURN (error);
	vp = ndp->ni_vp;
	if (vp->v_type != VCHR && vp->v_type != VBLK) {
		error = EINVAL;
		goto out;
	}
	if (error = VOP_GETATTR(vp, &vattr, ndp->ni_cred))
		goto out;
	if (ndp->ni_cred->cr_uid != vattr.va_uid &&
	    (error = suser(ndp->ni_cred, &u.u_acflag)))
		goto out;
	if (vp->v_usecount > 1 || (vp->v_flag & VALIASED))
		vgoneall(vp);
out:
	vrele(vp);
	RETURN (error);
}

getvnode(ofile, fdes, fpp)
	struct file *ofile[];
	struct file **fpp;
	int fdes;
{
	struct file *fp;

	if ((unsigned)fdes >= NOFILE || (fp = ofile[fdes]) == NULL)
		return (EBADF);
	if (fp->f_type != DTYPE_VNODE)
		return (EINVAL);
	*fpp = fp;
	return (0);
}
