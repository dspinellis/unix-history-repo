/*
 * Copyright (c) 1993 Jan-Simon Pendry
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)procfs_vfsops.c	8.6 (Berkeley) %G%
 *
 * From:
 *	$Id: procfs_vfsops.c,v 3.1 1993/12/15 09:40:17 jsp Exp $
 */

/*
 * procfs VFS interface
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/syslog.h>
#include <sys/mount.h>
#include <sys/signalvar.h>
#include <sys/vnode.h>
#include <miscfs/procfs/procfs.h>
#include <vm/vm.h>			/* for PAGE_SIZE */

/*
 * VFS Operations.
 *
 * mount system call
 */
/* ARGSUSED */
procfs_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	u_int size;

	if (UIO_MX & (UIO_MX-1)) {
		log(LOG_ERR, "procfs: invalid directory entry size");
		return (EINVAL);
	}

	if (mp->mnt_flag & MNT_UPDATE)
		return (EOPNOTSUPP);

	mp->mnt_flag |= MNT_LOCAL;
	mp->mnt_data = 0;
	vfs_getnewfsid(mp);

	(void) copyinstr(path, (caddr_t)mp->mnt_stat.f_mntonname, MNAMELEN, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);

	size = sizeof("procfs") - 1;
	bcopy("procfs", mp->mnt_stat.f_mntfromname, size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);

	return (0);
}

/*
 * unmount system call
 */
procfs_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	int error;
	extern int doforce;
	int flags = 0;

	if (mntflags & MNT_FORCE) {
		/* procfs can never be rootfs so don't check for it */
		if (!doforce)
			return (EINVAL);
		flags |= FORCECLOSE;
	}

	if (error = vflush(mp, 0, flags))
		return (error);

	return (0);
}

procfs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{

	return (procfs_allocvp(mp, vpp, 0, Proot));
}

/* ARGSUSED */
procfs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

/*
 * Get file system statistics.
 */
procfs_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	sbp->f_bsize = PAGE_SIZE;
	sbp->f_iosize = PAGE_SIZE;
	sbp->f_blocks = 1;	/* avoid divide by zero in some df's */
	sbp->f_bfree = 0;
	sbp->f_bavail = 0;
	sbp->f_files = maxproc;			/* approx */
	sbp->f_ffree = maxproc - nprocs;	/* approx */

	if (sbp != &mp->mnt_stat) {
		sbp->f_type = mp->mnt_vfc->vfc_typenum;
		bcopy(&mp->mnt_stat.f_fsid, &sbp->f_fsid, sizeof(sbp->f_fsid));
		bcopy(mp->mnt_stat.f_mntonname, sbp->f_mntonname, MNAMELEN);
		bcopy(mp->mnt_stat.f_mntfromname, sbp->f_mntfromname, MNAMELEN);
	}

	return (0);
}

procfs_init(vfsp)
	struct vfsconf *vfsp;
{

	return (0);
}

#define procfs_fhtovp ((int (*) __P((struct mount *, struct fid *, \
	    struct mbuf *, struct vnode **, int *, struct ucred **)))einval)
#define procfs_quotactl ((int (*) __P((struct mount *, int, uid_t, caddr_t, \
	    struct proc *)))eopnotsupp)
#define procfs_sync ((int (*) __P((struct mount *, int, struct ucred *, \
	    struct proc *)))nullop)
#define procfs_sysctl ((int (*) __P((int *, u_int, void *, size_t *, void *, \
	    size_t, struct proc *)))eopnotsupp)
#define procfs_vget ((int (*) __P((struct mount *, ino_t, struct vnode **))) \
	    eopnotsupp)
#define procfs_vptofh ((int (*) __P((struct vnode *, struct fid *)))einval)

struct vfsops procfs_vfsops = {
	procfs_mount,
	procfs_start,
	procfs_unmount,
	procfs_root,
	procfs_quotactl,
	procfs_statfs,
	procfs_sync,
	procfs_vget,
	procfs_fhtovp,
	procfs_vptofh,
	procfs_init,
	procfs_sysctl,
};
