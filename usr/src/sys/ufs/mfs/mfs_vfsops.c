/*
 * Copyright (c) 1989, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfs_vfsops.c	7.27 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/mount.h>
#include <sys/signalvar.h>
#include <sys/vnode.h>
#include <sys/malloc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

#include <ufs/ffs/fs.h>
#include <ufs/ffs/ffs_extern.h>

#include <ufs/mfs/mfsnode.h>
#include <ufs/mfs/mfs_extern.h>

caddr_t	mfs_rootbase;	/* address of mini-root in kernel virtual memory */
u_long	mfs_rootsize;	/* size of mini-root in bytes */

static	int mfs_minor;	/* used for building internal dev_t */

extern int (**mfs_vnodeop_p)();

/*
 * mfs vfs operations.
 */
struct vfsops mfs_vfsops = {
	mfs_mount,
	mfs_start,
	ffs_unmount,
	ffs_root,
	ufs_quotactl,
	mfs_statfs,
	ffs_sync,
	ffs_vget,
	ffs_fhtovp,
	ffs_vptofh,
	mfs_init,
};

/*
 * Called by main() when mfs is going to be mounted as root.
 *
 * Name is updated by mount(8) after booting.
 */
#define ROOTNAME	"mfs_root"

mfs_mountroot()
{
	extern struct vnode *rootvp;
	register struct fs *fs;
	register struct mount *mp;
	struct proc *p = curproc;	/* XXX */
	struct ufsmount *ump;
	struct mfsnode *mfsp;
	u_int size;
	int error;

	/*
	 * Get vnodes for swapdev and rootdev.
	 */
	if (bdevvp(swapdev, &swapdev_vp) || bdevvp(rootdev, &rootvp))
		panic("mfs_mountroot: can't setup bdevvp's");

	mp = malloc((u_long)sizeof(struct mount), M_MOUNT, M_WAITOK);
	mp->mnt_op = &mfs_vfsops;
	mp->mnt_flag = MNT_RDONLY;
	mp->mnt_mounth = NULLVP;
	mfsp = malloc(sizeof *mfsp, M_MFSNODE, M_WAITOK);
	rootvp->v_data = mfsp;
	rootvp->v_op = mfs_vnodeop_p;
	rootvp->v_tag = VT_MFS;
	mfsp->mfs_baseoff = mfs_rootbase;
	mfsp->mfs_size = mfs_rootsize;
	mfsp->mfs_vnode = rootvp;
	mfsp->mfs_pid = p->p_pid;
	mfsp->mfs_buflist = (struct buf *)0;
	if (error = ffs_mountfs(rootvp, mp, p)) {
		free(mp, M_MOUNT);
		free(mfsp, M_MFSNODE);
		return (error);
	}
	if (error = vfs_lock(mp)) {
		(void)ffs_unmount(mp, 0, p);
		free(mp, M_MOUNT);
		free(mfsp, M_MFSNODE);
		return (error);
	}
	rootfs = mp;
	mp->mnt_next = mp;
	mp->mnt_prev = mp;
	mp->mnt_vnodecovered = NULLVP;
	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	bzero(fs->fs_fsmnt, sizeof(fs->fs_fsmnt));
	fs->fs_fsmnt[0] = '/';
	bcopy((caddr_t)fs->fs_fsmnt, (caddr_t)mp->mnt_stat.f_mntonname,
	    MNAMELEN);
	(void) copystr(ROOTNAME, mp->mnt_stat.f_mntfromname, MNAMELEN - 1,
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
	(void)ffs_statfs(mp, &mp->mnt_stat, p);
	vfs_unlock(mp);
	inittodr((time_t)0);
	return (0);
}

/*
 * This is called early in boot to set the base address and size
 * of the mini-root.
 */
mfs_initminiroot(base)
	caddr_t base;
{
	struct fs *fs = (struct fs *)(base + SBOFF);
	extern int (*mountroot)();

	/* check for valid super block */
	if (fs->fs_magic != FS_MAGIC || fs->fs_bsize > MAXBSIZE ||
	    fs->fs_bsize < sizeof(struct fs))
		return (0);
	mountroot = mfs_mountroot;
	mfs_rootbase = base;
	mfs_rootsize = fs->fs_fsize * fs->fs_size;
	rootdev = makedev(255, mfs_minor++);
	return (mfs_rootsize);
}

/*
 * VFS Operations.
 *
 * mount system call
 */
/* ARGSUSED */
int
mfs_mount(mp, path, data, ndp, p)
	register struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	struct vnode *devvp;
	struct mfs_args args;
	struct ufsmount *ump;
	register struct fs *fs;
	register struct mfsnode *mfsp;
	u_int size;
	int error;

	if (mp->mnt_flag & MNT_UPDATE) {
		ump = VFSTOUFS(mp);
		fs = ump->um_fs;
		if (fs->fs_ronly && (mp->mnt_flag & MNT_RDONLY) == 0)
			fs->fs_ronly = 0;
		return (0);
	}
	if (error = copyin(data, (caddr_t)&args, sizeof (struct mfs_args)))
		return (error);
	error = getnewvnode(VT_MFS, (struct mount *)0, mfs_vnodeop_p, &devvp);
	if (error)
		return (error);
	devvp->v_type = VBLK;
	if (checkalias(devvp, makedev(255, mfs_minor++), (struct mount *)0))
		panic("mfs_mount: dup dev");
	mfsp = (struct mfsnode *)malloc(sizeof *mfsp, M_MFSNODE, M_WAITOK);
	devvp->v_data = mfsp;
	mfsp->mfs_baseoff = args.base;
	mfsp->mfs_size = args.size;
	mfsp->mfs_vnode = devvp;
	mfsp->mfs_pid = p->p_pid;
	mfsp->mfs_buflist = (struct buf *)0;
	if (error = ffs_mountfs(devvp, mp, p)) {
		mfsp->mfs_buflist = (struct buf *)-1;
		vrele(devvp);
		return (error);
	}
	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	(void) copyinstr(path, fs->fs_fsmnt, sizeof(fs->fs_fsmnt) - 1, &size);
	bzero(fs->fs_fsmnt + size, sizeof(fs->fs_fsmnt) - size);
	bcopy((caddr_t)fs->fs_fsmnt, (caddr_t)mp->mnt_stat.f_mntonname,
		MNAMELEN);
	(void) copyinstr(args.name, mp->mnt_stat.f_mntfromname, MNAMELEN - 1,
		&size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
	(void) mfs_statfs(mp, &mp->mnt_stat, p);
	return (0);
}

int	mfs_pri = PWAIT | PCATCH;		/* XXX prob. temp */

/*
 * Used to grab the process and keep it in the kernel to service
 * memory filesystem I/O requests.
 *
 * Loop servicing I/O requests.
 * Copy the requested data into or out of the memory filesystem
 * address space.
 */
/* ARGSUSED */
int
mfs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{
	register struct vnode *vp = VFSTOUFS(mp)->um_devvp;
	register struct mfsnode *mfsp = VTOMFS(vp);
	register struct buf *bp;
	register caddr_t base;
	int error = 0;

	base = mfsp->mfs_baseoff;
	while (mfsp->mfs_buflist != (struct buf *)(-1)) {
		while (bp = mfsp->mfs_buflist) {
			mfsp->mfs_buflist = bp->av_forw;
			mfs_doio(bp, base);
			wakeup((caddr_t)bp);
		}
		/*
		 * If a non-ignored signal is received, try to unmount.
		 * If that fails, clear the signal (it has been "processed"),
		 * otherwise we will loop here, as tsleep will always return
		 * EINTR/ERESTART.
		 */
		if (error = tsleep((caddr_t)vp, mfs_pri, "mfsidl", 0))
			if (dounmount(mp, MNT_NOFORCE, p) != 0)
				CLRSIG(p, CURSIG(p));
	}
	return (error);
}

/*
 * Get file system statistics.
 */
mfs_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	int error;

	error = ffs_statfs(mp, sbp, p);
	sbp->f_type = MOUNT_MFS;
	return (error);
}
