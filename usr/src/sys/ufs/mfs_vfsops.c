/*
 * Copyright (c) 1989, 1990 The Regents of the University of California.
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
 *	@(#)mfs_vfsops.c	7.17 (Berkeley) 6/28/90
 */

#include "param.h"
#include "time.h"
#include "kernel.h"
#include "user.h"
#include "proc.h"
#include "buf.h"
#include "mount.h"
#include "vnode.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/ufsmount.h"
#include "../ufs/mfsnode.h"
#include "../ufs/fs.h"

extern struct vnodeops mfs_vnodeops;

/*
 * mfs vfs operations.
 */
int mfs_mount();
int mfs_start();
int ufs_unmount();
int ufs_root();
int ufs_quotactl();
int mfs_statfs();
int ufs_sync();
int ufs_fhtovp();
int ufs_vptofh();
int mfs_init();

struct vfsops mfs_vfsops = {
	mfs_mount,
	mfs_start,
	ufs_unmount,
	ufs_root,
	ufs_quotactl,
	mfs_statfs,
	ufs_sync,
	ufs_fhtovp,
	ufs_vptofh,
	mfs_init,
};

/*
 * VFS Operations.
 *
 * mount system call
 */
/* ARGSUSED */
mfs_mount(mp, path, data, ndp)
	register struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
{
	struct vnode *devvp;
	struct mfs_args args;
	struct ufsmount *ump;
	register struct fs *fs;
	register struct mfsnode *mfsp;
	static int mfs_minor;
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
	error = getnewvnode(VT_MFS, (struct mount *)0, &mfs_vnodeops, &devvp);
	if (error)
		return (error);
	devvp->v_type = VBLK;
	if (checkalias(devvp, makedev(255, mfs_minor++), (struct mount *)0))
		panic("mfs_mount: dup dev");
	mfsp = VTOMFS(devvp);
	mfsp->mfs_baseoff = args.base;
	mfsp->mfs_size = args.size;
	mfsp->mfs_vnode = devvp;
	mfsp->mfs_pid = u.u_procp->p_pid;
	mfsp->mfs_buflist = (struct buf *)0;
	if (error = mountfs(devvp, mp)) {
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
	(void) mfs_statfs(mp, &mp->mnt_stat);
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
mfs_start(mp, flags)
	struct mount *mp;
	int flags;
{
	register struct vnode *vp = VFSTOUFS(mp)->um_devvp;
	register struct mfsnode *mfsp = VTOMFS(vp);
	register struct buf *bp;
	register caddr_t base;
	struct proc *p = u.u_procp;
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
			if (dounmount(mp, MNT_NOFORCE) != 0)
				CLRSIG(p, CURSIG(p));
	}
	return (error);
}

/*
 * Get file system statistics.
 */
mfs_statfs(mp, sbp)
	struct mount *mp;
	struct statfs *sbp;
{
	int error;

	error = ufs_statfs(mp, sbp);
	sbp->f_type = MOUNT_MFS;
	return (error);
}
