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
 *	@(#)mfs_vfsops.c	7.4 (Berkeley) %G%
 */

#include "param.h"
#include "time.h"
#include "user.h"
#include "proc.h"
#include "buf.h"
#include "mount.h"
#include "vnode.h"
#include "../ufs/ufsmount.h"
#include "../ufs/inode.h"
#include "../ufs/fs.h"

extern struct vnodeops mfs_vnodeops;

/*
 * mfs vfs operations.
 */
int mfs_mount();
int mfs_start();
int ufs_unmount();
int ufs_root();
int ufs_statfs();
int ufs_sync();
int ufs_fhtovp();
int ufs_vptofh();

struct vfsops mfs_vfsops = {
	mfs_mount,
	mfs_start,
	ufs_unmount,
	ufs_root,
	ufs_statfs,
	ufs_sync,
	ufs_fhtovp,
	ufs_vptofh,
};

/*
 * VFS Operations.
 *
 * mount system call
 */
mfs_mount(mp, path, data, ndp)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
{
	struct vnode *devvp;
	struct mfs_args args;
	struct ufsmount *ump;
	register struct fs *fs;
	static int mfs_minor;
	u_int size;
	int error;

	if (mp->m_flag & M_UPDATE) {
		ump = VFSTOUFS(mp);
		fs = ump->um_fs;
		if (fs->fs_ronly && (mp->m_flag & M_RDONLY) == 0)
			fs->fs_ronly = 0;
		return (0);
	}
	if (error = copyin(data, (caddr_t)&args, sizeof (struct mfs_args)))
		return (error);
	if ((error = bdevvp(NODEV, &devvp)) != 0)
		return (error);
	devvp->v_op = &mfs_vnodeops;
	devvp->v_rdev = makedev(255, mfs_minor++);
	VTOI(devvp)->i_diroff = (long)args.base;
	VTOI(devvp)->i_endoff = args.size;
	error = mountfs(devvp, mp);
	if (error) {
		vrele(devvp);
		return (error);
	}
	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	(void) copyinstr(path, fs->fs_fsmnt, sizeof(fs->fs_fsmnt) - 1, &size);
	bzero(fs->fs_fsmnt + size, sizeof(fs->fs_fsmnt) - size);
	(void) copyinstr(args.name, ump->um_mntname, MNAMELEN - 1, &size);
	bzero(ump->um_mntname + size, MNAMELEN - size);
	return (0);
}

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
	register struct inode *ip = VTOI(vp);
	register struct buf *bp;
	register caddr_t base;

	base = (caddr_t)ip->i_diroff;
	if (setjmp(&u.u_qsave)) {
		/*
		 * We have received a signal, so try to unmount.
		 */
		(void) dounmount(mp, MNT_NOFORCE);
	} else {
		sleep((caddr_t)vp, PWAIT);
	}
	while (ip->i_spare[0] != -1) {
		while (bp = (struct buf *)ip->i_spare[0]) {
			ip->i_spare[0] = (long)bp->av_forw;
			mfs_doio(bp, base);
			wakeup((caddr_t)bp);
		}
		sleep((caddr_t)vp, PWAIT);
	}
	return (0);
}
