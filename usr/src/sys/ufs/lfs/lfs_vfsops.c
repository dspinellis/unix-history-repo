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
 *	@(#)lfs_vfsops.c	7.15 (Berkeley) %G%
 */


#include "param.h"
#include "systm.h"
#include "time.h"
#include "kernel.h"
#include "namei.h"
#include "vnode.h"
#include "mount.h"
#include "buf.h"
#include "file.h"
#include "disklabel.h"
#include "ioctl.h"
#include "errno.h"
#include "malloc.h"
#include "../ufs/fs.h"
#include "../ufs/ufsmount.h"
#include "../ufs/inode.h"
#include "ioctl.h"
#include "disklabel.h"
#include "stat.h"

/*
 * ufs vfs operations.
 */
int ufs_mount();
int ufs_unmount();
int ufs_root();
int ufs_statfs();
int ufs_sync();
int ufs_fhtovp();
int ufs_vptofh();

struct vfsops ufs_vfsops = {
	ufs_mount,
	ufs_unmount,
	ufs_root,
	ufs_statfs,
	ufs_sync,
	ufs_fhtovp,
	ufs_vptofh
};

/*
 * ufs mount table.
 */
struct ufsmount mounttab[NMOUNT];

/*
 * Called by vfs_mountroot when ufs is going to be mounted as root
 *
 * XXX - Need to have a way of figuring the name of the root device
 */
#define ROOTNAME	"root device"

ufs_mountroot()
{
	register struct mount *mp;
	extern struct vnode *rootvp;
	struct ufsmount *ump;
	register struct fs *fs;
	u_int size;
	int error;

	mp = (struct mount *)malloc((u_long)sizeof(struct mount),
		M_MOUNT, M_WAITOK);
	mp->m_op = &ufs_vfsops;
	mp->m_flag = 0;
	mp->m_exroot = 0;
	error = mountfs(rootvp, mp);
	if (error) {
		free((caddr_t)mp, M_MOUNT);
		return (error);
	}
	error = vfs_add((struct vnode *)0, mp, 0);
	if (error) {
		(void)ufs_unmount(mp, 0);
		free((caddr_t)mp, M_MOUNT);
		return (error);
	}
	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	fs->fs_fsmnt[0] = '/';
	bzero(fs->fs_fsmnt + 1, sizeof(fs->fs_fsmnt) - 1);
	(void) copystr(ROOTNAME, ump->um_mntname, MNAMELEN - 1, &size);
	bzero(ump->um_mntname + size, MNAMELEN - size);
	vfs_unlock(mp);
	inittodr(fs->fs_time);
	return (0);
}

/*
 * VFS Operations.
 *
 * mount system call
 */
ufs_mount(mp, path, data, ndp)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
{
	struct vnode *devvp;
	struct ufs_args args;
	struct ufsmount *ump;
	register struct fs *fs;
	u_int size;
	int error;

	if (error = copyin(data, (caddr_t)&args, sizeof (struct ufs_args)))
		return (error);
	if ((error = getmdev(&devvp, args.fspec, ndp)) != 0)
		return (error);
	error = mountfs(devvp, mp);
	if (error) {
		vrele(devvp);
		return (error);
	}
	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	(void) copyinstr(path, fs->fs_fsmnt, sizeof(fs->fs_fsmnt) - 1, &size);
	bzero(fs->fs_fsmnt + size, sizeof(fs->fs_fsmnt) - size);
	(void) copyinstr(args.fspec, ump->um_mntname, MNAMELEN - 1, &size);
	bzero(ump->um_mntname + size, MNAMELEN - size);
	return (0);
}

/*
 * Common code for mount and mountroot
 */
mountfs(devvp, mp)
	struct vnode *devvp;
	struct mount *mp;
{
	register struct ufsmount *ump;
	struct ufsmount *fmp = NULL;
	struct buf *bp = NULL;
	register struct fs *fs;
	dev_t dev = devvp->v_rdev;
	struct partinfo dpart;
	int havepart = 0, blks;
	caddr_t base, space;
	int havepart = 0, blks;
	int error, i, size;
	int needclose = 0;
	int ronly = (mp->m_flag & M_RDONLY) != 0;

	for (ump = &mounttab[0]; ump < &mounttab[NMOUNT]; ump++) {
		if (ump->um_fs == NULL) {
			if (fmp == NULL)
				fmp = ump;
		} else if (dev == ump->um_dev) {
			return (EBUSY);		/* needs translation */
		}
	}
	    (*bdevsw[major(dev)].d_open)(dev, ronly ? FREAD : FREAD|FWRITE,
	        S_IFBLK);
	if (error) {
		ump->um_fs = NULL;
		return (error);
	}
	needclose = 1;
	if (VOP_IOCTL(devvp, DIOCGPART, (caddr_t)&dpart, FREAD,
	    (struct ucred *)0) != 0)
		size = DEV_BSIZE;
	else {
		havepart = 1;
		size = dpart.disklab->d_secsize;
	}
	if (error = bread(devvp, SBLOCK, SBSIZE, &bp)) {
		ump->um_fs = NULL;
		goto out;
	}
	fs = bp->b_un.b_fs;
		ump->um_fs = NULL;
		error = EINVAL;		/* XXX also needs translation */
		goto out;
	}
	ump->um_fs = (struct fs *)malloc((u_long)fs->fs_sbsize, M_SUPERBLK,
	    M_WAITOK);
	bcopy((caddr_t)bp->b_un.b_addr, (caddr_t)ump->um_fs,
	   (u_int)fs->fs_sbsize);
	brelse(bp);
	bp = NULL;
	fs = ump->um_fs;
	fs->fs_ronly = ronly;
	if (ronly == 0)
		fs->fs_fmod = 1;
	if (havepart) {
		dpart.part->p_fstype = FS_BSDFFS;
		dpart.part->p_fsize = fs->fs_fsize;
		dpart.part->p_frag = fs->fs_frag;
		dpart.part->p_cpg = fs->fs_cpg;
	}
#ifdef SECSIZE
	/*
	 * If we have a disk label, force per-partition
	 * filesystem information to be correct
	 * and set correct current fsbtodb shift.
	 */
#endif SECSIZE
	if (havepart) {
		dpart.part->p_fstype = FS_BSDFFS;
		dpart.part->p_fsize = fs->fs_fsize;
		dpart.part->p_frag = fs->fs_frag;
#ifdef SECSIZE
#ifdef tahoe
		/*
		 * Save the original fsbtodb shift to restore on updates.
		 * (Console doesn't understand fsbtodb changes.)
		 */
		fs->fs_sparecon[0] = fs->fs_fsbtodb;
#endif
		i = fs->fs_fsize / size;
		for (fs->fs_fsbtodb = 0; i > 1; i >>= 1)
			fs->fs_fsbtodb++;
#endif SECSIZE
		fs->fs_dbsize = size;
	}
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	base = space = (caddr_t)malloc((u_long)fs->fs_cssize, M_SUPERBLK,
	    M_WAITOK);
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
#ifdef SECSIZE
		tp = bread(dev, fsbtodb(fs, fs->fs_csaddr + i), size,
		    fs->fs_dbsize);
#else SECSIZE
		error = bread(devvp, fsbtodb(fs, fs->fs_csaddr + i), size, &bp);
		if (error) {
			free((caddr_t)base, M_SUPERBLK);
			goto out;
		}
		bcopy((caddr_t)bp->b_un.b_addr, space, (u_int)size);
		fs->fs_csp[fragstoblks(fs, i)] = (struct csum *)space;
		space += size;
		brelse(bp);
		bp = NULL;
	}
	mp->m_data = (qaddr_t)ump;
	mp->m_bsize = fs->fs_bsize;
	mp->m_fsize = fs->fs_fsize;
	mp->m_fsid.val[0] = (long)dev;
	mp->m_fsid.val[1] = MOUNT_UFS;
	ump->um_mountp = mp;
	ump->um_dev = dev;
	ump->um_devvp = devvp;
	ump->um_qinod = NULL;

	/* Sanity checks for old file systems.			   XXX */
	fs->fs_npsect = MAX(fs->fs_npsect, fs->fs_nsect);	/* XXX */
	fs->fs_interleave = MAX(fs->fs_interleave, 1);		/* XXX */
	if (fs->fs_postblformat == FS_42POSTBLFMT)		/* XXX */
		fs->fs_nrpos = 8;				/* XXX */

	return (0);
out:
	if (needclose)
		(void) VOP_CLOSE(devvp, ronly ? FREAD : FREAD|FWRITE,
			(struct ucred *)0);
	if (ump->um_fs) {
		free((caddr_t)ump->um_fs, M_SUPERBLK);
		ump->um_fs = NULL;
	}
	if (bp)
		brelse(bp);
	return (error);
}


/*
 * unmount system call
 */
ufs_unmount(mp, flags)
	struct mount *mp;
	int flags;
{
	register struct ufsmount *ump;
	register struct fs *fs;
	dev_t dev;
	int error, ronly;

	if (flags & MNT_FORCE)
		return (EINVAL);
	ump = VFSTOUFS(mp);
	dev = ump->um_dev;
#ifdef QUOTA
	if ((error = iflush(dev, mp->m_qinod)) && !forcibly)
#else
	if ((error = iflush(dev)) && !forcibly)
#endif
		return (error);
#ifdef QUOTA
	(void)closedq(ump);
	/*
	 * Here we have to iflush again to get rid of the quota inode.
	 * A drag, but it would be ugly to cheat, & this doesn't happen often.
	 */
	(void)iflush(dev, (struct inode *)NULL);
#endif
	fs = ump->um_fs;
	ronly = !fs->fs_ronly;
	free((caddr_t)fs->fs_csp[0], M_SUPERBLK);
	error = closei(dev, IFBLK, fs->fs_ronly? FREAD : FREAD|FWRITE);
	irele(ip);
	return (error);
}

/*
 * Return root of a filesystem
 */
ufs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	struct inode tip, *ip;
	int error;

	tip.i_dev = VFSTOUFS(mp)->um_dev;
	tip.i_vnode.v_mount = mp;
	error = iget(&tip, (ino_t)ROOTINO, &ip);
	if (error)
		return (error);
	*vpp = ITOV(ip);
	return (0);
}

/*
 * Get file system statistics.
 */
ufs_statfs(mp, sbp)
	struct mount *mp;
	register struct statfs *sbp;
{
	register struct ufsmount *ump;
	register struct fs *fs;

	ump = VFSTOUFS(mp);
	fs = ump->um_fs;
	if (fs->fs_magic != FS_MAGIC)
		panic("ufs_statfs");
	sbp->f_type = MOUNT_UFS;
	sbp->f_flags = mp->m_flag &~ (M_MLOCK|M_MWAIT);
	sbp->f_fsize = fs->fs_fsize;
	sbp->f_bsize = fs->fs_bsize;
	sbp->f_blocks = fs->fs_dsize;
	sbp->f_bfree = fs->fs_cstotal.cs_nbfree * fs->fs_frag +
		fs->fs_cstotal.cs_nffree;
	sbp->f_bavail = (fs->fs_dsize * (100 - fs->fs_minfree) / 100) -
		(fs->fs_dsize - sbp->f_bfree);
	if (sbp->f_bavail < 0)
		sbp->f_bavail = 0;
	sbp->f_files =  fs->fs_ncg * fs->fs_ipg;
	sbp->f_ffree = fs->fs_cstotal.cs_nifree;
	sbp->f_fsid = mp->m_fsid;
	bcopy((caddr_t)fs->fs_fsmnt, (caddr_t)&sbp->f_mntonname[0], MNAMELEN);
	bcopy((caddr_t)ump->um_mntname, (caddr_t)&sbp->f_mntfromname[0],
		MNAMELEN);
	return (0);
}

int	syncprt = 0;

/*
 * Go through the disk queues to initiate sandbagged IO;
 * go through the inodes to write those that have been modified;
 * initiate the writing of the super block if it has been modified.
 */
ufs_sync(mp, waitfor)
	struct mount *mp;
	int waitfor;
{
	register struct inode *ip;
	register struct ufsmount *ump = VFSTOUFS(mp);
	register struct fs *fs;
	int error = 0;
	static int updlock = 0;

	if (syncprt)
		bufstats();
	if (updlock)
		return (EBUSY);
	fs = ump->um_fs;
	if (fs == (struct fs *)1)
		return (0);
	updlock++;
	/*
	 * Write back modified superblock.
	 * Consistency check that the superblock
	 * is still in the buffer cache.
	 */
	if (fs->fs_fmod != 0) {
		if (fs->fs_ronly != 0) {		/* XXX */
			printf("fs = %s\n", fs->fs_fsmnt);
			panic("update: rofs mod");
		}
		fs->fs_fmod = 0;
		fs->fs_time = time.tv_sec;
		error = sbupdate(ump, waitfor);
	}
	/*
	 * Write back each (modified) inode.
	 */
	for (ip = inode; ip < inodeNINODE; ip++) {
		if (ip->i_devvp != ump->um_devvp ||
		    (ip->i_flag & ILOCKED) != 0 || ITOV(ip)->v_count == 0 ||
		    (ip->i_flag & (IMOD|IACC|IUPD|ICHG)) == 0)
			continue;
		ILOCK(ip);
		ITOV(ip)->v_count++;
		error = iupdat(ip, &time, &time, waitfor == MNT_WAIT);
		iput(ip);
	}
	updlock = 0;
	/*
	 * Force stale buffer cache information to be flushed.
	 */
	bflush(ump->um_devvp->v_rdev);
	return (error);
}

/*
 * Write a superblock and associated information back to disk.
 */
sbupdate(mp, waitfor)
	struct ufsmount *mp;
	int waitfor;
{
	register struct fs *fs = mp->um_fs;
	register struct buf *bp;
	int blks;
	caddr_t space;
	int i, size, error = 0;

#ifdef SECSIZE
	bp = getblk(mp->m_dev, (daddr_t)fsbtodb(fs, SBOFF / fs->fs_fsize),
	    (int)fs->fs_sbsize, fs->fs_dbsize);
#else SECSIZE
	bp = getblk(mp->um_devvp, SBLOCK, (int)fs->fs_sbsize);
#endif SECSIZE
	bcopy((caddr_t)fs, bp->b_un.b_addr, (u_int)fs->fs_sbsize);
	/* Restore compatibility to old file systems.		   XXX */
	if (fs->fs_postblformat == FS_42POSTBLFMT)		/* XXX */
		bp->b_un.b_fs->fs_nrpos = -1;			/* XXX */
#ifdef SECSIZE
#ifdef tahoe
	/* restore standard fsbtodb shift */
	bp->b_un.b_fs->fs_fsbtodb = fs->fs_sparecon[0];
	bp->b_un.b_fs->fs_sparecon[0] = 0;
#endif
#endif SECSIZE
	if (waitfor == MNT_WAIT)
		error = bwrite(bp);
	else
		bawrite(bp);
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = (caddr_t)fs->fs_csp[0];
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
#ifdef SECSIZE
		bp = getblk(mp->m_dev, fsbtodb(fs, fs->fs_csaddr + i), size,
		    fs->fs_dbsize);
#else SECSIZE
		bp = getblk(mp->um_devvp, fsbtodb(fs, fs->fs_csaddr + i), size);
#endif SECSIZE
		bcopy(space, bp->b_un.b_addr, (u_int)size);
		space += size;
		if (waitfor == MNT_WAIT)
			error = bwrite(bp);
		else
			bawrite(bp);
	}
	return (error);
}

/*
 * Print out statistics on the current allocation of the buffer pool.
 * Can be enabled to print out on every ``sync'' by setting "syncprt"
 * above.
 */
bufstats()
{
	int s, i, j, count;
	register struct buf *bp, *dp;
	int counts[MAXBSIZE/CLBYTES+1];
	static char *bname[BQUEUES] = { "LOCKED", "LRU", "AGE", "EMPTY" };

	for (bp = bfreelist, i = 0; bp < &bfreelist[BQUEUES]; bp++, i++) {
		count = 0;
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			counts[j] = 0;
		s = splbio();
		for (dp = bp->av_forw; dp != bp; dp = dp->av_forw) {
			counts[dp->b_bufsize/CLBYTES]++;
			count++;
		}
		splx(s);
		printf("%s: total-%d", bname[i], count);
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			if (counts[j] != 0)
				printf(", %d-%d", j * CLBYTES, counts[j]);
		printf("\n");
	}
}

/*
 * File handle to vnode
 */
ufs_fhtovp(mp, fhp, vpp)
	struct mount *mp;
	struct fid *fhp;
	struct vnode **vpp;
{
	register struct ufid *ufhp;
	struct inode tip, *ip;
	int error;

	ufhp = (struct ufid *)fhp;
	tip.i_dev = VFSTOUFS(mp)->um_dev;
	tip.i_vnode.v_mount = mp;
	if (error = iget(&tip, ufhp->ufid_ino, &ip)) {
		*vpp = NULL;
		return (error);
	}
	if (ip->i_gen != ufhp->ufid_gen) {
		iput(ip);
		*vpp = NULL;
		return (EINVAL);
	}
	*vpp = ITOV(ip);
	return (0);
}

/*
 * Vnode pointer to File handle, should never happen.
 */
/* ARGSUSED */
ufs_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{
	register struct inode *ip = VTOI(vp);
	register struct ufid *ufhp;

	ufhp = (struct ufid *)fhp;
	ufhp->ufid_len = sizeof(struct ufid);
	ufhp->ufid_ino = ip->i_number;
	ufhp->ufid_gen = ip->i_gen;
	return (0);
}

/*
 * Common code for mount and quota.
 * Check that the user's argument is a reasonable
 * thing on which to mount, and return the device number if so.
 */
getmdev(devvpp, fname, ndp)
	struct vnode **devvpp;
	caddr_t fname;
	register struct nameidata *ndp;
{
	register struct vnode *vp;
	int error;

	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	if (error = namei(ndp)) {
		if (error == ENOENT)
			return (ENODEV);	/* needs translation */
		return (error);
	}
	vp = ndp->ni_vp;
	if (vp->v_type != VBLK) {
		vput(vp);
		return (ENOTBLK);
	}
	if (major(vp->v_rdev) >= nblkdev)
		return (ENXIO);
	iunlock(VTOI(vp));
	*devvpp = vp;
	return (0);
}
