/*
 * Copyright (c) 1989, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_vfsops.c	7.63 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/proc.h>
#include <sys/kernel.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/mount.h>
#include <sys/buf.h>
#include <sys/file.h>
#include <sys/disklabel.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/malloc.h>
#include "ioctl.h"
#include "disklabel.h"
#include "stat.h"

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

static int lfs_mountfs __P((struct vnode *, struct mount *, struct proc *));

struct vfsops lfs_vfsops = {
	lfs_mount,
	ufs_start,
	lfs_unmount,
	ufs_root,
	ufs_quotactl,
	lfs_statfs,
	lfs_sync,
	ufs_fhtovp,
	ufs_vptofh,
	lfs_init,
};

int
lfs_mountroot()
{
	panic("lfs_mountroot");		/* XXX -- implement */
}

/*
 * VFS Operations.
 *
 * mount system call
 */
lfs_mount(mp, path, data, ndp, p)
	register struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	struct vnode *devvp;
	struct ufs_args args;
	struct ufsmount *ump;
	register struct lfs *fs;				/* LFS */
	u_int size;
	int error;

	if (error = copyin(data, (caddr_t)&args, sizeof (struct ufs_args)))
		return (error);

	/* Until LFS can do NFS right.		XXX */
	if (args.exflags & MNT_EXPORTED)
		return (EINVAL);
	/*
	 * Process export requests.
	 */
	if ((args.exflags & MNT_EXPORTED) || (mp->mnt_flag & MNT_EXPORTED)) {
		if (args.exflags & MNT_EXPORTED)
			mp->mnt_flag |= MNT_EXPORTED;
		else
			mp->mnt_flag &= ~MNT_EXPORTED;
		if (args.exflags & MNT_EXRDONLY)
			mp->mnt_flag |= MNT_EXRDONLY;
		else
			mp->mnt_flag &= ~MNT_EXRDONLY;
		mp->mnt_exroot = args.exroot;
	}
	/*
	 * If updating, check whether changing from read-only to
	 * read/write; if there is no device name, that's all we do.
	 */
	if (mp->mnt_flag & MNT_UPDATE) {
		ump = VFSTOUFS(mp);
#ifdef NOTLFS							/* LFS */
		fs = ump->um_fs;
		if (fs->fs_ronly && (mp->mnt_flag & MNT_RDONLY) == 0)
			fs->fs_ronly = 0;
#else
		fs = ump->um_lfs;
		if (fs->lfs_ronly && (mp->mnt_flag & MNT_RDONLY) == 0)
			fs->lfs_ronly = 0;
#endif
		if (args.fspec == 0)
			return (0);
	}
	/*
	 * Not an update, or updating the name: look up the name
	 * and verify that it refers to a sensible block device.
	 */
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = args.fspec;
	if (error = namei(ndp, p))
		return (error);
	devvp = ndp->ni_vp;
	if (devvp->v_type != VBLK) {
		vrele(devvp);
		return (ENOTBLK);
	}
	if (major(devvp->v_rdev) >= nblkdev) {
		vrele(devvp);
		return (ENXIO);
	}
	if ((mp->mnt_flag & MNT_UPDATE) == 0)
		error = lfs_mountfs(devvp, mp, p);		/* LFS */
	else {
		if (devvp != ump->um_devvp)
			error = EINVAL;	/* needs translation */
		else
			vrele(devvp);
	}
	if (error) {
		vrele(devvp);
		return (error);
	}
	ump = VFSTOUFS(mp);
	fs = ump->um_lfs;					/* LFS */
#ifdef NOTLFS							/* LFS */
	(void) copyinstr(path, fs->fs_fsmnt, sizeof(fs->fs_fsmnt) - 1, &size);
	bzero(fs->fs_fsmnt + size, sizeof(fs->fs_fsmnt) - size);
	bcopy((caddr_t)fs->fs_fsmnt, (caddr_t)mp->mnt_stat.f_mntonname,
	    MNAMELEN);
	(void) copyinstr(args.fspec, mp->mnt_stat.f_mntfromname, MNAMELEN - 1, 
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
	(void) ufs_statfs(mp, &mp->mnt_stat, p);
#else
	(void)copyinstr(path, fs->lfs_fsmnt, sizeof(fs->lfs_fsmnt) - 1, &size);
	bzero(fs->lfs_fsmnt + size, sizeof(fs->lfs_fsmnt) - size);
	bcopy((caddr_t)fs->lfs_fsmnt, (caddr_t)mp->mnt_stat.f_mntonname,
	    MNAMELEN);
	(void) copyinstr(args.fspec, mp->mnt_stat.f_mntfromname, MNAMELEN - 1, 
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
	(void) lfs_statfs(mp, &mp->mnt_stat, p);
#endif
	return (0);
}

/*
 * Common code for mount and mountroot
 * LFS specific
 */
static int
lfs_mountfs(devvp, mp, p)
	register struct vnode *devvp;
	struct mount *mp;
	struct proc *p;
{
	extern struct vnode *rootvp;
	register struct lfs *fs;
	register struct ufsmount *ump;
	struct inode *ip;
	struct vnode *vp;
	struct buf *bp;
	struct partinfo dpart;
	daddr_t seg_addr;
	dev_t dev;
	int error, i, ronly, size;

	if (error = VOP_OPEN(devvp, ronly ? FREAD : FREAD|FWRITE, NOCRED, p))
		return (error);

	if (VOP_IOCTL(devvp, DIOCGPART, (caddr_t)&dpart, FREAD, NOCRED, p) != 0)
		size = DEV_BSIZE;
	else {
		size = dpart.disklab->d_secsize;
#ifdef NEVER_USED
		dpart.part->p_fstype = FS_LFS;
		dpart.part->p_fsize = fs->lfs_fsize;	/* frag size */
		dpart.part->p_frag = fs->lfs_frag;	/* frags per block */
		dpart.part->p_cpg = fs->lfs_segshift;	/* segment shift */
#endif
	}

	/* Don't free random space on error. */
	bp = NULL;
	ump = NULL;

	/* Read in the superblock. */
	if (error = bread(devvp, LFS_LABELPAD / size, LFS_SBPAD, NOCRED, &bp))
		goto out;
		error = EINVAL;		/* XXX needs translation */
		goto out;
	}
#ifdef DEBUG
	lfs_dump_super(fs);
#endif

	/* Allocate the mount structure, copy the superblock into it. */
	ump = (struct ufsmount *)malloc(sizeof *ump, M_UFSMNT, M_WAITOK);
	ump->um_lfs = malloc(sizeof(struct lfs), M_SUPERBLK, M_WAITOK);
	bcopy(bp->b_un.b_addr, ump->um_lfs, sizeof(struct lfs));
	if (sizeof(struct lfs) < LFS_SBPAD)			/* XXX why? */
		bp->b_flags |= B_INVAL;
	brelse(bp);
	bp = NULL;

	/* Set up the I/O information */
	fs->lfs_iocount = 0;
	fs->lfs_seglist = NULL;

	/* Set the file system readonly/modify bits. */
	fs = ump->um_lfs;
	fs->lfs_ronly = ronly;
	if (ronly == 0)
		fs->lfs_fmod = 1;

	/* Initialize the mount structure. */
	dev = devvp->v_rdev;
	mp->mnt_data = (qaddr_t)ump;
	mp->mnt_stat.f_fsid.val[0] = (long)dev;
	mp->mnt_stat.f_fsid.val[1] = MOUNT_LFS;	
	mp->mnt_flag |= MNT_LOCAL;
	ump->um_mountp = mp;
	ump->um_dev = dev;
	ump->um_devvp = devvp;
	for (i = 0; i < MAXQUOTAS; i++)
		ump->um_quotas[i] = NULLVP;

	/* Initialize UFS glue. */
	ump->um_blkatoff = lfs_blkatoff;
	ump->um_write = lfs_write;
	ump->um_iget = lfs_iget;
	ump->um_ialloc = lfs_ialloc;
	ump->um_ifree = lfs_ifree;
	ump->um_itrunc = lfs_itrunc;
	ump->um_iupdat = lfs_iupdat;
	ump->um_bwrite = lfs_bwrite;
	ump->um_bmap = lfs_bmap;

	/* Read the ifile disk inode and store it in a vnode. */
	error = bread(devvp, fs->lfs_idaddr, fs->lfs_bsize, NOCRED, &bp);
	if (error)
		goto out;
	error = lfs_vcreate(mp, LFS_IFILE_INUM, &vp);
	if (error)
		goto out;
	ip = VTOI(vp);

	/* The ifile inode is stored in the superblock. */
	fs->lfs_ivnode = vp;

	/* Copy the on-disk inode into place. */
	ip->i_din = *lfs_ifind(fs, LFS_IFILE_INUM, bp->b_un.b_dino);
	brelse(bp);

	/* Initialize the associated vnode */
	vp->v_type = IFTOVT(ip->i_mode);

	/*
	 * Read in the segusage table.
	 * 
	 * Since we always explicitly write the segusage table at a checkpoint,
	 * we're assuming that it is continguous on disk.
	 */
	seg_addr = ip->i_din.di_db[0];
	size = fs->lfs_segtabsz << fs->lfs_bshift;
	fs->lfs_segtab = malloc(size, M_SUPERBLK, M_WAITOK);
	error = bread(devvp, seg_addr, size, NOCRED, &bp);
	if (error) {
		free(fs->lfs_segtab, M_SUPERBLK);
		goto out;
	}
	bcopy((caddr_t)bp->b_un.b_addr, fs->lfs_segtab, size);
	brelse(bp);
	devvp->v_specflags |= SI_MOUNTEDON;
	VREF(ip->i_devvp);

	return (0);
out:
	if (bp)
		brelse(bp);
	(void)VOP_CLOSE(devvp, ronly ? FREAD : FREAD|FWRITE, NOCRED, p);
	if (ump) {
		free(ump->um_lfs, M_SUPERBLK);
		free(ump, M_UFSMNT);
		mp->mnt_data = (qaddr_t)0;
	}
	return (error);
}

/*
 * unmount system call
 */
lfs_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	extern int doforce;
	register struct ufsmount *ump;
	register struct lfs *fs;				/* LFS */
	int i, error, ronly, flags = 0;
	int ndirty;						/* LFS */

printf("lfs_unmount\n");
	if (mntflags & MNT_FORCE) {
		if (!doforce || mp == rootfs)
			return (EINVAL);
		flags |= FORCECLOSE;
	}
	if (error = lfs_segwrite(mp, 1))
		return(error);

ndirty = lfs_umountdebug(mp);
printf("lfs_umountdebug: returned %d dirty\n", ndirty);
return(0);
	if (mntinvalbuf(mp))
		return (EBUSY);
	ump = VFSTOUFS(mp);
		return (error);
#ifdef QUOTA
	if (mp->mnt_flag & MNT_QUOTA) {
		if (error = vflush(mp, NULLVP, SKIPSYSTEM|flags))
			return (error);
		for (i = 0; i < MAXQUOTAS; i++) {
			if (ump->um_quotas[i] == NULLVP)
				continue;
			quotaoff(p, mp, i);
		}
		/*
		 * Here we fall through to vflush again to ensure
		 * that we have gotten rid of all the system vnodes.
		 */
	}
#endif
	if (error = vflush(mp, NULLVP, flags))
		return (error);
#ifdef NOTLFS							/* LFS */
	fs = ump->um_fs;
	ronly = !fs->fs_ronly;
#else
	fs = ump->um_lfs;
	ronly = !fs->lfs_ronly;
#endif
 * Get file system statistics.
 */
lfs_statfs(mp, sbp, p)
	struct mount *mp;
	register struct statfs *sbp;
	struct proc *p;
{
	register struct lfs *fs;
	register struct ufsmount *ump;

	ump = VFSTOUFS(mp);
#ifdef NOTLFS							/* LFS */
	fs = ump->um_fs;
	if (fs->fs_magic != FS_MAGIC)
		panic("ufs_statfs");
	sbp->f_type = MOUNT_UFS;
	sbp->f_fsize = fs->fs_fsize;
	sbp->f_bsize = fs->fs_bsize;
	sbp->f_blocks = fs->fs_dsize;
	sbp->f_bfree = fs->fs_cstotal.cs_nbfree * fs->fs_frag +
		fs->fs_cstotal.cs_nffree;
	sbp->f_bavail = (fs->fs_dsize * (100 - fs->fs_minfree) / 100) -
		(fs->fs_dsize - sbp->f_bfree);
	sbp->f_files =  fs->fs_ncg * fs->fs_ipg - ROOTINO;
	sbp->f_ffree = fs->fs_cstotal.cs_nifree;
#else
	fs = ump->um_lfs;
	if (fs->lfs_magic != LFS_MAGIC)
		panic("lfs_statfs: magic");
	sbp->f_type = MOUNT_LFS;
	sbp->f_fsize = fs->lfs_bsize;
	sbp->f_bsize = fs->lfs_bsize;
	sbp->f_blocks = fs->lfs_dsize;
	sbp->f_bfree = fs->lfs_bfree;
	sbp->f_bavail = (fs->lfs_dsize * (100 - fs->lfs_minfree) / 100) -
		(fs->lfs_dsize - sbp->f_bfree);
	sbp->f_files = fs->lfs_nfiles;
	sbp->f_ffree = fs->lfs_bfree * INOPB(fs);
#endif
	if (sbp != &mp->mnt_stat) {
		bcopy((caddr_t)mp->mnt_stat.f_mntonname,
			(caddr_t)&sbp->f_mntonname[0], MNAMELEN);
		bcopy((caddr_t)mp->mnt_stat.f_mntfromname,
			(caddr_t)&sbp->f_mntfromname[0], MNAMELEN);
	}
	return (0);
}

/*
 * Go through the disk queues to initiate sandbagged IO;
 * go through the inodes to write those that have been modified;
 * initiate the writing of the super block if it has been modified.
 *
 * Note: we are always called with the filesystem marked `MPBUSY'.
 */
lfs_sync(mp, waitfor)
	struct mount *mp;
	int waitfor;
{
	extern int syncprt;
	static int sync_lock, sync_want;
	int error;

printf("lfs_sync\n");

	/*
	 * Meta data blocks are only marked dirty, not busy, so LFS syncs
	 * must be single threaded.
	 */
	while (sync_lock) {
		sync_want = 1;
		if (error = tsleep(&sync_lock, PLOCK | PCATCH, "lfs sync", 0))
			return (error);
	}
	sync_lock = 1;

	if (syncprt)
		ufs_bufstats();

	/* All syncs must be checkpoints until roll-forward is implemented. */
	error = lfs_segwrite(mp, 1);		
#ifdef QUOTA
	qsync(mp);
#endif
	sync_lock = 0;
	if (sync_want) {
		sync_want = 0;
		wakeup(&sync_lock);
	}
	return (error);
}
