/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ffs_vfsops.c	7.11 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "inode.h"
#include "proc.h"
#include "fs.h"
#include "buf.h"
#include "mount.h"
#include "file.h"
#include "conf.h"
#include "ioctl.h"
#include "disklabel.h"
#include "stat.h"
#include "malloc.h"
#include "ioctl.h"
#include "disklabel.h"
#include "stat.h"

smount()
{
	register struct a {
		char	*fspec;
		char	*freg;
		int	ronly;
	} *uap = (struct a *)u.u_ap;
	dev_t dev;
	register struct inode *ip;
	register struct fs *fs;
	register struct nameidata *ndp = &u.u_nd;
	u_int len;

	u.u_error = getmdev(&dev, uap->fspec);
	if (u.u_error)
		return;
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = (caddr_t)uap->freg;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	if (ip->i_count != 1) {
		iput(ip);
		u.u_error = EBUSY;
		return;
	}
	if ((ip->i_mode&IFMT) != IFDIR) {
		iput(ip);
		u.u_error = ENOTDIR;
		return;
	}
	fs = mountfs(dev, uap->ronly, ip);
	if (fs == 0) {
		iput(ip);
		return;
	}
	(void) copyinstr(uap->freg, fs->fs_fsmnt, sizeof(fs->fs_fsmnt)-1, &len);
	bzero(fs->fs_fsmnt + len, sizeof (fs->fs_fsmnt) - len);
}

struct fs *
mountfs(dev, ronly, ip)
	dev_t dev;
	int ronly;
	struct inode *ip;
{
	register struct mount *mp;
	struct mount *fmp = NULL;
	register struct buf *bp = NULL;
	register struct fs *fs;
	struct partinfo dpart;
	int havepart = 0, blks;
	caddr_t base, space;
	int i, size;
	register error;
	int needclose = 0;

	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++) {
		if (mp->m_fs == NULL) {
			if (fmp == NULL)
				fmp = mp;
		} else if (dev == mp->m_dev) {
			u.u_error = EBUSY;		/* XXX */
			return ((struct fs *) NULL);
		}
	}
	if ((mp = fmp) == NULL) {
		u.u_error = EMFILE;		/* needs translation      XXX */
		return ((struct fs *) NULL);
	}
	mp->m_fs = (struct fs *)1;	/* just to reserve this slot */
	mp->m_dev = dev;
	mp->m_inodp = NULL;
	error =
	    (*bdevsw[major(dev)].d_open)(dev, ronly ? FREAD : FREAD|FWRITE,
	        S_IFBLK);
	if (error) {
		u.u_error = error;
		mp->m_fs = NULL;
		return ((struct fs *) NULL);
	}
	needclose = 1;
	if ((*bdevsw[major(dev)].d_ioctl)(dev, DIOCGPART,
	    (caddr_t)&dpart, FREAD) == 0) {
		havepart = 1;
		size = dpart.disklab->d_secsize;
	} else
		size = DEV_BSIZE;
#ifdef SECSIZE
	/*
	 * If possible, determine hardware sector size
	 * and adjust fsbtodb to correspond.
	 */
#endif SECSIZE
	if ((*bdevsw[major(dev)].d_ioctl)(dev, DIOCGPART,
	    (caddr_t)&dpart, FREAD) == 0) {
		havepart = 1;
		size = dpart.disklab->d_secsize;
#ifdef SECSIZE
		if (size < MINSECSIZE) {
			error = EINVAL;
			goto out;
		}
#endif SECSIZE
	} else
		size = DEV_BSIZE;
#ifdef SECSIZE
	tp = bread(dev, (daddr_t)(SBOFF / size), SBSIZE, size);
#else SECSIZE
	bp = bread(dev, SBLOCK, SBSIZE);
	if (bp->b_flags & B_ERROR) {
		mp->m_fs = NULL;
		goto out;
	}
	fs = bp->b_un.b_fs;
	if (fs->fs_magic != FS_MAGIC || fs->fs_bsize > MAXBSIZE ||
	    fs->fs_bsize < sizeof(struct fs)) {
		error = EINVAL;		/* also needs translation */
		goto out;
	}
	mp->m_fs = (struct fs *)malloc((u_long)fs->fs_sbsize, M_SUPERBLK,
	    M_WAITOK);
	bcopy((caddr_t)bp->b_un.b_addr, (caddr_t)mp->m_fs,
	   (u_int)fs->fs_sbsize);
	brelse(bp);
	bp = NULL;
	fs = mp->m_fs;
	fs->fs_ronly = (ronly != 0);
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
	if (space == NULL) {
		error = ENOMEM;
		goto out;
	}
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
#ifdef SECSIZE
		tp = bread(dev, fsbtodb(fs, fs->fs_csaddr + i), size,
		    fs->fs_dbsize);
#else SECSIZE
		bp = bread(dev, fsbtodb(fs, fs->fs_csaddr + i), size);
		if (bp->b_flags&B_ERROR) {
			free((caddr_t)base, M_SUPERBLK);
			goto out;
		}
		bcopy((caddr_t)bp->b_un.b_addr, space, (u_int)size);
		fs->fs_csp[fragstoblks(fs, i)] = (struct csum *)space;
		space += size;
		brelse(bp);
		bp = NULL;
	}
	mp->m_inodp = ip;
	if (ip) {
		ip->i_flag |= IMOUNT;
		cacheinval(ip);
		iunlock(ip);
	}
	/* Sanity checks for old file systems.			   XXX */
	fs->fs_npsect = MAX(fs->fs_npsect, fs->fs_nsect);	/* XXX */
	fs->fs_interleave = MAX(fs->fs_interleave, 1);		/* XXX */
	if (fs->fs_postblformat == FS_42POSTBLFMT)		/* XXX */
		fs->fs_nrpos = 8;				/* XXX */

	return (fs);
out:
	if (needclose)
		(void) closei(dev, IFBLK, ronly? FREAD : FREAD|FWRITE);
	if (mp->m_fs) {
		free((caddr_t)mp->m_fs, M_SUPERBLK);
		mp->m_fs = NULL;
	}
	if (bp)
		brelse(bp);
	u.u_error = error ? error : EIO;			/* XXX */
	return ((struct fs *) NULL);
}

umount()
{
	struct a {
		char	*fspec;
	} *uap = (struct a *)u.u_ap;

	u.u_error = unmount1(uap->fspec, 0);
}

unmount1(fname, forcibly)
	caddr_t fname;
	int forcibly;
{
	dev_t dev;
	register struct mount *mp;
	int error;
	register struct inode *ip;
	register struct fs *fs;

	forcibly = 0;					/* XXX */
	forcibly = 0;					/* XXX */
	error = getmdev(&dev, fname);
	if (error)
		return (error);
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_fs != NULL && dev == mp->m_dev)
			goto found;
	return (EINVAL);
found:
	xumount(dev);	/* remove unused sticky files from text table */
	nchinval(dev);	/* flush the name cache */
	update();
#ifdef QUOTA
	if ((error = iflush(dev, mp->m_qinod)) && !forcibly)
#else
	if ((error = iflush(dev)) && !forcibly)
#endif
		return (error);
#ifdef QUOTA
	closedq(mp);
	/*
	 * Here we have to iflush again to get rid of the quota inode.
	 * A drag, but it would be ugly to cheat, & this doesn't happen often.
	 */
	(void)iflush(dev, (struct inode *)NULL);
#endif
	ip = mp->m_inodp;
	ip->i_flag &= ~IMOUNT;
	fs = mp->m_fs;
	free((caddr_t)fs->fs_csp[0], M_SUPERBLK);
	free((caddr_t)mp->m_fs, M_SUPERBLK);
	mp->m_fs = NULL;
	mp->m_dev = NODEV;
	mpurge(mp - &mount[0]);
	error = closei(dev, IFBLK, fs->fs_ronly? FREAD : FREAD|FWRITE);
	irele(ip);
	return (error);
}

sbupdate(mp)
	struct mount *mp;
{
	register struct fs *fs = mp->m_fs;
	register struct buf *bp;
	int blks;
	caddr_t space;
	int i, size;

#ifdef SECSIZE
	bp = getblk(mp->m_dev, (daddr_t)fsbtodb(fs, SBOFF / fs->fs_fsize),
	    (int)fs->fs_sbsize, fs->fs_dbsize);
#else SECSIZE
	bp = getblk(mp->m_dev, SBLOCK, (int)fs->fs_sbsize);
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
	bwrite(bp);
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
		bp = getblk(mp->m_dev, fsbtodb(fs, fs->fs_csaddr + i), size);
#endif SECSIZE
		bcopy(space, bp->b_un.b_addr, (u_int)size);
		space += size;
		bwrite(bp);
	}
}

/*
 * Common code for mount and umount.
 * Check that the user's argument is a reasonable
 * thing on which to mount, and return the device number if so.
 */
getmdev(pdev, fname)
	caddr_t fname;
	dev_t *pdev;
{
	dev_t dev;
	register struct inode *ip;
	register struct nameidata *ndp = &u.u_nd;

	if (!suser())
		return (u.u_error);
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	ip = namei(ndp);
	if (ip == NULL) {
		if (u.u_error == ENOENT)
			return (ENODEV); /* needs translation */
		return (u.u_error);
	}
	if ((ip->i_mode&IFMT) != IFBLK) {
		iput(ip);
		return (ENOTBLK);
	}
	dev = (dev_t)ip->i_rdev;
	iput(ip);
	if (major(dev) >= nblkdev)
		return (ENXIO);
	*pdev = dev;
	return (0);
}
