/*
 * Copyright (c) 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_inode.c	7.50 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mount.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/kernel.h>
#include <sys/malloc.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

int
lfs_init()
{
#ifdef VERBOSE
	printf("lfs_init\n");
#endif
	return (ufs_init());
}

/*
 * Look up an LFS dinode number to find its incore vnode.  If not already
 * in core, read it in from the specified device.  Return the inode locked.
 * Detection and handling of mount points must be done by the calling routine.
 */
int
lfs_vget(mntp, ino, vpp)
	struct mount *mntp;
	ino_t ino;
	struct vnode **vpp;
{
	register struct lfs *fs;
	register struct inode *ip;
	struct buf *bp;
	struct vnode *vp;
	struct ufsmount *ump;
	dev_t dev;
	int error;

#ifdef VERBOSE
	printf("lfs_vget\n");
#endif
	ump = VFSTOUFS(mntp);
	dev = ump->um_dev;
	if ((*vpp = ufs_ihashget(dev, ino)) != NULL)
		return (0);

	/* Allocate new vnode/inode. */
	if (error = lfs_vcreate(mntp, ino, &vp)) {
		*vpp = NULL;
		return (error);
	}
	/*
	 * Put it onto its hash chain and lock it so that other requests for
	 * this inode will block if they arrive while we are sleeping waiting
	 * for old data structures to be purged or for the contents of the
	 * disk portion of this inode to be read.
	 */
	ip = VTOI(vp);
	ufs_ihashins(ip);

	/* Read in the disk contents for the inode, copy into the inode. */
	ip->i_lfs = fs = ump->um_lfs;
	if (error = bread(ump->um_devvp, lfs_itod(fs, ino),
	    (int)fs->lfs_bsize, NOCRED, &bp)) {
		/*
		 * The inode does not contain anything useful, so it
		 * would be misleading to leave it on its hash chain.
		 * Iput() will return it to the free list.
		 */
		remque(ip);
		ip->i_forw = ip;
		ip->i_back = ip;

		/* Unlock and discard unneeded inode. */
		ufs_iput(ip);
		brelse(bp);
		*vpp = NULL;
		return (error);
	}
	ip->i_din = *lfs_ifind(fs, ino, bp->b_un.b_dino);
	brelse(bp);

	/*
	 * Initialize the vnode from the inode, check for aliases.  In all
	 * cases re-init ip, the underlying vnode/inode may have changed.
	 */
	if (error = ufs_vinit(mntp, &lfs_specops, LFS_FIFOOPS, &vp)) {
		ufs_iput(ip);
		*vpp = NULL;
		return (error);
	}
	/*
	 * Finish inode initialization now that aliasing has been resolved.
	 */
	ip->i_devvp = ump->um_devvp;
	VREF(ip->i_devvp);
	*vpp = vp;
	return (0);
}

int
lfs_update(vp, ta, tm, waitfor)
	register struct vnode *vp;
	struct timeval *ta, *tm;
        int waitfor;
{
	struct inode *ip;

#ifdef VERBOSE
	printf("lfs_update\n");
#endif
	if (vp->v_mount->mnt_flag & MNT_RDONLY)
		return (0);
	ip = VTOI(vp);
	if ((ip->i_flag & (IUPD|IACC|ICHG|IMOD)) == 0)
		return (0);
	if (ip->i_flag&IACC)
		ip->i_atime = ta->tv_sec;
	if (ip->i_flag&IUPD) {
		ip->i_mtime = tm->tv_sec;
		INCRQUAD((ip)->i_modrev);
	}
	if (ip->i_flag&ICHG)
		ip->i_ctime = time.tv_sec;
	ip->i_flag &= ~(IUPD|IACC|ICHG|IMOD);

	/*
	 * XXX
	 * I'm not real sure what to do here; once we have fsync and partial
	 * segments working in the LFS context, this must be fixed to be
	 * correct.  The contents of the inode have to be pushed back to
	 * stable storage; note that the ifile contains the access time of
	 * the inode and must be updated as well.
	 */
	return (0);
}

/*
 * Truncate the inode ip to at most length size.
 *
 * NB: triple indirect blocks are untested.
 */
/* ARGSUSED */
int
lfs_truncate(ovp, length, flags)
	struct vnode *ovp;
	u_long length;
	int flags;
{
	register struct lfs *fs;
	register struct inode *oip;
	struct buf *bp;
	daddr_t lbn;
	int error, offset, size;

#ifdef VERBOSE
	printf("lfs_truncate\n");
#endif
	vnode_pager_setsize(ovp, length);
	oip = VTOI(ovp);

	/* If length is larger than the file, just update the times. */
	if (oip->i_size <= length) {
		oip->i_flag |= ICHG|IUPD;
		ITIMES(oip, &time, &time);
		return (0);
	}

	/*
	 * Update the size of the file. If the file is not being truncated to
	 * a block boundry, the contents of the partial block following the end
	 * of the file must be zero'ed in case it ever become accessable again
	 * because of subsequent file growth.
	 */
	fs = oip->i_lfs;
	offset = blkoff(fs, length);
	if (offset == 0)
		oip->i_size = length;
	else {
		lbn = lblkno(fs, length);
#ifdef QUOTA
		if (error = getinoquota(oip))
			return (error);
#endif	
		if (error = bread(ovp, lbn, fs->lfs_bsize, NOCRED, &bp))
			return (error);
		oip->i_size = length;
		size = blksize(fs);
		(void)vnode_pager_uncache(ovp);
		bzero(bp->b_un.b_addr + offset, (unsigned)(size - offset));
		allocbuf(bp, size);
		lfs_bwrite(bp);
	}
	/* XXX: BZERO INODE BLOCK POINTERS HERE, FOR CONSISTENCY. */
	(void)vinvalbuf(ovp, length > 0);
	return (0);
}
