/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_inode.c	7.45 (Berkeley) %G%
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

#include <ufs/quota.h>
#include <ufs/inode.h>
#include <ufs/ufsmount.h>
#include <ufs/ufs_extern.h>

#include <lfs/lfs.h>
#include <lfs/lfs_extern.h>

extern int prtactive;	/* 1 => print out reclaim of active vnodes */

int
lfs_init()
{
	return (ufs_init());
}

/*
 * Look up an LFS dinode number to find its incore vnode.  If not already
 * in core, read it in from the specified device.  Return the inode locked.
 * Detection and handling of mount points must be done by the calling routine.
 */
int
lfs_iget(pip, ino, ipp)
	struct inode *pip;
	ino_t ino;
	struct inode **ipp;
{
	register LFS *fs;
	register struct inode *ip;
	register struct vnode *vp;
	struct buf *bp;
	struct inode *nip;
	struct mount *mntp;
	struct vnode *nvp;
	dev_t dev;
	int error;

printf("lfs_iget ino %d\n", ino);

	if (ino < ROOTINO)
		return (EINVAL);

	dev = pip->i_dev;
	if ((*ipp = ufs_ihashget(dev, ino)) != NULL)
		return (0);

	/* Allocate new vnode/inode. */
	mntp = ITOV(pip)->v_mount;
	error = lfs_vcreate(mntp, ino, &nvp);
	if (error) {
		*ipp = NULL;
		return (error);
	}
	ip = VTOI(nvp);
		
	/*
	 * Put it onto its hash chain and lock it so that other requests for
	 * this inode will block if they arrive while we are sleeping waiting
	 * for old data structures to be purged or for the contents of the
	 * disk portion of this inode to be read.
	 */
	ufs_ihashins(ip);

	/* Read in the disk contents for the inode, copy into the inode. */
	fs = VFSTOUFS(mntp)->um_lfs;
	if (error = bread(VFSTOUFS(mntp)->um_devvp, lfs_itod(fs, ino),
	    (int)fs->lfs_bsize, NOCRED, &bp)) {
		/*
		 * The inode does not contain anything useful, so it would
		 * be misleading to leave it on its hash chain.  Iput() will
		 * take care of putting it back on the free list.
		 */
		remque(ip);
		ip->i_forw = ip;
		ip->i_back = ip;

		/* Unlock and discard unneeded inode. */
		ufs_iput(ip);
		brelse(bp);
		*ipp = NULL;
		return (error);
	}
	ip->i_din = *lfs_ifind(fs, ino, bp->b_un.b_dino);
	brelse(bp);

	/* Initialize the vnode from the inode, check for aliases. */
	if (error = ufs_vinit(mntp, ip, &nip)) {
		ufs_iput(ip);
		*ipp = NULL;
		return (error);
	}
	*ipp = nip;
	return (0);
}

int
lfs_iupdat(ip, ta, tm, waitfor)
	register struct inode *ip;
	struct timeval *ta, *tm;
        int waitfor;
{
	/*
	 * XXX
	 * This isn't right, but ufs_iupdat() isn't either.
	 */
	ITIMES(ip, ta, tm);
	return (0);
}

/*
 * Truncate the inode ip to at most length size.
 *
 * NB: triple indirect blocks are untested.
 */
/* ARGSUSED */
int
lfs_itrunc(oip, length, flags)
	register struct inode *oip;
	u_long length;
	int flags;
{
	register LFS *fs;
	struct buf *bp;
	daddr_t lbn;
	int error, offset, size;

	vnode_pager_setsize(ITOV(oip), length);

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
		if (error = bread(ITOV(oip), lbn, fs->lfs_bsize, NOCRED, &bp))
			return (error);
		oip->i_size = length;
		size = blksize(fs);				/* LFS */
		(void) vnode_pager_uncache(ITOV(oip));
		bzero(bp->b_un.b_addr + offset, (unsigned)(size - offset));
		allocbuf(bp, size);
		lfs_bwrite(bp);
	}
	/* BZERO INODE BLOCK POINTERS HERE, FOR CONSISTENCY XXX */
}
