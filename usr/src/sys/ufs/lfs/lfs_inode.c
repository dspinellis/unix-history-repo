/*
 * Copyright (c) 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_inode.c	7.58 (Berkeley) %G%
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

static struct dinode *lfs_ifind __P((struct lfs *, ino_t, struct dinode *));

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
	struct ifile *ifp;
	struct vnode *vp;
	struct ufsmount *ump;
	daddr_t daddr;
	dev_t dev;
	int error;

#ifdef VERBOSE
	printf("lfs_vget\n");
#endif
	ump = VFSTOUFS(mntp);
	dev = ump->um_dev;
	if ((*vpp = ufs_ihashget(dev, ino)) != NULL)
		return (0);

	/* Translate the inode number to a disk address. */
	fs = ump->um_lfs;
	if (ino == LFS_IFILE_INUM)
		daddr = fs->lfs_idaddr;
	else {
		LFS_IENTRY(ifp, fs, ino, bp);
		daddr = ifp->if_daddr;
		brelse(bp);
		if (daddr == LFS_UNUSED_DADDR)
			return (ENOENT);
	}

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

	/*
	 * XXX
	 * This may not need to be here, logically it should go down with
	 * the i_devvp initialization.
	 * Ask Kirk.
	 */
	ip->i_lfs = ump->um_lfs;

	/* Read in the disk contents for the inode, copy into the inode. */
	if (error =
	    bread(ump->um_devvp, daddr, (int)fs->lfs_bsize, NOCRED, &bp)) {
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

/* Search a block for a specific dinode. */
static struct dinode *
lfs_ifind(fs, ino, dip)
	struct lfs *fs;
	ino_t ino;
	register struct dinode *dip;
{
	register int cnt;

#ifdef VERBOSE
	printf("lfs_ifind: inode %d\n", ino);
#endif
	for (cnt = INOPB(fs); cnt--; ++dip)
		if (dip->di_inum == ino)
			return (dip);

	panic("lfs_ifind: dinode %u not found", ino);
	/* NOTREACHED */
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

	/* Push back the vnode and any dirty blocks it may have. */
	return (waitfor ? lfs_vflush(vp) : 0);
}

/* Update segment usage information when removing a block. */
#define UPDATE_SEGUSE \
	if (lastseg != -1) { \
		LFS_SEGENTRY(sup, fs, lastseg, sup_bp); \
		sup->su_nbytes -= num << fs->lfs_bshift; \
		LFS_UBWRITE(sup_bp); \
		blocksreleased += num; \
	}

#define SEGDEC { \
	if (daddr != UNASSIGNED) { \
		if (lastseg != (seg = datosn(fs, daddr))) { \
			UPDATE_SEGUSE; \
			num = 1; \
			lastseg = seg; \
		} else \
			++num; \
	} \
}

/*
 * Truncate the inode ip to at most length size.  Update segment usage
 * table information.
 */
/* ARGSUSED */
int
lfs_truncate(vp, length, flags)
	struct vnode *vp;
	u_long length;
	int flags;
{
	register INDIR *ap;
	register int i;
	register daddr_t *daddrp;
	struct buf *bp, *sup_bp;
	struct ifile *ifp;
	struct inode *ip;
	struct lfs *fs;
	INDIR a[NIADDR + 2], a_end[NIADDR + 2];
	SEGUSE *sup;
	daddr_t daddr, lastblock, lbn, olastblock;
	off_t off;
	long blocksreleased;
	int error, depth, lastseg, num, offset, seg, size;

#ifdef VERBOSE
	printf("lfs_truncate\n");
#endif
	vnode_pager_setsize(vp, length);

	ip = VTOI(vp);
	fs = ip->i_lfs;

	/* If truncating the file to 0, update the version number. */
	if (length == 0) {
		LFS_IENTRY(ifp, fs, ip->i_number, bp);
		++ifp->if_version;
		LFS_UBWRITE(bp);
	}

	/* If length is larger than the file, just update the times. */
	if (ip->i_size <= length) {
		ip->i_flag |= ICHG|IUPD;
		return (lfs_update(vp, &time, &time, 1));
	}

	/*
	 * Calculate index into inode's block list of last direct and indirect
	 * blocks (if any) which we want to keep.  Lastblock is 0 when the
	 * file is truncated to 0.
	 */
	lastblock = lblkno(fs, length + fs->lfs_bsize - 1);
	olastblock = lblkno(fs, ip->i_size + fs->lfs_bsize - 1) - 1;

	/*
	 * Update the size of the file. If the file is not being truncated to
	 * a block boundry, the contents of the partial block following the end
	 * of the file must be zero'ed in case it ever become accessable again
	 * because of subsequent file growth.
	 */
	offset = blkoff(fs, length);
	if (offset == 0)
		ip->i_size = length;
	else {
		lbn = lblkno(fs, length);
#ifdef QUOTA
		if (error = getinoquota(ip))
			return (error);
#endif	
		if (error = bread(vp, lbn, fs->lfs_bsize, NOCRED, &bp))
			return (error);
		ip->i_size = length;
		size = blksize(fs);
		(void)vnode_pager_uncache(vp);
		bzero(bp->b_un.b_addr + offset, (unsigned)(size - offset));
		allocbuf(bp, size);
		LFS_UBWRITE(bp);
	}
	/*
	 * Modify sup->su_nbyte counters for each deleted block; keep track
	 * of number of blocks removed for ip->i_blocks.
	 */
	blocksreleased = 0;
	num = 0;
	lastseg = -1;

	for (lbn = olastblock; lbn >= lastblock;) {
		lfs_bmaparray(vp, lbn, &daddr, a, &depth);
		if (lbn == olastblock)
			for (i = NIADDR + 2; i--;)
				a_end[i] = a[i];
		switch (depth) {
		case 0:				/* Direct block. */
			daddr = ip->i_db[lbn];
			SEGDEC;
			ip->i_db[lbn] = 0;
			--lbn;
			break;
#ifdef DIAGNOSTIC
		case 1:				/* An indirect block. */
			panic("lfs_truncate: lfs_bmaparray returned depth 1");
			/* NOTREACHED */
#endif
		default:			/* Chain of indirect blocks. */
			ap = a + --depth;
			if (ap->in_off > 0 && lbn != lastblock) {
				lbn -= ap->in_off < lbn - lastblock ?
				    ap->in_off : lbn - lastblock;
				break;
			}
			for (; depth && (ap->in_off == 0 || lbn == lastblock);
			    --ap, --depth) {
				/*
				 * XXX
				 * The indirect block may not yet exist, so
				 * bread will create one just so we can free
				 * it.
				 */
				if (bread(vp,
				    ap->in_lbn, fs->lfs_bsize, NOCRED, &bp))
					panic("lfs_truncate: bread bno %d",
					    ap->in_lbn);
				daddrp = bp->b_un.b_daddr + ap->in_off;
				for (i = ap->in_off;
				    i++ <= a_end[depth].in_off;) {
					daddr = *daddrp++;
					SEGDEC;
				}
				a_end[depth].in_off=NINDIR(fs)-1;
				if (ap->in_off > 0 && lbn == lastblock) {
					bzero(bp->b_un.b_daddr + ap->in_off,
					    fs->lfs_bsize - 
					    ap->in_off * sizeof(daddr_t));
					LFS_UBWRITE(bp);
				} else 
					brelse (bp);
			}
			if (a[1].in_off == 0) {
				off = a[0].in_off;
				daddr = ip->i_ib[off];
				SEGDEC;
				ip->i_ib[off] = 0;
			}
			if (lbn == lastblock || lbn <= NDADDR)
				--lbn;
			else {
				lbn -= NINDIR(fs);
				if (lbn < lastblock)
					lbn = lastblock;
			}
		}
	}
	UPDATE_SEGUSE;
	ip->i_blocks -= blocksreleased;
	/* 
	 * XXX
	 * Currently, we don't know when we allocate an indirect block, so
	 * ip->i_blocks isn't getting incremented appropriately.  As a result,
	 * when we delete any indirect blocks, we get a bad number here.
	 */
	if (ip->i_blocks < 0)
		ip->i_blocks = 0;
	ip->i_flag |= ICHG|IUPD;
	(void)vinvalbuf(vp, length > 0); 
	error = lfs_update(vp, &time, &time, MNT_WAIT);
	return (0);
}
