/*
 * Copyright (c) 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_inode.c	7.68 (Berkeley) %G%
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

#include <vm/vm.h>

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
lfs_vget (ap)
	struct vop_vget_args *ap;
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
	ump = VFSTOUFS(ap->a_mp);
	dev = ump->um_dev;
	if ((*ap->a_vpp = ufs_ihashget(dev, ap->a_ino)) != NULL)
		return (0);

	/* Translate the inode number to a disk address. */
	fs = ump->um_lfs;
	if (ap->a_ino == LFS_IFILE_INUM)
		daddr = fs->lfs_idaddr;
	else {
		LFS_IENTRY(ifp, fs, ap->a_ino, bp);
		daddr = ifp->if_daddr;
		brelse(bp);
		if (daddr == LFS_UNUSED_DADDR)
			return (ENOENT);
	}

	/* Allocate new vnode/inode. */
	if (error = lfs_vcreate(ap->a_mp, ap->a_ino, &vp)) {
		*ap->a_vpp = NULL;
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
		*ap->a_vpp = NULL;
		return (error);
	}
	ip->i_din = *lfs_ifind(fs, ap->a_ino, bp->b_un.b_dino);
	brelse(bp);

	/*
	 * Initialize the vnode from the inode, check for aliases.  In all
	 * cases re-init ip, the underlying vnode/inode may have changed.
	 */
	if (error = ufs_vinit(ap->a_mp, lfs_specop_p, LFS_FIFOOPS, &vp)) {
		ufs_iput(ip);
		*ap->a_vpp = NULL;
		return (error);
	}
	/*
	 * Finish inode initialization now that aliasing has been resolved.
	 */
	ip->i_devvp = ump->um_devvp;
	VREF(ip->i_devvp);
	*ap->a_vpp = vp;
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
lfs_update (ap)
	struct vop_update_args *ap;
{
	struct vnode *vp = ap->a_vp;
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
		ip->i_atime.ts_sec = ap->a_ta->tv_sec;
	if (ip->i_flag&IUPD) {
		ip->i_mtime.ts_sec = ap->a_tm->tv_sec;
		(ip)->i_modrev++;
	}
	if (ip->i_flag&ICHG)
		ip->i_ctime.ts_sec = time.tv_sec;
	ip->i_flag &= ~(IUPD|IACC|ICHG|IMOD);

	/* Push back the vnode and any dirty blocks it may have. */
	return (ap->a_waitfor ? lfs_vflush(vp) : 0);
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
lfs_truncate (ap)
	struct vop_truncate_args *ap;
{
	USES_VOP_UPDATE;
	register INDIR *inp;
	register int i;
	register daddr_t *daddrp;
	struct buf *bp, *sup_bp;
	struct ifile *ifp;
	struct inode *ip;
	struct lfs *fs;
	INDIR a[NIADDR + 2], a_end[NIADDR + 2];
	SEGUSE *sup;
	daddr_t daddr, lastblock, lbn, olastblock;
	long off, blocksreleased;
	int error, depth, lastseg, num, offset, seg, size;

#ifdef VERBOSE
	printf("lfs_truncate\n");
#endif
	vnode_pager_setsize(ap->a_vp, (u_long)ap->a_length);

	ip = VTOI(ap->a_vp);
	fs = ip->i_lfs;

	/* If truncating the file to 0, update the version number. */
	if (ap->a_length == 0) {
		LFS_IENTRY(ifp, fs, ip->i_number, bp);
		++ifp->if_version;
		LFS_UBWRITE(bp);
	}

	/* If ap->a_length is larger than the file, just update the times. */
	if (ip->i_size <= ap->a_length) {
		ip->i_flag |= ICHG|IUPD;
		return (VOP_UPDATE(ap->a_vp, &time, &time, 1));
	}

	/*
	 * Calculate index into inode's block list of last direct and indirect
	 * blocks (if any) which we want to keep.  Lastblock is 0 when the
	 * file is truncated to 0.
	 */
	lastblock = lblkno(fs, ap->a_length + fs->lfs_bsize - 1);
	olastblock = lblkno(fs, ip->i_size + fs->lfs_bsize - 1) - 1;

	/*
	 * Update the size of the file. If the file is not being truncated to
	 * a block boundry, the contents of the partial block following the end
	 * of the file must be zero'ed in case it ever become accessable again
	 * because of subsequent file growth.
	 */
	offset = blkoff(fs, ap->a_length);
	if (offset == 0)
		ip->i_size = ap->a_length;
	else {
		lbn = lblkno(fs, ap->a_length);
#ifdef QUOTA
		if (error = getinoquota(ip))
			return (error);
#endif	
		if (error = bread(ap->a_vp, lbn, fs->lfs_bsize, NOCRED, &bp))
			return (error);
		ip->i_size = ap->a_length;
		size = blksize(fs);
		(void)vnode_pager_uncache(ap->a_vp);
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
		lfs_bmaparray(ap->a_vp, lbn, &daddr, a, &depth);
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
			inp = a + --depth;
			if (inp->in_off > 0 && lbn != lastblock) {
				lbn -= inp->in_off < lbn - lastblock ?
				    inp->in_off : lbn - lastblock;
				break;
			}
			for (; depth && (inp->in_off == 0 || lbn == lastblock);
			    --inp, --depth) {
				/*
				 * XXX
				 * The indirect block may not yet exist, so
				 * bread will create one just so we can free
				 * it.
				 */
				if (bread(ap->a_vp,
				    inp->in_lbn, fs->lfs_bsize, NOCRED, &bp))
					panic("lfs_truncate: bread bno %d",
					    inp->in_lbn);
				daddrp = bp->b_un.b_daddr + inp->in_off;
				for (i = inp->in_off;
				    i++ <= a_end[depth].in_off;) {
					daddr = *daddrp++;
					SEGDEC;
				}
				a_end[depth].in_off = NINDIR(fs) - 1;
				if (inp->in_off == 0)
					brelse (bp);
				else {
					bzero(bp->b_un.b_daddr + inp->in_off,
					    fs->lfs_bsize - 
					    inp->in_off * sizeof(daddr_t));
					LFS_UBWRITE(bp);
				}
			}
			if (depth == 0 && a[1].in_off == 0) {
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
	(void)vinvalbuf(ap->a_vp, ap->a_length > 0); 
	error = VOP_UPDATE(ap->a_vp, &time, &time, MNT_WAIT);
	return (0);
}
