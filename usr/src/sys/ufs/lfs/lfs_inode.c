/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)lfs_inode.c	7.16 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mount.h"
#include "user.h"
#include "file.h"
#include "buf.h"
#include "cmap.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "../ufs/fs.h"
#include "../ufs/ufsmount.h"
#ifdef QUOTA
#include "../ufs/quota.h"
#endif
#include "kernel.h"
#include "malloc.h"

#define	INOHSZ	512
#if	((INOHSZ&(INOHSZ-1)) == 0)
#define	INOHASH(dev,ino)	(((dev)+(ino))&(INOHSZ-1))
#else
#define	INOHASH(dev,ino)	(((unsigned)((dev)+(ino)))%INOHSZ)
#endif

union ihead {
	union  ihead *ih_head[2];
	struct inode *ih_chain[2];
} ihead[INOHSZ];

/*
 * Initialize hash links for inodes.
 */
ufs_init()
{
	register int i;
	register union ihead *ih = ihead;

	if (VN_MAXPRIVATE < sizeof(struct inode))
		panic("ihinit: too small");
	for (i = INOHSZ; --i >= 0; ih++) {
		ih->ih_head[0] = ih;
		ih->ih_head[1] = ih;
	}
}

/*
 * Look up an vnode/inode by device,inumber.
 * If it is in core (in the inode structure),
 * honor the locking protocol.
 * If it is not in core, read it in from the
 * specified device.
 * Callers must check for mount points!!
 * In all cases, a pointer to a locked
 * inode structure is returned.
 */
iget(xp, ino, ipp)
	struct inode *xp;
	ino_t ino;
	struct inode **ipp;
{
	dev_t dev = xp->i_dev;
	struct mount *mntp = ITOV(xp)->v_mount;
	register struct fs *fs = VFSTOUFS(mntp)->um_fs;
	extern struct vnodeops ufs_vnodeops, spec_inodeops;
	register struct inode *ip, *iq;
	register struct vnode *vp;
	struct vnode *nvp;
	struct buf *bp;
	struct dinode *dp;
	union  ihead *ih;
	int error;

	ih = &ihead[INOHASH(dev, ino)];
loop:
	for (ip = ih->ih_chain[0]; ip != (struct inode *)ih; ip = ip->i_forw) {
		if (ino != ip->i_number || dev != ip->i_dev)
			continue;
		if ((ip->i_flag&ILOCKED) != 0) {
			ip->i_flag |= IWANT;
			sleep((caddr_t)ip, PINOD);
			goto loop;
		}
		if (vget(ITOV(ip)))
			goto loop;
		*ipp = ip;
		return(0);
	}
	/*
	 * Allocate a new inode.
	 */
	if (error = getnewvnode(VT_UFS, mntp, &ufs_vnodeops, &nvp)) {
		*ipp = 0;
		return (error);
	}
	ip = VTOI(nvp);
	ip->i_vnode = nvp;
	ip->i_flag = 0;
	ip->i_devvp = 0;
	ip->i_lastr = 0;
	ip->i_mode = 0;
	ip->i_flags = 0;
#ifdef QUOTA
	ip->i_dquot = NODQUOT;
#endif
	/*
	 * Put it onto its hash chain and lock it so that other requests for
	 * this inode will block if they arrive while we are sleeping waiting
	 * for old data structures to be purged or for the contents of the
	 * disk portion of this inode to be read.
	 */
	ip->i_dev = dev;
	ip->i_number = ino;
	insque(ip, ih);
	ILOCK(ip);
	/*
	 * Read in the disk contents for the inode.
	 */
	if (error = bread(VFSTOUFS(mntp)->um_devvp, fsbtodb(fs, itod(fs, ino)),
	    (int)fs->fs_bsize, NOCRED, &bp)) {
		/*
		 * Unlock and discard unneeded inode.
		 */
		iput(ip);
		brelse(bp);
		*ipp = 0;
		return (error);
	}
	dp = bp->b_un.b_dino;
	dp += itoo(fs, ino);
	ip->i_din = *dp;
	brelse(bp);
	/*
	 * Initialize the associated vnode
	 */
	vp = ITOV(ip);
	vp->v_type = IFTOVT(ip->i_mode);
	if (vp->v_type == VCHR || vp->v_type == VBLK) {
		vp->v_rdev = ip->i_rdev;
		vp->v_op = &spec_inodeops;
		if (nvp = checkalias(vp, mntp)) {
			/*
			 * Reinitialize aliased inode.
			 */
			vp = nvp;
			iq = VTOI(vp);
			iq->i_vnode = vp;
			iq->i_lastr = 0;
			iq->i_flags = 0;
			ILOCK(iq);
			iq->i_din = ip->i_din;
			iq->i_dev = dev;
			iq->i_number = ino;
			insque(iq, ih);
			/*
			 * Discard unneeded vnode
			 */
			ip->i_mode = 0;
			iput(ip);
			ip = iq;
		}
	}
	if (ino == ROOTINO)
		vp->v_flag |= VROOT;
	/*
	 * Finish inode initialization.
	 */
	ip->i_fs = fs;
	ip->i_devvp = VFSTOUFS(mntp)->um_devvp;
	VREF(ip->i_devvp);
#ifdef QUOTA
	if (ip->i_mode != 0)
		ip->i_dquot = inoquota(ip);
#endif
	/*
	 * Set up a generation number for this inode if it does not
	 * already have one. This should only happen on old filesystems.
	 */
	if (ip->i_gen == 0) {
		if (++nextgennumber < (u_long)time.tv_sec)
			nextgennumber = time.tv_sec;
		ip->i_gen = nextgennumber;
		if ((vp->v_mount->m_flag & M_RDONLY) == 0)
			ip->i_flag |= IMOD;
	}
	*ipp = ip;
	return (0);
}

/*
 * Unlock and decrement the reference count of an inode structure.
 */
iput(ip)
	register struct inode *ip;
{

	if ((ip->i_flag & ILOCKED) == 0)
		panic("iput");
	IUNLOCK(ip);
	vrele(ITOV(ip));
}

/*
 * Last reference to an inode, write the inode out and if necessary,
 * truncate and deallocate the file.
 */
ufs_inactive(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);
	int mode, error = 0;

	if (vp->v_count != 0)
		printf("ufs_inactive: pushing active ino %d dev 0x%x\n",
			ip->i_number, ip->i_dev);
	/*
	 * Get rid of inodes related to stale file handles.
	 */
	if (ip->i_mode == 0) {
		vgone(vp);
		return (0);
	}
	ILOCK(ip);
	if (ip->i_nlink <= 0 && (vp->v_mount->m_flag & M_RDONLY) == 0) {
		error = itrunc(ip, (u_long)0);
		mode = ip->i_mode;
		ip->i_mode = 0;
		ip->i_rdev = 0;
		ip->i_flag |= IUPD|ICHG;
		ifree(ip, ip->i_number, mode);
#ifdef QUOTA
		(void) chkiq(ip->i_dev, ip, ip->i_uid, 0);
		dqrele(ip->i_dquot);
		ip->i_dquot = NODQUOT;
#endif
	}
	IUPDAT(ip, &time, &time, 0);
	IUNLOCK(ip);
	ip->i_flag = 0;
	/*
	 * If we are done with the inode, reclaim it
	 * so that it can be reused immediately.
	 */
	if (vp->v_count == 0 && ip->i_mode == 0)
		vgone(vp);
	return (error);
}

/*
 * Reclaim an inode so that it can be used for other purposes.
 */
ufs_reclaim(vp)
	register struct vnode *vp;
{
	register struct inode *iq, *ip = VTOI(vp);

	if (vp->v_count != 0)
		printf("ufs_reclaim: pushing active ino %d dev 0x%x\n",
			ip->i_number, ip->i_dev);
	/*
	 * Remove the inode from its hash chain.
	 */
	remque(ip);
	ip->i_forw = ip;
	ip->i_back = ip;
	/*
	 * Purge old data structures associated with the inode.
	 */
	cache_purge(vp);
	if (ip->i_devvp) {
		vrele(ip->i_devvp);
		ip->i_devvp = 0;
	}
#ifdef QUOTA
	dqrele(ip->i_dquot);
	ip->i_dquot = NODQUOT;
#endif
	ip->i_flag = 0;
	return (0);
}

/*
 * Check accessed and update flags on an inode structure.
 * If any is on, update the inode with the current time.
 * If waitfor is given, then must ensure I/O order,
 * so wait for write to complete.
 */
iupdat(ip, ta, tm, waitfor)
	register struct inode *ip;
	struct timeval *ta, *tm;
	int waitfor;
{
	struct buf *bp;
	struct vnode *vp = ITOV(ip);
	struct dinode *dp;
	register struct fs *fs;

	fs = ip->i_fs;
	if ((ip->i_flag & (IUPD|IACC|ICHG|IMOD)) == 0)
		return (0);
	if (vp->v_mount->m_flag & M_RDONLY)
		return (0);
	error = bread(ip->i_devvp, fsbtodb(fs, itod(fs, ip->i_number)),
		(int)fs->fs_bsize, NOCRED, &bp);
	if (error) {
		brelse(bp);
		return (error);
	}
	if (ip->i_flag&IACC)
		ip->i_atime = ta->tv_sec;
	if (ip->i_flag&IUPD)
		ip->i_mtime = tm->tv_sec;
	if (ip->i_flag&ICHG)
		ip->i_ctime = time.tv_sec;
	ip->i_flag &= ~(IUPD|IACC|ICHG|IMOD);
	dp = bp->b_un.b_dino + itoo(fs, ip->i_number);
	*dp = ip->i_din;
	if (waitfor) {
		return (bwrite(bp));
	} else {
		bdwrite(bp);
		return (0);
	}
}

#define	SINGLE	0	/* index of single indirect block */
#define	DOUBLE	1	/* index of double indirect block */
#define	TRIPLE	2	/* index of triple indirect block */
/*
 * Truncate the inode ip to at most length size.  Free affected disk
 * blocks -- the blocks of the file are removed in reverse order.
 *
 * NB: triple indirect blocks are untested.
 */
itrunc(oip, length)
	register struct inode *oip;
	u_long length;
{
	register daddr_t lastblock;
	daddr_t bn, lbn, lastiblock[NIADDR];
	register struct fs *fs;
	register struct inode *ip;
	struct buf *bp;
	int offset, osize, size, level;
	long count, nblocks, blocksreleased = 0;
	register int i;
	int error, allerror = 0;
	struct inode tip;

	if (oip->i_size <= length) {
		oip->i_flag |= ICHG|IUPD;
		error = iupdat(oip, &time, &time, 1);
		return (error);
	}
	/*
	 * Calculate index into inode's block list of
	 * last direct and indirect blocks (if any)
	 * which we want to keep.  Lastblock is -1 when
	 * the file is truncated to 0.
	 */
	fs = oip->i_fs;
	lastblock = lblkno(fs, length + fs->fs_bsize - 1) - 1;
	lastiblock[SINGLE] = lastblock - NDADDR;
	lastiblock[DOUBLE] = lastiblock[SINGLE] - NINDIR(fs);
	lastiblock[TRIPLE] = lastiblock[DOUBLE] - NINDIR(fs) * NINDIR(fs);
	nblocks = btodb(fs->fs_bsize);
	/*
	 * Update the size of the file. If the file is not being
	 * truncated to a block boundry, the contents of the
	 * partial block following the end of the file must be
	 * zero'ed in case it ever become accessable again because
	 * of subsequent file growth.
	 */
	osize = oip->i_size;
	offset = blkoff(fs, length);
	if (offset == 0) {
		oip->i_size = length;
	} else {
		lbn = lblkno(fs, length);
		error = balloc(oip, lbn, offset, &bn, B_CLRBUF);
		if (error)
			return (error);
		if ((long)bn < 0)
			panic("itrunc: hole");
		oip->i_size = length;
		size = blksize(fs, oip, lbn);
		count = howmany(size, CLBYTES);
			munhash(oip->i_devvp, bn + i * CLBYTES / DEV_BSIZE);
		error = bread(oip->i_devvp, bn, size, NOCRED, &bp);
		if (error) {
			oip->i_size = osize;
			brelse(bp);
			return (error);
		}
		bzero(bp->b_un.b_addr + offset, (unsigned)(size - offset));
		bdwrite(bp);
	}
	/*
	 * Update file and block pointers
	 * on disk before we start freeing blocks.
	 * If we crash before free'ing blocks below,
	 * the blocks will be returned to the free list.
	 * lastiblock values are also normalized to -1
	 * for calls to indirtrunc below.
	 */
	tip = *oip;
	tip.i_size = osize;
	for (level = TRIPLE; level >= SINGLE; level--)
		if (lastiblock[level] < 0) {
			oip->i_ib[level] = 0;
			lastiblock[level] = -1;
		}
	for (i = NDADDR - 1; i > lastblock; i--)
		oip->i_db[i] = 0;
	oip->i_flag |= ICHG|IUPD;
	allerror = syncip(oip);

	/*
	 * Indirect blocks first.
	 */
	ip = &tip;
	for (level = TRIPLE; level >= SINGLE; level--) {
		bn = ip->i_ib[level];
		if (bn != 0) {
			error = indirtrunc(ip, bn, lastiblock[level], level,
				&count);
			if (error)
				allerror = error;
			blocksreleased += count;
			if (lastiblock[level] < 0) {
				ip->i_ib[level] = 0;
				blkfree(ip, bn, (off_t)fs->fs_bsize);
				blocksreleased += nblocks;
			}
		}
		if (lastiblock[level] >= 0)
			goto done;
	}

	/*
	 * All whole direct blocks or frags.
	 */
	for (i = NDADDR - 1; i > lastblock; i--) {
		register off_t bsize;

		bn = ip->i_db[i];
		if (bn == 0)
			continue;
		ip->i_db[i] = 0;
		bsize = (off_t)blksize(fs, ip, i);
		blkfree(ip, bn, bsize);
		blocksreleased += btodb(bsize);
	}
	if (lastblock < 0)
		goto done;

	/*
	 * Finally, look for a change in size of the
	 * last direct block; release any frags.
	 */
	bn = ip->i_db[lastblock];
	if (bn != 0) {
		off_t oldspace, newspace;

		/*
		 * Calculate amount of space we're giving
		 * back as old block size minus new block size.
		 */
		oldspace = blksize(fs, ip, lastblock);
		ip->i_size = length;
		newspace = blksize(fs, ip, lastblock);
		if (newspace == 0)
			panic("itrunc: newspace");
		if (oldspace - newspace > 0) {
			/*
			 * Block number of space to be free'd is
			 * the old block # plus the number of frags
			 * required for the storage we're keeping.
			 */
			bn += numfrags(fs, newspace);
			blkfree(ip, bn, oldspace - newspace);
			blocksreleased += btodb(oldspace - newspace);
		}
	}
done:
/* BEGIN PARANOIA */
	for (level = SINGLE; level <= TRIPLE; level++)
		if (ip->i_ib[level] != oip->i_ib[level])
			panic("itrunc1");
	for (i = 0; i < NDADDR; i++)
		if (ip->i_db[i] != oip->i_db[i])
			panic("itrunc2");
/* END PARANOIA */
	oip->i_blocks -= blocksreleased;
	if (oip->i_blocks < 0)			/* sanity */
		oip->i_blocks = 0;
	oip->i_flag |= ICHG;
#ifdef QUOTA
	(void) chkdq(oip, -blocksreleased, 0);
#endif
	return (allerror);
}

/*
 * Release blocks associated with the inode ip and
 * stored in the indirect block bn.  Blocks are free'd
 * in LIFO order up to (but not including) lastbn.  If
 * level is greater than SINGLE, the block is an indirect
 * block and recursive calls to indirtrunc must be used to
 * cleanse other indirect blocks.
 *
 * NB: triple indirect blocks are untested.
 */
indirtrunc(ip, bn, lastbn, level, countp)
	register struct inode *ip;
	daddr_t bn, lastbn;
	int level;
	long *countp;
{
	register int i;
	struct buf *bp;
	register struct fs *fs = ip->i_fs;
	register daddr_t *bap;
	daddr_t *copy, nb, last;
	long blkcount, factor;
	int nblocks, blocksreleased = 0;
	int error, allerror = 0;

	/*
	 * Calculate index in current block of last
	 * block to be kept.  -1 indicates the entire
	 * block so we need not calculate the index.
	 */
	factor = 1;
	for (i = SINGLE; i < level; i++)
		factor *= NINDIR(fs);
	last = lastbn;
	if (lastbn > 0)
		last /= factor;
	nblocks = btodb(fs->fs_bsize);
	/*
	 * Get buffer of block pointers, zero those 
	 * entries corresponding to blocks to be free'd,
	 * and update on disk copy first.
	 */
#ifdef SECSIZE
	bp = bread(ip->i_dev, fsbtodb(fs, bn), (int)fs->fs_bsize,
	    fs->fs_dbsize);
#else SECSIZE
	error = bread(ip->i_devvp, fsbtodb(fs, bn), (int)fs->fs_bsize,
		NOCRED, &bp);
	if (error) {
		brelse(bp);
		*countp = 0;
		return (error);
	}
	bap = bp->b_un.b_daddr;
	MALLOC(copy, daddr_t *, fs->fs_bsize, M_TEMP, M_WAITOK);
	bcopy((caddr_t)bap, (caddr_t)copy, (u_int)fs->fs_bsize);
	bzero((caddr_t)&bap[last + 1],
	  (u_int)(NINDIR(fs) - (last + 1)) * sizeof (daddr_t));
	error = bwrite(bp);
	if (error)
		allerror = error;
	bap = copy;

	/*
	 * Recursively free totally unused blocks.
	 */
	for (i = NINDIR(fs) - 1; i > last; i--) {
		nb = bap[i];
		if (nb == 0)
			continue;
		if (level > SINGLE) {
			error = indirtrunc(ip, nb, (daddr_t)-1, level - 1,
				&blkcount);
			if (error)
				allerror = error;
			blocksreleased += blkcount;
		}
		blkfree(ip, nb, (off_t)fs->fs_bsize);
		blocksreleased += nblocks;
	}

	/*
	 * Recursively free last partial block.
	 */
	if (level > SINGLE && lastbn >= 0) {
		last = lastbn % factor;
		nb = bap[i];
		if (nb != 0) {
			error = indirtrunc(ip, nb, last, level - 1, &blkcount);
			if (error)
				allerror = error;
			blocksreleased += blkcount;
		}
	}
	FREE(copy, M_TEMP);
	*countp = blocksreleased;
	return (allerror);
}

/*
 * Remove any inodes in the inode cache belonging to dev.
 *
 * There should not be any active ones, return error if any are found
 * (nb: this is a user error, not a system err).
 */
int busyprt = 0;	/* patch to print out busy inodes */

#ifdef QUOTA
iflush(mp, iq)
	struct mount *mp;
	struct inode *iq;
#else
iflush(mp)
	struct mount *mp;
#endif
{
	register struct vnode *vp, *nvp;
	register struct inode *ip;
	int busy = 0;

	for (vp = mp->m_mounth; vp; vp = nvp) {
		nvp = vp->v_mountf;
		ip = VTOI(vp);
#ifdef QUOTA
		if (ip == iq)
			continue;
#endif
		if (vp->v_count) {
			busy++;
			if (!busyprt)
				continue;
			printf("%s %d on dev 0x%x count %d type %d\n",
			    "iflush: busy inode ", ip->i_number, ip->i_dev,
			    vp->v_count, vp->v_type);
			continue;
		}
		/*
		 * With v_count == 0, all we need to do is clear out the
		 * vnode data structures and we are done.
		 */
		vgone(vp);
	}
	if (busy)
		return (EBUSY);
	return (0);
}

/*
 * Lock an inode. If its already locked, set the WANT bit and sleep.
 */
ilock(ip)
	register struct inode *ip;
{

	while (ip->i_flag & ILOCKED) {
		ip->i_flag |= IWANT;
		(void) sleep((caddr_t)ip, PINOD);
	}
	ip->i_flag |= ILOCKED;
}

/*
 * Unlock an inode.  If WANT bit is on, wakeup.
 */
iunlock(ip)
	register struct inode *ip;
{

	if ((ip->i_flag & ILOCKED) == 0)
		printf("unlocking unlocked inode %d on dev 0x%x\n",
			ip->i_number, ip->i_dev);
	ip->i_flag &= ~ILOCKED;
	if (ip->i_flag&IWANT) {
		ip->i_flag &= ~IWANT;
		wakeup((caddr_t)ip);
	}
}

/*
 * Check mode permission on inode pointer. Mode is READ, WRITE or EXEC.
 * The mode is shifted to select the owner/group/other fields. The
 * super user is granted all permissions.
 *
 * NB: Called from vnode op table. It seems this could all be done
 * using vattr's but...
 */
iaccess(ip, mode, cred)
	register struct inode *ip;
	register int mode;
	struct ucred *cred;
{
	register gid_t *gp;
	int i;

	/*
	 * If you're the super-user, you always get access.
	 */
	if (cred->cr_uid == 0)
		return (0);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group. If not a member of the
	 * group, then check public access.
	 */
	if (cred->cr_uid != ip->i_uid) {
		mode >>= 3;
		gp = cred->cr_groups;
		for (i = 0; i < cred->cr_ngroups; i++, gp++)
			if (ip->i_gid == *gp)
				goto found;
		mode >>= 3;
found:
		;
	}
	if ((ip->i_mode & mode) != 0)
		return (0);
	return (EACCES);
}
