/*	lfs_inode.c	4.29	82/10/19	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#ifdef QUOTA
#include "../h/quota.h"
#endif
#include "../h/kernel.h"

#define	INOHSZ	63
#if	((INOHSZ&(INOHSZ-1)) == 0)
#define	INOHASH(dev,ino)	(((dev)+(ino))&(INOHSZ-1))
#else
#define	INOHASH(dev,ino)	(((dev)+(ino))%INOHSZ)
#endif

union ihead {				/* inode LRU cache, Chris Maltby */
	union  ihead *ih_head[2];
	struct inode *ih_chain[2];
} ihead[INOHSZ];

struct inode *ifreeh, **ifreet;

/*
 * Initialize hash links for inodes
 * and build inode free list.
 */
ihinit()
{
	register int i;
	register struct inode *ip = inode;
	register union  ihead *ih = ihead;

	for (i = INOHSZ; --i >= 0; ih++) {
		ih->ih_head[0] = ih;
		ih->ih_head[1] = ih;
	}
	ifreeh = ip;
	ifreet = &ip->i_freef;
	ip->i_freeb = &ifreeh;
	ip->i_forw = ip;
	ip->i_back = ip;
	for (i = ninode; --i > 0; ) {
		++ip;
		ip->i_forw = ip;
		ip->i_back = ip;
		*ifreet = ip;
		ip->i_freeb = ifreet;
		ifreet = &ip->i_freef;
	}
	ip->i_freef = NULL;
}

#ifdef notdef
/*
 * Find an inode if it is incore.
 * This is the equivalent, for inodes,
 * of ``incore'' in bio.c or ``pfind'' in subr.c.
 */
struct inode *
ifind(dev, ino)
	dev_t dev;
	ino_t ino;
{
	register struct inode *ip;
	register union  ihead *ih;

	ih = &ihead[INOHASH(dev, ino)];
	for (ip = ih->ih_chain[0]; ip != (struct inode *)ih; ip = ip->i_forw)
		if (ino==ip->i_number && dev==ip->i_dev)
			return (ip);
	return ((struct inode *)0);
}
#endif notdef

/*
 * Look up an inode by device,inumber.
 * If it is in core (in the inode structure),
 * honor the locking protocol.
 * If it is not in core, read it in from the
 * specified device.
 * If the inode is mounted on, perform
 * the indicated indirection.
 * In all cases, a pointer to a locked
 * inode structure is returned.
 *
 * panic: no imt -- if the mounted file
 *	system is not in the mount table.
 *	"cannot happen"
 */
struct inode *
iget(dev, fs, ino)
	dev_t dev;
	register struct fs *fs;
	ino_t ino;
{
	register struct inode *ip;
	register union  ihead *ih;
	register struct mount *mp;
	register struct buf *bp;
	register struct dinode *dp;
	register struct inode *iq;

loop:
	if (getfs(dev) != fs)
		panic("iget: bad fs");
	ih = &ihead[INOHASH(dev, ino)];
	for (ip = ih->ih_chain[0]; ip != (struct inode *)ih; ip = ip->i_forw)
		if (ino == ip->i_number && dev == ip->i_dev) {
			if ((ip->i_flag&ILOCKED) != 0) {
				ip->i_flag |= IWANT;
				sleep((caddr_t)ip, PINOD);
				goto loop;
			}
			if ((ip->i_flag&IMOUNT) != 0) {
				for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
					if(mp->m_inodp == ip) {
						dev = mp->m_dev;
						fs = mp->m_bufp->b_un.b_fs;
						ino = ROOTINO;
						goto loop;
					}
				panic("no imt");
			}
			if (ip->i_count == 0) {		/* ino on free list */
				if (iq = ip->i_freef)
					iq->i_freeb = ip->i_freeb;
				else
					ifreet = ip->i_freeb;
				*ip->i_freeb = iq;
				ip->i_freef = NULL;
				ip->i_freeb = NULL;
			}
			ip->i_count++;
			ip->i_flag |= ILOCKED;
			return(ip);
		}

	if ((ip = ifreeh) == NULL) {
		tablefull("inode");
		u.u_error = ENFILE;
		return(NULL);
	}
	if (iq = ip->i_freef)
		iq->i_freeb = &ifreeh;
	ifreeh = iq;
	ip->i_freef = NULL;
	ip->i_freeb = NULL;
	/*
	 * Now to take inode off the hash chain it was on
	 * (initially, or after an iflush, it is on a "hash chain"
	 * consisting entirely of itself, and pointed to by no-one,
	 * but that doesn't matter), and put it on the chain for
	 * its new (ino, dev) pair
	 */
	remque(ip);
	insque(ip, ih);
#ifdef QUOTA
	dqrele(ip->i_dquot);
#endif
	ip->i_dev = dev;
	ip->i_fs = fs;
	ip->i_number = ino;
	ip->i_flag = ILOCKED;
	ip->i_count++;
	ip->i_lastr = 0;
	bp = bread(dev, fsbtodb(fs, itod(fs, ino)), (int)fs->fs_bsize);
	/*
	 * Check I/O errors
	 */
	if ((bp->b_flags&B_ERROR) != 0) {
		brelse(bp);
		/*
		 * the inode doesn't contain anything useful, so it would
		 * be misleading to leave it on its hash chain.
		 * 'iput' will take care of putting it back on the free list.
		 */
		remque(ip);
		ip->i_forw = ip;
		ip->i_back = ip;
		/*
		 * we also loose its inumber, just in case (as iput
		 * doesn't do that any more) - but as it isn't on its
		 * hash chain, I doubt if this is really necessary .. kre
		 * (probably the two methods are interchangable)
		 */
		ip->i_number = 0;
#ifdef QUOTA
		ip->i_dquot = NODQUOT;
#endif
		iput(ip);
		return(NULL);
	}
	dp = bp->b_un.b_dino;
	dp += itoo(fs, ino);
	ip->i_ic = dp->di_ic;
	brelse(bp);
#ifdef QUOTA
	if (ip->i_mode == 0)
		ip->i_dquot = NODQUOT;
	else
		ip->i_dquot = inoquota(ip);
#endif
	return (ip);
}

/*
 * Decrement reference count of
 * an inode structure.
 * On the last reference,
 * write the inode out and if necessary,
 * truncate and deallocate the file.
 */
iput(ip)
	register struct inode *ip;
{

	if ((ip->i_flag & ILOCKED) == 0)
		panic("iput");
	iunlock(ip);
	irele(ip);
}

irele(ip)
	register struct inode *ip;
{
	int mode;

	if (ip->i_count == 1) {
		ip->i_flag |= ILOCKED;
		if (ip->i_nlink <= 0) {
			itrunc(ip, 0);
			mode = ip->i_mode;
			ip->i_mode = 0;
			ip->i_rdev = 0;
			ip->i_flag |= IUPD|ICHG;
			ifree(ip, ip->i_number, mode);
#ifdef QUOTA
			chkiq(ip->i_dev, ip, ip->i_uid, 0);
			dqrele(ip->i_dquot);
			ip->i_dquot = NODQUOT;
#endif
		}
		IUPDAT(ip, &time, &time, 0);
		iunlock(ip);
		ip->i_flag = 0;
		/*
		 * Put the inode on the end of the free list.
		 * Possibly in some cases it would be better to
		 * put the inode at the head of the free list,
		 * (eg: where i_mode == 0 || i_number == 0)
		 * but I will think about that later .. kre
		 * (i_number is rarely 0 - only after an i/o error in iget,
		 * where i_mode == 0, the inode will probably be wanted
		 * again soon for an ialloc, so possibly we should keep it)
		 */
		if (ifreeh) {
			*ifreet = ip;
			ip->i_freeb = ifreet;
		} else {
			ifreeh = ip;
			ip->i_freeb = &ifreeh;
		}
		ip->i_freef = NULL;
		ifreet = &ip->i_freef;
	}
	ip->i_count--;
}

/*
 * Check accessed and update flags on
 * an inode structure.
 * If any is on, update the inode
 * with the current time.
 * If waitfor is given, then must insure
 * i/o order so wait for write to complete.
 */
iupdat(ip, ta, tm, waitfor)
	register struct inode *ip;
	struct timeval *ta, *tm;
	int waitfor;
{
	register struct buf *bp;
	struct dinode *dp;
	register struct fs *fp;

	fp = ip->i_fs;
	if ((ip->i_flag & (IUPD|IACC|ICHG)) != 0) {
		if (fp->fs_ronly)
			return;
		bp = bread(ip->i_dev, fsbtodb(fp, itod(fp, ip->i_number)),
			(int)fp->fs_bsize);
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			return;
		}
		if (ip->i_flag&IACC)
			ip->i_atime = ta->tv_sec;
		if (ip->i_flag&IUPD)
			ip->i_mtime = tm->tv_sec;
		if (ip->i_flag&ICHG)
			ip->i_ctime = time.tv_sec;
		ip->i_flag &= ~(IUPD|IACC|ICHG);
		dp = bp->b_un.b_dino + itoo(fp, ip->i_number);
		dp->di_ic = ip->i_ic;
		if (waitfor)
			bwrite(bp);
		else
			bdwrite(bp);
	}
}

/*
 * Truncate the inode ip to at most
 * length size.  Free affected disk
 * blocks -- the blocks of the file
 * are removed in reverse order.
 */
itrunc(ip, length)
	register struct inode *ip;
	register int length;
{
	register i;
	daddr_t bn;
	struct inode itmp;
	register struct fs *fs;
#ifdef QUOTA
	register long cnt = 0;
	long tloop();
#endif
	/*
	 * Only plain files, directories and symbolic
	 * links contain blocks.
	 */
	i = ip->i_mode & IFMT;
	if (i != IFREG && i != IFDIR && i != IFLNK)
		return;
	if (ip->i_size <= length)
		return;

	/*
	 * Clean inode on disk before freeing blocks
	 * to insure no duplicates if system crashes.
	 */
	itmp = *ip;
	itmp.i_size = length;
	for (i = 0; i < NDADDR; i++)
		itmp.i_db[i] = 0;
	for (i = 0; i < NIADDR; i++)
		itmp.i_ib[i] = 0;
	itmp.i_flag |= ICHG|IUPD;
	iupdat(&itmp, &time, &time, 1);
	ip->i_flag &= ~(IUPD|IACC|ICHG);

	/*
	 * Now return blocks to free list... if machine
	 * crashes, they will be harmless MISSING blocks.
	 */
	fs = ip->i_fs;
	/*
	 * release double indirect block first
	 */
	bn = ip->i_ib[NIADDR-1];
	if (bn != (daddr_t)0) {
		ip->i_ib[NIADDR - 1] = (daddr_t)0;
#ifdef QUOTA
		cnt +=
#endif
			tloop(ip, bn, 1);
	}
	/*
	 * release single indirect blocks second
	 */
	for (i = NIADDR - 2; i >= 0; i--) {
		bn = ip->i_ib[i];
		if (bn != (daddr_t)0) {
			ip->i_ib[i] = (daddr_t)0;
#ifdef QUOTA
			cnt +=
#endif
				tloop(ip, bn, 0);
		}
	}
	/*
	 * finally release direct blocks
	 */
	for (i = NDADDR - 1; i>=0; i--) {
		bn = ip->i_db[i];
		if (bn == (daddr_t)0)
			continue;
		ip->i_db[i] = (daddr_t)0;
#ifndef QUOTA
		fre(ip, bn, (off_t)blksize(fs, ip, i));
#else
		fre(ip, bn, size = (off_t)blksize(fs, ip, i));
		cnt += size / DEV_BSIZE;
#endif
	}
	ip->i_size = 0;
	/*
	 * Inode was written and flags updated above.
	 * No need to modify flags here.
	 */
#ifdef QUOTA
	(void) chkdq(ip, -cnt, 0);
#endif
}

#ifdef QUOTA
long
#endif
tloop(ip, bn, indflg)
	register struct inode *ip;
	daddr_t bn;
	int indflg;
{
	register i;
	register struct buf *bp;
	register daddr_t *bap;
	register struct fs *fs;
	daddr_t nb;
#ifdef QUOTA
	register long cnt = 0;
#endif

	bp = NULL;
	fs = ip->i_fs;
	for (i = NINDIR(fs) - 1; i >= 0; i--) {
		if (bp == NULL) {
			bp = bread(ip->i_dev, fsbtodb(fs, bn),
				(int)fs->fs_bsize);
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				return;
			}
			bap = bp->b_un.b_daddr;
		}
		nb = bap[i];
		if (nb == (daddr_t)0)
			continue;
		if (indflg) {
#ifdef QUOTA
			cnt +=
#endif
				tloop(ip, nb, 0);
		} else {
			fre(ip, nb, (int)fs->fs_bsize);
#ifdef QUOTA
			cnt += fs->fs_bsize / DEV_BSIZE;
#endif
		}
	}
	if (bp != NULL)
		brelse(bp);
	fre(ip, bn, (int)fs->fs_bsize);
#ifdef QUOTA
	cnt += fs->fs_bsize / DEV_BSIZE;
	return(cnt);
#endif
}

/*
 * remove any inodes in the inode cache belonging to dev
 *
 * There should not be any active ones, return error if any are found
 * (nb: this is a user error, not a system err)
 *
 * Also, count the references to dev by block devices - this really
 * has nothing to do with the object of the procedure, but as we have
 * to scan the inode table here anyway, we might as well get the
 * extra benefit.
 *
 * this is called from sumount()/sys3.c when dev is being unmounted
 */
#ifdef QUOTA
iflush(dev, iq)
	dev_t dev;
	struct inode *iq;
#else
iflush(dev)
	dev_t dev;
#endif
{
	register struct inode *ip;
	register open = 0;

	for (ip = inode; ip < inodeNINODE; ip++) {
#ifdef QUOTA
		if (ip != iq && ip->i_dev == dev)
#else
		if (ip->i_dev == dev)
#endif
			if (ip->i_count)
				return(-1);
			else {
				remque(ip);
				ip->i_forw = ip;
				ip->i_back = ip;
				/*
				 * as i_count == 0, the inode was on the free
				 * list already, just leave it there, it will
				 * fall off the bottom eventually. We could
				 * perhaps move it to the head of the free
				 * list, but as umounts are done so
				 * infrequently, we would gain very little,
				 * while making the code bigger.
				 */
#ifdef QUOTA
				dqrele(ip->i_dquot);
				ip->i_dquot = NODQUOT;
#endif
			}
		else if (ip->i_count && (ip->i_mode&IFMT)==IFBLK &&
		    ip->i_rdev == dev)
			open++;
	}
	return (open);
}

/*
 * Lock an inode. If its already locked, set the WANT bit and sleep.
 */
ilock(ip)
	register struct inode *ip;
{

	ILOCK(ip);
}

/*
 * Unlock an inode.  If WANT bit is on, wakeup.
 */
iunlock(ip)
	register struct inode *ip;
{

	IUNLOCK(ip);
}
