/* Copyright (c) 1981 Regents of the University of California */

static char vers[] = "@(#)ffs_alloc.c 1.3 %G%";

/*	alloc.c	4.8	81/03/08	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mount.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"

long	hashalloc();
long	alloccg();
long	ialloccg();

struct buf *
alloc(dev, ip, bpref, size)
	dev_t dev;
	struct inode *ip;
	daddr_t bpref;
	int size;
{
	daddr_t bno;
	register struct fs *fs;
	struct buf *bp;
	int cg;
	
	fs = getfs(dev);
	if (fs->fs_nbfree == 0 && size == BSIZE)
		goto nospace;
	if (bpref == 0)
		cg = itog(ip->i_number, fs);
	else
		cg = dtog(bpref, fs);
	bno = hashalloc(dev, fs, cg, (long)bpref, size, alloccg);
	if (bno == 0)
		goto nospace;
	bp = getblk(dev, bno, size);
	clrbuf(bp);
	return (bp);
nospace:
	fserr(fs, "file system full");
	uprintf("\n%s: write failed, file system is full\n", fs->fs_fsmnt);
	u.u_error = ENOSPC;
	return (NULL);
}

struct buf *
realloccg(dev, ip, bpref, osize, nsize)
	dev_t dev;
	struct inode *ip;
	daddr_t bpref;
	int osize, nsize;
{
	daddr_t bno;
	register struct fs *fs;
	struct buf *bp;
	int cg;
	
	fs = getfs(dev);
	if (bpref == 0)
		cg = itog(ip->i_number, fs);
	else
		cg = dtog(bpref, fs);
	bno = fragalloc(dev, fs, cg, (long)bpref, osize, nsize);
	if (bno == 0)
		goto nospace;
	bp = getblk(dev, bno, osize);
	bp->b_bcount += nsize - osize;
	blkclr(bp->b_un.b_addr + osize, nsize - osize);
	return (bp);
nospace:
	fserr(fs, "file system full");
	uprintf("\n%s: write failed, file system is full\n", fs->fs_fsmnt);
	u.u_error = ENOSPC;
	return (NULL);
}

struct inode *
ialloc(dev, ipref, mode)
	dev_t dev;
	ino_t ipref;
	int mode;
{
	daddr_t ino;
	register struct fs *fs;
	register struct inode *ip;
	int cg;
	
	fs = getfs(dev);
	if (fs->fs_nifree == 0)
		goto noinodes;
	cg = itog(ipref, fs);
	ino = hashalloc(dev, fs, cg, (long)ipref, mode, ialloccg);
	if (ino == 0)
		goto noinodes;
	ip = iget(dev, ino);
	if (ip == NULL) {
		ifree(dev, ino);
		return (NULL);
	}
	if (ip->i_mode)
		panic("ialloc: dup alloc");
	return (ip);
noinodes:
	fserr(fs, "out of inodes");
	uprintf("\n%s: create failed, no inodes free\n", fs->fs_fsmnt);
	u.u_error = ENOSPC;
	return (NULL);
}

dipref(dev)
	dev_t dev;
{
	register struct fs *fs;
	int cg, minndir, mincg;

	fs = getfs(dev);
	minndir = fs->fs_cs[0].cs_ndir;
	mincg = 0;
	for (cg = 1; cg < fs->fs_ncg; cg++)
		if (fs->fs_cs[cg].cs_ndir < minndir) {
			mincg = cg;
			minndir = fs->fs_cs[cg].cs_ndir;
			if (minndir == 0)
				break;
		}
	return (fs->fs_ipg * mincg);
}

long
hashalloc(dev, fs, cg, pref, size, allocator)
	dev_t dev;
	register struct fs *fs;
	int cg;
	long pref;
	int size;	/* size for data blocks, mode for inodes */
	long (*allocator)();
{
	long result;
	int i, icg = cg;

	/*
	 * 1: preferred cylinder group
	 */
	result = (*allocator)(dev, fs, cg, pref, size);
	if (result)
		return (result);
	/*
	 * 2: quadratic rehash
	 */
	for (i = 1; i < fs->fs_ncg; i *= 2) {
		cg += i;
		if (cg >= fs->fs_ncg)
			cg -= fs->fs_ncg;
		result = (*allocator)(dev, fs, cg, 0, size);
		if (result)
			return (result);
	}
	/*
	 * 3: brute force search
	 */
	cg = icg;
	for (i = 0; i < fs->fs_ncg; i++) {
		result = (*allocator)(dev, fs, cg, 0, size);
		if (result)
			return (result);
		cg++;
		if (cg == fs->fs_ncg)
			cg = 0;
	}
	return (0);
}

daddr_t
fragalloc(dev, fs, cg, pref, osize, nsize)
	dev_t dev;
	register struct fs *fs;
	int cg;
	long pref;
	int osize, nsize;
{
	struct buf *bp;
	struct cg *cgp;
	int i;

	if ((unsigned)osize > BSIZE || osize % FSIZE != 0 ||
	    (unsigned)nsize > BSIZE || nsize % FSIZE != 0)
		panic("fragalloc: bad size");
	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
	if (pref) {
		pref %= fs->fs_fpg;
		for (i = osize / FSIZE; i < nsize / FSIZE; i++) {
			if (isclr(cgp->cg_free, pref + i))
				break;
		}
		if (i == nsize / FSIZE)
			goto extendit;
	}
	/*
	 * MUST FIND ALTERNATE LOCATION
	 */
	panic("fragalloc: reallocation too hard!");
	brelse(bp);
	return (0);
extendit:
	for (i = osize / FSIZE; i < nsize / FSIZE; i++) {
		clrbit(cgp->cg_free, pref + i);
		cgp->cg_nffree--;
		fs->fs_nffree--;
	}
	fs->fs_fmod++;
	bdwrite(bp);
	return (cg * fs->fs_fpg + pref);
}

daddr_t
alloccg(dev, fs, cg, bpref, size)
	dev_t dev;
	struct fs *fs;
	int cg;
	daddr_t bpref;
	int size;
{
	struct buf *bp;
	struct cg *cgp;
	int i;

	if ((unsigned)size > BSIZE || size % FSIZE != 0)
		panic("alloccg: bad size");
	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
	if (bpref) {
		bpref %= fs->fs_fpg;
		if (isblock(cgp->cg_free, bpref/FRAG))
			goto gotit;
	} else
		bpref = cgp->cg_rotor;
	for (i = 0; i < cgp->cg_ndblk; i += FRAG) {
		bpref += FRAG;
		if (bpref >= cgp->cg_ndblk)
			bpref = 0;
		if (isblock(cgp->cg_free, bpref/FRAG)) {
			cgp->cg_rotor = bpref;
			goto gotit;
		}
	}
	brelse(bp);
	return (0);
gotit:
	if (size == BSIZE) {
		clrblock(cgp->cg_free, bpref/FRAG);
		cgp->cg_nbfree--;
		fs->fs_nbfree--;
		fs->fs_cs[cg].cs_nbfree--;
		i = bpref * NSPF;
		cgp->cg_b[i/fs->fs_spc][i%fs->fs_nsect*NRPOS/fs->fs_nsect]--;
	} else {
		cgp->cg_nffree += FRAG;
		fs->fs_nffree += FRAG;
		for (i = 0; i < size / FSIZE; i++) {
			clrbit(cgp->cg_free, bpref + i);
			cgp->cg_nffree--;
			fs->fs_nffree--;
		}
		cgp->cg_nbfree--;
		fs->fs_nbfree--;
		fs->fs_cs[cg].cs_nbfree--;
		i = bpref * NSPF;
		cgp->cg_b[i/fs->fs_spc][i%fs->fs_nsect*NRPOS/fs->fs_nsect]--;
	}
	fs->fs_fmod++;
	bdwrite(bp);
	return (cg * fs->fs_fpg + bpref);
}
	
long
ialloccg(dev, fs, cg, ipref, mode)
	dev_t dev;
	struct fs *fs;
	int cg;
	daddr_t ipref;
	int mode;
{
	struct buf *bp;
	struct cg *cgp;
	int i;

	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
	if (cgp->cg_nifree == 0) {
		brelse(bp);
		return (0);
	}
	if (ipref) {
		ipref %= fs->fs_ipg;
		if (isclr(cgp->cg_iused, ipref))
			goto gotit;
	} else
		ipref = cgp->cg_irotor;
	for (i = 0; i < fs->fs_ipg; i++) {
		ipref++;
		if (ipref >= fs->fs_ipg)
			ipref = 0;
		if (isclr(cgp->cg_iused, ipref)) {
			cgp->cg_irotor = ipref;
			goto gotit;
		}
	}
	brelse(bp);
	return (0);
gotit:
	setbit(cgp->cg_iused, ipref);
	cgp->cg_nifree--;
	fs->fs_nifree--;
	fs->fs_cs[cg].cs_nifree--;
	fs->fs_fmod++;
	if ((mode & IFMT) == IFDIR) {
		cgp->cg_ndir++;
		fs->fs_cs[cg].cs_ndir++;
	}
	bdwrite(bp);
	return (cg * fs->fs_ipg + ipref);
}

fre(dev, bno, size)
	dev_t dev;
	daddr_t bno;
	int size;
{
	register struct fs *fs;
	register struct cg *cgp;
	register struct buf *bp;
	int i;
	int cg;

	if ((unsigned)size > BSIZE || size % FSIZE != 0)
		panic("free: bad size");
	fs = getfs(dev);
	cg = dtog(bno, fs);
	if (badblock(fs, bno))
		return;
	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return;
	cgp = bp->b_un.b_cg;
	bno %= fs->fs_fpg;
	if (size == BSIZE) {
		if (isblock(cgp->cg_free, bno/FRAG))
			panic("free: freeing free block");
		setblock(cgp->cg_free, bno/FRAG);
		cgp->cg_nbfree++;
		fs->fs_nbfree++;
		fs->fs_cs[cg].cs_nbfree++;
		i = bno * NSPF;
		cgp->cg_b[i/fs->fs_spc][i%fs->fs_nsect*NRPOS/fs->fs_nsect]++;
	} else {
		for (i = 0; i < size / FSIZE; i++) {
			if (isset(cgp->cg_free, bno + i))
				panic("free: freeing free frag");
			setbit(cgp->cg_free, bno + i);
			cgp->cg_nffree++;
			fs->fs_nffree++;
		}
		if (isblock(cgp->cg_free, (bno - bno % FRAG) / FRAG)) {
			cgp->cg_nffree -= FRAG;
			fs->fs_nffree -= FRAG;
			cgp->cg_nbfree++;
			fs->fs_nbfree++;
			fs->fs_cs[cg].cs_nbfree++;
			i = bno * NSPF;
			cgp->cg_b[i / fs->fs_spc]
				 [i % fs->fs_nsect * NRPOS / fs->fs_nsect]++;
		}
	}
	fs->fs_fmod++;
	bdwrite(bp);
}

ifree(dev, ino, mode)
	dev_t dev;
	ino_t ino;
	int mode;
{
	register struct fs *fs;
	register struct cg *cgp;
	register struct buf *bp;
	int i;
	int cg;

	fs = getfs(dev);
	if ((unsigned)ino >= fs->fs_ipg*fs->fs_ncg)
		panic("ifree: range");
	cg = itog(ino, fs);
	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return;
	cgp = bp->b_un.b_cg;
	ino %= fs->fs_ipg;
	if (isclr(cgp->cg_iused, ino))
		panic("ifree: freeing free inode");
	clrbit(cgp->cg_iused, ino);
	cgp->cg_nifree++;
	fs->fs_nifree++;
	fs->fs_cs[cg].cs_nifree++;
	if ((mode & IFMT) == IFDIR) {
		cgp->cg_ndir--;
		fs->fs_cs[cg].cs_ndir--;
	}
	fs->fs_fmod++;
	bdwrite(bp);
}

badblock(fs, bn)
	register struct fs *fs;
	daddr_t bn;
{

	if ((unsigned)bn >= fs->fs_size || bn < cgdmin(dtog(bn, fs), fs)) {
		fserr(fs, "bad block");
		return (1);
	}
	return (0);
}

/*
 * getfs maps a device number into
 * a pointer to the incore super
 * block.  The algorithm is a linear
 * search through the mount table.
 * A consistency check of the
 * in core free-block and i-node
 * counts is performed.
 *
 * panic: no fs -- the device is not mounted.
 *	this "cannot happen"
 */
struct fs *
getfs(dev)
	dev_t dev;
{
	register struct mount *mp;
	register struct fs *fs;

	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != NULL && mp->m_dev == dev) {
			fs = mp->m_bufp->b_un.b_fs;
			if (fs->fs_magic != FS_MAGIC)
				panic("getfs: bad magic");
			return (fs);
		}
	panic("getfs: no fs");
	return (NULL);
}

/*
 * Fserr prints the name of a file system
 * with an error diagnostic, in the form
 *	fs: error message
 */
fserr(fs, cp)
	struct fs *fs;
	char *cp;
{

	printf("%s: %s\n", fs->fs_fsmnt, cp);
}

/*
 * Getfsx returns the index in the file system
 * table of the specified device.  The swap device
 * is also assigned a pseudo-index.  The index may
 * be used as a compressed indication of the location
 * of a block, recording
 *	<getfsx(dev),blkno>
 * rather than
 *	<dev, blkno>
 * provided the information need remain valid only
 * as long as the file system is mounted.
 */
getfsx(dev)
	dev_t dev;
{
	register struct mount *mp;

	if (dev == swapdev)
		return (MSWAPX);
	for(mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_dev == dev)
			return (mp - &mount[0]);
	return (-1);
}

/*
 * Update is the internal name of 'sync'.  It goes through the disk
 * queues to initiate sandbagged IO; goes through the inodes to write
 * modified nodes; and it goes through the mount table to initiate modified
 * super blocks.
 */
update()
{
	register struct inode *ip;
	register struct mount *mp;
	register struct buf *bp;
	struct fs *fs;
	time_t tim;
	int i;

	if (updlock)
		return;
	updlock++;
	/*
	 * Write back modified superblocks.
	 * Consistency check that the superblock
	 * of each file system is still in the buffer cache.
	 */
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != NULL) {
			fs = mp->m_bufp->b_un.b_fs;
			if (fs->fs_fmod == 0)
				continue;
			if (fs->fs_ronly != 0)
				panic("update: rofs mod");
			bp = getblk(mp->m_dev, SBLOCK, BSIZE);
			fs->fs_fmod = 0;
			fs->fs_time = TIME;
			if (bp->b_un.b_fs != fs)
				panic("update: bad b_fs");
			bwrite(bp);
			for (i = 0; i < cssize(fs); i += BSIZE) {
				bp = getblk(mp->m_dev, csaddr(fs) + i / FSIZE,
					BSIZE);
				bcopy(fs->fs_cs + i, bp->b_un.b_addr, BSIZE);
				bwrite(bp);
			}
		}
	/*
	 * Write back each (modified) inode.
	 */
	for (ip = inode; ip < inodeNINODE; ip++)
		if((ip->i_flag&ILOCK)==0 && ip->i_count) {
			ip->i_flag |= ILOCK;
			ip->i_count++;
			tim = TIME;
			iupdat(ip, &tim, &tim, 0);
			iput(ip);
		}
	updlock = 0;
	/*
	 * Force stale buffer cache information to be flushed,
	 * for all devices.
	 */
	bflush(NODEV);
}
