/* Copyright (c) 1981 Regents of the University of California */

static char vers[] = "@(#)lfs_alloc.c 1.15 %G%";

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

extern u_long		hashalloc();
extern ino_t		ialloccg();
extern daddr_t		alloccg();
extern daddr_t		alloccgblk();
extern daddr_t		fragextend();
extern daddr_t		blkpref();
extern daddr_t		mapsearch();
extern int		inside[], around[];
extern unsigned char	*fragtbl[];

/*
 * Allocate a block in the file system.
 * 
 * The size of the requested block is given, which must be some
 * multiple of fs_fsize and <= fs_bsize.
 * A preference may be optionally specified. If a preference is given
 * the following hierarchy is used to allocate a block:
 *   1) allocate the requested block.
 *   2) allocate a rotationally optimal block in the same cylinder.
 *   3) allocate a block in the same cylinder group.
 *   4) quadradically rehash into other cylinder groups, until an
 *      available block is located.
 * If no block preference is given the following heirarchy is used
 * to allocate a block:
 *   1) allocate a block in the cylinder group that contains the
 *      inode for the file.
 *   2) quadradically rehash into other cylinder groups, until an
 *      available block is located.
 */
struct buf *
alloc(dev, ip, bpref, size)
	dev_t dev;
	register struct inode *ip;
	daddr_t bpref;
	int size;
{
	daddr_t bno;
	register struct fs *fs;
	register struct buf *bp;
	int cg;
	
	fs = getfs(dev);
	if ((unsigned)size > fs->fs_bsize || size % fs->fs_fsize != 0)
		panic("alloc: bad size");
	if (size == fs->fs_bsize && fs->fs_cstotal.cs_nbfree == 0)
		goto nospace;
	if (u.u_uid != 0 &&
	    fs->fs_cstotal.cs_nbfree * fs->fs_frag + fs->fs_cstotal.cs_nffree <
	      fs->fs_dsize * fs->fs_minfree / 100)
		goto nospace;
	if (bpref >= fs->fs_size)
		bpref = 0;
	if (bpref == 0)
		cg = itog(fs, ip->i_number);
	else
		cg = dtog(fs, bpref);
	bno = (daddr_t)hashalloc(dev, fs, cg, (long)bpref, size, alloccg);
	if (bno == 0)
		goto nospace;
	bp = getblk(dev, fsbtodb(fs, bno), size);
	clrbuf(bp);
	return (bp);
nospace:
	fserr(fs, "file system full");
	uprintf("\n%s: write failed, file system is full\n", fs->fs_fsmnt);
	u.u_error = ENOSPC;
	return (NULL);
}

/*
 * Reallocate a fragment to a bigger size
 *
 * The number and size of the old block is given, and a preference
 * and new size is also specified. The allocator attempts to extend
 * the original block. Failing that, the regular block allocator is
 * invoked to get an appropriate block.
 */
struct buf *
realloccg(dev, bprev, bpref, osize, nsize)
	dev_t dev;
	daddr_t bprev, bpref;
	int osize, nsize;
{
	daddr_t bno;
	register struct fs *fs;
	register struct buf *bp, *obp;
	caddr_t cp;
	int cg;
	
	fs = getfs(dev);
	if ((unsigned)osize > fs->fs_bsize || osize % fs->fs_fsize != 0 ||
	    (unsigned)nsize > fs->fs_bsize || nsize % fs->fs_fsize != 0)
		panic("realloccg: bad size");
	if (u.u_uid != 0 &&
	    fs->fs_cstotal.cs_nbfree * fs->fs_frag + fs->fs_cstotal.cs_nffree <
	      fs->fs_dsize * fs->fs_minfree / 100)
		goto nospace;
	if (bprev != 0)
		cg = dtog(fs, bprev);
	else
		panic("realloccg: bad bprev");
	bno = fragextend(dev, fs, cg, (long)bprev, osize, nsize);
	if (bno != 0) {
		bp = bread(dev, fsbtodb(fs, bno), osize);
		if (bp->b_flags & B_ERROR)
			return (0);
		bp->b_bcount = nsize;
		blkclr(bp->b_un.b_addr + osize, nsize - osize);
		return (bp);
	}
	if (bpref >= fs->fs_size)
		bpref = 0;
	bno = (daddr_t)hashalloc(dev, fs, cg, (long)bpref, nsize, alloccg);
	if (bno != 0) {
		/*
		 * make a new copy
		 */
		obp = bread(dev, fsbtodb(fs, bprev), osize);
		if (obp->b_flags & B_ERROR)
			return (0);
		bp = getblk(dev, fsbtodb(fs, bno), nsize);
		cp = bp->b_un.b_addr;
		bp->b_un.b_addr = obp->b_un.b_addr;
		obp->b_un.b_addr = cp;
		obp->b_flags |= B_INVAL;
		brelse(obp);
		fre(dev, bprev, (off_t)osize);
		blkclr(bp->b_un.b_addr + osize, nsize - osize);
		return(bp);
	}
nospace:
	/*
	 * no space available
	 */
	fserr(fs, "file system full");
	uprintf("\n%s: write failed, file system is full\n", fs->fs_fsmnt);
	u.u_error = ENOSPC;
	return (NULL);
}

/*
 * Allocate an inode in the file system.
 * 
 * A preference may be optionally specified. If a preference is given
 * the following hierarchy is used to allocate an inode:
 *   1) allocate the requested inode.
 *   2) allocate an inode in the same cylinder group.
 *   3) quadradically rehash into other cylinder groups, until an
 *      available inode is located.
 * If no inode preference is given the following heirarchy is used
 * to allocate an inode:
 *   1) allocate an inode in cylinder group 0.
 *   2) quadradically rehash into other cylinder groups, until an
 *      available inode is located.
 */
struct inode *
ialloc(dev, ipref, mode)
	dev_t dev;
	ino_t ipref;
	int mode;
{
	ino_t ino;
	register struct fs *fs;
	register struct inode *ip;
	int cg;
	
	fs = getfs(dev);
	if (fs->fs_cstotal.cs_nifree == 0)
		goto noinodes;
	if (ipref >= fs->fs_ncg * fs->fs_ipg)
		ipref = 0;
	cg = itog(fs, ipref);
	ino = (ino_t)hashalloc(dev, fs, cg, (long)ipref, mode, ialloccg);
	if (ino == 0)
		goto noinodes;
	ip = iget(dev, ino);
	if (ip == NULL) {
		ifree(dev, ino, 0);
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

/*
 * Find a cylinder to place a directory.
 *
 * The policy implemented by this algorithm is to select from
 * among those cylinder groups with above the average number of
 * free inodes, the one with the smallest number of directories.
 */
dirpref(dev)
	dev_t dev;
{
	register struct fs *fs;
	int cg, minndir, mincg, avgifree;

	fs = getfs(dev);
	avgifree = fs->fs_cstotal.cs_nifree / fs->fs_ncg;
	minndir = fs->fs_ipg;
	mincg = 0;
	for (cg = 0; cg < fs->fs_ncg; cg++)
		if (fs->fs_cs(fs, cg).cs_ndir < minndir &&
		    fs->fs_cs(fs, cg).cs_nifree >= avgifree) {
			mincg = cg;
			minndir = fs->fs_cs(fs, cg).cs_ndir;
		}
	return (fs->fs_ipg * mincg);
}

/*
 * Select a cylinder to place a large block of data.
 *
 * The policy implemented by this algorithm is to maintain a
 * rotor that sweeps the cylinder groups. When a block is 
 * needed, the rotor is advanced until a cylinder group with
 * greater than the average number of free blocks is found.
 */
daddr_t
blkpref(dev)
	dev_t dev;
{
	register struct fs *fs;
	int cg, avgbfree;

	fs = getfs(dev);
	avgbfree = fs->fs_cstotal.cs_nbfree / fs->fs_ncg;
	for (cg = fs->fs_cgrotor + 1; cg < fs->fs_ncg; cg++)
		if (fs->fs_cs(fs, cg).cs_nbfree >= avgbfree) {
			fs->fs_cgrotor = cg;
			return (fs->fs_fpg * cg + fs->fs_frag);
		}
	for (cg = 0; cg <= fs->fs_cgrotor; cg++)
		if (fs->fs_cs(fs, cg).cs_nbfree >= avgbfree) {
			fs->fs_cgrotor = cg;
			return (fs->fs_fpg * cg + fs->fs_frag);
		}
	return (0);
}

/*
 * Implement the cylinder overflow algorithm.
 *
 * The policy implemented by this algorithm is:
 *   1) allocate the block in its requested cylinder group.
 *   2) quadradically rehash on the cylinder group number.
 *   3) brute force search for a free block.
 */
/*VARARGS5*/
u_long
hashalloc(dev, fs, cg, pref, size, allocator)
	dev_t dev;
	register struct fs *fs;
	int cg;
	long pref;
	int size;	/* size for data blocks, mode for inodes */
	u_long (*allocator)();
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

/*
 * Determine whether a fragment can be extended.
 *
 * Check to see if the necessary fragments are available, and 
 * if they are, allocate them.
 */
daddr_t
fragextend(dev, fs, cg, bprev, osize, nsize)
	dev_t dev;
	register struct fs *fs;
	int cg;
	long bprev;
	int osize, nsize;
{
	register struct buf *bp;
	register struct cg *cgp;
	long bno;
	int frags, bbase;
	int i;

	frags = nsize / fs->fs_fsize;
	bbase = bprev % fs->fs_frag;
	if (bbase > (bprev + frags - 1) % fs->fs_frag) {
		/* cannot extend across a block boundry */
		return (0);
	}
	bp = bread(dev, fsbtodb(fs, cgtod(fs, cg)), fs->fs_bsize);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
	bno = dtogd(fs, bprev);
	for (i = osize / fs->fs_fsize; i < frags; i++)
		if (isclr(cgp->cg_free, bno + i)) {
			brelse(bp);
			return (0);
		}
	/*
	 * the current fragment can be extended
	 * deduct the count on fragment being extended into
	 * increase the count on the remaining fragment (if any)
	 * allocate the extended piece
	 */
	for (i = frags; i < fs->fs_frag - bbase; i++)
		if (isclr(cgp->cg_free, bno + i))
			break;
	cgp->cg_frsum[i - osize / fs->fs_fsize]--;
	if (i != frags)
		cgp->cg_frsum[i - frags]++;
	for (i = osize / fs->fs_fsize; i < frags; i++) {
		clrbit(cgp->cg_free, bno + i);
		cgp->cg_cs.cs_nffree--;
		fs->fs_cstotal.cs_nffree--;
		fs->fs_cs(fs, cg).cs_nffree--;
	}
	fs->fs_fmod++;
	bdwrite(bp);
	return (bprev);
}

/*
 * Determine whether a block can be allocated.
 *
 * Check to see if a block of the apprpriate size is available,
 * and if it is, allocate it.
 */
daddr_t
alloccg(dev, fs, cg, bpref, size)
	dev_t dev;
	register struct fs *fs;
	int cg;
	daddr_t bpref;
	int size;
{
	register struct buf *bp;
	register struct cg *cgp;
	int bno, frags;
	int allocsiz;
	register int i;

	if (fs->fs_cs(fs, cg).cs_nbfree == 0 && size == fs->fs_bsize)
		return (0);
	bp = bread(dev, fsbtodb(fs, cgtod(fs, cg)), fs->fs_bsize);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
	if (size == fs->fs_bsize) {
		bno = alloccgblk(fs, cgp, bpref);
		bdwrite(bp);
		return (bno);
	}
	/*
	 * check to see if any fragments are already available
	 * allocsiz is the size which will be allocated, hacking
	 * it down to a smaller size if necessary
	 */
	frags = size / fs->fs_fsize;
	for (allocsiz = frags; allocsiz < fs->fs_frag; allocsiz++)
		if (cgp->cg_frsum[allocsiz] != 0)
			break;
	if (allocsiz == fs->fs_frag) {
		/*
		 * no fragments were available, so a block will be 
		 * allocated, and hacked up
		 */
		if (cgp->cg_cs.cs_nbfree == 0) {
			brelse(bp);
			return (0);
		}
		bno = alloccgblk(fs, cgp, bpref);
		bpref = dtogd(fs, bno);
		for (i = frags; i < fs->fs_frag; i++)
			setbit(cgp->cg_free, bpref + i);
		i = fs->fs_frag - frags;
		cgp->cg_cs.cs_nffree += i;
		fs->fs_cstotal.cs_nffree += i;
		fs->fs_cs(fs, cg).cs_nffree += i;
		cgp->cg_frsum[i]++;
		bdwrite(bp);
		return (bno);
	}
	bno = mapsearch(fs, cgp, bpref, allocsiz);
	if (bno == 0)
		return (0);
	for (i = 0; i < frags; i++)
		clrbit(cgp->cg_free, bno + i);
	cgp->cg_cs.cs_nffree -= frags;
	fs->fs_cstotal.cs_nffree -= frags;
	fs->fs_cs(fs, cg).cs_nffree -= frags;
	cgp->cg_frsum[allocsiz]--;
	if (frags != allocsiz)
		cgp->cg_frsum[allocsiz - frags]++;
	bdwrite(bp);
	return (cg * fs->fs_fpg + bno);
}

/*
 * Allocate a block in a cylinder group.
 *
 * This algorithm implements the following policy:
 *   1) allocate the requested block.
 *   2) allocate a rotationally optimal block in the same cylinder.
 *   3) allocate the next available block on the block rotor for the
 *      specified cylinder group.
 * Note that this routine only allocates fs_bsize blocks; these
 * blocks may be fragmented by the routine that allocates them.
 */
daddr_t
alloccgblk(fs, cgp, bpref)
	struct fs *fs;
	register struct cg *cgp;
	daddr_t bpref;
{
	daddr_t bno;
	int cylno, pos;
	short *cylbp;
	register int i;

	if (bpref == 0) {
		bpref = cgp->cg_rotor;
		goto norot;
	}
	bpref &= ~(fs->fs_frag - 1);
	bpref = dtogd(fs, bpref);
	/*
	 * if the requested block is available, use it
	 */
	if (isblock(fs, cgp->cg_free, bpref/fs->fs_frag)) {
		bno = bpref;
		goto gotit;
	}
	/*
	 * check for a block available on the same cylinder
	 */
	cylno = cbtocylno(fs, bpref);
	if (cgp->cg_btot[cylno] == 0)
		goto norot;
	if (fs->fs_cpc == 0) {
		/*
		 * block layout info is not available, so just have
		 * to take any block in this cylinder.
		 */
		bpref = howmany(fs->fs_spc * cylno, NSPF(fs));
		goto norot;
	}
	/*
	 * find a block that is rotationally optimal
	 */
	cylbp = cgp->cg_b[cylno];
	if (fs->fs_rotdelay == 0) {
		pos = cbtorpos(fs, bpref);
	} else {
		/*
		 * here we convert ms of delay to frags as:
		 * (frags) = (ms) * (rev/sec) * (sect/rev) /
		 *	((sect/frag) * (ms/sec))
		 * then round up to the next rotational position
		 */
		bpref += fs->fs_rotdelay * fs->fs_rps * fs->fs_nsect /
		    (NSPF(fs) * 1000);
		pos = cbtorpos(fs, bpref);
		pos = (pos + 1) % NRPOS;
	}
	/*
	 * check the summary information to see if a block is 
	 * available in the requested cylinder starting at the
	 * optimal rotational position and proceeding around.
	 */
	for (i = pos; i < NRPOS; i++)
		if (cylbp[i] > 0)
			break;
	if (i == NRPOS)
		for (i = 0; i < pos; i++)
			if (cylbp[i] > 0)
				break;
	if (cylbp[i] > 0) {
		/*
		 * found a rotational position, now find the actual
		 * block. A panic if none is actually there.
		 */
		pos = cylno % fs->fs_cpc;
		bno = (cylno - pos) * fs->fs_spc / NSPB(fs);
		if (fs->fs_postbl[pos][i] == -1)
			panic("alloccgblk: cyl groups corrupted");
		for (i = fs->fs_postbl[pos][i]; ; i += fs->fs_rotbl[i]) {
			if (isblock(fs, cgp->cg_free, bno + i)) {
				bno = (bno + i) * fs->fs_frag;
				goto gotit;
			}
			if (fs->fs_rotbl[i] == 0)
				break;
		}
		panic("alloccgblk: can't find blk in cyl");
	}
norot:
	/*
	 * no blocks in the requested cylinder, so take next
	 * available one in this cylinder group.
	 */
	bno = mapsearch(fs, cgp, bpref, fs->fs_frag);
	if (bno == 0)
		return (0);
	cgp->cg_rotor = bno;
gotit:
	clrblock(fs, cgp->cg_free, bno/fs->fs_frag);
	cgp->cg_cs.cs_nbfree--;
	fs->fs_cstotal.cs_nbfree--;
	fs->fs_cs(fs, cgp->cg_cgx).cs_nbfree--;
	cylno = cbtocylno(fs, bno);
	cgp->cg_b[cylno][cbtorpos(fs, bno)]--;
	cgp->cg_btot[cylno]--;
	fs->fs_fmod++;
	return (cgp->cg_cgx * fs->fs_fpg + bno);
}
	
/*
 * Determine whether an inode can be allocated.
 *
 * Check to see if an inode is available, and if it is,
 * allocate it using the following policy:
 *   1) allocate the requested inode.
 *   2) allocate the next available inode after the requested
 *      inode in the specified cylinder group.
 */
ino_t
ialloccg(dev, fs, cg, ipref, mode)
	dev_t dev;
	register struct fs *fs;
	int cg;
	daddr_t ipref;
	int mode;
{
	register struct buf *bp;
	register struct cg *cgp;
	int i;

	if (fs->fs_cs(fs, cg).cs_nifree == 0)
		return (0);
	bp = bread(dev, fsbtodb(fs, cgtod(fs, cg)), fs->fs_bsize);
	if (bp->b_flags & B_ERROR)
		return (0);
	cgp = bp->b_un.b_cg;
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
	cgp->cg_cs.cs_nifree--;
	fs->fs_cstotal.cs_nifree--;
	fs->fs_cs(fs, cg).cs_nifree--;
	fs->fs_fmod++;
	if ((mode & IFMT) == IFDIR) {
		cgp->cg_cs.cs_ndir++;
		fs->fs_cstotal.cs_ndir++;
		fs->fs_cs(fs, cg).cs_ndir++;
	}
	bdwrite(bp);
	return (cg * fs->fs_ipg + ipref);
}

/*
 * Free a block or fragment.
 *
 * The specified block or fragment is placed back in the
 * free map. If a fragment is deallocated, a possible 
 * block reassembly is checked.
 */
fre(dev, bno, size)
	dev_t dev;
	daddr_t bno;
	off_t size;
{
	register struct fs *fs;
	register struct cg *cgp;
	register struct buf *bp;
	int cg, blk, frags, bbase;
	register int i;

	fs = getfs(dev);
	if ((unsigned)size > fs->fs_bsize || size % fs->fs_fsize != 0)
		panic("free: bad size");
	cg = dtog(fs, bno);
	if (badblock(fs, bno))
		return;
	bp = bread(dev, fsbtodb(fs, cgtod(fs, cg)), fs->fs_bsize);
	if (bp->b_flags & B_ERROR)
		return;
	cgp = bp->b_un.b_cg;
	bno = dtogd(fs, bno);
	if (size == fs->fs_bsize) {
		if (isblock(fs, cgp->cg_free, bno/fs->fs_frag))
			panic("free: freeing free block");
		setblock(fs, cgp->cg_free, bno/fs->fs_frag);
		cgp->cg_cs.cs_nbfree++;
		fs->fs_cstotal.cs_nbfree++;
		fs->fs_cs(fs, cg).cs_nbfree++;
		i = cbtocylno(fs, bno);
		cgp->cg_b[i][cbtorpos(fs, bno)]++;
		cgp->cg_btot[i]++;
	} else {
		bbase = bno - (bno % fs->fs_frag);
		/*
		 * decrement the counts associated with the old frags
		 */
		blk = ((cgp->cg_free[bbase / NBBY] >> (bbase % NBBY)) &
		       (0xff >> (NBBY - fs->fs_frag)));
		fragacct(fs, blk, cgp->cg_frsum, -1);
		/*
		 * deallocate the fragment
		 */
		frags = size / fs->fs_fsize;
		for (i = 0; i < frags; i++) {
			if (isset(cgp->cg_free, bno + i))
				panic("free: freeing free frag");
			setbit(cgp->cg_free, bno + i);
			cgp->cg_cs.cs_nffree++;
			fs->fs_cstotal.cs_nffree++;
			fs->fs_cs(fs, cg).cs_nffree++;
		}
		/*
		 * add back in counts associated with the new frags
		 */
		blk = ((cgp->cg_free[bbase / NBBY] >> (bbase % NBBY)) &
		       (0xff >> (NBBY - fs->fs_frag)));
		fragacct(fs, blk, cgp->cg_frsum, 1);
		/*
		 * if a complete block has been reassembled, account for it
		 */
		if (isblock(fs, cgp->cg_free, bbase / fs->fs_frag)) {
			cgp->cg_cs.cs_nffree -= fs->fs_frag;
			fs->fs_cstotal.cs_nffree -= fs->fs_frag;
			fs->fs_cs(fs, cg).cs_nffree -= fs->fs_frag;
			cgp->cg_cs.cs_nbfree++;
			fs->fs_cstotal.cs_nbfree++;
			fs->fs_cs(fs, cg).cs_nbfree++;
			i = cbtocylno(fs, bbase);
			cgp->cg_b[i][cbtorpos(fs, bbase)]++;
			cgp->cg_btot[i]++;
		}
	}
	fs->fs_fmod++;
	bdwrite(bp);
}

/*
 * Free an inode.
 *
 * The specified inode is placed back in the free map.
 */
ifree(dev, ino, mode)
	dev_t dev;
	ino_t ino;
	int mode;
{
	register struct fs *fs;
	register struct cg *cgp;
	register struct buf *bp;
	int cg;

	fs = getfs(dev);
	if ((unsigned)ino >= fs->fs_ipg*fs->fs_ncg)
		panic("ifree: range");
	cg = itog(fs, ino);
	bp = bread(dev, fsbtodb(fs, cgtod(fs, cg)), fs->fs_bsize);
	if (bp->b_flags & B_ERROR)
		return;
	cgp = bp->b_un.b_cg;
	ino %= fs->fs_ipg;
	if (isclr(cgp->cg_iused, ino))
		panic("ifree: freeing free inode");
	clrbit(cgp->cg_iused, ino);
	cgp->cg_cs.cs_nifree++;
	fs->fs_cstotal.cs_nifree++;
	fs->fs_cs(fs, cg).cs_nifree++;
	if ((mode & IFMT) == IFDIR) {
		cgp->cg_cs.cs_ndir--;
		fs->fs_cstotal.cs_ndir--;
		fs->fs_cs(fs, cg).cs_ndir--;
	}
	fs->fs_fmod++;
	bdwrite(bp);
}

/*
 * Find a block of the specified size in the specified cylinder group.
 *
 * It is a panic if a request is made to find a block if none are
 * available.
 */
daddr_t
mapsearch(fs, cgp, bpref, allocsiz)
	register struct fs *fs;
	register struct cg *cgp;
	daddr_t bpref;
	int allocsiz;
{
	daddr_t bno;
	int start, len, loc, i;
	int blk, field, subfield, pos;

	/*
	 * find the fragment by searching through the free block
	 * map for an appropriate bit pattern
	 */
	if (bpref)
		start = dtogd(fs, bpref) / NBBY;
	else
		start = cgp->cg_frotor / NBBY;
	len = roundup(fs->fs_fpg - 1, NBBY) / NBBY - start;
	loc = scanc(len, &cgp->cg_free[start], fragtbl[fs->fs_frag],
		1 << (allocsiz - 1));
	if (loc == 0) {
		len = start - 1;
		start = fs->fs_dblkno / NBBY;
		loc = scanc(len, &cgp->cg_free[start], fragtbl[fs->fs_frag],
			1 << (allocsiz - 1));
		if (loc == 0) {
			panic("alloccg: map corrupted");
			return (0);
		}
	}
	bno = (start + len - loc) * NBBY;
	cgp->cg_frotor = bno;
	/*
	 * found the byte in the map
	 * sift through the bits to find the selected frag
	 */
	for (i = 0; i < NBBY; i += fs->fs_frag) {
		blk = (cgp->cg_free[bno / NBBY] >> i) &
		      (0xff >> NBBY - fs->fs_frag);
		blk <<= 1;
		field = around[allocsiz];
		subfield = inside[allocsiz];
		for (pos = 0; pos <= fs->fs_frag - allocsiz; pos++) {
			if ((blk & field) == subfield) {
				return (bno + i + pos);
			}
			field <<= 1;
			subfield <<= 1;
		}
	}
	panic("alloccg: block not in map");
	return (0);
}

/*
 * Update the frsum fields to reflect addition or deletion 
 * of some frags.
 */
fragacct(fs, fragmap, fraglist, cnt)
	struct fs *fs;
	int fragmap;
	long fraglist[];
	int cnt;
{
	int inblk;
	register int field, subfield;
	register int siz, pos;

	inblk = (int)(fragtbl[fs->fs_frag][fragmap]) << 1;
	fragmap <<= 1;
	for (siz = 1; siz < fs->fs_frag; siz++) {
		if (((1 << siz) & inblk) == 0)
			continue;
		field = around[siz];
		subfield = inside[siz];
		for (pos = siz; pos <= fs->fs_frag; pos++) {
			if ((fragmap & field) == subfield) {
				fraglist[siz] += cnt;
				pos += siz;
				field <<= siz;
				subfield <<= siz;
			}
			field <<= 1;
			subfield <<= 1;
		}
	}
}

/*
 * Check that a specified block number is in range.
 */
badblock(fs, bn)
	register struct fs *fs;
	daddr_t bn;
{

	if ((unsigned)bn >= fs->fs_size || bn < cgdmin(fs, dtog(fs, bn))) {
		fserr(fs, "bad block");
		return (1);
	}
	return (0);
}

/*
 * Getfs maps a device number into a pointer to the incore super block.
 *
 * The algorithm is a linear search through the mount table. A
 * consistency check of the super block magic number is performed.
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
 * Fserr prints the name of a file system with an error diagnostic.
 * 
 * The form of the error message is:
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
 * modified nodes; and it goes through the mount table to initiate
 * the writing of the modified super blocks.
 */
update()
{
	register struct inode *ip;
	register struct mount *mp;
	register struct buf *bp;
	struct fs *fs;
	time_t tim;
	int i, blks;

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
			bp = getblk(mp->m_dev, SBLOCK, SBSIZE);
			fs->fs_fmod = 0;
			fs->fs_time = TIME;
			if (bp->b_un.b_fs != fs)
				panic("update: bad b_fs");
			bwrite(bp);
			blks = howmany(fs->fs_cssize, fs->fs_bsize);
			for (i = 0; i < blks; i++) {
				bp = getblk(mp->m_dev,
				    fsbtodb(fs, fs->fs_csaddr + (i * fs->fs_frag)),
				    fs->fs_bsize);
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

/*
 * block operations
 *
 * check if a block is available
 */
isblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	unsigned char mask;

	switch (fs->fs_frag) {
	case 8:
		return (cp[h] == 0xff);
	case 4:
		mask = 0x0f << ((h & 0x1) << 2);
		return ((cp[h >> 1] & mask) == mask);
	case 2:
		mask = 0x03 << ((h & 0x3) << 1);
		return ((cp[h >> 2] & mask) == mask);
	case 1:
		mask = 0x01 << (h & 0x7);
		return ((cp[h >> 3] & mask) == mask);
	default:
		panic("isblock bad fs_frag");
		return;
	}
}

/*
 * take a block out of the map
 */
clrblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	switch ((fs)->fs_frag) {
	case 8:
		cp[h] = 0;
		return;
	case 4:
		cp[h >> 1] &= ~(0x0f << ((h & 0x1) << 2));
		return;
	case 2:
		cp[h >> 2] &= ~(0x03 << ((h & 0x3) << 1));
		return;
	case 1:
		cp[h >> 3] &= ~(0x01 << (h & 0x7));
		return;
	default:
		panic("clrblock bad fs_frag");
		return;
	}
}

/*
 * put a block into the map
 */
setblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	switch (fs->fs_frag) {
	case 8:
		cp[h] = 0xff;
		return;
	case 4:
		cp[h >> 1] |= (0x0f << ((h & 0x1) << 2));
		return;
	case 2:
		cp[h >> 2] |= (0x03 << ((h & 0x3) << 1));
		return;
	case 1:
		cp[h >> 3] |= (0x01 << (h & 0x7));
		return;
	default:
		panic("setblock bad fs_frag");
		return;
	}
}
