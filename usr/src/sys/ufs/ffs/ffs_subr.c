/*	ffs_subr.c	4.3	82/11/13	*/

#ifdef KERNEL
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mount.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/quota.h"
#include "../h/kernel.h"
#else
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mount.h>
#include <sys/fs.h>
#include <sys/conf.h>
#include <sys/buf.h>
#include <sys/inode.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/quota.h>
#include <sys/kernel.h>
#endif

#ifdef KERNEL
int	syncprt = 0;

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
	struct fs *fs;

	if (syncprt)
		bufstats();
	if (updlock)
		return;
	updlock++;
	/*
	 * Write back modified superblocks.
	 * Consistency check that the superblock
	 * of each file system is still in the buffer cache.
	 */
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++) {
		if (mp->m_bufp == NULL)
			continue;
		fs = mp->m_bufp->b_un.b_fs;
		if (fs->fs_fmod == 0)
			continue;
		if (fs->fs_ronly != 0) {		/* XXX */
			printf("fs = %s\n", fs->fs_fsmnt);
			panic("update: rofs mod");
		}
		fs->fs_fmod = 0;
		fs->fs_time = time.tv_sec;
		sbupdate(mp);
	}
	/*
	 * Write back each (modified) inode.
	 */
	for (ip = inode; ip < inodeNINODE; ip++) {
		if ((ip->i_flag & ILOCKED) != 0 || ip->i_count == 0)
			continue;
		ip->i_flag |= ILOCKED;
		ip->i_count++;
		iupdat(ip, &time, &time, 0);
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
 * Flush all the blocks associated with an inode.
 * Note that we make a more stringent check of
 * writing out any block in the buffer pool that may
 * overlap the inode. This brings the inode up to
 * date with recent mods to the cooked device.
 */
syncip(ip)
	register struct inode *ip;
{
	register struct fs *fs;
	long lbn, lastlbn;
	daddr_t blkno;

	fs = ip->i_fs;
	lastlbn = howmany(ip->i_size, fs->fs_bsize);
	for (lbn = 0; lbn < lastlbn; lbn++) {
		blkno = fsbtodb(fs, bmap(ip, lbn, B_READ));
		blkflush(ip->i_dev, blkno, blksize(fs, ip, lbn));
	}
}
#endif

extern	int around[9];
extern	int inside[9];
extern	u_char *fragtbl[];

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
		if ((inblk & (1 << (siz + (fs->fs_frag % NBBY)))) == 0)
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

#ifdef KERNEL
/*
 * Check that a specified block number is in range.
 */
badblock(fs, bn)
	register struct fs *fs;
	daddr_t bn;
{

	if ((unsigned)bn >= fs->fs_size) {
		printf("bad block %d, ", bn);
		fserr(fs, "bad block");
		return (1);
	}
	return (0);
}
#endif

/*
 * block operations
 *
 * check if a block is available
 */
isblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	daddr_t h;
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
		panic("isblock");
		return (NULL);
	}
}

/*
 * take a block out of the map
 */
clrblock(fs, cp, h)
	struct fs *fs;
	u_char *cp;
	daddr_t h;
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
		panic("clrblock");
	}
}

/*
 * put a block into the map
 */
setblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	daddr_t h;
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
		panic("setblock");
	}
}

#ifdef KERNEL
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

	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++) {
		if (mp->m_bufp == NULL || mp->m_dev != dev)
			continue;
		fs = mp->m_bufp->b_un.b_fs;
		if (fs->fs_magic != FS_MAGIC) {
			printf("dev = 0x%x, fs = %s\n", dev, fs->fs_fsmnt);
			panic("getfs: bad magic");
		}
		return (fs);
	}
	printf("dev = 0x%x\n", dev);
	panic("getfs: no fs");
	return (NULL);
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
 * Print out statistics on the current allocation of the buffer pool.
 * Can be enabled to print out on every ``sync'' by setting "syncprt"
 * above.
 */
bufstats()
{
	int s, i, j, count;
	register struct buf *bp, *dp;
	int counts[MAXBSIZE/CLBYTES+1];
	static char *bname[BQUEUES] = { "LOCKED", "LRU", "AGE", "EMPTY" };

	for (bp = bfreelist, i = 0; bp < &bfreelist[BQUEUES]; bp++, i++) {
		count = 0;
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			counts[j] = 0;
		s = spl6();
		for (dp = bp->av_forw; dp != bp; dp = dp->av_forw) {
			counts[dp->b_bufsize/CLBYTES]++;
			count++;
		}
		splx(s);
		printf("%s: total-%d", bname[i], count);
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			if (counts[j] != 0)
				printf(", %d-%d", j * CLBYTES, counts[j]);
		printf("\n");
	}
}
#endif
