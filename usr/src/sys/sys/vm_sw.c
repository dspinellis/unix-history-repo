/*	vm_sw.c	6.1	83/07/29	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/map.h"
#include "../h/uio.h"
#include "../h/file.h"
#include "../h/nami.h"

struct	buf rswbuf;
/*
 * Indirect driver for multi-controller paging.
 */
swstrategy(bp)
	register struct buf *bp;
{
	int sz, off, seg;
	dev_t dev;

#ifdef GENERIC
	/*
	 * A mini-root gets copied into the front of the swap
	 * and we run over top of the swap area just long
	 * enough for us to do a mkfs and restor of the real
	 * root (sure beats rewriting standalone restor).
	 */
#define	MINIROOTSIZE	4096
	if (rootdev == dumpdev)
		bp->b_blkno += MINIROOTSIZE;
#endif
	sz = howmany(bp->b_bcount, DEV_BSIZE);
	if (bp->b_blkno+sz > nswap) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	if (nswdev > 1) {
		off = bp->b_blkno % dmmax;
		if (off+sz > dmmax) {
			bp->b_flags |= B_ERROR;
			iodone(bp);
			return;
		}
		seg = bp->b_blkno / dmmax;
		dev = swdevt[seg % nswdev].sw_dev;
		seg /= nswdev;
		bp->b_blkno = seg*dmmax + off;
	} else
		dev = swdevt[0].sw_dev;
	bp->b_dev = dev;
	if (dev == 0)
		panic("swstrategy");
	(*bdevsw[major(dev)].d_strategy)(bp);
}

swread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (physio(swstrategy, &rswbuf, dev, B_READ, minphys, uio));
}

swwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (physio(swstrategy, &rswbuf, dev, B_WRITE, minphys, uio));
}

/*
 * System call swapon(name) enables swapping on device name,
 * which must be in the swdevsw.  Return EBUSY
 * if already swapping on this device.
 */
swapon()
{
	struct a {
		char	*name;
	};
	register struct inode *ip;
	dev_t dev;
	register struct swdevt *sp;

	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFBLK) {
		u.u_error = ENOTBLK;
		iput(ip);
		return;
	}
	dev = (dev_t)ip->i_rdev;
	iput(ip);
	if (major(dev) >= nblkdev) {
		u.u_error = ENXIO;
		return;
	}
	/*
	 * Search starting at second table entry,
	 * since first (primary swap area) is freed at boot.
	 */
	for (sp = &swdevt[1]; sp->sw_dev; sp++)
		if (sp->sw_dev == dev) {
			if (sp->sw_freed) {
				u.u_error = EBUSY;
				return;
			}
			swfree(sp - swdevt);
			return;
		}
	u.u_error = ENODEV;
}

/*
 * Swfree(index) frees the index'th portion of the swap map.
 * Each of the nswdev devices provides 1/nswdev'th of the swap
 * space, which is laid out with blocks of dmmax pages circularly
 * among the devices.
 */
swfree(index)
	int index;
{
	register swblk_t vsbase;
	register long blk;
	dev_t dev;
	register swblk_t dvbase;
	register int nblks;

	dev = swdevt[index].sw_dev;
	(*bdevsw[major(dev)].d_open)(dev, FREAD|FWRITE);
	swdevt[index].sw_freed = 1;
	nblks = swdevt[index].sw_nblks;
	for (dvbase = 0; dvbase < nblks; dvbase += dmmax) {
		blk = nblks - dvbase;
		if ((vsbase = index*dmmax + dvbase*nswdev) >= nswap)
			panic("swfree");
		if (blk > dmmax)
			blk = dmmax;
		if (vsbase == 0) {
			/*
			 * Can't free a block starting at 0 in the swapmap
			 * but need some space for argmap so use 1/2 this
			 * hunk which needs special treatment anyways.
			 */
			argdev = swdevt[0].sw_dev;
			rminit(argmap, (long)(blk/2-ctod(CLSIZE)),
			    (long)ctod(CLSIZE), "argmap", ARGMAPSIZE);
			/*
			 * First of all chunks... initialize the swapmap
			 * the second half of the hunk.
			 */
			rminit(swapmap, (long)blk/2, (long)blk/2,
			    "swap", nswapmap);
		} else
			rmfree(swapmap, blk, vsbase);
	}
}
