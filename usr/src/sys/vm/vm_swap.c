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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vm_swap.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "user.h"
#include "vnode.h"
#include "map.h"
#include "file.h"
#include "stat.h"

/*
 * Indirect driver for multi-controller paging.
 */
swstrategy(bp)
	register struct buf *bp;
{
	int sz, off, seg, index;
	register struct swdevt *sp;

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
		biodone(bp);
		return;
	}
	if (nswdev > 1) {
		off = bp->b_blkno % dmmax;
		if (off+sz > dmmax) {
			bp->b_flags |= B_ERROR;
			biodone(bp);
			return;
		}
		seg = bp->b_blkno / dmmax;
		index = seg % nswdev;
		seg /= nswdev;
		bp->b_blkno = seg*dmmax + off;
	} else
		index = 0;
	sp = &swdevt[index];
#ifdef SECSIZE
	bp->b_blkno <<= sp->sw_bshift;
	bp->b_blksize = sp->sw_blksize;
#endif SECSIZE
	bp->b_dev = sp->sw_dev;
	if (bp->b_dev == 0)
		panic("swstrategy");
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
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
	} *uap = (struct a *)u.u_ap;
	register struct vnode *vp;
	dev_t dev;
	register struct swdevt *sp;
	register struct nameidata *ndp = &u.u_nd;

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	if (u.u_error = namei(ndp))
		return;
	vp = ndp->ni_vp;
	if (vp->v_type != VBLK) {
		vrele(vp);
		u.u_error = ENOTBLK;
		return;
	}
	dev = (dev_t)vp->v_rdev;
	if (major(dev) >= nblkdev) {
		vrele(vp);
		u.u_error = ENXIO;
		return;
	}
	for (sp = &swdevt[0]; sp->sw_dev; sp++)
		if (sp->sw_dev == dev) {
			if (sp->sw_freed) {
				vrele(vp);
				u.u_error = EBUSY;
				return;
			}
			u.u_error = swfree(sp - swdevt);
			return;
		}
	vrele(vp);
	u.u_error = EINVAL;
}

#ifdef SECSIZE
long	argdbsize;		/* XXX */

#endif SECSIZE
/*
 * Swfree(index) frees the index'th portion of the swap map.
 * Each of the nswdev devices provides 1/nswdev'th of the swap
 * space, which is laid out with blocks of dmmax pages circularly
 * among the devices.
 */
swfree(index)
	int index;
{
	register struct swdevt *sp;
	register struct swdevt *sp;
	register swblk_t vsbase;
	register long blk;
	struct vnode *vp;
	register swblk_t dvbase;
	register int nblks;
	int error;
	int error;

	sp = &swdevt[index];
	dev = sp->sw_dev;
	if (error = (*bdevsw[major(dev)].d_open)(dev, FREAD|FWRITE, S_IFBLK))
		return (error);
	sp->sw_freed = 1;
	nblks = sp->sw_nblks;
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
			argdev = sp->sw_dev;
#ifdef SECSIZE
			argdbsize = sp->sw_blksize;
			rminit(argmap,
			   ((blk / 2) * DEV_BSIZE - CLBYTES) / argdbsize,
			   CLBYTES / argdbsize, "argmap", ARGMAPSIZE);
#else SECSIZE
			rminit(argmap, (long)(blk/2-ctod(CLSIZE)),
			    (long)ctod(CLSIZE), "argmap", ARGMAPSIZE);
#endif SECSIZE
			/*
			 * First of all chunks... initialize the swapmap
			 * the second half of the hunk.
			 */
			rminit(swapmap, (long)(blk/2), (long)(blk/2),
			    "swap", nswapmap);
		} else if (dvbase == 0) {
			/*
			 * Don't use the first cluster of the device
			 * in case it starts with a label or boot block.
			 */
			rmfree(swapmap, blk - ctod(CLSIZE),
			    vsbase + ctod(CLSIZE));
		} else if (dvbase == 0) {
			/*
			 * Don't use the first cluster of the device
			 * in case it starts with a label or boot block.
			 */
			rmfree(swapmap, blk - ctod(CLSIZE),
			    vsbase + ctod(CLSIZE));
		} else
			rmfree(swapmap, blk, vsbase);
	}
	return (0);
	return (0);
}
