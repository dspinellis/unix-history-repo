/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vm_sw.c	7.14 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "user.h"
#include "vnode.h"
#include "specdev.h"
#include "map.h"
#include "file.h"

/*
 * Indirect driver for multi-controller paging.
 */
swstrategy(bp)
	register struct buf *bp;
{
	int sz, off, seg, index;
	register struct swdevt *sp;
	struct vnode *vp;

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
	if ((bp->b_dev = sp->sw_dev) == 0)
		panic("swstrategy");
	if (sp->sw_vp == NULL) {
		bp->b_error |= B_ERROR;
		biodone(bp);
		return;
	}
	VHOLD(sp->sw_vp);
	if ((bp->b_flags & B_READ) == 0) {
		if (vp = bp->b_vp) {
			vp->v_numoutput--;
			if ((vp->v_flag & VBWAIT) && vp->v_numoutput <= 0) {
				vp->v_flag &= ~VBWAIT;
				wakeup((caddr_t)&vp->v_numoutput);
			}
		}
		sp->sw_vp->v_numoutput++;
	}
	if (bp->b_vp != NULL)
		brelvp(bp);
	bp->b_vp = sp->sw_vp;
	VOP_STRATEGY(bp);
}

/*
 * System call swapon(name) enables swapping on device name,
 * which must be in the swdevsw.  Return EBUSY
 * if already swapping on this device.
 */
/* ARGSUSED */
swapon(p, uap, retval)
	struct proc *p;
	struct args {
		char	*name;
	} *uap;
	int *retval;
{
	register struct vnode *vp;
	register struct swdevt *sp;
	register struct nameidata *ndp = &u.u_nd;
	dev_t dev;
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	if (error = namei(ndp))
		return (error);
	vp = ndp->ni_vp;
	if (vp->v_type != VBLK) {
		vrele(vp);
		return (ENOTBLK);
	}
	dev = (dev_t)vp->v_rdev;
	if (major(dev) >= nblkdev) {
		vrele(vp);
		return (ENXIO);
	}
	for (sp = &swdevt[0]; sp->sw_dev; sp++)
		if (sp->sw_dev == dev) {
			if (sp->sw_freed) {
				vrele(vp);
				return (EBUSY);
			}
			sp->sw_vp = vp;
			if (error = swfree(sp - swdevt)) {
				vrele(vp);
				return (error);
			}
			return (0);
		}
	vrele(vp);
	return (EINVAL);
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
	register struct swdevt *sp;
	register swblk_t vsbase;
	register long blk;
	struct vnode *vp;
	register swblk_t dvbase;
	register int nblks;
	int error;

	sp = &swdevt[index];
	vp = sp->sw_vp;
	if (error = VOP_OPEN(vp, FREAD|FWRITE, u.u_cred))
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
			if (argdev_vp)
				vrele(argdev_vp);
			VREF(vp);
			argdev_vp = vp;
			rminit(argmap, (long)(blk/2-ctod(CLSIZE)),
			    (long)ctod(CLSIZE), "argmap", ARGMAPSIZE);
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
		} else
			rmfree(swapmap, blk, vsbase);
	}
	return (0);
}
