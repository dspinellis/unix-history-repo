/*
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_swap.c	8.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/namei.h>
#include <sys/dmap.h>		/* XXX */
#include <sys/vnode.h>
#include <sys/map.h>
#include <sys/file.h>

#include <miscfs/specfs/specdev.h>
#include "stat.h"

/*
 * Indirect driver for multi-controller paging.
 */

int	nswap, nswdev;
#ifdef SEQSWAP
int	niswdev;		/* number of interleaved swap devices */
int	niswap;			/* size of interleaved swap area */
#endif

/*
 * Set up swap devices.
 * Initialize linked list of free swap
 * headers. These do not actually point
 * to buffers, but rather to pages that
 * are being swapped in and out.
 */
void
swapinit()
{
	register int i;
	register struct buf *sp = swbuf;
	register struct proc *p = &proc0;	/* XXX */
	struct swdevt *swp;
	int error;

	/*
	 * Count swap devices, and adjust total swap space available.
	 * Some of the space will not be countable until later (dynamically
	 * configurable devices) and some of the counted space will not be
	 * available until a swapon() system call is issued, both usually
	 * happen when the system goes multi-user.
	 *
	 * If using NFS for swap, swdevt[0] will already be bdevvp'd.	XXX
	 */
#ifdef SEQSWAP
	nswdev = niswdev = 0;
	nswap = niswap = 0;
	/*
	 * All interleaved devices must come first
	 */
	for (swp = swdevt; swp->sw_dev != NODEV || swp->sw_vp != NULL; swp++) {
		if (swp->sw_flags & SW_SEQUENTIAL)
			break;
		niswdev++;
		if (swp->sw_nblks > niswap)
			niswap = swp->sw_nblks;
	}
	niswap = roundup(niswap, dmmax);
	niswap *= niswdev;
	if (swdevt[0].sw_vp == NULL &&
	    bdevvp(swdevt[0].sw_dev, &swdevt[0].sw_vp))
		panic("swapvp");
	/*
	 * The remainder must be sequential
	 */
	for ( ; swp->sw_dev != NODEV; swp++) {
		if ((swp->sw_flags & SW_SEQUENTIAL) == 0)
			panic("binit: mis-ordered swap devices");
		nswdev++;
		if (swp->sw_nblks > 0) {
			if (swp->sw_nblks % dmmax)
				swp->sw_nblks -= (swp->sw_nblks % dmmax);
			nswap += swp->sw_nblks;
		}
	}
	nswdev += niswdev;
	if (nswdev == 0)
		panic("swapinit");
	nswap += niswap;
#else
	nswdev = 0;
	nswap = 0;
	for (swp = swdevt; swp->sw_dev != NODEV || swp->sw_vp != NULL; swp++) {
		nswdev++;
		if (swp->sw_nblks > nswap)
			nswap = swp->sw_nblks;
	}
	if (nswdev == 0)
		panic("swapinit");
	if (nswdev > 1)
		nswap = ((nswap + dmmax - 1) / dmmax) * dmmax;
	nswap *= nswdev;
	if (swdevt[0].sw_vp == NULL &&
	    bdevvp(swdevt[0].sw_dev, &swdevt[0].sw_vp))
		panic("swapvp");
#endif
	if (nswap == 0)
		printf("WARNING: no swap space found\n");
	else if (error = swfree(p, 0)) {
		printf("swfree errno %d\n", error);	/* XXX */
		panic("swapinit swfree 0");
	}

	/*
	 * Now set up swap buffer headers.
	 */
	bswlist.b_actf = sp;
	for (i = 0; i < nswbuf - 1; i++, sp++) {
		sp->b_actf = sp + 1;
		sp->b_rcred = sp->b_wcred = p->p_ucred;
		sp->b_vnbufs.qe_next = NOLIST;
	}
	sp->b_rcred = sp->b_wcred = p->p_ucred;
	sp->b_vnbufs.qe_next = NOLIST;
	sp->b_actf = NULL;
}

void
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
	if (bp->b_blkno + sz > nswap) {
		bp->b_flags |= B_ERROR;
		biodone(bp);
		return;
	}
	if (nswdev > 1) {
#ifdef SEQSWAP
		if (bp->b_blkno < niswap) {
			if (niswdev > 1) {
				off = bp->b_blkno % dmmax;
				if (off+sz > dmmax) {
					bp->b_flags |= B_ERROR;
					biodone(bp);
					return;
				}
				seg = bp->b_blkno / dmmax;
				index = seg % niswdev;
				seg /= niswdev;
				bp->b_blkno = seg*dmmax + off;
			} else
				index = 0;
		} else {
			register struct swdevt *swp;

			bp->b_blkno -= niswap;
			for (index = niswdev, swp = &swdevt[niswdev];
			     swp->sw_dev != NODEV;
			     swp++, index++) {
				if (bp->b_blkno < swp->sw_nblks)
					break;
				bp->b_blkno -= swp->sw_nblks;
			}
			if (swp->sw_dev == NODEV ||
			    bp->b_blkno+sz > swp->sw_nblks) {
				bp->b_flags |= B_ERROR;
				biodone(bp);
				return;
			}
		}
#else
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
#endif
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
struct swapon_args {
	char	*name;
};
/* ARGSUSED */
int
swapon(p, uap, retval)
	struct proc *p;
	struct swapon_args *uap;
	int *retval;
{
	register struct vnode *vp;
	register struct swdevt *sp;
	dev_t dev;
	int error;
	struct nameidata nd;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	NDINIT(&nd, LOOKUP, FOLLOW, UIO_USERSPACE, uap->name, p);
	if (error = namei(&nd))
		return (error);
	vp = nd.ni_vp;
	if (vp->v_type != VBLK) {
		vrele(vp);
		return (ENOTBLK);
	}
	dev = (dev_t)vp->v_rdev;
	if (major(dev) >= nblkdev) {
		vrele(vp);
		return (ENXIO);
	}
	for (sp = &swdevt[0]; sp->sw_dev != NODEV; sp++) {
		if (sp->sw_dev == dev) {
			if (sp->sw_flags & SW_FREED) {
				vrele(vp);
				return (EBUSY);
			}
			u.u_error = swfree(sp - swdevt);
			return (0);
		}
#ifdef SEQSWAP
		/*
		 * If we have reached a non-freed sequential device without
		 * finding what we are looking for, it is an error.
		 * That is because all interleaved devices must come first
		 * and sequential devices must be freed in order.
		 */
		if ((sp->sw_flags & (SW_SEQUENTIAL|SW_FREED)) == SW_SEQUENTIAL)
			break;
#endif
	}
	vrele(vp);
	return (EINVAL);
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
int
swfree(p, index)
	struct proc *p;
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
#ifdef SEQSWAP
		if ((vsbase = index*dmmax + dvbase*niswdev) >= niswap)
			panic("swfree");
#else
		if ((vsbase = index*dmmax + dvbase*nswdev) >= nswap)
			panic("swfree");
#endif
		if (blk > dmmax)
			blk = dmmax;
		if (vsbase == 0) {
			/*
			 * First of all chunks... initialize the swapmap.
			 * Don't use the first cluster of the device
			 * in case it starts with a label or boot block.
			 */
			rminit(swapmap, blk - ctod(CLSIZE),
			    vsbase + ctod(CLSIZE), "swap", nswapmap);
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
