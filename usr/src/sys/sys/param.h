/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)param.h	6.12 (Berkeley) %G%
 */

/*
 * Machine type dependent parameters.
 */
#ifdef KERNEL
#include "../machine/machparam.h"
#else
#include <machine/machparam.h>
#endif

#define	NPTEPG		(NBPG/(sizeof (struct pte)))

/*
 * Machine-independent constants
 */
#define	NMOUNT	20		/* number of mountable file systems */
/* NMOUNT must be <= 255 unless c_mdev (cmap.h) is expanded */
#define	MSWAPX	NMOUNT		/* pseudo mount table index for swapdev */
#define	MAXUPRC	25		/* max processes per user */
#define	NOFILE	64		/* max open files per process */
#define	CANBSIZ	256		/* max size of typewriter line */
#define	NCARGS	20480		/* # characters in exec arglist */
#define	NGROUPS	16		/* max number groups */

#define	NOGROUP	-1		/* marker for empty group set member */

/*
 * Priorities
 */
#define	PSWP	0
#define	PINOD	10
#define	PRIBIO	20
#define	PRIUBA	24
#define	PZERO	25
#define	PPIPE	26
#define	PWAIT	30
#define	PLOCK	35
#define	PSLEP	40
#define	PUSER	50

#define	NZERO	0

/*
 * Signals
 */
#ifdef KERNEL
#include "signal.h"
#else
#include <signal.h>
#endif

#define	ISSIG(p) \
	((p)->p_sig && ((p)->p_flag&STRC || \
	 ((p)->p_sig &~ ((p)->p_sigignore | (p)->p_sigmask))) && issig())

/*
 * Fundamental constants of the implementation.
 */
#define	NBBY	8		/* number of bits in a byte */
#define	NBPW	sizeof(int)	/* number of bytes in an integer */

#define	NULL	0
#define	CMASK	022		/* default mask for file creation */
#define	NODEV	(dev_t)(-1)

/*
 * Clustering of hardware pages on machines with ridiculously small
 * page sizes is done here.  The paging subsystem deals with units of
 * CLSIZE pte's describing NBPG (from vm.h) pages each.
 *
 * NOTE: SSIZE, SINCR and UPAGES must be multiples of CLSIZE
 */
#define	CLBYTES		(CLSIZE*NBPG)
#define	CLOFSET		(CLSIZE*NBPG-1)	/* for clusters, like PGOFSET */
#define	claligned(x)	((((int)(x))&CLOFSET)==0)
#define	CLOFF		CLOFSET
#define	CLSHIFT		(PGSHIFT+CLSIZELOG2)

#if CLSIZE==1
#define	clbase(i)	(i)
#define	clrnd(i)	(i)
#else
/* give the base virtual address (first of CLSIZE) */
#define	clbase(i)	((i) &~ (CLSIZE-1))
/* round a number of clicks up to a whole cluster */
#define	clrnd(i)	(((i) + (CLSIZE-1)) &~ (CLSIZE-1))
#endif

#ifndef INTRLVE
/* macros replacing interleaving functions */
#define	dkblock(bp)	((bp)->b_blkno)
#define	dkunit(bp)	(minor((bp)->b_dev) >> 3)
#endif

#define	CBSIZE	28		/* number of chars in a clist block */
#define	CROUND	0x1F		/* clist rounding; sizeof(int *) + CBSIZE -1*/

/*
 * File system parameters and macros.
 *
 * The file system is made out of blocks of at most MAXBSIZE units,
 * with smaller units (fragments) only in the last direct block.
 * MAXBSIZE primarily determines the size of buffers in the buffer
 * pool. It may be made larger without any effect on existing
 * file systems; however making it smaller make make some file
 * systems unmountable.
 *
 * Note that the blocked devices are assumed to have DEV_BSIZE
 * "sectors" and that fragments must be some multiple of this size.
 * Block devices are read in BLKDEV_IOSIZE units. This number must
 * be a power of two and in the range of
 *	DEV_BSIZE <= BLKDEV_IOSIZE <= MAXBSIZE
 * This size has no effect upon the file system, but is usually set
 * to the block size of the root file system, so as to maximize the
 * speed of ``fsck''.
 */
#define	MAXBSIZE	8192
#define	DEV_BSIZE	512
#define	DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define BLKDEV_IOSIZE	2048
#define MAXFRAG 	8

#define	btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> DEV_BSHIFT)
#define	dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and will be after we
 * add an entry to cdevsw for that purpose.  For now though
 * just use DEV_BSIZE.
 */
#define	bdbtofsb(bn)	((bn) / (BLKDEV_IOSIZE/DEV_BSIZE))

/*
 * MAXPATHLEN defines the longest permissable path length
 * after expanding symbolic links. It is used to allocate
 * a temporary buffer from the buffer pool in which to do the
 * name expansion, hence should be a power of two, and must
 * be less than or equal to MAXBSIZE.
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name. It should be set high
 * enough to allow all legitimate uses, but halt infinite loops
 * reasonably quickly.
 */
#define MAXPATHLEN	1024
#define MAXSYMLINKS	8

/*
 * bit map related macros
 */
#define	setbit(a,i)	((a)[(i)/NBBY] |= 1<<((i)%NBBY))
#define	clrbit(a,i)	((a)[(i)/NBBY] &= ~(1<<((i)%NBBY)))
#define	isset(a,i)	((a)[(i)/NBBY] & (1<<((i)%NBBY)))
#define	isclr(a,i)	(((a)[(i)/NBBY] & (1<<((i)%NBBY))) == 0)

/*
 * Macros for fast min/max.
 */
#define	MIN(a,b) (((a)<(b))?(a):(b))
#define	MAX(a,b) (((a)>(b))?(a):(b))

/*
 * Macros for counting and rounding.
 */
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))

/*
 * Maximum size of hostname recognized and stored in the kernel.
 */
#define MAXHOSTNAMELEN	64

#ifndef KERNEL
#include	<sys/types.h>
#else
#ifndef LOCORE
#include	"types.h"
#endif
#endif
