/* "@(#)param.h 2.1 3/25/82" */

/*	param.h	4.18	82/06/11	*/

/*
 * Tunable variables which do not usually vary per system.
 *
 * The sizes of most system tables are configured
 * into each system description.  The file system buffer
 * cache size is assigned based on available memory.
 * The tables whose sizes don't vary often are given here.
 */

#define	NMOUNT	15		/* number of mountable file systems */
#define	MSWAPX	15		/* pseudo mount table index for swapdev */
#define	MAXUPRC	25		/* max processes per user */
#define	SSIZE	4		/* initial stack size (*512 bytes) */
#define	SINCR	4		/* increment of stack (*512 bytes) */
#define	NOFILE	20		/* max open files per process */
/* NOFILE MUST NOT BE >= 31; SEE pte.h */
#define	CANBSIZ	256		/* max size of typewriter line */
#define	NCARGS	10240		/* # characters in exec arglist */

/*
 * priorities
 * probably should not be
 * altered too much
 */

#define	PSWP	0
#define	PINOD	10
#define	PRIBIO	20
#define	PRIUBA	24
#define	PZERO	25
#define	PPIPE	26
#define	PWAIT	30
#define	PSLEP	40
#define	PUSER	50

#define	NZERO	20

/*
 * signals
 * dont change
 */

#ifndef	NSIG
#include <signal.h>
#endif

/*
 * Return values from tsleep().
 */
#define	TS_OK	0	/* normal wakeup */
#define	TS_TIME	1	/* timed-out wakeup */
#define	TS_SIG	2	/* asynchronous signal wakeup */

/*
 * fundamental constants of the implementation--
 * cannot be changed easily.
 */

#define	NBBY		8		/* number of bits in a byte */
#define	NBPW		sizeof(int)	/* number of bytes in an integer */
#define	NBPG		512
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	PGSHIFT		9		/* LOG2(NBPG) */

#define	UPAGES	8		/* pages of u-area */
#define	NULL	0
#define	CMASK	0		/* default mask for file creation */
#define	NODEV	(dev_t)(-1)
#define	NGRPS	256		/* max number groups */

/*
 * Clustering of hardware pages on machines with ridiculously small
 * page sizes is done here.  The paging subsystem deals with units of
 * CLSIZE pte's describing NBPG (from vm.h) pages each... BSIZE must
 * be CLSIZE*NBPG in the current implementation, that is the paging subsystem
 * deals with the same size blocks that the file system uses.
 *
 * NOTE: SSIZE, SINCR and UPAGES must be multiples of CLSIZE
 */
#define	CLSIZE		2
#define	CLBYTES		(CLSIZE*NBPG)
#define	CLOFSET		(CLSIZE*NBPG-1)	/* for clusters, like PGOFSET */
#define	claligned(x)	((((int)(x))&CLOFSET)==0)
#define	CLOFF		CLOFSET
#define	CLSHIFT		(PGSHIFT+1)

/* give the base virtual address (first of CLSIZE) */
#define	clbase(i)	((i) &~ (CLSIZE-1))

/* round a number of clicks up to a whole cluster */
#define	clrnd(i)	(((i) + (CLSIZE-1)) &~ (CLSIZE-1))

#ifndef INTRLVE
/* macros replacing interleaving functions */
#define	dkblock(bp)	((bp)->b_blkno)
#define	dkunit(bp)	(minor((bp)->b_dev) >> 3)
#endif

#define	CBSIZE	28		/* number of chars in a clist block */
#define	CROUND	0x1F		/* clist rounding; sizeof(int *) + CBSIZE -1*/

/*
 * Some macros for units conversion
 */
/* Core clicks (512 bytes) to segments and vice versa */
#define	ctos(x)	(x)
#define	stoc(x)	(x)

/* Core clicks (512 bytes) to disk blocks */
#define	ctod(x)	(x)

/* clicks to bytes */
#define	ctob(x)	((x)<<9)

/* bytes to clicks */
#define	btoc(x)	((((unsigned)(x)+511)>>9))

#ifndef KERNEL
#include	<sys/types.h>
#else
#include	"../h/types.h"
#endif

/*
 * Machine-dependent bits and macros
 */
#define	UMODE	PSL_CURMOD		/* usermode bits */
#define	USERMODE(ps)	(((ps) & UMODE) == UMODE)

#define	BASEPRI(ps)	(((ps) & PSL_IPL) != 0)

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
#define BLKDEV_IOSIZE	4096
#define MAXFRAG 	8

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
 * Provide about n microseconds of delay.
 */
#define	DELAY(n)	{ register int N = (n); while (--N > 0); }
