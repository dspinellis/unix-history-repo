/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)param.h	7.21 (Berkeley) %G%
 */

#define	BSD	199103		/* March, 1991 system version (year & month) */
#define BSD4_3	1
#define BSD4_4	0.5

#include <sys/syslimits.h>

#ifndef LOCORE
#include <sys/types.h>
#endif

/*
 * Machine-independent constants (some used in following include files)
 */
#define	MAXUPRC		CHILD_MAX	/* max processes per user */
#define	NOFILE		OPEN_MAX	/* max open files per process */
#define	NCARGS		ARG_MAX		/* # characters in exec arglist */
#define	MAXINTERP	32		/* max interpreter file name length */
#define	NGROUPS		NGROUPS_MAX	/* max number groups */
#define MAXHOSTNAMELEN	256		/* maximum hostname size */
#define	MAXCOMLEN	16		/* maximum command name remembered */
	/* MAXCOMLEN should be >= sizeof(ac_comm) (acct.h)  */
#define	MAXLOGNAME	12		/* maximum login name length */
	/* MAXLOGNAME must be >= UT_NAMESIZE (<utmp.h>) */

#define	NOGROUP		65535		/* marker for empty group set member */

/*
 * More types and definitions used throughout the kernel
 */
#ifdef KERNEL
#include <sys/cdefs.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/ucred.h>
#include <sys/uio.h>
#endif

/*
 * Signals
 */
#include <sys/signal.h>

/*
 * Machine type dependent parameters.
 */
#include <machine/param.h>
#include <machine/endian.h>
#include <machine/limits.h>

/*
 * Priorities.  Note that with 32 run queues,
 * differences less than 4 are insignificant.
 */
#define	PSWP	0
#define	PVM	4
#define	PINOD	8
#define	PRIBIO	16
#define	PVFS	20
#define	PSOCK	24
#define	PZERO	25		/* No longer magic, shouldn't be here XXX */
#define	PWAIT	32
#define	PLOCK	36
#define	PPAUSE	40
#define	PUSER	50
#define	MAXPRI	127		/* priorities range from 0 through MAXPRI */

#define	PRIMASK	0x0ff
#define	PCATCH	0x100		/* or'd with pri for tsleep to check signals */

#define	NZERO	0		/* default "nice" */

#define	NBPW	sizeof(int)	/* number of bytes in an integer */

#ifndef NULL
#define	NULL	0
#endif
#define	CMASK	022		/* default mask for file creation */
#define	NODEV	(dev_t)(-1)

/*
 * Clustering of hardware pages on machines with ridiculously small
 * page sizes is done here.  The paging subsystem deals with units of
 * CLSIZE pte's describing NBPG (from machine/machparam.h) pages each.
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

/* CBLOCK is the size of a clist block, must be power of 2 */
#define	CBLOCK	64
#define CBQSIZE	(CBLOCK/NBBY)	/* quote bytes/cblock - can do better */
#define	CBSIZE	(CBLOCK - sizeof(struct cblock *) - CBQSIZE) /* data chars/clist */
#define	CROUND	(CBLOCK - 1)				/* clist rounding */

/*
 * File system parameters and macros.
 *
 * The file system is made out of blocks of at most MAXBSIZE units,
 * with smaller units (fragments) only in the last direct block.
 * MAXBSIZE primarily determines the size of buffers in the buffer
 * pool. It may be made larger without any effect on existing
 * file systems; however making it smaller make make some file
 * systems unmountable.
 */
#define	MAXBSIZE	8192
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
#define	MAXPATHLEN	PATH_MAX
#define MAXSYMLINKS	8

/*
 * bit map related macros
 */
#define	setbit(a,i)	((a)[(i)/NBBY] |= 1<<((i)%NBBY))
#define	clrbit(a,i)	((a)[(i)/NBBY] &= ~(1<<((i)%NBBY)))
#define	isset(a,i)	((a)[(i)/NBBY] & (1<<((i)%NBBY)))
#define	isclr(a,i)	(((a)[(i)/NBBY] & (1<<((i)%NBBY))) == 0)

/*
 * Macros for counting and rounding.
 */
#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))
#define powerof2(x)	((((x)-1)&(x))==0)

/*
 * Macros for fast min/max:
 * with inline expansion, the "function" is faster.
 */
#ifdef KERNEL
#define	MIN(a,b) min((a), (b))
#define	MAX(a,b) max((a), (b))
#else
#define	MIN(a,b) (((a)<(b))?(a):(b))
#define	MAX(a,b) (((a)>(b))?(a):(b))
#endif

/*
 * Constants for setting the parameters of the kernel memory allocator.
 *
 * 2 ** MINBUCKET is the smallest unit of memory that will be
 * allocated. It must be at least large enough to hold a pointer.
 *
 * Units of memory less or equal to MAXALLOCSAVE will permanently
 * allocate physical memory; requests for these size pieces of
 * memory are quite fast. Allocations greater than MAXALLOCSAVE must
 * always allocate and free physical memory; requests for these
 * size allocations should be done infrequently as they will be slow.
 * Constraints: CLBYTES <= MAXALLOCSAVE <= 2 ** (MINBUCKET + 14)
 * and MAXALLOCSIZE must be a power of two.
 */
#define MINBUCKET	4		/* 4 => min allocation of 16 bytes */
#define MAXALLOCSAVE	(2 * CLBYTES)

/*
 * Scale factor for scaled integers used to count %cpu time and load avgs.
 *
 * The number of CPU `tick's that map to a unique `%age' can be expressed
 * by the formula (1 / (2 ^ (FSHIFT - 11))).  The maximum load average that
 * can be calculated (assuming 32 bits) can be closely approximated using
 * the formula (2 ^ (2 * (16 - FSHIFT))) for (FSHIFT < 15).
 *
 * For the scheduler to maintain a 1:1 mapping of CPU `tick' to `%age',
 * FSHIFT must be at least 11; this gives us a maximum load avg of ~1024.
 */
#define	FSHIFT	11		/* bits to right of fixed binary point */
#define FSCALE	(1<<FSHIFT)
