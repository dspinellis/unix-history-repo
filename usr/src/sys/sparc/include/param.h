/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)param.h	7.5 (Berkeley) %G%
 *
 * from: $Header: param.h,v 1.13 92/11/26 02:04:38 torek Exp $ (LBL)
 */

/*
 * Machine dependent constants for Sun-4c (SPARCstation)
 */
#define	MACHINE	"sparc"

#ifdef KERNEL				/* XXX */
#include <machine/cpu.h>		/* XXX */
#endif					/* XXX */

/*
 * Round p (pointer or byte index) up to a correctly-aligned value for
 * the machine's strictest data type.  The result is u_int and must be
 * cast to any desired pointer type.
 */
#define	ALIGNBYTES	7
#define	ALIGN(p)	(((u_int)(p) + ALIGNBYTES) & ~ALIGNBYTES)

#define	NBPG		4096		/* bytes/page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	PGSHIFT		12		/* log2(NBPG) */

#define	KERNBASE	0xf8000000	/* start of kernel virtual space */
#define	KERNTEXTOFF	0xf8004000	/* start of kernel text */
#define	BTOPKERNBASE	((u_long)KERNBASE >> PG_SHIFT)

#define	DEV_BSIZE	512
#define	DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define	BLKDEV_IOSIZE	2048
#define	MAXPHYS		(64 * 1024)

#define	CLSIZE		1
#define	CLSIZELOG2	0

/* NOTE: SSIZE and UPAGES must be multiples of CLSIZE */
#define	SSIZE		1		/* initial stack size/NBPG */
#define	UPAGES		2		/* pages of u-area */

/*
 * Constants related to network buffer management.
 * MCLBYTES must be no larger than CLBYTES (the software page size), and,
 * on machines that exchange pages of input or output buffers with mbuf
 * clusters (MAPPED_MBUFS), MCLBYTES must also be an integral multiple
 * of the hardware page size.
 */
#define	MSIZE		128		/* size of an mbuf */
#define	MCLBYTES	2048		/* enough for whole Ethernet packet */
#define	MCLSHIFT	11		/* log2(MCLBYTES) */
#define	MCLOFSET	(MCLBYTES - 1)

#ifndef NMBCLUSTERS
#ifdef GATEWAY
#define	NMBCLUSTERS	512		/* map size, max cluster allocation */
#else
#define	NMBCLUSTERS	256		/* map size, max cluster allocation */
#endif
#endif

/*
 * Size of kernel malloc arena in CLBYTES-sized logical pages.
 */
#ifndef	NKMEMCLUSTERS
#define	NKMEMCLUSTERS	(6 * 1024 * 1024 / CLBYTES)
#endif

/* pages ("clicks") (4096 bytes) to disk blocks */
#define	ctod(x)	((x) << (PGSHIFT - DEV_BSHIFT))
#define	dtoc(x)	((x) >> (PGSHIFT - DEV_BSHIFT))
#define	dtob(x)	((x) << DEV_BSHIFT)

/* pages to bytes */
#define	ctob(x)	((x) << PGSHIFT)

/* bytes to pages */
#define	btoc(x)	(((unsigned)(x) + PGOFSET) >> PGSHIFT)

#define	btodb(bytes)		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> DEV_BSHIFT)
#define	dbtob(db)		/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and should use the bsize
 * field from the disk label.
 * For now though just use DEV_BSIZE.
 */
#define	bdbtofsb(bn)	((bn) / (BLKDEV_IOSIZE / DEV_BSIZE))

#ifdef KERNEL
#ifndef LOCORE
#define	DELAY(n)	delay(n)
#endif
#else
#define	DELAY(n)	{ register volatile int N = (n); while (--N > 0); }
#endif
