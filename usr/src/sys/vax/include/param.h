/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)param.h	7.7 (Berkeley) %G%
 */

/*
 * Machine dependent constants for vax.
 */

#define	MACHINE	"vax"

#ifndef ENDIAN
/*
 * Definitions for byte order,
 * according to byte significance from low address to high.
 */
#define	LITTLE	1234		/* least-significant byte first (vax) */
#define	BIG	4321		/* most-significant byte first */
#define	PDP	3412		/* LSB first in word, MSW first in long (pdp) */
#define	ENDIAN	LITTLE		/* byte order on vax */

/*
 * Macros for network/external number representation conversion.
 */
#if ENDIAN == BIG && !defined(lint)
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#define	htonl(x)	(x)
#define	htons(x)	(x)
#else
unsigned short	ntohs(), htons();
unsigned long	ntohl(), htonl();
#endif

#define	KERNBASE	0x80000000	/* start of kernel virtual */
#define	BTOPKERNBASE	((u_long)KERNBASE >> PGSHIFT)

#define	KERNBASE	0x80000000	/* start of kernel virtual */
#define	BTOPKERNBASE	((u_long)KERNBASE >> PGSHIFT)

#define	NBPG		512		/* bytes/page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	PGSHIFT		9		/* LOG2(NBPG) */
#define	NPTEPG		(NBPG/(sizeof (struct pte)))

#ifndef SECSIZE
#define	DEV_BSIZE	512
#define	DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define BLKDEV_IOSIZE	2048
#define	MAXPHYS		(63 * 1024)	/* max raw I/O transfer size */
#else SECSIZE
/*
 * Devices without disk labels and the swap virtual device
 * use "blocks" of exactly pagesize.  Devices with disk labels
 * use device-dependent sector sizes for block and character interfaces.
 */
#define	DEV_BSIZE	NBPG
#define	DEV_BSHIFT	PGSHIFT		/* log2(DEV_BSIZE) */
#define BLKDEV_IOSIZE	NBPG		/* NBPG for unlabeled block devices */
#endif SECSIZE
#define	MAXPHYS		(63 * 1024)	/* max raw I/O transfer size */

#define	CLSIZE		2
#define	CLSIZELOG2	1

#define	SSIZE		4		/* initial stack size/NBPG */
#define	SINCR		4		/* increment of stack/NBPG */

#define	UPAGES		10		/* pages of u-area */

/*
 * Some macros for units conversion
 */
/* Core clicks (512 bytes) to segments and vice versa */
#define	ctos(x)	(x)
#define	stoc(x)	(x)

#ifndef SECSIZE
/* Core clicks (512 bytes) to disk blocks */
#define	ctod(x)	(x)
#define	dtoc(x)	(x)
#define	dtob(x)	((x)<<PGSHIFT)
#else SECSIZE
/* Core clicks (512 bytes) to disk blocks; deprecated */
#define	ctod(x)	(x)				/* XXX */
#define	dtoc(x)	(x)				/* XXX */
#define	dtob(x)	((x)<<PGSHIFT)			/* XXX */
#endif SECSIZE

/* clicks to bytes */
#define	ctob(x)	((x)<<9)

/* bytes to clicks */
#define	btoc(x)	((((unsigned)(x)+511)>>9))

#ifndef SECSIZE
#define	btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> DEV_BSHIFT)
#define	dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and will be if we
 * add an entry to cdevsw/bdevsw for that purpose.
 * For now though just use DEV_BSIZE.
 */
#define	bdbtofsb(bn)	((bn) / (BLKDEV_IOSIZE/DEV_BSIZE))
#else SECSIZE
/* bytes to "disk blocks" and back; deprecated */
#define	btodb(bytes)	((unsigned)(bytes) >> DEV_BSHIFT)	/* XXX */
#define	dbtob(db)	((unsigned)(db) << DEV_BSHIFT)		/* XXX */
#endif SECSIZE

/*
 * Macros to decode processor status word.
 */
#define	USERMODE(ps)	(((ps) & PSL_CURMOD) == PSL_CURMOD)
#define	BASEPRI(ps)	(((ps) & PSL_IPL) == 0)

#ifdef KERNEL
#ifndef LOCORE
int	cpuspeed;
#define	DELAY(n)	{ register int N = cpuspeed * (n); while (--N > 0); }
#endif

#else KERNEL
#define	DELAY(n)	{ register int N = (n); while (--N > 0); }
#endif KERNEL
#endif ENDIAN
