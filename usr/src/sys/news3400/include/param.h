/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: machparam.h 1.11 89/08/14$
 *
 *	@(#)param.h	7.3 (Berkeley) %G%
 */

/*
 * Machine dependent constants for DEC Station 3100.
 */
#define	MACHINE	"news"
#define COFF

/*
 * Round p (pointer or byte index) up to a correctly-aligned value for all
 * data types (int, long, ...).   The result is u_int and must be cast to
 * any desired pointer type.
 */
#define	ALIGN(p)	(((u_int)(p) + 3) &~ 3)

#define	NBPG		4096		/* bytes/page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	PGSHIFT		12		/* LOG2(NBPG) */
#define	NPTEPG		(NBPG/4)

#define	KERNBASE	0x80000000	/* start of kernel virtual */
#define	BTOPKERNBASE	((u_long)KERNBASE >> PGSHIFT)

#define	DEV_BSIZE	512
#define	DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define BLKDEV_IOSIZE	2048
#define	MAXPHYS		(24 * 1024)	/* max raw I/O transfer size */

#define	CLSIZE		1
#define	CLSIZELOG2	0

/* NOTE: SSIZE, SINCR and UPAGES must be multiples of CLSIZE */
#define	SSIZE		1		/* initial stack size/NBPG */
#define	SINCR		1		/* increment of stack/NBPG */

#define	UPAGES		2		/* pages of u-area */
#define	KERNELSTACK	0xffffe000	/* virtual address of kernel stack */
#define	UADDR		0xffffc000	/* address of u */
#define	UVPN		(UADDR>>PGSHIFT)/* virtual page number of u */

/*
 * Constants related to network buffer management.
 * MCLBYTES must be no larger than CLBYTES (the software page size), and,
 * on machines that exchange pages of input or output buffers with mbuf
 * clusters (MAPPED_MBUFS), MCLBYTES must also be an integral multiple
 * of the hardware page size.
 */
#define	MSIZE		128		/* size of an mbuf */
#define	MCLBYTES	1024
#define	MCLSHIFT	10
#define	MCLOFSET	(MCLBYTES - 1)
#ifndef NMBCLUSTERS
#ifdef GATEWAY
#define	NMBCLUSTERS	512		/* map size, max cluster allocation */
#else
#define	NMBCLUSTERS	256		/* map size, max cluster allocation */
#endif
#endif

/*
 * Size of kernel malloc arena in CLBYTES-sized logical pages
 */ 
#ifndef NKMEMCLUSTERS
#define	NKMEMCLUSTERS	(2048*1024/CLBYTES)
#endif

/* pages ("clicks") (4096 bytes) to disk blocks */
#define	ctod(x)	((x)<<(PGSHIFT-DEV_BSHIFT))
#define	dtoc(x)	((x)>>(PGSHIFT-DEV_BSHIFT))
#define	dtob(x)	((x)<<DEV_BSHIFT)

/* pages to bytes */
#define	ctob(x)	((x)<<PGSHIFT)

/* bytes to pages */
#define	btoc(x)	(((unsigned)(x)+(NBPG-1))>>PGSHIFT)

#define	btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> DEV_BSHIFT)
#define	dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and should use the bsize
 * field from the disk label.
 * For now though just use DEV_BSIZE.
 */
#define	bdbtofsb(bn)	((bn) / (BLKDEV_IOSIZE/DEV_BSIZE))

/*
 * Mach derived conversion macros
 */
#define pmax_round_page(x)	((((unsigned)(x)) + NBPG - 1) & ~(NBPG-1))
#define pmax_trunc_page(x)	((unsigned)(x) & ~(NBPG-1))
#define pmax_btop(x)		((unsigned)(x) >> PGSHIFT)
#define pmax_ptob(x)		((unsigned)(x) << PGSHIFT)

#ifdef news3400
#ifdef PMAXSPL
#define	splnet		Mach_spl0
#define	splbio		Mach_spl0
#define	spltty		Mach_spl1
#define	splimp		Mach_spl1
#define	splclock	Mach_spl2
#define	splstatclock	Mach_spl2
#else
#define	splnet		spl2
#define	splsoftclock	spl2
#define	splbio		spl3
#define	spltty		spl4
#define	splimp		spl4
#define	splclock	spl5
#define	splstatclock	spl5
#endif /* PMAXSPL */
#endif /* news3400 */

#ifdef KERNEL
#ifndef LOCORE
extern	int cpuspeed;
#define	DELAY(n)	{ register int N = cpuspeed * (n) / 2; while (--N > 0); }
#endif
#else /* !KERNEL */
#define	DELAY(n)	{ register int N = (n); while (--N > 0); }
#endif /* !KERNEL */

#ifndef LOCORE
extern int	intrcnt[];
extern char	*intrnames[];
#endif /* !LOCORE */

#define	INTR_CLOCK	0
#define	INTR_SOFTCLK	1
#define	INTR_SOFTINT	2
#define	INTR_AST	3
#define	INTR_SCSI00	4
#define	INTR_SCSI01	5
#define	INTR_SCSI02	6
#define	INTR_SCSI03	7
#define	INTR_SCSI04	8
#define	INTR_SCSI05	9
#define	INTR_SCSI06	10
#define	INTR_SCSI07	11
#define	INTR_SCSI10	12
#define	INTR_SCSI11	13
#define	INTR_SCSI12	14
#define	INTR_SCSI13	15
#define	INTR_SCSI14	16
#define	INTR_SCSI15	17
#define	INTR_SCSI16	18
#define	INTR_SCSI17	19
#define	INTR_ETHER0	20
#define	INTR_ETHER1	21
#define	INTR_ETHER2	22
#define	INTR_VME2	23
#define	INTR_VME4	24
#define	INTR_RS0	25
#define	INTR_RS1	26
#define	INTR_RS2	27
#define	INTR_RS3	28
#define	INTR_RS4	29
#define	INTR_RS5	30
#define	INTR_RS6	31
#define	INTR_RS7	32
#define	INTR_RS8	33
#define	INTR_RS9	34
#define	INTR_RS10	35
#define	INTR_RS11	36
#define	INTR_PRINTER	37
#define	INTR_FD		38
#define	INTR_AUDIO	39
#define	INTR_KEYBOARD	40
#define	INTR_MOUSE	41
#define	INTR_BITMAP	42
#define	INTR_FDDI	43
#define INTR_RENDER	44

#define	NINTRSLOT	45		/* # of intrcnt[] slot */
