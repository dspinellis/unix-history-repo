/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)vmparam.h	5.3 (Berkeley) %G%
 */

/*
 * Machine dependent constants for 386.
 */

/*
 * Virtual address space arrangement. On 386, both user and kernel
 * share the address space, not unlike the arrangements on the vax.
 * USRTEXT is the start of the user text/data space, while USRSTACK
 * is the top (end) of the user stack. Immediately above the user stack
 * resides the user structure, which is UPAGES long and contains the
 * kernel stack. As such, UPAGES is the number of pages from the beginning
 * of the P1 region to the beginning of the user stack. Also, the P0
 * region begins with user text and ends with user data.
 * Immediately after the user structure is the kernal address space.
 */

#define	USRTEXT		0
#define	USRSTACK	0xFDFFE000	/* Sysbase - UPAGES*NBPG */
#define	BTOPUSRSTACK	(0xFE000-(UPAGES))	/* btop(USRSTACK) */

#define P1PAGES		0xFE000
#define	LOWPAGES	0
#define HIGHPAGES	UPAGES

/*
 * Virtual memory related constants, all in bytes
 */
#define	MAXTSIZ		(6*1024*1024)		/* max text size */
#ifndef DFLDSIZ
#define	DFLDSIZ		(6*1024*1024)		/* initial data size limit */
#endif
#ifndef MAXDSIZ
#define	MAXDSIZ		(32*1024*1024)		/* max data size */
#endif
#ifndef	DFLSSIZ
#define	DFLSSIZ		(512*1024)		/* initial stack size limit */
#endif
#ifndef	MAXSSIZ
#define	MAXSSIZ		MAXDSIZ			/* max stack size */
#endif

/*
 * Default sizes of swap allocation chunks (see dmap.h).
 * The actual values may be changed in vminit() based on MAXDSIZ.
 * With MAXDSIZ of 16Mb and NDMAP of 38, dmmax will be 1024.
 */
#define	DMMIN	32			/* smallest swap allocation */
#define	DMMAX	4096			/* largest potential swap allocation */
#define	DMTEXT	1024			/* swap allocation for text */

/*
 * Sizes of the system and user portions of the system page table.
 */
#define	SYSPTSIZE 	(2*NPTEPG)
#define	USRPTSIZE 	(2*NPTEPG)

/*
 * Size of User Raw I/O map
 */
#define	USRIOSIZE 	30

/*
 * The size of the clock loop.
 */
#define	LOOPPAGES	(maxfree - firstfree)

/*
 * The time for a process to be blocked before being very swappable.
 * This is a number of seconds which the system takes as being a non-trivial
 * amount of real time.  You probably shouldn't change this;
 * it is used in subtle ways (fractions and multiples of it are, that is, like
 * half of a ``long time'', almost a long time, etc.)
 * It is related to human patience and other factors which don't really
 * change over time.
 */
#define	MAXSLP 		20

/*
 * A swapped in process is given a small amount of core without being bothered
 * by the page replacement algorithm.  Basically this says that if you are
 * swapped in you deserve some resources.  We protect the last SAFERSS
 * pages against paging and will just swap you out rather than paging you.
 * Note that each process has at least UPAGES+CLSIZE pages which are not
 * paged anyways (this is currently 8+2=10 pages or 5k bytes), so this
 * number just means a swapped in process is given around 25k bytes.
 * Just for fun: current memory prices are 4600$ a megabyte on VAX (4/22/81),
 * so we loan each swapped in process memory worth 100$, or just admit
 * that we don't consider it worthwhile and swap it out to disk which costs
 * $30/mb or about $0.75.
 * { wfj 6/16/89: Retail AT memory expansion $800/megabyte, loan of $17
 *   on disk costing $7/mb or $0.18 (in memory still 100:1 in cost!) }
 */
#define	SAFERSS		8		/* nominal ``small'' resident set size
					   protected against replacement */

/*
 * DISKRPM is used to estimate the number of paging i/o operations
 * which one can expect from a single disk controller.
 */
#define	DISKRPM		60

/*
 * Klustering constants.  Klustering is the gathering
 * of pages together for pagein/pageout, while clustering
 * is the treatment of hardware page size as though it were
 * larger than it really is.
 *
 * KLMAX gives maximum cluster size in CLSIZE page (cluster-page)
 * units.  Note that KLMAX*CLSIZE must be <= DMMIN in dmap.h.
 */

#define	KLMAX	(4/CLSIZE)
#define	KLSEQL	(2/CLSIZE)		/* in klust if vadvise(VA_SEQL) */
#define	KLIN	(4/CLSIZE)		/* default data/stack in klust */
#define	KLTXT	(4/CLSIZE)		/* default text in klust */
#define	KLOUT	(4/CLSIZE)

/*
 * KLSDIST is the advance or retard of the fifo reclaim for sequential
 * processes data space.
 */
#define	KLSDIST	3		/* klusters advance/retard for seq. fifo */

/*
 * Paging thresholds (see vm_sched.c).
 * Strategy of 1/19/85:
 *	lotsfree is 512k bytes, but at most 1/4 of memory
 *	desfree is 200k bytes, but at most 1/8 of memory
 *	minfree is 64k bytes, but at most 1/2 of desfree
 */
#define	LOTSFREE	(512 * 1024)
#define	LOTSFREEFRACT	4
#define	DESFREE		(200 * 1024)
#define	DESFREEFRACT	8
#define	MINFREE		(64 * 1024)
#define	MINFREEFRACT	2

/*
 * There are two clock hands, initially separated by HANDSPREAD bytes
 * (but at most all of user memory).  The amount of time to reclaim
 * a page once the pageout process examines it increases with this
 * distance and decreases as the scan rate rises.
 */
#define	HANDSPREAD	(2 * 1024 * 1024)

/*
 * The number of times per second to recompute the desired paging rate
 * and poke the pagedaemon.
 */
#define	RATETOSCHEDPAGING	4

/*
 * Believed threshold (in megabytes) for which interleaved
 * swapping area is desirable.
 */
#define	LOTSOFMEM	2

#define	mapin(pte, v, pfnum, prot) \
	{(*(int *)(pte) = ((pfnum)<<PGSHIFT) | (prot)); tlbflush(); }

/*
 * Invalidate a cluster (optimized here for standard CLSIZE).
 */
#if CLSIZE == 1
#define	tbiscl(v)
#endif

/*
 * inline assembler macros
 */

/*
 * Flush MMU TLB
 */

#define	tlbflush()	asm(" movl %%cr3,%%eax; movl %%eax,%%cr3 " ::: "ax")

#ifndef I386_CR3PAT
#define	I386_CR3PAT	0x0
#endif

#define _cr3() ({u_long rtn; \
	asm (" movl %%cr3,%%eax; movl %%eax,%0 " \
		: "=g" (rtn) \
		: \
		: "ax"); \
	rtn; \
})

#define load_cr3(s) ({ u_long val; \
	val = (s) | I386_CR3PAT; \
	asm ("movl %0,%%eax; movl %%eax,%%cr3" \
		:  \
		: "g" (val) \
		: "ax"); \
})
