/*	vmparam.h	1.3	5/25/85	*/
/*	vmparam.h	6.1	83/07/29	*/

/*
 * Machine dependent constants for TAHOE
 */
/*
 * USRTEXT is the start of the user text/data space, while USRSTACK
 * is the top (end) of the user stack.  LOWPAGES and HIGHPAGES are
 * the number of pages from the beginning of the P0 region to the
 * beginning of the text and from the beginning of the P2 region to the
 * beginning of the stack respectively.
 */
					/* number of ptes per page */
#define	USRTEXT		0
#define	USRSTACK	(0xC0000000-UPAGES*NBPG)
					/* Start of user stack */
#define	P2PAGES		0x100000	/* number of pages in P2 region */
#define	LOWPAGES	0
#define	HIGHPAGES	UPAGES

/*
 * Virtual memory related constants
 */
#define	SLOP	32
#define	MAXTSIZ		(6*1024-SLOP)		/* max text size (clicks) */
#define	MAXDSIZ		(19*2048-32-SLOP)	/* max data size (clicks) */
#define	MAXSSIZ		(19*2048-32-SLOP)	/* max stack size (clicks) */

/*
 * Sizes of the system and user portions of the system page table.
 */
/* SYSPTSIZE IS SILLY; IT SHOULD BE COMPUTED AT BOOT TIME */
#define	SYSPTSIZE	((30+MAXUSERS)*NPTEPG/4)
#define	USRPTSIZE 	(2*NPTEPG)

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
 */
#define	SAFERSS		16		/* nominal ``small'' resident set size
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

#define	KLMAX	(32/CLSIZE)
#define	KLSEQL	(16/CLSIZE)		/* in klust if vadvise(VA_SEQL) */
#define	KLIN	(8/CLSIZE)		/* default data/stack in klust */
#define	KLTXT	(4/CLSIZE)		/* default text in klust */
#define	KLOUT	(32/CLSIZE)

/*
 * KLSDIST is the advance or retard of the fifo reclaim for sequential
 * processes data space.
 */
#define	KLSDIST	3		/* klusters advance/retard for seq. fifo */

/*
 * Paging thresholds (see vm_sched.c).
 * Strategy of 4/22/81:
 *	lotsfree is 1/4 of memory free.
 *	desfree is 200k bytes, but at most 1/8 of memory
 *	minfree is 64k bytes, but at most 1/2 of desfree
 */
#define	LOTSFREEFRACT	4
#define	DESFREE		(200 * 1024)
#define	DESFREEFRACT	8
#define	MINFREE		(64 * 1024)
#define	MINFREEFRACT	2

/*
 * Believed threshold (in megabytes) for which interleaved
 * swapping area is desirable.
 */
#define	LOTSOFMEM	2

/*
 * BEWARE THIS DEFINITION WORKS ONLY WITH COUNT OF 1
 */
#define	mapin(pte, v, pfnum, count, prot) \
	(*(int *)(pte) = (pfnum) | (prot), mtpr(ptob(v), TBIS))

/* 
 * The following constant is used to initialize the map of the
 * system page table i/o entries.
 * It's value should be the highest i/o address used by all the 
 * controllers handled in the system as specified in ubminit 
 * structure in ioconf.c.
*/
#define MAXIOADDR 0xffffee45	/* highest physical io address */

/* Number of entries in the system page pable for i/o space */
#define IOSIZE	(( (MAXIOADDR - IOBASE+ NBPG -1) >> PGSHIFT )+1)
#define TBUFSIZ	10		/* maximum tape buffer size */
#define	ACEBPTE	32		/* ACC Ethernet (ACE) I/O window	*/
