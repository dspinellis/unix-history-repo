/*	vm.h	2.1	1/5/80	*/

/*
 * Machine dependent constants
 */
#define	NBBY		8		/* number of bits in a byte */
#define	NBPG		512		/* number of bytes per page */
#define	PGSHIFT		9		/* LOG2(NBPG) */
#define	NPTEPG		(NBPG/(sizeof (struct pte)))
					/* number of ptes per page */
#define	PGOFSET		(NBPG-1)	/* byte offset into page */
#define	CLOFSET		(CLSIZE*NBPG-1)	/* for clusters, like PGOFSET */
#define	USRSTACK	0x80000000	/* Start of user stack */
#define	P1TOP		0x200000	/* boundary between P0 and P1 regions */
#define	AST		0x04000000	/* ast level */

/*
 * Virtual memory related constants
 *
 * note: USRPTSIZE is well known in locore.s
 */
#define	MAXTSIZ		(4*2048)	/* max virtual text size in clicks */
#define	MAXDSIZ		(4*2048)	/* max virtual data size in clicks */
#define	MAXSSIZ		(1024)		/* max virtual stack size in clicks */
#define	USRPTSIZE 	(8*NPTEPG)	/* max number of pages of page tables
					   for resident processes, this is
					   known in locore.s */

/*
 * Page clustering macros.
 * 
 * dirtycl(pte)			is the page cluster dirty?
 * anycl(pte,fld)		does any pte in the cluster has fld set?
 * zapcl(pte,fld) = val		set all fields fld in the cluster to val
 * distcl(pte)			distribute high bits to cluster; note that
 *				distcl copies everything but pg_pfnum,
 *				INCLUDING pg_m!!!
 *
 * In all cases, pte must be the low pte in the cluster, even if
 * the segment grows backwards (e.g. the stack).
 */
#define	H(pte)	((struct hpte *)(pte))

#if CLSIZE==1
#define	dirtycl(pte)	dirty(pte)
#define	anycl(pte,fld)	((pte)->fld)
#define	zapcl(pte,fld)	(pte)->fld
#define	distcl(pte)
#endif

#if CLSIZE==2
#define	dirtycl(pte)	(dirty(pte) || dirty((pte)+1))
#define	anycl(pte,fld)	((pte)->fld || (((pte)+1)->fld))
#define	zapcl(pte,fld)	(pte)[1].fld = (pte)[0].fld
#endif

#if CLSIZE==4
#define	dirtycl(pte) \
    (dirty(pte) || dirty((pte)+1) || dirty((pte)+2) || dirty((pte)+3))
#define	anycl(pte,fld) \
    ((pte)->fld || (((pte)+1)->fld) || (((pte)+2)->fld) || (((pte)+3)->fld))
#define	zapcl(pte,fld) \
    (pte)[3].fld = (pte)[2].fld = (pte)[1].fld = (pte)[0].fld
#endif

#ifndef distcl
#define	distcl(pte)	zapcl(H(pte),pg_high)
#endif

/*
 * Tunable performance parameters
 *
 * These may vary per-cpu due to configuration as well as the flavor of
 * the local job mix.  MAXPGIO in particular is dependent on the number
 * of disk drives and controllers available locally.
 */
#define	LOOPSIZ		((maxfree - firstfree) / CLSIZE)
					/* loop circumference */
#define	LOTSFREE	((maxfree - firstfree) / 8)
					/* very high mark to freeze scans */
#define	DESFREE 	64		/* minimum desirable free memory */
#define	MINFREE 	32		/* water mark to run swap daemon */
#define	MAXSLP 		20		/* max blocked time (in seconds) allowed
					   before being very swappable */
/* SLOWSCAN AND FASTSCAN SHOULD BE MADE DEPENDENT ON LOOPSIZ */
#define	SLOWSCAN	30		/* seconds per loop when memory easy */
#define	FASTSCAN	20		/* seconds per loop when memory tight */
#define	SAFERSS		32		/* nominal ``small'' resident set size
					   protected against replacement */
#define	MAXPGIO		40		/* max desired paging i/o per second,
					   if exceeded, and freemem < desfree
					   then we try to swap someone out */

/*
 * Virtual memory related conversion macros
 */

/* Core clicks to number of pages of page tables needed to map that much */
#define	ctopt(x)	(((x)+NPTEPG-1)/NPTEPG)

/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	vtotp(p, v)	((int)(v))
#define	vtodp(p, v)	((int)((v) - (p)->p_tsize))
#define	vtosp(p, v)	((int)(btop(USRSTACK) - 1 - (v)))
#define	tptov(p, i)	((unsigned)(i))
#define	dptov(p, i)	((unsigned)((p)->p_tsize + (i)))
#define	sptov(p, i)	((unsigned)(btop(USRSTACK) - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	isassv(p, v)	((v) & P1TOP)
#define	isatsv(p, v)	((v) < (p)->p_tsize)
#define	isadsv(p, v)	((v) >= (p)->p_tsize && !isassv(p, v))

/* Tell whether pte's are text|data|stack */
#define	isaspte(p, pte)		((pte) > sptopte(p, (p)->p_ssize))
#define	isatpte(p, pte)		((pte) < dptopte(p, 0))
#define	isadpte(p, pte)		(!isaspte(p, pte) && !isatpte(p, pte))

/* Text|data|stack pte's to segment page numbers and back */
#define	ptetotp(p, pte)		((pte) - (p)->p_p0br)
#define	ptetodp(p, pte)		((pte) - ((p)->p_p0br + (p)->p_tsize))
#define	ptetosp(p, pte)		(((p)->p_p0br + (p)->p_szpt*NPTEPG - 1) - (pte))
#define	tptopte(p, i)		((p)->p_p0br + (i))
#define	dptopte(p, i)		((p)->p_p0br + (p)->p_tsize + (i))
#define	sptopte(p, i)		(((p)->p_p0br + (p)->p_szpt*NPTEPG - 1) - (i))

/* Bytes to pages without rounding, and back */
#define	btop(x)		(((unsigned)(x)) >> PGSHIFT)
#define	ptob(x)		((caddr_t)((x) << PGSHIFT))

/* Turn virtual addresses into kernel map indices */
#define	kmxtob(a)	(usrpt + (a) * NPTEPG)
#define	btokmx(b)	(((b) - usrpt) / NPTEPG)

/* Average new into old with aging factor time */
#define	ave(smooth, cnt, time) \
	smooth = ((time - 1) * (smooth) + (cnt)) / (time)

/*
 * Virtual memory related instrumentation
 */
struct vmmeter
{
	unsigned v_swpin;	/* swapins */
	unsigned v_swpout;	/* swapouts */
	unsigned v_pswpin;	/* pages swapped in */
	unsigned v_pswpout;	/* pages swapped out */
	unsigned v_pgin;	/* pageins */
	unsigned v_pgout;	/* pageouts */
	unsigned v_intrans;	/* intransit blocking page faults */
	unsigned v_pgrec;	/* total page reclaims */
	unsigned v_exfod;	/* pages filled on demand from executables */
	unsigned v_zfod;	/* pages zero filled on demand */
	unsigned v_vrfod;	/* fills of pages mapped by vread() */
	unsigned v_nexfod;	/* number of exfod's created */
	unsigned v_nzfod;	/* number of zfod's created */
	unsigned v_nvrfod;	/* number of vrfod's created */
	unsigned v_pgfrec;	/* page reclaims from free list */
	unsigned v_faults;	/* total faults taken */
	unsigned v_scan;	/* scans in page out daemon */
	unsigned v_rev;		/* revolutions of the hand */
	unsigned v_dfree;	/* pages freed by daemon */
	unsigned v_swtch;	/* context switches */
};
#ifdef KERNEL
struct	vmmeter cnt, rate, sum;
#endif

/* systemwide totals computed every five seconds */
struct vmtotal
{
	short	t_rq;		/* length of the run queue */
	short	t_dw;		/* jobs in ``disk wait'' (neg priority) */
	short	t_pw;		/* jobs in page wait */
	short	t_sl;		/* jobs sleeping in core */
	short	t_sw;		/* swapped out runnable/short block jobs */
	short	t_vm;		/* total virtual memory */
	short	t_avm;		/* active virtual memory */
	short	t_rm;		/* total real memory in use */
	short	t_arm;		/* active real memory */
	short	t_vmtxt;	/* virtual memory used by text */
	short	t_avmtxt;	/* active virtual memory used by text */
	short	t_rmtxt;	/* real memory used by text */
	short	t_armtxt;	/* active real memory used by text */
	short	t_free;		/* free memory pages */
};
#ifdef KERNEL
struct	vmtotal total;
#endif

struct	forkstat
{
	int	cntfork;
	int	cntvfork;
	int	sizfork;
	int	sizvfork;
};
#ifdef KERNEL
struct	forkstat forkstat;
#endif

struct	swptstat
{
	int	pteasy;		/* easy pt swaps */
	int	ptexpand;	/* pt expansion swaps */
	int	ptshrink;	/* pt shrinking swaps */
	int	ptpack;		/* pt swaps involving spte copying */
};
#ifdef KERNEL
struct	swptstat swptstat;
#endif

#ifdef KERNEL
int	freemem;		/* remaining blocks of free memory */
int	avefree;		/* moving average of remaining free blocks */
int	deficit;		/* estimate of needs of new swapped in procs */
int	nscan;			/* number of scans in last second */
int	multprog;		/* current multiprogramming degree */
int	desscan;		/* desired pages scanned per second */

unsigned rectime;		/* accumulator for reclaim times */
unsigned pgintime;		/* accumulator for page in times */

/* writable copies of tunables */
int	maxpgio;		/* max paging i/o per sec before start swaps */
int	maxslp;			/* max sleep time before very swappable */
int	lotsfree;		/* max free before clock freezes */
int	minfree;		/* minimum free pages before swapping begins */
int	desfree;		/* no of pages to try to keep free via daemon */
int	saferss;		/* no pages not to steal; decays with slptime */
#endif
