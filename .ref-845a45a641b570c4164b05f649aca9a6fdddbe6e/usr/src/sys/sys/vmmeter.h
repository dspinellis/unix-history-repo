/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vmmeter.h	8.1 (Berkeley) %G%
 */

/*
 * System wide statistics counters.
 */
struct vmmeter {
	/*
	 * General system activity.
	 */
	unsigned v_swtch;	/* context switches */
	unsigned v_trap;	/* calls to trap */
	unsigned v_syscall;	/* calls to syscall() */
	unsigned v_intr;	/* device interrupts */
	unsigned v_soft;	/* software interrupts */
	unsigned v_faults;	/* total faults taken */
	/*
	 * Virtual memory activity.
	 */
	unsigned v_lookups;	/* object cache lookups */
	unsigned v_hits;	/* object cache hits */
	unsigned v_vm_faults;	/* number of address memory faults */
	unsigned v_cow_faults;	/* number of copy-on-writes */
	unsigned v_swpin;	/* swapins */
	unsigned v_swpout;	/* swapouts */
	unsigned v_pswpin;	/* pages swapped in */
	unsigned v_pswpout;	/* pages swapped out */
	unsigned v_pageins;	/* number of pageins */
	unsigned v_pageouts;	/* number of pageouts */
	unsigned v_pgpgin;	/* pages paged in */
	unsigned v_pgpgout;	/* pages paged out */
	unsigned v_intrans;	/* intransit blocking page faults */
	unsigned v_reactivated;	/* number of pages reactivated from free list */
	unsigned v_rev;		/* revolutions of the hand */
	unsigned v_scan;	/* scans in page out daemon */
	unsigned v_dfree;	/* pages freed by daemon */
	unsigned v_pfree;	/* pages freed by exiting processes */
	unsigned v_zfod;	/* pages zero filled on demand */
	unsigned v_nzfod;	/* number of zfod's created */
	/*
	 * Distribution of page usages.
	 */
	unsigned v_page_size;	/* page size in bytes */
	unsigned v_kernel_pages;/* number of pages in use by kernel */
	unsigned v_free_target;	/* number of pages desired free */
	unsigned v_free_min;	/* minimum number of pages desired free */
	unsigned v_free_count;	/* number of pages free */
	unsigned v_wire_count;	/* number of pages wired down */
	unsigned v_active_count;/* number of pages active */
	unsigned v_inactive_target; /* number of pages desired inactive */
	unsigned v_inactive_count;  /* number of pages inactive */
};
#ifdef KERNEL
struct	vmmeter cnt;
#endif

/* systemwide totals computed every five seconds */
struct vmtotal
{
	short	t_rq;		/* length of the run queue */
	short	t_dw;		/* jobs in ``disk wait'' (neg priority) */
	short	t_pw;		/* jobs in page wait */
	short	t_sl;		/* jobs sleeping in core */
	short	t_sw;		/* swapped out runnable/short block jobs */
	long	t_vm;		/* total virtual memory */
	long	t_avm;		/* active virtual memory */
	long	t_rm;		/* total real memory in use */
	long	t_arm;		/* active real memory */
	long	t_vmshr;	/* shared virtual memory */
	long	t_avmshr;	/* active shared virtual memory */
	long	t_rmshr;	/* shared real memory */
	long	t_armshr;	/* active shared real memory */
	long	t_free;		/* free memory pages */
};
#ifdef KERNEL
struct	vmtotal total;
#endif

/*
 * Optional instrumentation.
 */
#ifdef PGINPROF

#define	NDMON	128
#define	NSMON	128

#define	DRES	20
#define	SRES	5

#define	PMONMIN	20
#define	PRES	50
#define	NPMON	64

#define	RMONMIN	130
#define	RRES	5
#define	NRMON	64

/* data and stack size distribution counters */
unsigned int	dmon[NDMON+1];
unsigned int	smon[NSMON+1];

/* page in time distribution counters */
unsigned int	pmon[NPMON+2];

/* reclaim time distribution counters */
unsigned int	rmon[NRMON+2];

int	pmonmin;
int	pres;
int	rmonmin;
int	rres;

unsigned rectime;		/* accumulator for reclaim times */
unsigned pgintime;		/* accumulator for page in times */
#endif
