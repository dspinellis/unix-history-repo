/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vmmeter.h	8.2 (Berkeley) %G%
 */

/*
 * System wide statistics counters.
 */
struct vmmeter {
	/*
	 * General system activity.
	 */
	u_int v_swtch;		/* context switches */
	u_int v_trap;		/* calls to trap */
	u_int v_syscall;	/* calls to syscall() */
	u_int v_intr;		/* device interrupts */
	u_int v_soft;		/* software interrupts */
	u_int v_faults;		/* total faults taken */
	/*
	 * Virtual memory activity.
	 */
	u_int v_lookups;	/* object cache lookups */
	u_int v_hits;		/* object cache hits */
	u_int v_vm_faults;	/* number of address memory faults */
	u_int v_cow_faults;	/* number of copy-on-writes */
	u_int v_swpin;		/* swapins */
	u_int v_swpout;		/* swapouts */
	u_int v_pswpin;		/* pages swapped in */
	u_int v_pswpout;	/* pages swapped out */
	u_int v_pageins;	/* number of pageins */
	u_int v_pageouts;	/* number of pageouts */
	u_int v_pgpgin;		/* pages paged in */
	u_int v_pgpgout;	/* pages paged out */
	u_int v_intrans;	/* intransit blocking page faults */
	u_int v_reactivated;	/* number of pages reactivated from free list */
	u_int v_rev;		/* revolutions of the hand */
	u_int v_scan;		/* scans in page out daemon */
	u_int v_dfree;		/* pages freed by daemon */
	u_int v_pfree;		/* pages freed by exiting processes */
	u_int v_zfod;		/* pages zero filled on demand */
	u_int v_nzfod;		/* number of zfod's created */
	/*
	 * Distribution of page usages.
	 */
	u_int v_page_size;	/* page size in bytes */
	u_int v_kernel_pages;	/* number of pages in use by kernel */
	u_int v_free_target;	/* number of pages desired free */
	u_int v_free_min;	/* minimum number of pages desired free */
	u_int v_free_count;	/* number of pages free */
	u_int v_wire_count;	/* number of pages wired down */
	u_int v_active_count;	/* number of pages active */
	u_int v_inactive_target; /* number of pages desired inactive */
	u_int v_inactive_count;  /* number of pages inactive */
};
#ifdef KERNEL
struct	vmmeter cnt;
#endif

/* systemwide totals computed every five seconds */
struct vmtotal
{
	int16_t	t_rq;		/* length of the run queue */
	int16_t	t_dw;		/* jobs in ``disk wait'' (neg priority) */
	int16_t	t_pw;		/* jobs in page wait */
	int16_t	t_sl;		/* jobs sleeping in core */
	int16_t	t_sw;		/* swapped out runnable/short block jobs */
	int32_t	t_vm;		/* total virtual memory */
	int32_t	t_avm;		/* active virtual memory */
	int32_t	t_rm;		/* total real memory in use */
	int32_t	t_arm;		/* active real memory */
	int32_t	t_vmshr;	/* shared virtual memory */
	int32_t	t_avmshr;	/* active shared virtual memory */
	int32_t	t_rmshr;	/* shared real memory */
	int32_t	t_armshr;	/* active shared real memory */
	int32_t	t_free;		/* free memory pages */
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
u_int	dmon[NDMON+1];
u_int	smon[NSMON+1];

/* page in time distribution counters */
u_int	pmon[NPMON+2];

/* reclaim time distribution counters */
u_int	rmon[NRMON+2];

int	pmonmin;
int	pres;
int	rmonmin;
int	rres;

u_int rectime;		/* accumulator for reclaim times */
u_int pgintime;		/* accumulator for page in times */
#endif
