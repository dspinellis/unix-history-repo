/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmmeter.h	6.3 (Berkeley) %G%
 */

/*
 * Virtual memory related instrumentation
 */
struct vmmeter
{
#define	v_first	v_swtch
	unsigned v_swtch;	/* context switches */
	unsigned v_trap;	/* calls to trap */
	unsigned v_syscall;	/* calls to syscall() */
	unsigned v_intr;	/* device interrupts */
	unsigned v_soft;	/* software interrupts */
	unsigned v_pdma;	/* pseudo-dma interrupts */
	unsigned v_pswpin;	/* pages swapped in */
	unsigned v_pswpout;	/* pages swapped out */
	unsigned v_pgin;	/* pageins */
	unsigned v_pgout;	/* pageouts */
	unsigned v_pgpgin;	/* pages paged in */
	unsigned v_pgpgout;	/* pages paged out */
	unsigned v_intrans;	/* intransit blocking page faults */
	unsigned v_pgrec;	/* total page reclaims */
	unsigned v_xsfrec;	/* found in free list rather than on swapdev */
	unsigned v_xifrec;	/* found in free list rather than in filsys */
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
	unsigned v_seqfree;	/* pages taken from sequential programs */
	unsigned v_dfree;	/* pages freed by daemon */
	unsigned v_fastpgrec;	/* fast reclaims in locore */
#define	v_last v_fastpgrec
	unsigned v_swpin;	/* swapins */
	unsigned v_swpout;	/* swapouts */
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
	int	t_vm;		/* total virtual memory */
	int	t_avm;		/* active virtual memory */
	short	t_rm;		/* total real memory in use */
	short	t_arm;		/* active real memory */
	int	t_vmtxt;	/* virtual memory used by text */
	int	t_avmtxt;	/* active virtual memory used by text */
	short	t_rmtxt;	/* real memory used by text */
	short	t_armtxt;	/* active real memory used by text */
	short	t_free;		/* free memory pages */
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
