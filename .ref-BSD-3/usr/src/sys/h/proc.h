/*	proc.h	2.1	1/5/80	*/

/*
 * One structure allocated per active
 * process. It contains all data needed
 * about the process while the
 * process may be swapped out.
 * Other per process data (user.h)
 * is swapped with the process.
 */
struct	proc
{
	char	p_stat;
	char	p_slptime;	/* time since last block */
	short	p_flag;
	short	p_cpu;		/* cpu usage for scheduling */
	short	p_time;		/* resident time for scheduling */
	char	p_pri;		/* priority, negative is high */
	char	p_nice;		/* nice for cpu usage */
	short	p_sig;		/* signals pending to this process */
	short	p_uid;		/* user id, used to direct tty signals */
	short	p_pgrp;		/* name of process group leader */
	short	p_pid;		/* unique process id */
	short	p_ppid;		/* process id of parent */
	short	p_addr[UPAGES];	/* page table entries of u-area */
	short	p_poip;		/* count of page outs in progress */
	short	p_szpt;		/* copy of page table size */
	size_t	p_tsize;	/* size of text (clicks) */
	size_t	p_dsize;	/* size of data space (clicks) */
	size_t	p_ssize;	/* copy of stack size (clicks) */
	size_t 	p_rssize; 	/* current resident set size in clicks */
	size_t	p_swrss;	/* resident set size before last swap */
	swblk_t	p_swaddr;	/* disk address of u area when swapped */
	caddr_t p_wchan;	/* event process is awaiting */
	struct	text *p_textp;	/* pointer to text structure */
	struct	proc *p_link;	/* linked list of running processes */
	int	p_clktim;	/* time to alarm clock signal */
	struct	pte *p_p0br;	/* page table base P0BR */
	struct	proc *p_xlink;	/* linked list of procs sharing same text */
	short	p_faults;	/* faults in last second */
	short	p_aveflt;	/* average of p_faults into past */
	short	p_ndx;		/* proc index for memall (because of vfork) */
};

#ifdef	KERNEL
extern	struct proc proc[];	/* the proc table itself */

struct	proc *runq;		/* pointer to linked list of running processes */
#endif

/* stat codes */
#define	SSLEEP	1		/* awaiting an event */
#define	SWAIT	2		/* (abandoned state) */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */

/* flag codes */
#define	SLOAD	0x0001		/* in core */
#define	SSYS	0x0002		/* swapper or pager process */
#define	SLOCK	0x0004		/* process being swapped out */
#define	SSWAP	0x0008		/* save area flag */
#define	STRC	0x0010		/* process is being traced */
#define	SWTED	0x0020		/* another tracing flag */
#define	SULOCK	0x0040		/* user settable lock in core */
#define	SPAGE	0x0080		/* process in page wait state */
#define	SKEEP	0x0100		/* another flag to prevent swap out */
#define	SDLYU	0x0200		/* delayed unlock of pages */
#define	SWEXIT	0x0400		/* working on exiting */
#define	SUNUSED	0x0800		/* used to be page out in progress */
#define	SVFORK	0x1000		/* process resulted from vfork() */
#define	SVFDONE	0x2000		/* another vfork flag */
#define	SNOVM	0x4000		/* no vm, parent in a vfork() */
#define	SPAGI	0x8000		/* init data space on demand, from inode */

/*
 * parallel proc structure
 * to replace part with times
 * to be passed to parent process
 * in ZOMBIE state.
 */
struct	xproc
{
	char	xp_stat;
	char	xp_slptime;
	short	xp_flag;
	short	xp_cpu;		/* cpu usage for scheduling */
	short	xp_time;	/* resident time for scheduling */
	char	xp_pri;		/* priority, negative is high */
	char	xp_nice;	/* nice for cpu usage */
	short	xp_sig;		/* signals pending to this process */
	short	xp_uid;		/* user id, used to direct tty signals */
	short	xp_pgrp;	/* name of process group leader */
	short	xp_pid;		/* unique process id */
	short	xp_ppid;	/* process id of parent */
	short	xp_xstat;	/* Exit status for wait */
	time_t	xp_utime;	/* user time, this proc */
	time_t	xp_stime;	/* system time, this proc */
};
