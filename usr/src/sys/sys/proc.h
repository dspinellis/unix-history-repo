/*	proc.h	4.8	81/04/23	*/

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
	struct	proc *p_link;	/* linked list of running processes */
	struct	proc *p_rlink;
	struct	pte *p_addr;	/* u-area kernel map address */
	char	p_usrpri;	/* user-priority based on p_cpu and p_nice */
	char	p_pri;		/* priority, negative is high */
	char	p_cpu;		/* cpu usage for scheduling */
	char	p_stat;
	char	p_time;		/* resident time for scheduling */
	char	p_nice;		/* nice for cpu usage */
	char	p_slptime;	/* time since last block */
	char	p_cursig;
	long	p_sig;		/* signals pending to this process */
	long	p_siga0;	/* low bit of 2 bit signal action */
	long	p_siga1;	/* high bit of 2 bit signal action */
#define	p_ignsig p_siga0	/* ignored signal mask */
	int	p_flag;
	short	p_uid;		/* user id, used to direct tty signals */
	short	p_pgrp;		/* name of process group leader */
	short	p_pid;		/* unique process id */
	short	p_ppid;		/* process id of parent */
	short	p_poip;		/* count of page outs in progress */
	short	p_szpt;		/* copy of page table size */
	size_t	p_tsize;	/* size of text (clicks) */
	size_t	p_dsize;	/* size of data space (clicks) */
	size_t	p_ssize;	/* copy of stack size (clicks) */
	size_t 	p_rssize; 	/* current resident set size in clicks */
	size_t	p_maxrss;	/* copy of u.u_limit[MAXRSS] */
	size_t	p_swrss;	/* resident set size before last swap */
	swblk_t	p_swaddr;	/* disk address of u area when swapped */
	caddr_t p_wchan;	/* event process is awaiting */
	struct	text *p_textp;	/* pointer to text structure */
	int	p_clktim;	/* time to alarm clock signal */
	struct	pte *p_p0br;	/* page table base P0BR */
	struct	proc *p_xlink;	/* linked list of procs sharing same text */
	short	p_cpticks;	/* ticks of cpu time */
	float	p_pctcpu;	/* %cpu for this process during p_time */
	short	p_ndx;		/* proc index for memall (because of vfork) */
	short	p_idhash;	/* hashed based on p_pid for kill+exit+... */
	struct	proc *p_pptr;	/* pointer to process structure of parent */
};

#define	PIDHSZ		63
#define	PIDHASH(pid)	((pid) % PIDHSZ)

#ifdef KERNEL
short	pidhash[PIDHSZ];

struct	proc *pfind();
#endif

#ifdef	KERNEL
struct	proc *proc, *procNPROC;	/* the proc table itself */
int	nproc;

#define	NQS	32		/* 32 run queues */
struct	prochd {
	struct	proc *ph_link;	/* linked list of running processes */
	struct	proc *ph_rlink;
} qs[NQS];
int	whichqs;		/* bit mask summarizing non-empty qs's */
#endif

/* stat codes */
#define	SSLEEP	1		/* awaiting an event */
#define	SWAIT	2		/* (abandoned state) */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */

/* flag codes */
#define	SLOAD	0x000001	/* in core */
#define	SSYS	0x000002	/* swapper or pager process */
#define	SLOCK	0x000004	/* process being swapped out */
#define	SSWAP	0x000008	/* save area flag */
#define	STRC	0x000010	/* process is being traced */
#define	SWTED	0x000020	/* another tracing flag */
#define	SULOCK	0x000040	/* user settable lock in core */
#define	SPAGE	0x000080	/* process in page wait state */
#define	SKEEP	0x000100	/* another flag to prevent swap out */
#define	SDLYU	0x000200	/* delayed unlock of pages */
#define	SWEXIT	0x000400	/* working on exiting */
#define	SPHYSIO	0x000800	/* doing physical i/o (bio.c) */
#define	SVFORK	0x001000	/* process resulted from vfork() */
#define	SVFDONE	0x002000	/* another vfork flag */
#define	SNOVM	0x004000	/* no vm, parent in a vfork() */
#define	SPAGI	0x008000	/* init data space on demand, from inode */
#define	SSEQL	0x010000	/* user warned of sequential vm behavior */
#define	SUANOM	0x020000	/* user warned of random vm behavior */
#define	STIMO	0x040000	/* timing out during sleep */
#define	SDETACH	0x080000	/* detached inherited by init */
#define	SNUSIG	0x100000	/* using new signal mechanism */
#define	SOWEUPC	0x200000	/* owe process an addupc() call at next ast */

/*
 * parallel proc structure
 * to replace part with times
 * to be passed to parent process
 * in ZOMBIE state.
 *
 * THIS SHOULD BE DONE WITH A union() CONSTRUCTION
 */
struct	xproc
{
	struct	proc *xp_link;
	struct	proc *xp_rlink;
	struct	pte *xp_addr;
	char	xp_usrpri;
	char	xp_pri;		/* priority, negative is high */
	char	xp_cpu;		/* cpu usage for scheduling */
	char	xp_stat;
	char	xp_time;	/* resident time for scheduling */
	char	xp_nice;	/* nice for cpu usage */
	char	xp_slptime;
	char	p_cursig;
	int	xp_sig;		/* signals pending to this process */
	int	xp_siga0;
	int	xp_siga1;
	int	xp_flag;
	short	xp_uid;		/* user id, used to direct tty signals */
	short	xp_pgrp;	/* name of process group leader */
	short	xp_pid;		/* unique process id */
	short	xp_ppid;	/* process id of parent */
	short	xp_xstat;	/* Exit status for wait */
	struct	vtimes xp_vm;
};
