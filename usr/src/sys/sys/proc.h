/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)proc.h	7.4 (Berkeley) %G%
 */

/*
 * One structure allocated per session.
 */
struct	session {
	struct	proc *s_leader;	/* pointer to session leader */
	int	s_count;	/* number of pgrps in session */
};

/*
 * One structure allocated per process group.
 */
struct	pgrp {
	struct	pgrp *pg_hforw;	/* forward link in hash bucket */
	struct	proc *pg_mem;	/* pointer to pgrp members */
	struct	session *pg_session;	/* pointer to session */
	pid_t	pg_id;		/* pgrp id */
	short	pg_jobc;	/* # procs qualifying pgrp for job control */
};

/*
 * One structure allocated per active
 * process. It contains all data needed
 * about the process while the
 * process may be swapped out.
 * Other per process data (user.h)
 * is swapped with the process.
 */
struct	proc {
	struct	proc *p_link;	/* linked list of running processes */
	struct	proc *p_rlink;
	struct	proc *p_nxt;	/* linked list of allocated proc slots */
	struct	proc **p_prev;		/* also zombies, and free proc's */
	struct	pte *p_addr;	/* u-area kernel map address */
	char	p_usrpri;	/* user-priority based on p_cpu and p_nice */
	char	p_pri;		/* priority, negative is high */
	char	p_cpu;		/* cpu usage for scheduling */
	char	p_stat;
	char	p_time;		/* resident time for scheduling */
	char	p_nice;		/* nice for cpu usage */
	char	p_slptime;	/* time since last block */
	char	p_cursig;
	int	p_sig;		/* signals pending to this process */
	int	p_sigmask;	/* current signal mask */
	int	p_sigignore;	/* signals being ignored */
	int	p_sigcatch;	/* signals being caught by user */
	int	p_flag;
	uid_t	p_uid;		/* user id, used to direct tty signals */
	pid_t	p_pid;		/* unique process id */
	pid_t	p_ppid;		/* process id of parent */
	u_short	p_xstat;	/* Exit status for wait */
	struct	rusage *p_ru;	/* mbuf holding exit information */
	short	p_poip;		/* page outs in progress */
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
	struct	pte *p_p0br;	/* page table base P0BR */
	struct	proc *p_xlink;	/* linked list of procs sharing same text */
	short	p_cpticks;	/* ticks of cpu time */
	float	p_pctcpu;	/* %cpu for this process during p_time */
	short	p_ndx;		/* proc index for memall (because of vfork) */
	short	p_idhash;	/* hashed based on p_pid for kill+exit+... */
	struct	proc *p_pptr;	/* pointer to process structure of parent */
	struct	proc *p_cptr;	/* pointer to youngest living child */
	struct	proc *p_osptr;	/* pointer to older sibling processes */
	struct	proc *p_ysptr;	/* pointer to younger siblings */
	struct 	pgrp *p_pgrp;	/* pointer to process group */
#define p_session p_pgrp->pg_session
#define p_pgid	p_pgrp->pg_id
	struct	proc *p_pgrpnxt; /* pointer to next process in process group */
	struct	itimerval p_realtimer;
	struct	quota *p_quota;	/* quotas for this process */
	int	p_traceflag;	/* kernel tracing flags (facilities) */
	struct	inode *p_tracep;/* trace to inode */
#if defined(tahoe)
	int	p_ckey;		/* code cache key */
	int	p_dkey;		/* data cache key */
#endif
};

#define	PIDHSZ		64
#define	PIDHASH(pid)	((pid) & (PIDHSZ - 1))

#ifdef KERNEL
pid_t	pidhash[PIDHSZ];
struct	proc *pfind();
struct	pgrp *pgrphash[PIDHSZ];
struct 	pgrp *pgfind();		/* find process group by id */
struct	proc *proc, *procNPROC;	/* the proc table itself */
struct	proc *freeproc, *zombproc, *allproc;
			/* lists of procs in various states */
int	nproc;

#define	NQS	32		/* 32 run queues */
struct	prochd {
	struct	proc *ph_link;	/* linked list of running processes */
	struct	proc *ph_rlink;
} qs[NQS];
int	whichqs;		/* bit mask summarizing non-empty qs's */

#define SESS_LEADER(p)	((p)->p_session->s_leader == (p))
#define PGRP_JOBC(p)	(((p)->p_pgrp != (p)->p_pptr->p_pgrp) && \
			((p)->p_session == (p)->p_pptr->p_session))
#endif

/* stat codes */
#define	SSLEEP	1		/* awaiting an event */
#define	SWAIT	2		/* (abandoned state) */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */

/* flag codes */
#define	SLOAD	0x0000001	/* in core */
#define	SSYS	0x0000002	/* swapper or pager process */
#define	SLOCK	0x0000004	/* process being swapped out */
#define	SSWAP	0x0000008	/* save area flag */
#define	STRC	0x0000010	/* process is being traced */
#define	SWTED	0x0000020	/* another tracing flag */
#define	SULOCK	0x0000040	/* user settable lock in core */
#define	SPAGE	0x0000080	/* process in page wait state */
#define	SKEEP	0x0000100	/* another flag to prevent swap out */
#define	SOMASK	0x0000200	/* restore old mask after taking signal */
#define	SWEXIT	0x0000400	/* working on exiting */
#define	SPHYSIO	0x0000800	/* doing physical i/o (bio.c) */
#define	SVFORK	0x0001000	/* process resulted from vfork() */
#define	SVFDONE	0x0002000	/* another vfork flag */
#define	SNOVM	0x0004000	/* no vm, parent in a vfork() */
#define	SPAGI	0x0008000	/* init data space on demand, from inode */
#define	SSEQL	0x0010000	/* user warned of sequential vm behavior */
#define	SUANOM	0x0020000	/* user warned of random vm behavior */
#define	STIMO	0x0040000	/* timing out during sleep */
/* was SDETACH */
#define	SOUSIG	0x0100000	/* using old signal mechanism */
#define	SOWEUPC	0x0200000	/* owe process an addupc() call at next ast */
#define	SSEL	0x0400000	/* selecting; wakeup/waiting danger */
#define	SLOGIN	0x0800000	/* a login process (legit child of init) */
#define	SPTECHG	0x1000000	/* pte's for process have changed */
#define SKTR	0x2000000	/* pass kernel tracing flags to children */
#define SEXEC	0x8000000	/* process called exec */
