/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)proc.h	7.24 (Berkeley) %G%
 */

#include "vm/vm.h"			/* XXX */

/*
 * One structure allocated per session.
 */
struct	session {
	int	s_count;		/* ref cnt; pgrps in session */
	struct	proc *s_leader;		/* session leader */
	struct	vnode *s_ttyvp;		/* vnode of controlling terminal */
	struct	tty *s_ttyp;		/* controlling terminal */
	char	s_login[MAXLOGNAME];	/* setlogin() name */
};

/*
 * One structure allocated per process group.
 */
struct	pgrp {
	struct	pgrp *pg_hforw;		/* forward link in hash bucket */
	struct	proc *pg_mem;		/* pointer to pgrp members */
	struct	session *pg_session;	/* pointer to session */
	pid_t	pg_id;			/* pgrp id */
	int	pg_jobc;	/* # procs qualifying pgrp for job control */
};

/*
 * Description of a process.
 * This structure contains the information needed to manage a thread
 * of control, known in UN*X as a process; it has references to substructures
 * containing descriptions of things that the process uses, but may share
 * with related processes.  The process structure and the substructures
 * are always addressible except for those marked "(proc only)" below,
 * which might be addressible only on a processor on which the process
 * is running.
 */
struct	proc {
	struct	proc *p_link;		/* doubly-linked run/sleep queue */
	struct	proc *p_rlink;
	struct	proc *p_nxt;		/* linked list of active procs */
	struct	proc **p_prev;		/*    and zombies */

	/* substructures: */
	struct	pcred *p_cred;		/* process owner's identity */
	struct	filedesc *p_fd;		/* ptr to open files structure */
	struct	pstats *p_stats;	/* accounting/statistics (proc only) */
	struct	plimit *p_limit;	/* process limits */
	struct	vmspace *p_vmspace;	/* address space */
	struct	sigacts *p_sigacts;	/* signal actions, state (proc only) */

#define	p_ucred		p_cred->pc_ucred
#define	p_rlimit	p_limit->pl_rlimit

	int	p_flag;
	char	p_stat;
/*	char	p_space; */

	pid_t	p_pid;		/* unique process id */
	struct	proc *p_hash;	/* hashed based on p_pid for kill+exit+... */
	struct	proc *p_pgrpnxt; /* pointer to next process in process group */
	struct	proc *p_pptr;	/* pointer to process structure of parent */
	struct	proc *p_osptr;	/* pointer to older sibling processes */

/* The following fields are all zeroed upon creation in fork */
#define	p_startzero	p_ysptr
	struct	proc *p_ysptr;	/* pointer to younger siblings */
	struct	proc *p_cptr;	/* pointer to youngest living child */

	/* scheduling */
	u_int	p_cpu;		/* cpu usage for scheduling */
	int	p_cpticks;	/* ticks of cpu time */
	fixpt_t	p_pctcpu;	/* %cpu for this process during p_time */
	caddr_t p_wchan;	/* event process is awaiting */
	u_int	p_time;		/* resident/nonresident time for swapping */
	u_int	p_slptime;	/* time since last block */

	struct	itimerval p_realtimer;	/* alarm timer */
	struct	timeval p_utime;	/* user time */
	struct	timeval p_stime;	/* system time */

	int	p_traceflag;	/* kernel trace points */
	struct	vnode *p_tracep;/* trace to vnode */

	int	p_sig;		/* signals pending to this process */

/* end area that is zeroed on creation */
#define	p_endzero	p_startcopy

/* The following fields are all copied upon creation in fork */
	sigset_t p_sigmask;	/* current signal mask */
#define	p_startcopy	p_sigmask
	sigset_t p_sigignore;	/* signals being ignored */
	sigset_t p_sigcatch;	/* signals being caught by user */

	u_char	p_pri;		/* priority, negative is high */
	u_char	p_usrpri;	/* user-priority based on p_cpu and p_nice */
	char	p_nice;		/* nice for cpu usage */
/*	char	p_space; */

	struct 	pgrp *p_pgrp;	/* pointer to process group */
	char	p_comm[MAXCOMLEN+1];

/* end area that is copied on creation */
#define	p_endcopy	p_wmesg
	char	*p_wmesg;	/* reason for sleep */
	int	p_thread;	/* id for this "thread" (Mach glue) XXX */
	caddr_t	p_addr;		/* kernel virtual address of u-area */
	swblk_t	p_swaddr;	/* disk address of u area when swapped */
	int	*p_regs;	/* saved registers during syscall/trap */

	u_short	p_xstat;	/* Exit status for wait; also stop signal */
	u_short	p_dupfd;	/* sideways return value from fdopen XXX */
	u_short	p_acflag;	/* accounting flags */
/*	u_short	p_space; */
	struct	rusage *p_ru;	/* exit information XXX */

	long	p_spare[4];	/* tmp spares to avoid shifting eproc */
};

#define	p_session	p_pgrp->pg_session
#define	p_pgid		p_pgrp->pg_id

/* MOVE TO ucred.h? */
/*
 * Shareable process credentials (always resident).
 * This includes a reference to the current user credentials
 * as well as real and saved ids that may be used to change ids.
 */
struct	pcred {
	struct	ucred *pc_ucred;	/* current credentials */
	uid_t	p_ruid;			/* real user id */
	uid_t	p_svuid;		/* saved effective user id */
	gid_t	p_rgid;			/* real group id */
	gid_t	p_svgid;		/* saved effective group id */
	int	p_refcnt;		/* number of references */
};

/* 
 * getkerninfo() proc ops return arrays of augmented proc structures:
 */
struct kinfo_proc {
	struct	proc kp_proc;			/* proc structure */
	struct	eproc {
		struct	proc *e_paddr;		/* address of proc */
		struct	session *e_sess;	/* session pointer */
		struct	pcred e_pcred;		/* process credentials */
		struct	ucred e_ucred;		/* current credentials */
		struct	vmspace e_vm;		/* address space */
		pid_t	e_pgid;			/* process group id */
		short	e_jobc;			/* job control counter */
		dev_t	e_tdev;			/* controlling tty dev */
		pid_t	e_tpgid;		/* tty process group id */
		struct	session *e_tsess;	/* tty session pointer */
#define	WMESGLEN	7
		char	e_wmesg[WMESGLEN+1];	/* wchan message */
		segsz_t e_xsize;		/* text size */
		short	e_xrssize;		/* text rss */
		short	e_xccount;		/* text references */
		short	e_xswrss;
		long	e_flag;
#define	EPROC_CTTY	0x01	/* controlling tty vnode active */
#define	EPROC_SLEADER	0x02	/* session leader */
		long	e_spare[7];
	} kp_eproc;
};

#ifdef KERNEL
/*
 * We use process IDs <= PID_MAX;
 * PID_MAX + 1 must also fit in a pid_t
 * (used to represent "no process group").
 */
#define	PID_MAX		30000
#define	NO_PID		30001
#define	PIDHASH(pid)	((pid) & pidhashmask)
extern	int pidhashmask;		/* in param.c */
extern	struct proc *pidhash[];		/* in param.c */
struct	proc *pfind();			/* find process by id */
extern	struct pgrp *pgrphash[];	/* in param.c */
struct 	pgrp *pgfind();			/* find process group by id */
struct	proc *zombproc, *allproc;
extern	struct proc proc0;		/* process slot for swapper */
struct	proc *initproc, *pageproc;	/* process slots for init, pager */
#ifdef notyet
struct	proc *curproc;			/* current running proc */
#endif
					/* lists of procs in various states */
extern	int nprocs, maxproc;		/* current and max number of procs */

#define	NQS	32		/* 32 run queues */
struct	prochd {
	struct	proc *ph_link;	/* linked list of running processes */
	struct	proc *ph_rlink;
} qs[NQS];

int	whichqs;		/* bit mask summarizing non-empty qs's */

#define SESS_LEADER(p)	((p)->p_session->s_leader == (p))
#define	SESSHOLD(s)	((s)->s_count++)
#define	SESSRELE(s)	{ \
		if (--(s)->s_count == 0) \
			FREE(s, M_SESSION); \
		}
#endif

/* stat codes */
#define	SSLEEP	1		/* awaiting an event */
#define	SWAIT	2		/* (abandoned state) */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */

/* flag codes */
/* NEED TO CHECK which of these are still used */
#define	SLOAD	0x0000001	/* in core */
#define	SSYS	0x0000002	/* swapper or pager process */
#define	SLOCK	0x0000004	/* process being swapped out */
#define	SSWAP	0x0000008	/* save area flag */
#define	STRC	0x0000010	/* process is being traced */
#define	SWTED	0x0000020	/* another tracing flag */
#define	SSINTR	0x0000040	/* sleep is interruptible */
#define	SPAGE	0x0000080	/* process in page wait state */
#define	SKEEP	0x0000100	/* another flag to prevent swap out */
/*#define SOMASK	0x0000200	/* restore old mask after taking signal */
#define	SWEXIT	0x0000400	/* working on exiting */
#define	SPHYSIO	0x0000800	/* doing physical i/o */
#define	SPPWAIT	0x0001000	/* parent is waiting for child to exec/exit */
#define	SVFORK	SPARSYNC	/* process resulted from vfork() XXX */
/*#define SVFDONE	0x0002000	/* another vfork flag XXX */
/*#define SNOVM	0x0004000	/* no vm, parent in a vfork() XXX */
#define	SPAGV	0x0008000	/* init data space on demand, from vnode */
#define	SSEQL	0x0010000	/* user warned of sequential vm behavior */
#define	SUANOM	0x0020000	/* user warned of random vm behavior */
#define	STIMO	0x0040000	/* timing out during sleep */
#define	SNOCLDSTOP 0x0080000	/* no SIGCHLD when children stop */
#define	SCTTY	0x0100000	/* has a controlling terminal */
#define	SOWEUPC	0x0200000	/* owe process an addupc() call at next ast */
#define	SSEL	0x0400000	/* selecting; wakeup/waiting danger */
#define SEXEC	0x0800000	/* process called exec */
#define	SHPUX	0x1000000	/* HP-UX process (HPUXCOMPAT) */
#define	SULOCK	0x2000000	/* locked in core after swap error XXX */
#define	SPTECHG	0x4000000	/* pte's for process have changed XXX */
