/*-
 * Copyright (c) 1986, 1989, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)proc.h	7.37 (Berkeley) %G%
 */

#ifndef _PROC_H_
#define	_PROC_H_

#include <machine/proc.h>		/* machine-dependent proc substruct */
#include <sys/select.h>			/* for struct selinfo */

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
 * are always addressible except for those marked "(PROC ONLY)" below,
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
	struct	pstats *p_stats;	/* accounting/statistics (PROC ONLY) */
	struct	plimit *p_limit;	/* process limits */
	struct	vmspace *p_vmspace;	/* address space */
	struct	sigacts *p_sigacts;	/* signal actions, state (PROC ONLY) */

#define	p_ucred		p_cred->pc_ucred
#define	p_rlimit	p_limit->pl_rlimit

	int	p_flag;
	char	p_stat;
	char	p_pad1;

	pid_t	p_pid;		/* unique process id */
	struct	proc *p_hash;	/* hashed based on p_pid for kill+exit+... */
	struct	proc *p_pgrpnxt; /* pointer to next process in process group */
	struct	proc *p_pptr;	/* pointer to process structure of parent */
	struct	proc *p_osptr;	/* pointer to older sibling processes */

/* The following fields are all zeroed upon creation in fork */
#define	p_startzero	p_ysptr
	struct	proc *p_ysptr;	/* pointer to younger siblings */
	struct	proc *p_cptr;	/* pointer to youngest living child */
	pid_t	p_oppid;	/* save parent pid during ptrace XXX */
	short	p_dupfd;	/* sideways return value from fdopen XXX */

	/* scheduling */
	u_int	p_cpu;		/* cpu usage for scheduling */
	int	p_cpticks;	/* ticks of cpu time */
	fixpt_t	p_pctcpu;	/* %cpu for this process during p_time */
	caddr_t p_wchan;	/* event process is awaiting */
	char	*p_wmesg;	/* reason for sleep */
	u_int	p_time;		/* resident/nonresident time for swapping */
	u_int	p_slptime;	/* time since last block */

	struct	itimerval p_realtimer;	/* alarm timer */
	struct	timeval p_rtime;	/* real time */
	u_quad_t p_uticks;		/* statclock hits in user mode */
	u_quad_t p_sticks;		/* statclock hits in system mode */
	u_quad_t p_iticks;		/* statclock hits processing intr */

	int	p_traceflag;	/* kernel trace points */
	struct	vnode *p_tracep;/* trace to vnode */

	int	p_sig;		/* signals pending to this process */

	long	p_spare[6];	/* tmp spares to avoid shifting eproc */

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
	char	p_comm[MAXCOMLEN+1];

	struct 	pgrp *p_pgrp;	/* pointer to process group */

/* end area that is copied on creation */
#define	p_endcopy	p_thread
	int	p_thread;	/* id for this "thread" (Mach glue) XXX */
	struct	user *p_addr;	/* kernel virtual addr of u-area (PROC ONLY) */
	struct	mdproc p_md;	/* any machine-dependent fields */

	u_short	p_xstat;	/* Exit status for wait; also stop signal */
	u_short	p_acflag;	/* accounting flags */
	struct	rusage *p_ru;	/* exit information XXX */

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

/* stat codes */
#define	SSLEEP	1		/* awaiting an event */
#define	SWAIT	2		/* (abandoned state) */
#define	SRUN	3		/* running */
#define	SIDL	4		/* intermediate state in process creation */
#define	SZOMB	5		/* intermediate state in process termination */
#define	SSTOP	6		/* process being traced */

/* flag codes */
#define	SLOAD	0x0000001	/* in core */
#define	SSYS	0x0000002	/* system proc: no sigs, stats or swapping */
#define	SSINTR	0x0000004	/* sleep is interruptible */
#define	SCTTY	0x0000008	/* has a controlling terminal */
#define	SPPWAIT	0x0000010	/* parent is waiting for child to exec/exit */
#define SEXEC	0x0000020	/* process called exec */
#define	STIMO	0x0000040	/* timing out during sleep */
#define	SSEL	0x0000080	/* selecting; wakeup/waiting danger */
#define	SWEXIT	0x0000100	/* working on exiting */
#define	SNOCLDSTOP 0x0000200	/* no SIGCHLD when children stop */
/* the following three should probably be changed into a hold count */
#define	SLOCK	0x0000400	/* process being swapped out */
#define	SKEEP	0x0000800	/* another flag to prevent swap out */
#define	SPHYSIO	0x0001000	/* doing physical i/o */
#define	STRC	0x0004000	/* process is being traced */
#define	SWTED	0x0008000	/* another tracing flag */
#define	SUGID	0x0020000	/* had set id privileges since last exec */
#define	SADVLCK	0x0040000	/* process may hold a POSIX advisory lock */
#define	SPROFIL	0x0080000	/* has started profiling */
/* the following should be moved to machine-dependent areas */
#define	SOWEUPC	0x0002000	/* owe process an addupc() call at next ast */

#ifdef KERNEL
/*
 * We use process IDs <= PID_MAX;
 * PID_MAX + 1 must also fit in a pid_t
 * (used to represent "no process group").
 */
#define	PID_MAX		30000
#define	NO_PID		30001
#define	PIDHASH(pid)	((pid) & pidhashmask)

#define SESS_LEADER(p)	((p)->p_session->s_leader == (p))
#define	SESSHOLD(s)	((s)->s_count++)
#define	SESSRELE(s)	{ \
		if (--(s)->s_count == 0) \
			FREE(s, M_SESSION); \
	}

extern	int pidhashmask;		/* in param.c */
extern	struct proc *pidhash[];		/* in param.c */
struct	proc *pfind();			/* find process by id */
extern	struct pgrp *pgrphash[];	/* in param.c */
struct 	pgrp *pgfind();			/* find process group by id */
volatile struct proc *allproc;		/* list of active procs */
struct	proc *zombproc;			/* list of zombie procs */
extern	struct proc proc0;		/* process slot for swapper */
struct	proc *initproc, *pageproc;	/* process slots for init, pager */
extern	struct proc *curproc;		/* current running proc */
extern	int nprocs, maxproc;		/* current and max number of procs */

#define	NQS	32		/* 32 run queues */
struct	prochd {
	struct	proc *ph_link;	/* linked list of running processes */
	struct	proc *ph_rlink;
} qs[NQS];

int	whichqs;		/* bit mask summarizing non-empty qs's */

void	sleep __P((void *chan, int pri));
int	tsleep __P((void *chan, int pri, char *wmesg, int timo));
void	unsleep __P((struct proc *));
void	wakeup __P((void *chan));
void	setrun __P((struct proc *));
void	setpri __P((struct proc *));
void	swtch __P((void));

#endif	/* KERNEL */

#endif	/* !_PROC_H_ */
