/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)jobs.h	1.1 */

/*
 *	UNIX shell
 *	S. R. Bourne
 *	rewritten by David Korn
 *
 */
#ifdef JOBS

# if BSD || RT
# include	<sgtty.h>
# else
#  ifndef CBUNIX
#  include	<termio.h>
#  define _IOCTL_
#  endif /* CBUNIX */
# endif

# ifndef RT
#  ifndef _IOCTL_
#  include	<sys/ioctl.h>
# endif /* _IOCTL_ */
# endif /* RT */

# if u370 || uts
# define MAXJ	75
# else
# define MAXJ	32
# endif /* u370 */

/* JBYTES is the number of char's needed for MAXJ bits */
#define JBYTES	(1+((MAXJ-1)/(8)))

#endif	/* JOBS */


struct process
{
#ifdef JOBS
	struct process *p_next;	/* next process structure */
#endif	/* JOBS */
	int	p_pid;
#ifdef JOBS
	int	p_pgrp;			/* process group */
	unsigned char	p_job;		/* job number of process */
	unsigned char	p_flag;		/* flags - see below */
	unsigned short	p_sig;		/* signal number */
	long	p_name;			/* offset into history file for command */
# if BSD || RT
	struct sgttyb p_stty;		/* terminal state for job */
# else
#  ifdef CBUNIX
	struct ttiocb p_stty;
	struct termcb p_sterm;
#  else
	struct termio p_stty;		/* terminal state for job */
#  endif /* CBUNIX */
# endif /* BSD */
#endif	/* JOBS */
};

#ifdef JOBS
/* Process states */

#define P_RUNNING	1
#define P_STOPPED	2
#define P_NOTIFY	4
#define P_WAITED	8
#define P_STTY		16
#define P_PIPEJOB	32
#define P_COREDUMP	64

struct jobs
{
	int		cur_pgrp;
	int		mypid;
	short		cur_job;
	short		j_flag;
	int		p_numpost;	/* number of posted jobs */
	int		p_numrun;	/* number of running jobs */
	struct process	*p_pwlist;	/* head of process list */
	unsigned char	p_freejobs[JBYTES];	/* free jobs numbers */
# ifdef SXT
	int		pipe[2];	/* pipe used for synchronization */
	int		maxjob;		/* maximum number of channels */
# endif /* SXT */
};

extern struct jobs jobstat;

#define	J_PIPE		1	/* set when setting up a pipeline */
extern MSG	kill_usage;
extern MSG	bkill;
extern MSG	j_Done;
extern MSG	j_Running;
extern MSG	j_amp;
extern MSG	j_coredump;
extern MSG	j_cpid;
extern MSG	j_job;
extern MSG	j_kill;
extern MSG	j_no_jctl;
extern MSG	j_newtty;
extern MSG	j_no_proc;
extern MSG	j_no_job;
extern MSG	j_no_start;
extern MSG	j_not_tty;
extern MSG	j_oldtty;
extern MSG	j_perm;
extern MSG	j_running;
extern MSG	j_space;
extern MSG	j_terminate;
#endif	/* JOBS */
extern int parent;
extern char	*sysmsg[];
