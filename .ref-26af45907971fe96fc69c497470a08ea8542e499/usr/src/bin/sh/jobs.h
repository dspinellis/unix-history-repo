/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)jobs.h	8.1 (Berkeley) %G%
 */

/* Mode argument to forkshell.  Don't change FORK_FG or FORK_BG. */
#define FORK_FG 0
#define FORK_BG 1
#define FORK_NOJOB 2


/*
 * A job structure contains information about a job.  A job is either a
 * single process or a set of processes contained in a pipeline.  In the
 * latter case, pidlist will be non-NULL, and will point to a -1 terminated
 * array of pids.
 */

struct procstat {
	short pid;		/* process id */
	short status;		/* status flags (defined above) */
	char *cmd;		/* text of command being run */
};


/* states */
#define JOBSTOPPED 1		/* all procs are stopped */
#define JOBDONE 2		/* all procs are completed */


struct job {
	struct procstat ps0;	/* status of process */
	struct procstat *ps;	/* status or processes when more than one */
	short nprocs;		/* number of processes */
	short pgrp;		/* process group of this job */
	char state;		/* true if job is finished */
	char used;		/* true if this entry is in used */
	char changed;		/* true if status has changed */
#if JOBS
	char jobctl;		/* job running under job control */
#endif
};

extern short backgndpid;	/* pid of last background process */
extern int job_warning;		/* user was warned about stopped jobs */


#ifdef __STDC__
void setjobctl(int);
void showjobs(int);
struct job *makejob(union node *, int);
int forkshell(struct job *, union node *, int);
int waitforjob(struct job *);
char *commandtext(union node *);
#else
void setjobctl();
void showjobs();
struct job *makejob();
int forkshell();
int waitforjob();
char *commandtext();
#endif

#if ! JOBS
#define setjobctl(on)	/* do nothing */
#endif
