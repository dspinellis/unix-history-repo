/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.proc.c,v 3.19 1991/11/26 04:28:26 christos Exp $ */
/*
 * sh.proc.c: Job manipulations
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: sh.proc.c,v 3.19 1991/11/26 04:28:26 christos Exp $")

#include "ed.h"
#include "tc.h"
#include "tc.wait.h"

#if !defined(NSIG) && defined(SIGMAX)
# define NSIG (SIGMAX+1)
#endif /* !NSIG && SIGMAX */
#if !defined(NSIG) && defined(_NSIG)
# define NSIG _NSIG
#endif /* !NSIG && _NSIG */

#ifdef aiws
# undef HZ
# define HZ 16
#endif /* aiws */

#ifndef HZ
# define HZ	100		/* for division into seconds */
#endif

#if (defined(_BSD) && defined(_BSD_INCLUDES)) || (defined(IRIS4D) && __STDC__)
# define BSDWAIT
#endif
#ifndef WTERMSIG
# define WTERMSIG(w)	(((union wait *) &(w))->w_termsig)
# ifndef BSDWAIT
#  define BSDWAIT
# endif
#endif /* !WTERMSIG */
#ifndef WEXITSTATUS
# define WEXITSTATUS(w)	(((union wait *) &(w))->w_retcode)
#endif /* !WEXITSTATUS */
#ifndef WSTOPSIG
# define WSTOPSIG(w)	(((union wait *) &(w))->w_stopsig)
#endif /* WSTOPSIG */

#ifndef WCOREDUMP
# ifdef BSDWAIT
#  define WCOREDUMP(w)	(((union wait *) &(w))->w_coredump)
# else /* !BSDWAIT */
#  define WCOREDUMP(w)	((w) & 0200)
# endif /* !BSDWAIT */
#endif /* !WCOREDUMP */

/*
 * C Shell - functions that manage processes, handling hanging, termination
 */

#define BIGINDEX	9	/* largest desirable job index */

#ifdef BSDTIMES
# if defined(sun) || defined(hp9000)
static struct rusage zru = {{0L, 0L}, {0L, 0L}, 0L, 0L, 0L, 0L,
			    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L};

# else /* !sun && !hp9000 */
#  ifdef masscomp
/*
 * Initialization of this structure under RTU 4.1A & RTU 5.0 is problematic
 * because the first two elements are unions of a time_t and a struct timeval.
 * So we'll just have to trust the loader to do the "right thing", DAS DEC-90.
 */
static struct rusage zru;
#  else	/* masscomp */
static struct rusage zru = {{0L, 0L}, {0L, 0L}, 0, 0, 0, 0, 0, 0, 0, 
			    0, 0, 0, 0, 0, 0};
#  endif /* masscomp */
# endif	/* !sun && !hp9000 */
#else /* ! BSDTIMES */
# ifdef _SEQUENT_
static struct process_stats zru = {{0L, 0L}, {0L, 0L}, 0, 0, 0, 0, 0, 0, 0,
				   0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
# else /* !_SEQUENT_ */
static struct tms zru = {0L, 0L, 0L, 0L}, lru = {0L, 0L, 0L, 0L};
# endif	/* !_SEQUENT_ */
#endif /* !BSDTIMES */

#ifndef RUSAGE_CHILDREN
# define	RUSAGE_CHILDREN	-1
#endif

static	void		 pflushall	__P((void));
static	void		 pflush		__P((struct process *));
static	void		 pclrcurr	__P((struct process *));
static	void		 padd		__P((struct command *));
static	int		 pprint		__P((struct process *, int));
static	void		 ptprint	__P((struct process *));
static	void		 pads		__P((Char *));
static	void		 pkill		__P((Char **, int));
static	struct process	*pgetcurr	__P((struct process *));
static	void		 okpcntl	__P((void));

/*
 * pchild - called at interrupt level by the SIGCHLD signal
 *	indicating that at least one child has terminated or stopped
 *	thus at least one wait system call will definitely return a
 *	childs status.  Top level routines (like pwait) must be sure
 *	to mask interrupts when playing with the proclist data structures!
 */
sigret_t
/*ARGSUSED*/
pchild(snum)
int snum;
{
    register struct process *pp;
    register struct process *fp;
    register int pid;
    extern int insource;
#ifdef BSDWAIT
    union wait w;
#else /* !BSDWAIT */
    int     w;
#endif /* !BSDWAIT */
    int     jobflags;
#ifdef BSDTIMES
    struct rusage ru;
#else /* !BSDTIMES */
# ifdef _SEQUENT_
    struct process_stats ru;
    struct process_stats cpst1, cpst2;
    timeval_t tv;
# else /* !_SEQUENT_ */
    struct tms proctimes;

    if (!timesdone) {
	timesdone++;
	(void) times(&shtimes);
    }
# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */

#ifdef JOBDEBUG
    xprintf("pchild()\n");
#endif	/* JOBDEBUG */

/* Christos on where the signal(SIGCHLD, pchild) shoud be:
 *
 * I think that it should go *after* the wait, unlike most signal handlers.
 *
 * In release two (for which I have manuals), it says that wait will remove
 * the first child from the queue of dead children.
 * All the rest of the children that die while in the signal handler of the
 * SIGC(H)LD, will be placed in the queue. If signal is called to re-establish
 * the signal handler, and there are items in the queue, the process will
 * receive another SIGC(H)LD before signal returns. BTW this is from the
 * manual page on comp-sim... Maybe it is not applicable to the hp's, but
 * I read on the news in comp.unix.wizards or comp.unix.questions yesterday
 * that another person was claiming the the signal() call should be after
 * the wait().
 */

loop:
    errno = 0;			/* reset, just in case */
#ifdef JOBDEBUG
    xprintf("Waiting...\n");
    flush();
#endif
#ifdef BSDJOBS
# ifdef BSDTIMES
    /* both a wait3 and rusage */
#  if !defined(BSDWAIT) || defined(NeXT) || (defined(IRIS4D) && __STDC__)
    pid = wait3(&w,
       (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG), &ru);
#  else /* BSDWAIT */
    pid = wait3(&w.w_status,
       (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG), &ru);
#  endif /* BSDWAIT */
# else /* !BSDTIMES */
#  ifdef _SEQUENT_
    (void) get_process_stats(&tv, PS_SELF, 0, &cpst1);
    pid = waitpid(-1, &w,
	    (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG));
    (void) get_process_stats(&tv, PS_SELF, 0, &cpst2);
    pr_stat_sub(&cpst2, &cpst1, &ru);
#  else	/* !_SEQUENT_ */
#   ifndef POSIX
    /* we have a wait3, but no rusage stuff */
    pid = wait3(&w.w_status,
	 (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG), 0);
#   else /* POSIX */
    pid = waitpid(-1, &w,
	    (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG));
#   endif /* POSIX */
#  endif /* !_SEQUENT_ */
# endif	/* !BSDTIMES */
#else /* !BSDJOBS */
# ifdef BSDTIMES
#  define HAVEwait3
    /* both a wait3 and rusage */
#  ifdef hpux
    pid = wait3(&w.w_status, WNOHANG, 0);
#  else	/* !hpux */
    pid = wait3(&w.w_status, WNOHANG, &ru);
#  endif /* !hpux */
# else /* !BSDTIMES */
# ifdef ODT  /* For Sco Unix 3.2.0 or ODT 1.0 */
#  define HAVEwait3
    pid = waitpid(-1, &w,
	    (setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG));
# endif /* ODT */	    
# if defined(aiws) || defined(uts)
#  define HAVEwait3
    pid = wait3(&w.w_status, 
	(setintr && (intty || insource) ? WNOHANG | WUNTRACED : WNOHANG), 0);
# endif /* aiws || uts */
# ifndef HAVEwait3
#  ifdef UNRELSIGS
    /* no wait3, therefore no rusage */
    /* on Sys V, this may hang.  I hope it's not going to be a problem */
#   ifdef _MINIX
    pid = wait(&w);
#   else /* !_MINIX */
    pid = ourwait(&w.w_status);
#   endif /* _MINIX */
#  else	/* UNRELSIGS */
    /* 
     * XXX: for greater than 3 we should use waitpid(). 
     * but then again, SVR4 falls into the POSIX/BSDJOBS category.
     */
    pid = wait(&w.w_status);
#  endif /* SVID >= 3 */
# endif /* HAVEwait3 */
# endif	/* BSDTIMES */
# ifndef BSDSIGS
    (void) sigset(SIGCHLD, pchild);
# endif /* !BSDSIGS */
#endif /* BSDJOBS */

#ifdef JOBDEBUG
    {
	char    buffer[100];
	xsprintf(buffer, "pid %d, retval %x termsig %x retcode %x\n",
		 pid, w, WTERMSIG(w), WEXITSTATUS(w));
	xprintf(buffer);
	flush();
    }
#endif /* JOBDEBUG */

    if (pid <= 0) {
#ifdef JOBDEBUG
	xprintf("errno == %d\n", errno);
#endif
	if (errno == EINTR) {
	    errno = 0;
	    goto loop;
	}
	pnoprocesses = pid == -1;
#ifndef SIGVOID
	return (0);
#else /* !SIGVOID */
	return;
#endif /* SIGVOID */
    }
    for (pp = proclist.p_next; pp != NULL; pp = pp->p_next)
	if (pid == pp->p_procid)
	    goto found;
#ifndef BSDJOBS
    /* this should never have happened */
    stderror(ERR_SYNC, pid);
    xexit(0);
#else /* BSDJOBS */
    goto loop;
#endif /* BSDJOBS */
found:
    if (pid == atoi(short2str(value(STRchild))))
	unsetv(STRchild);
    pp->p_flags &= ~(PRUNNING | PSTOPPED | PREPORTED);
    if (WIFSTOPPED(w)) {
	pp->p_flags |= PSTOPPED;
	pp->p_reason = WSTOPSIG(w);
    }
    else {
	if (pp->p_flags & (PTIME | PPTIME) || adrof(STRtime))
#ifndef BSDTIMES
# ifdef _SEQUENT_
	    (void) get_process_stats(&pp->p_etime, PS_SELF, NULL, NULL);
# else	/* !_SEQUENT_ */
	    pp->p_etime = times(&proctimes);
# endif	/* !_SEQUENT_ */
#else /* BSDTIMES */
	    (void) gettimeofday(&pp->p_etime, NULL);
#endif /* BSDTIMES */


#if defined(BSDTIMES) || defined(_SEQUENT_)
	pp->p_rusage = ru;
#else /* !BSDTIMES && !_SEQUENT_ */
	(void) times(&proctimes);
	pp->p_utime = proctimes.tms_cutime - shtimes.tms_cutime;
	pp->p_stime = proctimes.tms_cstime - shtimes.tms_cstime;
	shtimes = proctimes;
#endif /* !BSDTIMES && !_SEQUENT_ */
	if (WIFSIGNALED(w)) {
	    if (WTERMSIG(w) == SIGINT)
		pp->p_flags |= PINTERRUPTED;
	    else
		pp->p_flags |= PSIGNALED;
	    if (WCOREDUMP(w))
		pp->p_flags |= PDUMPED;
	    pp->p_reason = WTERMSIG(w);
	}
	else {
	    pp->p_reason = WEXITSTATUS(w);
	    if (pp->p_reason != 0)
		pp->p_flags |= PAEXITED;
	    else
		pp->p_flags |= PNEXITED;
	}
    }
    jobflags = 0;
    fp = pp;
    do {
	if ((fp->p_flags & (PPTIME | PRUNNING | PSTOPPED)) == 0 &&
	    !child && adrof(STRtime) &&
#ifdef BSDTIMES
	    fp->p_rusage.ru_utime.tv_sec + fp->p_rusage.ru_stime.tv_sec
#else /* !BSDTIMES */
# ifdef _SEQUENT_
	    fp->p_rusage.ps_utime.tv_sec + fp->p_rusage.ps_stime.tv_sec
# else /* !_SEQUENT_ */
#  ifndef POSIX
	    (fp->p_utime + fp->p_stime) / HZ
#  else /* POSIX */
	    (fp->p_utime + fp->p_stime) / CLK_TCK
#  endif /* POSIX */
# endif /* !_SEQUENT_ */
#endif /* !BSDTIMES */
	    >= atoi(short2str(value(STRtime))))
	    fp->p_flags |= PTIME;
	jobflags |= fp->p_flags;
    } while ((fp = fp->p_friends) != pp);
    pp->p_flags &= ~PFOREGND;
    if (pp == pp->p_friends && (pp->p_flags & PPTIME)) {
	pp->p_flags &= ~PPTIME;
	pp->p_flags |= PTIME;
    }
    if ((jobflags & (PRUNNING | PREPORTED)) == 0) {
	fp = pp;
	do {
	    if (fp->p_flags & PSTOPPED)
		fp->p_flags |= PREPORTED;
	} while ((fp = fp->p_friends) != pp);
	while (fp->p_procid != fp->p_jobid)
	    fp = fp->p_friends;
	if (jobflags & PSTOPPED) {
	    if (pcurrent && pcurrent != fp)
		pprevious = pcurrent;
	    pcurrent = fp;
	}
	else
	    pclrcurr(fp);
	if (jobflags & PFOREGND) {
	    if (jobflags & (PSIGNALED | PSTOPPED | PPTIME) ||
#ifdef IIASA
		jobflags & PAEXITED ||
#endif /* IIASA */
		!eq(dcwd->di_name, fp->p_cwd->di_name)) {
		;		/* print in pjwait */
	    }
	    /* PWP: print a newline after ^C */
	    else if (jobflags & PINTERRUPTED)
#ifdef SHORT_STRINGS
		xputchar('\r' | QUOTE), xputchar('\n');
#else /* !SHORT_STRINGS */
		xprintf("\215\n");	/* \215 is a quoted ^M */
#endif /* !SHORT_STRINGS */
#ifdef notdef
		else if ((jobflags & (PTIME|PSTOPPED)) == PTIME)
				ptprint(fp);
#endif
	}
	else {
	    if (jobflags & PNOTIFY || adrof(STRnotify)) {
#ifdef SHORT_STRINGS
		xputchar('\r' | QUOTE), xputchar('\n');
#else /* !SHORT_STRINGS */
		xprintf("\215\n");	/* \215 is a quoted ^M */
#endif /* !SHORT_STRINGS */
		(void) pprint(pp, NUMBER | NAME | REASON);
		if ((jobflags & PSTOPPED) == 0)
		    pflush(pp);
		{
		    extern Char GettingInput;

		    if (GettingInput) {
			errno = 0;
			(void) Rawmode();
			ClearLines();
			ClearDisp();
			Refresh();
		    }
		}
	    }
	    else {
		fp->p_flags |= PNEEDNOTE;
		neednote++;
	    }
	}
    }
#if defined(BSDJOBS) || defined(HAVEwait3)
    goto loop;
#endif /* BSDJOBS || HAVEwait3 */
}

void
pnote()
{
    register struct process *pp;
    int     flags;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */

    neednote = 0;
    for (pp = proclist.p_next; pp != NULL; pp = pp->p_next) {
	if (pp->p_flags & PNEEDNOTE) {
#ifdef BSDSIGS
	    omask = sigblock(sigmask(SIGCHLD));
#else /* !BSDSIGS */
	    (void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
	    pp->p_flags &= ~PNEEDNOTE;
	    flags = pprint(pp, NUMBER | NAME | REASON);
	    if ((flags & (PRUNNING | PSTOPPED)) == 0)
		pflush(pp);
#ifdef BSDSIGS
	    (void) sigsetmask(omask);
#else /* !BSDSIGS */
	    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
	}
    }
}

/*
 * pwait - wait for current job to terminate, maintaining integrity
 *	of current and previous job indicators.
 */
void
pwait()
{
    register struct process *fp, *pp;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */

    /*
     * Here's where dead procs get flushed.
     */
#ifdef BSDSIGS
    omask = sigblock(sigmask(SIGCHLD));
#else /* !BSDSIGS */
    (void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
    for (pp = (fp = &proclist)->p_next; pp != NULL; pp = (fp = pp)->p_next)
	if (pp->p_procid == 0) {
	    fp->p_next = pp->p_next;
	    xfree((ptr_t) pp->p_command);
	    if (pp->p_cwd && --pp->p_cwd->di_count == 0)
		if (pp->p_cwd->di_next == 0)
		    dfree(pp->p_cwd);
	    xfree((ptr_t) pp);
	    pp = fp;
	}
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else /* !BSDSIGS */
    (void) sigrelse(SIGCHLD);
# ifdef notdef
    if (setintr)
	sigignore(SIGINT);
# endif
#endif /* !BSDSIGS */
    pjwait(pcurrjob);
}


/*
 * pjwait - wait for a job to finish or become stopped
 *	It is assumed to be in the foreground state (PFOREGND)
 */
void
pjwait(pp)
    register struct process *pp;
{
    register struct process *fp;
    int     jobflags, reason;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */
#ifdef UNRELSIGS
    sigret_t (*inthandler)();
#endif /* UNRELSIGS */

    while (pp->p_procid != pp->p_jobid)
	pp = pp->p_friends;
    fp = pp;

    do {
	if ((fp->p_flags & (PFOREGND | PRUNNING)) == PRUNNING)
	    xprintf("BUG: waiting for background job!\n");
    } while ((fp = fp->p_friends) != pp);
    /*
     * Now keep pausing as long as we are not interrupted (SIGINT), and the
     * target process, or any of its friends, are running
     */
    fp = pp;
#ifdef BSDSIGS
    omask = sigblock(sigmask(SIGCHLD));
#endif /* BSDSIGS */
#ifdef UNRELSIGS
    if (setintr)
        inthandler = signal(SIGINT, SIG_IGN);
#endif /* UNRELSIGS */
    for (;;) {
#ifndef BSDSIGS
	(void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
	jobflags = 0;
	do
	    jobflags |= fp->p_flags;
	while ((fp = (fp->p_friends)) != pp);
	if ((jobflags & PRUNNING) == 0)
	    break;
#ifdef JOBDEBUG
	xprintf("starting to sigpause for  SIGCHLD on %d\n", fp->p_procid);
#endif /* JOBDEBUG */
#ifdef BSDSIGS
	/* sigpause(sigblock((sigmask_t) 0) &~ sigmask(SIGCHLD)); */
	(void) sigpause(omask & ~sigmask(SIGCHLD));
#else /* !BSDSIGS */
	(void) sigpause(SIGCHLD);
#endif /* !BSDSIGS */
    }
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else /* !BSDSIGS */
    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
#ifdef UNRELSIGS
    if (setintr)
        (void) signal(SIGINT, inthandler);
#endif /* UNRELSIGS */
#ifdef BSDJOBS
    if (tpgrp > 0)		/* get tty back */
	(void) tcsetpgrp(FSHTTY, tpgrp);
#endif /* BSDJOBS */
    if ((jobflags & (PSIGNALED | PSTOPPED | PTIME)) ||
	!eq(dcwd->di_name, fp->p_cwd->di_name)) {
	if (jobflags & PSTOPPED) {
	    xprintf("\n");
	    if (adrof(STRlistjobs)) {
		Char   *jobcommand[3];

		jobcommand[0] = STRjobs;
		if (eq(value(STRlistjobs), STRlong))
		    jobcommand[1] = STRml;
		else
		    jobcommand[1] = NULL;
		jobcommand[2] = NULL;

		dojobs(jobcommand, NULL);
		(void) pprint(pp, SHELLDIR);
	    }
	    else
		(void) pprint(pp, AREASON | SHELLDIR);
	}
	else
	    (void) pprint(pp, AREASON | SHELLDIR);
    }
    if ((jobflags & (PINTERRUPTED | PSTOPPED)) && setintr &&
	(!gointr || !eq(gointr, STRminus))) {
	if ((jobflags & PSTOPPED) == 0)
	    pflush(pp);
	pintr1(0);
	/* NOTREACHED */
    }
    reason = 0;
    fp = pp;
    do {
	if (fp->p_reason)
	    reason = fp->p_flags & (PSIGNALED | PINTERRUPTED) ?
		fp->p_reason | META : fp->p_reason;
    } while ((fp = fp->p_friends) != pp);
    if ((reason != 0) && (adrof(STRprintexitvalue)))	/* PWP */
	xprintf("Exit %d\n", reason);
    set(STRstatus, putn(reason));
    if (reason && exiterr)
	exitstat();
    pflush(pp);
}

/*
 * dowait - wait for all processes to finish
 */

/*ARGSUSED*/
void
dowait(v, c)
    Char **v;
    struct command *c;
{
    register struct process *pp;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */

    pjobs++;
#ifdef BSDSIGS
    omask = sigblock(sigmask(SIGCHLD));
loop:
#else /* !BSDSIGS */
    if (setintr)
	(void) sigrelse(SIGINT);
loop:
    (void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
    for (pp = proclist.p_next; pp; pp = pp->p_next)
	if (pp->p_procid &&	/* pp->p_procid == pp->p_jobid && */
	    pp->p_flags & PRUNNING) {
#ifdef BSDSIGS
	    (void) sigpause((sigmask_t) 0);
#else /* !BSDSIGS */
	    (void) sigpause(SIGCHLD);
#endif /* !BSDSIGS */
	    goto loop;
	}
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else /* !BSDSIGS */
    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
    pjobs = 0;
}

/*
 * pflushall - flush all jobs from list (e.g. at fork())
 */
static void
pflushall()
{
    register struct process *pp;

    for (pp = proclist.p_next; pp != NULL; pp = pp->p_next)
	if (pp->p_procid)
	    pflush(pp);
}

/*
 * pflush - flag all process structures in the same job as the
 *	the argument process for deletion.  The actual free of the
 *	space is not done here since pflush is called at interrupt level.
 */
static void
pflush(pp)
    register struct process *pp;
{
    register struct process *np;
    register int idx;

    if (pp->p_procid == 0) {
	xprintf("BUG: process flushed twice");
	return;
    }
    while (pp->p_procid != pp->p_jobid)
	pp = pp->p_friends;
    pclrcurr(pp);
    if (pp == pcurrjob)
	pcurrjob = 0;
    idx = pp->p_index;
    np = pp;
    do {
	np->p_index = np->p_procid = 0;
	np->p_flags &= ~PNEEDNOTE;
    } while ((np = np->p_friends) != pp);
    if (idx == pmaxindex) {
	for (np = proclist.p_next, idx = 0; np; np = np->p_next)
	    if (np->p_index > idx)
		idx = np->p_index;
	pmaxindex = idx;
    }
}

/*
 * pclrcurr - make sure the given job is not the current or previous job;
 *	pp MUST be the job leader
 */
static void
pclrcurr(pp)
    register struct process *pp;
{
    if (pp == pcurrent)
	if (pprevious != NULL) {
	    pcurrent = pprevious;
	    pprevious = pgetcurr(pp);
	}
	else {
	    pcurrent = pgetcurr(pp);
	    pprevious = pgetcurr(pp);
	}
    else if (pp == pprevious)
	pprevious = pgetcurr(pp);
}

/* +4 here is 1 for '\0', 1 ea for << >& >> */
static Char command[PMAXLEN + 4];
static int cmdlen;
static Char *cmdp;

/*
 * palloc - allocate a process structure and fill it up.
 *	an important assumption is made that the process is running.
 */
void
palloc(pid, t)
    int     pid;
    register struct command *t;
{
    register struct process *pp;
    int     i;

    pp = (struct process *) xcalloc(1, (size_t) sizeof(struct process));
    pp->p_procid = pid;
    pp->p_flags = t->t_dflg & F_AMPERSAND ? PRUNNING : PRUNNING | PFOREGND;
    if (t->t_dflg & F_TIME)
	pp->p_flags |= PPTIME;
    cmdp = command;
    cmdlen = 0;
    padd(t);
    *cmdp++ = 0;
    if (t->t_dflg & F_PIPEOUT) {
	pp->p_flags |= PPOU;
	if (t->t_dflg & F_STDERR)
	    pp->p_flags |= PDIAG;
    }
    pp->p_command = Strsave(command);
    if (pcurrjob) {
	struct process *fp;

	/* careful here with interrupt level */
	pp->p_cwd = 0;
	pp->p_index = pcurrjob->p_index;
	pp->p_friends = pcurrjob;
	pp->p_jobid = pcurrjob->p_procid;
	for (fp = pcurrjob; fp->p_friends != pcurrjob; fp = fp->p_friends);
	fp->p_friends = pp;
    }
    else {
	pcurrjob = pp;
	pp->p_jobid = pid;
	pp->p_friends = pp;
	pp->p_cwd = dcwd;
	dcwd->di_count++;
	if (pmaxindex < BIGINDEX)
	    pp->p_index = ++pmaxindex;
	else {
	    struct process *np;

	    for (i = 1;; i++) {
		for (np = proclist.p_next; np; np = np->p_next)
		    if (np->p_index == i)
			goto tryagain;
		pp->p_index = i;
		if (i > pmaxindex)
		    pmaxindex = i;
		break;
	tryagain:;
	    }
	}
	if (pcurrent == NULL)
	    pcurrent = pp;
	else if (pprevious == NULL)
	    pprevious = pp;
    }
    pp->p_next = proclist.p_next;
    proclist.p_next = pp;
#ifdef BSDTIMES
    (void) gettimeofday(&pp->p_btime, NULL);
#else /* !BSDTIMES */
# ifdef _SEQUENT_
    (void) get_process_stats(&pp->p_btime, PS_SELF, NULL, NULL);
# else /* !_SEQUENT_ */
    {
	struct tms tmptimes;

	pp->p_btime = times(&tmptimes);
    }
# endif /* !_SEQUENT_ */
#endif /* !BSDTIMES */
}

static void
padd(t)
    register struct command *t;
{
    Char  **argp;

    if (t == 0)
	return;
    switch (t->t_dtyp) {

    case NODE_PAREN:
	pads(STRLparensp);
	padd(t->t_dspr);
	pads(STRspRparen);
	break;

    case NODE_COMMAND:
	for (argp = t->t_dcom; *argp; argp++) {
	    pads(*argp);
	    if (argp[1])
		pads(STRspace);
	}
	break;

    case NODE_OR:
    case NODE_AND:
    case NODE_PIPE:
    case NODE_LIST:
	padd(t->t_dcar);
	switch (t->t_dtyp) {
	case NODE_OR:
	    pads(STRspor2sp);
	    break;
	case NODE_AND:
	    pads(STRspand2sp);
	    break;
	case NODE_PIPE:
	    pads(STRsporsp);
	    break;
	case NODE_LIST:
	    pads(STRsemisp);
	    break;
	default:
	    break;
	}
	padd(t->t_dcdr);
	return;

    default:
	break;
    }
    if ((t->t_dflg & F_PIPEIN) == 0 && t->t_dlef) {
	pads((t->t_dflg & F_READ) ? STRspLarrow2sp : STRspLarrowsp);
	pads(t->t_dlef);
    }
    if ((t->t_dflg & F_PIPEOUT) == 0 && t->t_drit) {
	pads((t->t_dflg & F_APPEND) ? STRspRarrow2 : STRspRarrow);
	if (t->t_dflg & F_STDERR)
	    pads(STRand);
	pads(STRspace);
	pads(t->t_drit);
    }
}

static void
pads(cp)
    Char   *cp;
{
    register int i;

    /*
     * Avoid the Quoted Space alias hack! Reported by:
     * sam@john-bigboote.ICS.UCI.EDU (Sam Horrocks)
     */
    if (cp[0] == STRQNULL[0])
	cp++;

    i = Strlen(cp);

    if (cmdlen >= PMAXLEN)
	return;
    if (cmdlen + i >= PMAXLEN) {
	(void) Strcpy(cmdp, STRsp3dots);
	cmdlen = PMAXLEN;
	cmdp += 4;
	return;
    }
    (void) Strcpy(cmdp, cp);
    cmdp += i;
    cmdlen += i;
}

/*
 * psavejob - temporarily save the current job on a one level stack
 *	so another job can be created.  Used for { } in exp6
 *	and `` in globbing.
 */
void
psavejob()
{
    pholdjob = pcurrjob;
    pcurrjob = NULL;
}

/*
 * prestjob - opposite of psavejob.  This may be missed if we are interrupted
 *	somewhere, but pendjob cleans up anyway.
 */
void
prestjob()
{
    pcurrjob = pholdjob;
    pholdjob = NULL;
}

/*
 * pendjob - indicate that a job (set of commands) has been completed
 *	or is about to begin.
 */
void
pendjob()
{
    register struct process *pp, *tp;

    if (pcurrjob && (pcurrjob->p_flags & (PFOREGND | PSTOPPED)) == 0) {
	pp = pcurrjob;
	while (pp->p_procid != pp->p_jobid)
	    pp = pp->p_friends;
	xprintf("[%d]", pp->p_index);
	tp = pp;
	do {
	    xprintf(" %d", pp->p_procid);
	    pp = pp->p_friends;
	} while (pp != tp);
	xprintf("\n");
    }
    pholdjob = pcurrjob = 0;
}

/*
 * pprint - print a job
 */

/*
 * Hacks have been added for SVR4 to deal with pipe's being spawned in
 * reverse order
 *
 * David Dawes (dawes@physics.su.oz.au) Oct 1991
 */

static int
pprint(pp, flag)
    register struct process *pp;
    bool    flag;
{
    register status, reason;
    struct process *tp;
    extern char *linp, linbuf[];
    int     jobflags, pstatus, pcond;
    char   *format;

#ifdef BACKPIPE
    struct process *pipehead, *pipetail, *pmarker;
    int inpipe = 0;
#endif /* BACKPIPE */

    while (pp->p_procid != pp->p_jobid)
	pp = pp->p_friends;
    if (pp == pp->p_friends && (pp->p_flags & PPTIME)) {
	pp->p_flags &= ~PPTIME;
	pp->p_flags |= PTIME;
    }
    tp = pp;
    status = reason = -1;
    jobflags = 0;
    do {
#ifdef BACKPIPE
	/*
	 * The pipeline is reversed, so locate the real head of the pipeline
	 * if pp is at the tail of a pipe (and not already in a pipeline)
	 */
	if ((pp->p_friends->p_flags & PPOU) && !inpipe && (flag & NAME)) {
	    inpipe = 1;
	    pipetail = pp;
	    do 
		pp = pp->p_friends;
	    while (pp->p_friends->p_flags & PPOU);
	    pipehead = pp;
	    pmarker = pp;
	/*
	 * pmarker is used to hold the place of the proc being processed, so
	 * we can search for the next one downstream later.
	 */
	}
	pcond = (tp != pp || (inpipe && tp == pp));
#else /* !BACKPIPE */
	pcond = (tp != pp);
#endif /* BACKPIPE */	    

	jobflags |= pp->p_flags;
	pstatus = pp->p_flags & PALLSTATES;
	if (pcond && linp != linbuf && !(flag & FANCY) &&
	    ((pstatus == status && pp->p_reason == reason) ||
	     !(flag & REASON)))
	    xprintf(" ");
	else {
	    if (pcond && linp != linbuf)
		xprintf("\n");
	    if (flag & NUMBER) {
#ifdef BACKPIPE
		pcond = ((pp == tp && !inpipe) ||
			 (inpipe && pipetail == tp && pp == pipehead));
#else /* BACKPIPE */
		pcond = (pp == tp);
#endif /* BACKPIPE */
		if (pcond)
		    xprintf("[%d]%s %c ", pp->p_index,
			    pp->p_index < 10 ? " " : "",
			    pp == pcurrent ? '+' :
			    (pp == pprevious ? '-' : ' '));
		else
		    xprintf("       ");
	    }
	    if (flag & FANCY) {
#ifdef TCF
		extern char *sitename();

#endif /* TCF */
		xprintf("%5d ", pp->p_procid);
#ifdef TCF
		xprintf("%11s ", sitename(pp->p_procid));
#endif /* TCF */
	    }
	    if (flag & (REASON | AREASON)) {
		if (flag & NAME)
#ifdef SUSPENDED
		    format = "%-23s";
#else /* !SUSPENDED */
		    format = "%-21s";
#endif /* !SUSPENDED */
		else
		    format = "%s";
		if (pstatus == status)
		    if (pp->p_reason == reason) {
			xprintf(format, "");
			goto prcomd;
		    }
		    else
			reason = pp->p_reason;
		else {
		    status = pstatus;
		    reason = pp->p_reason;
		}
		switch (status) {

		case PRUNNING:
		    xprintf(format, "Running ");
		    break;

		case PINTERRUPTED:
		case PSTOPPED:
		case PSIGNALED:
		    /*
		     * tell what happened to the background job
		     * From: Michael Schroeder 
		     * <mlschroe@immd4.informatik.uni-erlangen.de>
		     */
		    if ((flag & REASON)
			|| ((flag & AREASON)
			    && reason != SIGINT
			    && (reason != SIGPIPE
				|| (pp->p_flags & PPOU) == 0)))
			xprintf(format, mesg[pp->p_reason & ASCII].pname);
		    else
			reason = -1;
		    break;

		case PNEXITED:
		case PAEXITED:
		    if (flag & REASON)
			if (pp->p_reason)
#ifdef SUSPENDED
			    xprintf("Exit %-18d", pp->p_reason);
#else /* SUSPENDED */
			    xprintf("Exit %-16d", pp->p_reason);
#endif /* SUSPENDED */
			else
			    xprintf(format, "Done");
		    break;

		default:
		    xprintf("BUG: status=%-9o", status);
		}
	    }
	}
prcomd:
	if (flag & NAME) {
	    xprintf("%s", short2str(pp->p_command));
	    if (pp->p_flags & PPOU)
		xprintf(" |");
	    if (pp->p_flags & PDIAG)
		xprintf("&");
	}
	if (flag & (REASON | AREASON) && pp->p_flags & PDUMPED)
	    xprintf(" (core dumped)");
	if (tp == pp->p_friends) {
	    if (flag & AMPERSAND)
		xprintf(" &");
	    if (flag & JOBDIR &&
		!eq(tp->p_cwd->di_name, dcwd->di_name)) {
		xprintf(" (wd: ");
		dtildepr(value(STRhome), tp->p_cwd->di_name);
		xprintf(")");
	    }
	}
	if (pp->p_flags & PPTIME && !(status & (PSTOPPED | PRUNNING))) {
	    if (linp != linbuf)
		xprintf("\n\t");
#if defined(BSDTIMES) || defined(_SEQUENT_)
	    prusage(&zru, &pp->p_rusage, &pp->p_etime,
		    &pp->p_btime);
#else /* !BSDTIMES && !SEQUENT */
	    lru.tms_utime = pp->p_utime;
	    lru.tms_stime = pp->p_stime;
	    lru.tms_cutime = 0;
	    lru.tms_cstime = 0;
	    prusage(&zru, &lru, pp->p_etime,
		    pp->p_btime);
#endif /* !BSDTIMES && !SEQUENT */

	}
#ifdef BACKPIPE
	pcond = ((tp == pp->p_friends && !inpipe) ||
		 (inpipe && pipehead->p_friends == tp && pp == pipetail));
#else  /* !BACKPIPE */
	pcond = (tp == pp->p_friends);
#endif /* BACKPIPE */
	if (pcond) {
	    if (linp != linbuf)
		xprintf("\n");
	    if (flag & SHELLDIR && !eq(tp->p_cwd->di_name, dcwd->di_name)) {
		xprintf("(wd now: ");
		dtildepr(value(STRhome), dcwd->di_name);
		xprintf(")\n");
	    }
	}
#ifdef BACKPIPE
	if (inpipe) {
	    /*
	     * if pmaker == pipetail, we are finished that pipeline, and
	     * can now skip to past the head
	     */
	    if (pmarker == pipetail) {
		inpipe = 0;
		pp = pipehead;
	    }
	    else {
	    /*
	     * set pp to one before the one we want next, so the while below
	     * increments to the correct spot.
	     */
		do
		    pp = pp->p_friends;
	    	while (pp->p_friends->p_friends != pmarker);
	    	pmarker = pp->p_friends;
	    }
	}
	pcond = ((pp = pp->p_friends) != tp || inpipe);
#else /* !BACKPIPE */
	pcond = ((pp = pp->p_friends) != tp);
#endif /* BACKPIPE */
    } while (pcond);

    if (jobflags & PTIME && (jobflags & (PSTOPPED | PRUNNING)) == 0) {
	if (jobflags & NUMBER)
	    xprintf("       ");
	ptprint(tp);
    }
    return (jobflags);
}

static void
ptprint(tp)
    register struct process *tp;
{
#ifdef BSDTIMES
    struct timeval tetime, diff;
    static struct timeval ztime;
    struct rusage ru;
    static struct rusage zru;
    register struct process *pp = tp;

    ru = zru;
    tetime = ztime;
    do {
	ruadd(&ru, &pp->p_rusage);
	tvsub(&diff, &pp->p_etime, &pp->p_btime);
	if (timercmp(&diff, &tetime, >))
	    tetime = diff;
    } while ((pp = pp->p_friends) != tp);
    prusage(&zru, &ru, &tetime, &ztime);
#else /* !BSDTIMES */
# ifdef _SEQUENT_
#  define timercmp(tvp, uvp, cmp) \
      ((tvp)->tv_sec cmp (uvp)->tv_sec || \
       (tvp)->tv_sec == (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec)
    timeval_t tetime, diff;
    static timeval_t ztime;
    struct process_stats ru;
    static struct process_stats zru;
    register struct process *pp = tp;

    ru = zru;
    tetime = ztime;
    do {
	ruadd(&ru, &pp->p_rusage);
	tvsub(&diff, &pp->p_etime, &pp->p_btime);
	if (timercmp(&diff, &tetime, >))
	    tetime = diff;
    } while ((pp = pp->p_friends) != tp);
    prusage(&zru, &ru, &tetime, &ztime);
# else /* !_SEQUENT_ */
#  ifndef POSIX
    static time_t ztime = 0;
    static time_t zu_time = 0;
    static time_t zs_time = 0;
    time_t  tetime, diff;
    time_t  u_time, s_time;

#  else	/* POSIX */
    static clock_t ztime = 0;
    static clock_t zu_time = 0;
    static clock_t zs_time = 0;
    clock_t tetime, diff;
    clock_t u_time, s_time;

#  endif /* POSIX */
    struct tms zts, rts;
    register struct process *pp = tp;

    u_time = zu_time;
    s_time = zs_time;
    tetime = ztime;
    do {
	u_time += pp->p_utime;
	s_time += pp->p_stime;
	diff = pp->p_etime - pp->p_btime;
	if (diff > tetime)
	    tetime = diff;
    } while ((pp = pp->p_friends) != tp);
    zts.tms_utime = zu_time;
    zts.tms_stime = zs_time;
    zts.tms_cutime = 0;
    zts.tms_cstime = 0;
    rts.tms_utime = u_time;
    rts.tms_stime = s_time;
    rts.tms_cutime = 0;
    rts.tms_cstime = 0;
    prusage(&zts, &rts, tetime, ztime);
# endif /* !_SEQUENT_ */
#endif	/* !BSDTIMES */
}

/*
 * dojobs - print all jobs
 */
/*ARGSUSED*/
void
dojobs(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;
    register int flag = NUMBER | NAME | REASON;
    int     i;

    if (chkstop)
	chkstop = 2;
    if (*++v) {
	if (v[1] || !eq(*v, STRml))
	    stderror(ERR_JOBS);
	flag |= FANCY | JOBDIR;
    }
    for (i = 1; i <= pmaxindex; i++)
	for (pp = proclist.p_next; pp; pp = pp->p_next)
	    if (pp->p_index == i && pp->p_procid == pp->p_jobid) {
		pp->p_flags &= ~PNEEDNOTE;
		if (!(pprint(pp, flag) & (PRUNNING | PSTOPPED)))
		    pflush(pp);
		break;
	    }
}

/*
 * dofg - builtin - put the job into the foreground
 */
/*ARGSUSED*/
void
dofg(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;

    okpcntl();
    ++v;
    do {
	pp = pfind(*v);
	pstart(pp, 1);
#ifndef BSDSIGS
# ifdef notdef
	if (setintr)
	    sigignore(SIGINT);
# endif
#endif /* !BSDSIGS */
	pjwait(pp);
    } while (*v && *++v);
}

/*
 * %... - builtin - put the job into the foreground
 */
/*ARGSUSED*/
void
dofg1(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;

    okpcntl();
    pp = pfind(v[0]);
    pstart(pp, 1);
#ifndef BSDSIGS
# ifdef notdef
    if (setintr)
	sigignore(SIGINT);
# endif
#endif /* !BSDSIGS */
    pjwait(pp);
}

/*
 * dobg - builtin - put the job into the background
 */
/*ARGSUSED*/
void
dobg(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;

    okpcntl();
    ++v;
    do {
	pp = pfind(*v);
	pstart(pp, 0);
    } while (*v && *++v);
}

/*
 * %... & - builtin - put the job into the background
 */
/*ARGSUSED*/
void
dobg1(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;

    pp = pfind(v[0]);
    pstart(pp, 0);
}

/*
 * dostop - builtin - stop the job
 */
/*ARGSUSED*/
void
dostop(v, c)
    Char  **v;
    struct command *c;
{
#ifdef BSDJOBS
    pkill(++v, SIGSTOP);
#endif /* BSDJOBS */
}

/*
 * dokill - builtin - superset of kill (1)
 */
/*ARGSUSED*/
void
dokill(v, c)
    Char  **v;
    struct command *c;
{
    register int signum, len = 0;
    register char *name;
    extern int T_Cols;

    v++;
    if (v[0] && v[0][0] == '-') {
	if (v[0][1] == 'l') {
	    for (signum = 1; signum <= NSIG; signum++) {
		if ((name = mesg[signum].iname) != NULL) {
		    len += strlen(name) + 1;
		    if (len >= T_Cols - 1) {
			xprintf("\n");
			len = strlen(name) + 1;
		    }
		    xprintf("%s ", name);
		}
	    }
	    xprintf("\n");
	    return;
	}
	if (Isdigit(v[0][1])) {
	    signum = atoi(short2str(v[0] + 1));
	    if (signum < 0 || signum > NSIG)
		stderror(ERR_NAME | ERR_BADSIG);
	}
	else {
	    for (signum = 1; signum <= NSIG; signum++)
		if (mesg[signum].iname &&
		    eq(&v[0][1], str2short(mesg[signum].iname)))
		    goto gotsig;
	    setname(short2str(&v[0][1]));
	    stderror(ERR_NAME | ERR_UNKSIG);
	}
gotsig:
	v++;
    }
    else
	signum = SIGTERM;
    pkill(v, signum);
}

static void
pkill(v, signum)
    Char  **v;
    int     signum;
{
    register struct process *pp, *np;
    int jobflags = 0, err1 = 0;
    pid_t     pid;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */
    Char   *cp;

#ifdef BSDSIGS
    omask = sigmask(SIGCHLD);
    if (setintr)
	omask |= sigmask(SIGINT);
    omask = sigblock(omask) & ~omask;
#else /* !BSDSIGS */
    if (setintr)
	(void) sighold(SIGINT);
    (void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
    gflag = 0, tglob(v);
    if (gflag) {
	v = globall(v);
	if (v == 0)
	    stderror(ERR_NAME | ERR_NOMATCH);
    }
    else {
	v = gargv = saveblk(v);
	trim(v);
    }

    while (v && (cp = *v)) {
	if (*cp == '%') {
	    np = pp = pfind(cp);
	    do
		jobflags |= np->p_flags;
	    while ((np = np->p_friends) != pp);
#ifdef BSDJOBS
	    switch (signum) {

	    case SIGSTOP:
	    case SIGTSTP:
	    case SIGTTIN:
	    case SIGTTOU:
		if ((jobflags & PRUNNING) == 0) {
# ifdef SUSPENDED
		    xprintf("%s: Already suspended\n",
			    short2str(cp));
# else /* !SUSPENDED */
		    xprintf("%s: Already stopped\n",
			    short2str(cp));
# endif /* !SUSPENDED */
		    err1++;
		    goto cont;
		}
		break;
		/*
		 * suspend a process, kill -CONT %, then type jobs; the shell
		 * says it is suspended, but it is running; thanks jaap..
		 */
	    case SIGCONT:
		pstart(pp, 0);
		goto cont;
	    default:
		break;
	    }
#endif /* BSDJOBS */
	    if (killpg(pp->p_jobid, signum) < 0) {
		xprintf("%s: %s\n", short2str(cp), strerror(errno));
		err1++;
	    }
#ifdef BSDJOBS
	    if (signum == SIGTERM || signum == SIGHUP)
		(void) killpg(pp->p_jobid, SIGCONT);
#endif /* BSDJOBS */
	}
	else if (!(Isdigit(*cp) || *cp == '-'))
	    stderror(ERR_NAME | ERR_JOBARGS);
	else {
	    pid = atoi(short2str(cp));
	    if (kill(pid, signum) < 0) {
		xprintf("%d: %s\n", pid, strerror(errno));
		err1++;
		goto cont;
	    }
#ifdef BSDJOBS
	    if (signum == SIGTERM || signum == SIGHUP)
		(void) kill(pid, SIGCONT);
#endif /* BSDJOBS */
	}
cont:
	v++;
    }
    if (gargv)
	blkfree(gargv), gargv = 0;
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else /* !BSDSIGS */
    (void) sigrelse(SIGCHLD);
    if (setintr)
	(void) sigrelse(SIGINT);
#endif /* !BSDSIGS */
    if (err1)
	stderror(ERR_SILENT);
}

/*
 * pstart - start the job in foreground/background
 */
void
pstart(pp, foregnd)
    register struct process *pp;
    int     foregnd;
{
    register struct process *np;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */
    long    jobflags = 0;

#ifdef BSDSIGS
    omask = sigblock(sigmask(SIGCHLD));
#else /* !BSDSIGS */
    (void) sighold(SIGCHLD);
#endif
    np = pp;
    do {
	jobflags |= np->p_flags;
	if (np->p_flags & (PRUNNING | PSTOPPED)) {
	    np->p_flags |= PRUNNING;
	    np->p_flags &= ~PSTOPPED;
	    if (foregnd)
		np->p_flags |= PFOREGND;
	    else
		np->p_flags &= ~PFOREGND;
	}
    } while ((np = np->p_friends) != pp);
    if (!foregnd)
	pclrcurr(pp);
    (void) pprint(pp, foregnd ? NAME | JOBDIR : NUMBER | NAME | AMPERSAND);
#ifdef BSDJOBS
    if (foregnd)
	(void) tcsetpgrp(FSHTTY, pp->p_jobid);
    if (jobflags & PSTOPPED)
	(void) killpg(pp->p_jobid, SIGCONT);
#endif /* BSDJOBS */
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else /* !BSDSIGS */
    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
}

void
panystop(neednl)
    bool    neednl;
{
    register struct process *pp;

    chkstop = 2;
    for (pp = proclist.p_next; pp; pp = pp->p_next)
	if (pp->p_flags & PSTOPPED)
	    stderror(ERR_STOPPED, neednl ? "\n" : "");
}

struct process *
pfind(cp)
    Char   *cp;
{
    register struct process *pp, *np;

    if (cp == 0 || cp[1] == 0 || eq(cp, STRcent2) || eq(cp, STRcentplus)) {
	if (pcurrent == NULL)
	    stderror(ERR_NAME | ERR_JOBCUR);
	return (pcurrent);
    }
    if (eq(cp, STRcentminus) || eq(cp, STRcenthash)) {
	if (pprevious == NULL)
	    stderror(ERR_NAME | ERR_JOBPREV);
	return (pprevious);
    }
    if (Isdigit(cp[1])) {
	int     idx = atoi(short2str(cp + 1));

	for (pp = proclist.p_next; pp; pp = pp->p_next)
	    if (pp->p_index == idx && pp->p_procid == pp->p_jobid)
		return (pp);
	stderror(ERR_NAME | ERR_NOSUCHJOB);
    }
    np = NULL;
    for (pp = proclist.p_next; pp; pp = pp->p_next)
	if (pp->p_procid == pp->p_jobid) {
	    if (cp[1] == '?') {
		register Char *dp;

		for (dp = pp->p_command; *dp; dp++) {
		    if (*dp != cp[2])
			continue;
		    if (prefix(cp + 2, dp))
			goto match;
		}
	    }
	    else if (prefix(cp + 1, pp->p_command)) {
	match:
		if (np)
		    stderror(ERR_NAME | ERR_AMBIG);
		np = pp;
	    }
	}
    if (np)
	return (np);
    stderror(ERR_NAME | cp[1] == '?' ? ERR_JOBPAT : ERR_NOSUCHJOB);
    /* NOTREACHED */
    return (0);
}


/*
 * pgetcurr - find most recent job that is not pp, preferably stopped
 */
static struct process *
pgetcurr(pp)
    register struct process *pp;
{
    register struct process *np;
    register struct process *xp = NULL;

    for (np = proclist.p_next; np; np = np->p_next)
	if (np != pcurrent && np != pp && np->p_procid &&
	    np->p_procid == np->p_jobid) {
	    if (np->p_flags & PSTOPPED)
		return (np);
	    if (xp == NULL)
		xp = np;
	}
    return (xp);
}

/*
 * donotify - flag the job so as to report termination asynchronously
 */
/*ARGSUSED*/
void
donotify(v, c)
    Char  **v;
    struct command *c;
{
    register struct process *pp;

    pp = pfind(*++v);
    pp->p_flags |= PNOTIFY;
}

/*
 * Do the fork and whatever should be done in the child side that
 * should not be done if we are not forking at all (like for simple builtin's)
 * Also do everything that needs any signals fiddled with in the parent side
 *
 * Wanttty tells whether process and/or tty pgrps are to be manipulated:
 *	-1:	leave tty alone; inherit pgrp from parent
 *	 0:	already have tty; manipulate process pgrps only
 *	 1:	want to claim tty; manipulate process and tty pgrps
 * It is usually just the value of tpgrp.
 */

int
pfork(t, wanttty)
    struct command *t;		/* command we are forking for */
    int     wanttty;
{
    register int pid;
    bool    ignint = 0;
    int     pgrp;
#ifdef BSDSIGS
    sigmask_t omask;
#endif /* BSDSIGS */
#ifdef SIGSYNCH
    sigvec_t osv;
    static sigvec_t nsv = {synch_handler, ~0, 0};
#endif /* SIGSYNCH */

    /*
     * A child will be uninterruptible only under very special conditions.
     * Remember that the semantics of '&' is implemented by disconnecting the
     * process from the tty so signals do not need to ignored just for '&'.
     * Thus signals are set to default action for children unless: we have had
     * an "onintr -" (then specifically ignored) we are not playing with
     * signals (inherit action)
     */
    if (setintr)
	ignint = (tpgrp == -1 && (t->t_dflg & F_NOINTERRUPT))
	    || (gointr && eq(gointr, STRminus));
    /*
     * Check for maximum nesting of 16 processes to avoid Forking loops
     */
    if (child == 16)
	stderror(ERR_NESTING, 16);
    /*
     * Hold SIGCHLD until we have the process installed in our table.
     */
#ifdef SIGSYNCH
    if (mysigvec(SIGSYNCH, &nsv, &osv))
	stderror(ERR_SYSTEM, "pfork: sigvec set", strerror(errno));
#endif /* SIGSYNCH */
#ifdef BSDSIGS
    omask = sigblock(sigmask(SIGCHLD));
#else /* !BSDSIGS */
    (void) sighold(SIGCHLD);
#endif /* !BSDSIGS */
    while ((pid = fork()) < 0)
	if (setintr == 0)
	    (void) sleep(FORKSLEEP);
	else {
#ifdef BSDSIGS
	    (void) sigsetmask(omask);
#else /* !BSDSIGS */
	    (void) sigrelse(SIGINT);
	    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
	    stderror(ERR_NOPROC);
	}
    if (pid == 0) {
	settimes();
	pgrp = pcurrjob ? pcurrjob->p_jobid : getpid();
	pflushall();
	pcurrjob = NULL;
#if !defined(BSDTIMES) && !defined(_SEQUENT_) 
	timesdone = 0;
#endif /* !defined(BSDTIMES) && !defined(_SEQUENT_) */
	child++;
	if (setintr) {
	    setintr = 0;	/* until I think otherwise */
#ifndef BSDSIGS
	    (void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
	    /*
	     * Children just get blown away on SIGINT, SIGQUIT unless "onintr
	     * -" seen.
	     */
	    (void) signal(SIGINT, ignint ? SIG_IGN : SIG_DFL);
	    (void) signal(SIGQUIT, ignint ? SIG_IGN : SIG_DFL);
#ifdef BSDJOBS
	    if (wanttty >= 0) {
		/* make stoppable */
		(void) signal(SIGTSTP, SIG_DFL);
		(void) signal(SIGTTIN, SIG_DFL);
		(void) signal(SIGTTOU, SIG_DFL);
	    }
#endif /* BSDJOBS */
	    (void) signal(SIGTERM, parterm);
	}
	else if (tpgrp == -1 && (t->t_dflg & F_NOINTERRUPT)) {
	    (void) signal(SIGINT, SIG_IGN);
	    (void) signal(SIGQUIT, SIG_IGN);
	}
#ifdef OREO
	sigignore(SIGIO);	/* ignore SIGIO in child too */
#endif /* OREO */

	pgetty(wanttty, pgrp);
	/*
	 * Nohup and nice apply only to NODE_COMMAND's but it would be nice
	 * (?!?) if you could say "nohup (foo;bar)" Then the parser would have
	 * to know about nice/nohup/time
	 */
	if (t->t_dflg & F_NOHUP)
	    (void) signal(SIGHUP, SIG_IGN);
	if (t->t_dflg & F_NICE)
#ifdef BSDNICE
	    (void) setpriority(PRIO_PROCESS, 0, t->t_nice);
#else /* !BSDNICE */
	    (void) nice(t->t_nice);
#endif /* !BSDNICE */
#ifdef F_VER
        if (t->t_dflg & F_VER) {
	    Setenv(STRSYSTYPE, t->t_systype ? STRbsd43 : STRsys53);
	    dohash(NULL, NULL);
	}
#endif /* F_VER */
#ifdef SIGSYNCH
	/* rfw 8/89 now parent can continue */
	if (kill(getppid(), SIGSYNCH))
	    stderror(ERR_SYSTEM, "pfork child: kill", strerror(errno));
#endif /* SIGSYNCH */

    }
    else {
#ifdef POSIXJOBS
	if (wanttty >= 0)
	    (void) setpgid(pid, pcurrjob ? pcurrjob->p_jobid : pid);
#endif /* POSIXJOBS */
	palloc(pid, t);
#ifdef SIGSYNCH
	/*
	 * rfw 8/89 Wait for child to own terminal.  Solves half of ugly
	 * synchronization problem.  With this change, we know that the only
	 * reason setpgrp to a previous process in a pipeline can fail is that
	 * the previous process has already exited. Without this hack, he may
	 * either have exited or not yet started to run.  Two uglies become
	 * one.
	 */
	sigpause(omask & ~SYNCHMASK);
	if (mysigvec(SIGSYNCH, &osv, NULL))
	    stderror(ERR_SYSTEM, "pfork parent: sigvec restore",
		     strerror(errno));
#endif /* SIGSYNCH */

#ifdef BSDSIGS
	(void) sigsetmask(omask);
#else /* !BSDSIGS */
	(void) sigrelse(SIGCHLD);
#endif /* !BSDSIGS */
    }
    return (pid);
}

static void
okpcntl()
{
    if (tpgrp == -1)
	stderror(ERR_JOBCONTROL);
    if (tpgrp == 0)
	stderror(ERR_JOBCTRLSUB);
}

/*
 * if we don't have vfork(), things can still go in the wrong order
 * resulting in the famous 'Stopped (tty output)'. But some systems
 * don't permit the setpgid() call, (these are more recent secure
 * systems such as ibm's aix), when they do. Then we'd rather print 
 * an error message than hang the shell!
 * I am open to suggestions how to fix that.
 */
void
pgetty(wanttty, pgrp)
    int     wanttty, pgrp;
{
#ifdef BSDJOBS
# if defined(BSDSIGS) && defined(POSIXJOBS)
    sigmask_t omask = 0;
# endif /* BSDSIGS && POSIXJOBS */

# ifdef JOBDEBUG
    xprintf("wanttty %d\n", wanttty);
# endif

# ifdef POSIXJOBS
    /*
     * christos: I am blocking the tty signals till I've set things
     * correctly....
     */
    if (wanttty > 0)
#  ifdef BSDSIGS
	omask = sigblock(sigmask(SIGTSTP)|sigmask(SIGTTIN)|sigmask(SIGTTOU));
#  else /* !BSDSIGS */
    {
	(void) sighold(SIGTSTP);
	(void) sighold(SIGTTIN);
	(void) sighold(SIGTTOU);
    }
#  endif /* !BSDSIGS */
# endif /* POSIXJOBS */

# ifndef POSIXJOBS
    if (wanttty > 0)
	(void) tcsetpgrp(FSHTTY, pgrp);
# endif /* !POSIXJOBS */

    /*
     * From: Michael Schroeder <mlschroe@immd4.informatik.uni-erlangen.de>
     * Don't check for tpgrp >= 0 so even non-interactive shells give
     * background jobs process groups Same for the comparison in the other part
     * of the #ifdef
     */
    if (wanttty >= 0)
	if (setpgid(0, pgrp) == -1) {
# ifdef BACKPIPE
	    /*
     	     * This usually happens in svr4 when the last command in a pipe
	     * either couldn't be started, or exits without waiting for input.
	     * Putting in the xexit() hangs the shell, so leave it out.
	     * (DHD)
     	     */
#  ifdef JOBDEBUG
	    xprintf("tcsh: setpgid error (%s).\n", strerror(errno));
	    xprintf("pgrp = %d, shell pid = %d\n",pgrp,getpid());
#  endif /* JOBDEBUG */
# else /* !BACKPIPE */
#  if !defined(ISC) && !defined(SCO) && !defined(cray)
	    /* XXX: Wrong but why? */
	    xprintf("tcsh: setpgid error (%s).\n", strerror(errno));
#  endif /* !ISC && !SCO && !cray */
	    xexit(0);
# endif /* BACKPIPE */
	}

# ifdef POSIXJOBS
    if (wanttty > 0) {
	(void) tcsetpgrp(FSHTTY, pgrp);
#  ifdef BSDSIGS
	(void) sigsetmask(omask);
#  else /* BSDSIGS */
	(void) sigrelse(SIGTSTP);
	(void) sigrelse(SIGTTIN);
	(void) sigrelse(SIGTTOU);
#  endif /* !BSDSIGS */
    }
# endif /* POSIXJOBS */

    if (tpgrp > 0)
	tpgrp = 0;		/* gave tty away */
#endif /* BSDJOBS */
}
