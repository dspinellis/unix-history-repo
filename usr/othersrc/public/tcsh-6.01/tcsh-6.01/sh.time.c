/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/sh.time.c,v 3.4 1991/11/26 04:41:23 christos Exp $ */
/*
 * sh.time.c: Shell time keeping and printing.
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

RCSID("$Id: sh.time.c,v 3.4 1991/11/26 04:41:23 christos Exp $")

#if defined(sun) && ! defined(MACH)
# include <machine/param.h>
#endif /* sun */

/*
 * C Shell - routines handling process timing and niceing
 */
#ifdef BSDTIMES
# ifndef RUSAGE_SELF
#  define	RUSAGE_SELF	0
#  define	RUSAGE_CHILDREN	-1
# endif	/* RUSAGE_SELF */
#else /* BSDTIMES */
struct tms times0;
#endif /* BSDTIMES */

#if !defined(BSDTIMES) && !defined(_SEQUENT_)
# ifdef POSIX
static	void	pdtimet	__P((clock_t, clock_t));
# else /* ! POSIX */
static	void	pdtimet	__P((time_t, time_t));
# endif /* ! POSIX */
#else /* BSDTIMES || _SEQUENT_ */
static	void	pdeltat	__P((timeval_t *, timeval_t *));
#endif /* BSDTIMES || _SEQUENT_ */

void
settimes()
{
#ifdef BSDTIMES
    struct rusage ruch;

    (void) gettimeofday(&time0, NULL);
    (void) getrusage(RUSAGE_SELF, &ru0);
    (void) getrusage(RUSAGE_CHILDREN, &ruch);
    ruadd(&ru0, &ruch);
#else
# ifdef _SEQUENT_
    struct process_stats ruch;

    (void) get_process_stats(&time0, PS_SELF, &ru0, &ruch);
    ruadd(&ru0, &ruch);
# else	/* _SEQUENT_ */
    time0 = times(&times0);
    times0.tms_stime += times0.tms_cstime;
    times0.tms_utime += times0.tms_cutime;
    times0.tms_cstime = 0;
    times0.tms_cutime = 0;
# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */
}

/*
 * dotime is only called if it is truly a builtin function and not a
 * prefix to another command
 */
/*ARGSUSED*/
void
dotime(v, c)
    Char **v;
    struct command *c;
{
#ifdef BSDTIMES
    timeval_t timedol;
    struct rusage ru1, ruch;

    (void) getrusage(RUSAGE_SELF, &ru1);
    (void) getrusage(RUSAGE_CHILDREN, &ruch);
    ruadd(&ru1, &ruch);
    (void) gettimeofday(&timedol, NULL);
    prusage(&ru0, &ru1, &timedol, &time0);
#else
# ifdef _SEQUENT_
    timeval_t timedol;
    struct process_stats ru1, ruch;

    (void) get_process_stats(&timedol, PS_SELF, &ru1, &ruch);
    ruadd(&ru1, &ruch);
    prusage(&ru0, &ru1, &timedol, &time0);
# else /* _SEQUENT_ */
#  ifndef POSIX
    time_t  timedol;
#  else /* POSIX */
    clock_t timedol;
#  endif /* POSIX */

    struct tms times_dol;

    timedol = times(&times_dol);
    times_dol.tms_stime += times_dol.tms_cstime;
    times_dol.tms_utime += times_dol.tms_cutime;
    times_dol.tms_cstime = 0;
    times_dol.tms_cutime = 0;
    prusage(&times0, &times_dol, timedol, time0);
# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */
}

/*
 * donice is only called when it on the line by itself or with a +- value
 */
/*ARGSUSED*/
void
donice(v, c)
    register Char **v;
    struct command *c;
{
    register Char *cp;
    int     nval = 0;

    v++, cp = *v++;
    if (cp == 0)
	nval = 4;
    else if (*v == 0 && any("+-", cp[0]))
	nval = getn(cp);
#ifdef BSDNICE
    (void) setpriority(PRIO_PROCESS, 0, nval);
#else /* BSDNICE */
    (void) nice(nval);
#endif /* BSDNICE */
}

#ifdef BSDTIMES
void
ruadd(ru, ru2)
    register struct rusage *ru, *ru2;
{
    tvadd(&ru->ru_utime, &ru2->ru_utime);
    tvadd(&ru->ru_stime, &ru2->ru_stime);
    if (ru2->ru_maxrss > ru->ru_maxrss)
	ru->ru_maxrss = ru2->ru_maxrss;

    ru->ru_ixrss += ru2->ru_ixrss;
    ru->ru_idrss += ru2->ru_idrss;
    ru->ru_isrss += ru2->ru_isrss;
    ru->ru_minflt += ru2->ru_minflt;
    ru->ru_majflt += ru2->ru_majflt;
    ru->ru_nswap += ru2->ru_nswap;
    ru->ru_inblock += ru2->ru_inblock;
    ru->ru_oublock += ru2->ru_oublock;
    ru->ru_msgsnd += ru2->ru_msgsnd;
    ru->ru_msgrcv += ru2->ru_msgrcv;
    ru->ru_nsignals += ru2->ru_nsignals;
    ru->ru_nvcsw += ru2->ru_nvcsw;
    ru->ru_nivcsw += ru2->ru_nivcsw;
}

#else /* BSDTIMES */
# ifdef _SEQUENT_
void
ruadd(ru, ru2)
    register struct process_stats *ru, *ru2;
{
    tvadd(&ru->ps_utime, &ru2->ps_utime);
    tvadd(&ru->ps_stime, &ru2->ps_stime);
    if (ru2->ps_maxrss > ru->ps_maxrss)
	ru->ps_maxrss = ru2->ps_maxrss;

    ru->ps_pagein += ru2->ps_pagein;
    ru->ps_reclaim += ru2->ps_reclaim;
    ru->ps_zerofill += ru2->ps_zerofill;
    ru->ps_pffincr += ru2->ps_pffincr;
    ru->ps_pffdecr += ru2->ps_pffdecr;
    ru->ps_swap += ru2->ps_swap;
    ru->ps_syscall += ru2->ps_syscall;
    ru->ps_volcsw += ru2->ps_volcsw;
    ru->ps_involcsw += ru2->ps_involcsw;
    ru->ps_signal += ru2->ps_signal;
    ru->ps_lread += ru2->ps_lread;
    ru->ps_lwrite += ru2->ps_lwrite;
    ru->ps_bread += ru2->ps_bread;
    ru->ps_bwrite += ru2->ps_bwrite;
    ru->ps_phread += ru2->ps_phread;
    ru->ps_phwrite += ru2->ps_phwrite;
}

# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */

#ifdef BSDTIMES

/*
 * PWP: the LOG1024 and pagetok stuff taken from the top command,
 * written by William LeFebvre
 */
/* Log base 2 of 1024 is 10 (2^10 == 1024) */
#define LOG1024         10

/* Convert clicks (kernel pages) to kbytes ... */
/* If there is no PGSHIFT defined, assume it is 11 */
/* Is this needed for compatability with some old flavor of 4.2 or 4.1? */
#ifndef PGSHIFT
# define pagetok(size)   ((size) << 1)
#else
# if PGSHIFT>10
#  define pagetok(size)   ((size) << (PGSHIFT - LOG1024))
# else
#  define pagetok(size)   ((size) >> (LOG1024 - PGSHIFT))
# endif
#endif

/*
 * if any other machines return wierd values in the ru_i* stuff, put
 * the adjusting macro here:
 */
#ifdef sun
# define IADJUST(i)	(pagetok(i)/2)
#else /* sun */
# define IADJUST(i)	(i)
#endif /* sun */

void
prusage(r0, r1, e, b)
    register struct rusage *r0, *r1;
    timeval_t *e, *b;

#else /* BSDTIMES */
# ifdef _SEQUENT_
void
prusage(r0, r1, e, b)
    register struct process_stats *r0, *r1;
    timeval_t *e, *b;

# else /* _SEQUENT_ */
void
prusage(bs, es, e, b)
    struct tms *bs, *es;

#  ifndef POSIX
    time_t  e, b;

#  else	/* POSIX */
    clock_t e, b;

#  endif /* POSIX */
# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */
{
#ifdef BSDTIMES
    register time_t t =
    (r1->ru_utime.tv_sec - r0->ru_utime.tv_sec) * 100 +
    (r1->ru_utime.tv_usec - r0->ru_utime.tv_usec) / 10000 +
    (r1->ru_stime.tv_sec - r0->ru_stime.tv_sec) * 100 +
    (r1->ru_stime.tv_usec - r0->ru_stime.tv_usec) / 10000;

#else
# ifdef _SEQUENT_
    register time_t t =
    (r1->ps_utime.tv_sec - r0->ps_utime.tv_sec) * 100 +
    (r1->ps_utime.tv_usec - r0->ps_utime.tv_usec) / 10000 +
    (r1->ps_stime.tv_sec - r0->ps_stime.tv_sec) * 100 +
    (r1->ps_stime.tv_usec - r0->ps_stime.tv_usec) / 10000;

# else /* _SEQUENT_ */
#  ifndef POSIX
    register time_t t = (es->tms_utime - bs->tms_utime +
			 es->tms_stime - bs->tms_stime) * 100 / HZ;

#  else /* POSIX */
    register clock_t t = (es->tms_utime - bs->tms_utime +
			  es->tms_stime - bs->tms_stime) * 100 / CLK_TCK;

#  endif /* POSIX */
# endif /* _SEQUENT_ */
#endif /* BSDTIMES */

    register char *cp;
    register long i;
    register struct varent *vp = adrof(STRtime);

#ifdef BSDTIMES
    int     ms =
    (e->tv_sec - b->tv_sec) * 100 + (e->tv_usec - b->tv_usec) / 10000;

    cp = "%Uu %Ss %E %P %X+%Dk %I+%Oio %Fpf+%Ww";
#else
# ifdef _SEQUENT_
    int     ms =
    (e->tv_sec - b->tv_sec) * 100 + (e->tv_usec - b->tv_usec) / 10000;

    cp = "%Uu %Ss %E %P %I+%Oio %Fpf+%Ww";
# else /* _SEQUENT_ */
#  ifndef POSIX
    time_t  ms = (e - b) * 100 / HZ;

#  else /* POSIX */
    clock_t ms = (e - b) * 100 / CLK_TCK;

#  endif /* POSIX */
    cp = "%Uu %Ss %E %P";

    /*
     * the tms stuff is not very precise, so we fudge it.
     * granularity fix: can't be more than 100% 
     * this breaks in multi-processor systems...
     * maybe I should take it out and let people see more then 100% 
     * utilizations.
     */
    if (ms < t && ms != 0)
	ms = t;
# endif /* _SEQUENT_ */
#endif /* BSDTIMES */
#ifdef TDEBUG
    xprintf("es->tms_utime %lu bs->tms_utime %lu\n",
	    es->tms_utime, bs->tms_utime);
    xprintf("es->tms_stime %lu bs->tms_stime %lu\n",
	    es->tms_stime, bs->tms_stime);
    xprintf("ms %lu e %lu b %lu\n", ms, e, b);
    xprintf("t %lu\n", t);
#endif /* TDEBUG */

    if (vp && vp->vec[0] && vp->vec[1])
	cp = short2str(vp->vec[1]);
    for (; *cp; cp++)
	if (*cp != '%')
	    xputchar(*cp);
	else if (cp[1])
	    switch (*++cp) {

	    case 'U':		/* user CPU time used */
#ifdef BSDTIMES
		pdeltat(&r1->ru_utime, &r0->ru_utime);
#else
# ifdef _SEQUENT_
		pdeltat(&r1->ps_utime, &r0->ps_utime);
# else /* _SEQUENT_ */
#  ifndef POSIX
		pdtimet(es->tms_utime, bs->tms_utime);
#  else /* POSIX */
		pdtimet(es->tms_utime, bs->tms_utime);
#  endif /* POSIX */
# endif /* _SEQUENT_ */
#endif /* BSDTIMES */
		break;

	    case 'S':		/* system CPU time used */
#ifdef BSDTIMES
		pdeltat(&r1->ru_stime, &r0->ru_stime);
#else
# ifdef _SEQUENT_
		pdeltat(&r1->ps_stime, &r0->ps_stime);
# else /* _SEQUENT_ */
#  ifndef POSIX
		pdtimet(es->tms_stime, bs->tms_stime);
#  else /* POSIX */
		pdtimet(es->tms_stime, bs->tms_stime);
#  endif /* POSIX */
# endif /* _SEQUENT_ */
#endif /* BSDTIMES */
		break;

	    case 'E':		/* elapsed (wall-clock) time */
#ifdef BSDTIMES
		pcsecs((long) ms);
#else /* BSDTIMES */
		pcsecs(ms);
#endif /* BSDTIMES */
		break;

	    case 'P':		/* percent time spent running */
		/* check if the process did not run */
		i = (ms == 0) ? 0 : (t * 1000 / ms);
		xprintf("%ld.%01ld%%", i / 10, i % 10);	/* nn.n% */
		break;

#ifdef BSDTIMES
	    case 'W':		/* number of swaps */
		i = r1->ru_nswap - r0->ru_nswap;
		xprintf("%ld", i);
		break;

	    case 'X':		/* (average) shared text size */
		xprintf("%ld", t == 0 ? 0L :
			IADJUST(r1->ru_ixrss - r0->ru_ixrss) / t);
		break;

	    case 'D':		/* (average) unshared data size */
		xprintf("%ld", t == 0 ? 0L :
			IADJUST(r1->ru_idrss + r1->ru_isrss -
				(r0->ru_idrss + r0->ru_isrss)) / t);
		break;

	    case 'K':		/* (average) total data memory used  */
		xprintf("%ld", t == 0 ? 0L :
			IADJUST((r1->ru_ixrss + r1->ru_isrss + r1->ru_idrss) -
			   (r0->ru_ixrss + r0->ru_idrss + r0->ru_isrss)) / t);
		break;

	    case 'M':		/* max. Resident Set Size */
#ifdef sun
# ifdef notdef
		xprintf("%ld", r1->ru_maxrss * 1024L/(long) getpagesize());
# endif
		xprintf("%ld", pagetok(r1->ru_maxrss));
#else
		xprintf("%ld", r1->ru_maxrss / 2L);
#endif				/* sun */
		break;

	    case 'F':		/* page faults */
		xprintf("%ld", r1->ru_majflt - r0->ru_majflt);
		break;

	    case 'R':		/* page reclaims */
		xprintf("%ld", r1->ru_minflt - r0->ru_minflt);
		break;

	    case 'I':		/* FS blocks in */
		xprintf("%ld", r1->ru_inblock - r0->ru_inblock);
		break;

	    case 'O':		/* FS blocks out */
		xprintf("%ld", r1->ru_oublock - r0->ru_oublock);
		break;

	    case 'r':		/* PWP: socket messages recieved */
		xprintf("%ld", r1->ru_msgrcv - r0->ru_msgrcv);
		break;

	    case 's':		/* PWP: socket messages sent */
		xprintf("%ld", r1->ru_msgsnd - r0->ru_msgsnd);
		break;

	    case 'k':		/* PWP: signals received */
		xprintf("%ld", r1->ru_nsignals - r0->ru_nsignals);
		break;

	    case 'w':		/* PWP: voluntary context switches (waits) */
		xprintf("%ld", r1->ru_nvcsw - r0->ru_nvcsw);
		break;

	    case 'c':		/* PWP: involuntary context switches */
		xprintf("%ld", r1->ru_nivcsw - r0->ru_nivcsw);
		break;
#else /* BSDTIMES */
# ifdef _SEQUENT_
	    case 'W':		/* number of swaps */
		i = r1->ps_swap - r0->ps_swap;
		xprintf("%ld", i);
		break;

	    case 'M':
		xprintf("%ld", r1->ps_maxrss / 2);
		break;

	    case 'F':
		xprintf("%ld", r1->ps_pagein - r0->ps_pagein);
		break;

	    case 'R':
		xprintf("%ld", r1->ps_reclaim - r0->ps_reclaim);
		break;

	    case 'I':
		xprintf("%ld", r1->ps_bread - r0->ps_bread);
		break;

	    case 'O':
		xprintf("%ld", r1->ps_bwrite - r0->ps_bwrite);
		break;

	    case 'k':
		xprintf("%ld", r1->ps_signal - r0->ps_signal);
		break;

	    case 'w':
		xprintf("%ld", r1->ps_volcsw - r0->ps_volcsw);
		break;

	    case 'c':
		xprintf("%ld", r1->ps_involcsw - r0->ps_involcsw);
		break;

	    case 'Z':
		xprintf("%ld", r1->ps_zerofill - r0->ps_zerofill);
		break;

	    case 'i':
		xprintf("%ld", r1->ps_pffincr - r0->ps_pffincr);
		break;

	    case 'd':
		xprintf("%ld", r1->ps_pffdecr - r0->ps_pffdecr);
		break;

	    case 'Y':
		xprintf("%ld", r1->ps_syscall - r0->ps_syscall);
		break;

	    case 'l':
		xprintf("%ld", r1->ps_lread - r0->ps_lread);
		break;

	    case 'm':
		xprintf("%ld", r1->ps_lwrite - r0->ps_lwrite);
		break;

	    case 'p':
		xprintf("%ld", r1->ps_phread - r0->ps_phread);
		break;

	    case 'q':
		xprintf("%ld", r1->ps_phwrite - r0->ps_phwrite);
		break;
# endif	/* _SEQUENT_ */
#endif /* BSDTIMES */
	    default:
		break;
	    }
    xputchar('\n');
}

#if defined(BSDTIMES) || defined(_SEQUENT_)
static void
pdeltat(t1, t0)
    timeval_t *t1, *t0;
{
    timeval_t td;

    tvsub(&td, t1, t0);
    xprintf("%ld.%03ld", td.tv_sec, td.tv_usec / 1000L);
}

void
tvadd(tsum, t0)
    timeval_t *tsum, *t0;
{

    tsum->tv_sec += t0->tv_sec;
    tsum->tv_usec += t0->tv_usec;
    if (tsum->tv_usec > 1000000)
	tsum->tv_sec++, tsum->tv_usec -= 1000000;
}

void
tvsub(tdiff, t1, t0)
    timeval_t *tdiff, *t1, *t0;
{

    tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
    tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
    if (tdiff->tv_usec < 0)
	tdiff->tv_sec--, tdiff->tv_usec += 1000000;
}

#else /* !BSDTIMES && !_SEQUENT_ */
static void
pdtimet(eval, bval)
#ifndef POSIX
    time_t  eval, bval;

#else /* POSIX */
    clock_t eval, bval;

#endif /* POSIX */
{
#ifndef POSIX
    time_t  val;

#else /* POSIX */
    clock_t val;

#endif /* POSIX */

#ifndef POSIX
    val = (eval - bval) * 100 / HZ;
#else /* POSIX */
    val = (eval - bval) * 100 / CLK_TCK;
#endif /* POSIX */

    xprintf("%ld.%02ld", val / 100, val - (val / 100 * 100));
}
#endif /* BSDTIMES || _SEQUENT_ */
