/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_time.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "dir.h"		/* XXX */
#include "user.h"
#include "kernel.h"
#include "proc.h"

#include "../machine/reg.h"
#include "../machine/cpu.h"

/* 
 * Time of day and interval timer support.
 *
 * These routines provide the kernel entry points to get and set
 * the time-of-day and per-process interval timers.  Subroutines
 * here provide support for adding and subtracting timeval structures
 * and decrementing interval timers, optionally reloading the interval
 * timers when they expire.
 */

gettimeofday()
{
	register struct a {
		struct	timeval *tp;
		struct	timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;

	if (uap->tp) {
		microtime(&atv);
		u.u_error = copyout((caddr_t)&atv, (caddr_t)uap->tp,
			sizeof (atv));
		if (u.u_error)
			return;
	}
	if (uap->tzp)
		u.u_error = copyout((caddr_t)&tz, (caddr_t)uap->tzp,
			sizeof (tz));
}

settimeofday()
{
	register struct a {
		struct	timeval *tv;
		struct	timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;
	struct timezone atz;

	if (uap->tv) {
		u.u_error = copyin((caddr_t)uap->tv, (caddr_t)&atv,
			sizeof (struct timeval));
		if (u.u_error)
			return;
		setthetime(&atv);
	}
	if (uap->tzp && suser()) {
		u.u_error = copyin((caddr_t)uap->tzp, (caddr_t)&atz,
			sizeof (atz));
		if (u.u_error == 0)
			tz = atz;
	}
}

setthetime(tv)
	struct timeval *tv;
{
	int s;

	if (!suser())
		return;
/* WHAT DO WE DO ABOUT PENDING REAL-TIME TIMEOUTS??? */
	boottime.tv_sec += tv->tv_sec - time.tv_sec;
	s = splhigh(); time = *tv; splx(s);
	resettodr();
}

extern	int tickadj;			/* "standard" clock skew, us./tick */
int	tickdelta;			/* current clock skew, us. per tick */
long	timedelta;			/* unapplied time correction, us. */
long	bigadj = 1000000;		/* use 10x skew above bigadj us. */

adjtime()
{
	register struct a {
		struct timeval *delta;
		struct timeval *olddelta;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv, oatv;
	register long ndelta;
	int s;

	if (!suser()) 
		return;
	u.u_error = copyin((caddr_t)uap->delta, (caddr_t)&atv,
		sizeof (struct timeval));
	if (u.u_error)
		return;
	ndelta = atv.tv_sec * 1000000 + atv.tv_usec;
	if (timedelta == 0)
		if (ndelta > bigadj)
			tickdelta = 10 * tickadj;
		else
			tickdelta = tickadj;
	if (ndelta % tickdelta)
		ndelta = ndelta / tickadj * tickadj;

	s = splclock();
	if (uap->olddelta) {
		oatv.tv_sec = timedelta / 1000000;
		oatv.tv_usec = timedelta % 1000000;
	}
	timedelta = ndelta;
	splx(s);

	if (uap->olddelta)
		(void) copyout((caddr_t)&oatv, (caddr_t)uap->olddelta,
			sizeof (struct timeval));
}

/*
 * Get value of an interval timer.  The process virtual and
 * profiling virtual time timers are kept in the u. area, since
 * they can be swapped out.  These are kept internally in the
 * way they are specified externally: in time until they expire.
 *
 * The real time interval timer is kept in the process table slot
 * for the process, and its value (it_value) is kept as an
 * absolute time rather than as a delta, so that it is easy to keep
 * periodic real-time signals from drifting.
 *
 * Virtual time timers are processed in the hardclock() routine of
 * kern_clock.c.  The real time timer is processed by a timeout
 * routine, called from the softclock() routine.  Since a callout
 * may be delayed in real time due to interrupt processing in the system,
 * it is possible for the real time timeout routine (realitexpire, given below),
 * to be delayed in real time past when it is supposed to occur.  It
 * does not suffice, therefore, to reload the real timer .it_value from the
 * real time timers .it_interval.  Rather, we compute the next time in
 * absolute time the timer should go off.
 */
getitimer()
{
	register struct a {
		u_int	which;
		struct	itimerval *itv;
	} *uap = (struct a *)u.u_ap;
	struct itimerval aitv;
	int s;

	if (uap->which > ITIMER_PROF) {
		u.u_error = EINVAL;
		return;
	}
	s = splclock();
	if (uap->which == ITIMER_REAL) {
		/*
		 * Convert from absoulte to relative time in .it_value
		 * part of real time timer.  If time for real time timer
		 * has passed return 0, else return difference between
		 * current time and time for the timer to go off.
		 */
		aitv = u.u_procp->p_realtimer;
		if (timerisset(&aitv.it_value))
			if (timercmp(&aitv.it_value, &time, <))
				timerclear(&aitv.it_value);
			else
				timevalsub(&aitv.it_value, &time);
	} else
		aitv = u.u_timer[uap->which];
	splx(s);
	u.u_error = copyout((caddr_t)&aitv, (caddr_t)uap->itv,
	    sizeof (struct itimerval));
}

setitimer()
{
	register struct a {
		u_int	which;
		struct	itimerval *itv, *oitv;
	} *uap = (struct a *)u.u_ap;
	struct itimerval aitv;
	register struct itimerval *itvp;
	int s;
	register struct proc *p = u.u_procp;

	if (uap->which > ITIMER_PROF) {
		u.u_error = EINVAL;
		return;
	}
	itvp = uap->itv;
	if (itvp && (u.u_error = copyin((caddr_t)itvp, (caddr_t)&aitv,
	    sizeof(struct itimerval))))
		return;
	if (uap->itv = uap->oitv) {
		getitimer();
		if (u.u_error)
			return;
	}
	if (itvp == 0)
		return;
	if (itimerfix(&aitv.it_value) || itimerfix(&aitv.it_interval)) {
		u.u_error = EINVAL;
		return;
	}
	s = splclock();
	if (uap->which == ITIMER_REAL) {
		untimeout(realitexpire, (caddr_t)p);
		if (timerisset(&aitv.it_value)) {
			timevaladd(&aitv.it_value, &time);
			timeout(realitexpire, (caddr_t)p, hzto(&aitv.it_value));
		}
		p->p_realtimer = aitv;
	} else
		u.u_timer[uap->which] = aitv;
	splx(s);
}

/*
 * Real interval timer expired:
 * send process whose timer expired an alarm signal.
 * If time is not set up to reload, then just return.
 * Else compute next time timer should go off which is > current time.
 * This is where delay in processing this timeout causes multiple
 * SIGALRM calls to be compressed into one.
 */
realitexpire(p)
	register struct proc *p;
{
	int s;

	psignal(p, SIGALRM);
	if (!timerisset(&p->p_realtimer.it_interval)) {
		timerclear(&p->p_realtimer.it_value);
		return;
	}
	for (;;) {
		s = splclock();
		timevaladd(&p->p_realtimer.it_value,
		    &p->p_realtimer.it_interval);
		if (timercmp(&p->p_realtimer.it_value, &time, >)) {
			timeout(realitexpire, (caddr_t)p,
			    hzto(&p->p_realtimer.it_value));
			splx(s);
			return;
		}
		splx(s);
	}
}

/*
 * Check that a proposed value to load into the .it_value or
 * .it_interval part of an interval timer is acceptable, and
 * fix it to have at least minimal value (i.e. if it is less
 * than the resolution of the clock, round it up.)
 */
itimerfix(tv)
	struct timeval *tv;
{

	if (tv->tv_sec < 0 || tv->tv_sec > 100000000 ||
	    tv->tv_usec < 0 || tv->tv_usec >= 1000000)
		return (EINVAL);
	if (tv->tv_sec == 0 && tv->tv_usec != 0 && tv->tv_usec < tick)
		tv->tv_usec = tick;
	return (0);
}

/*
 * Decrement an interval timer by a specified number
 * of microseconds, which must be less than a second,
 * i.e. < 1000000.  If the timer expires, then reload
 * it.  In this case, carry over (usec - old value) to
 * reducint the value reloaded into the timer so that
 * the timer does not drift.  This routine assumes
 * that it is called in a context where the timers
 * on which it is operating cannot change in value.
 */
itimerdecr(itp, usec)
	register struct itimerval *itp;
	int usec;
{

	if (itp->it_value.tv_usec < usec) {
		if (itp->it_value.tv_sec == 0) {
			/* expired, and already in next interval */
			usec -= itp->it_value.tv_usec;
			goto expire;
		}
		itp->it_value.tv_usec += 1000000;
		itp->it_value.tv_sec--;
	}
	itp->it_value.tv_usec -= usec;
	usec = 0;
	if (timerisset(&itp->it_value))
		return (1);
	/* expired, exactly at end of interval */
expire:
	if (timerisset(&itp->it_interval)) {
		itp->it_value = itp->it_interval;
		itp->it_value.tv_usec -= usec;
		if (itp->it_value.tv_usec < 0) {
			itp->it_value.tv_usec += 1000000;
			itp->it_value.tv_sec--;
		}
	} else
		itp->it_value.tv_usec = 0;		/* sec is already 0 */
	return (0);
}

/*
 * Add and subtract routines for timevals.
 * N.B.: subtract routine doesn't deal with
 * results which are before the beginning,
 * it just gets very confused in this case.
 * Caveat emptor.
 */
timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_usec;
	timevalfix(t1);
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
	timevalfix(t1);
}

timevalfix(t1)
	struct timeval *t1;
{

	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}
