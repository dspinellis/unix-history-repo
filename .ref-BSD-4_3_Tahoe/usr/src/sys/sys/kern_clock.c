/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_clock.c	7.2 (Berkeley) 11/3/86
 */

#include "../machine/reg.h"
#include "../machine/psl.h"

#include "param.h"
#include "systm.h"
#include "dkstat.h"
#include "callout.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "vm.h"
#include "text.h"

#if defined(vax) || defined(tahoe)
#include "../machine/mtpr.h"
#include "../machine/clock.h"
#endif

#ifdef GPROF
#include "gprof.h"
#endif

/*
 * Clock handling routines.
 *
 * This code is written to operate with two timers which run
 * independently of each other. The main clock, running at hz
 * times per second, is used to do scheduling and timeout calculations.
 * The second timer does resource utilization estimation statistically
 * based on the state of the machine phz times a second. Both functions
 * can be performed by a single clock (ie hz == phz), however the 
 * statistics will be much more prone to errors. Ideally a machine
 * would have separate clocks measuring time spent in user state, system
 * state, interrupt state, and idle state. These clocks would allow a non-
 * approximate measure of resource utilization.
 */

/*
 * TODO:
 *	time of day, system/user timing, timeouts, profiling on separate timers
 *	allocate more timeout table slots when table overflows.
 */

/*
 * Bump a timeval by a small number of usec's.
 */
#define BUMPTIME(t, usec) { \
	register struct timeval *tp = (t); \
 \
	tp->tv_usec += (usec); \
	if (tp->tv_usec >= 1000000) { \
		tp->tv_usec -= 1000000; \
		tp->tv_sec++; \
	} \
}

/*
 * The hz hardware interval timer.
 * We update the events relating to real time.
 * If this timer is also being used to gather statistics,
 * we run through the statistics gathering routine as well.
 */
/*ARGSUSED*/
hardclock(pc, ps)
	caddr_t pc;
	int ps;
{
	register struct callout *p1;
	register struct proc *p;
	register int s;
	int needsoft = 0;
	extern int tickdelta;
	extern long timedelta;

	/*
	 * Update real-time timeout queue.
	 * At front of queue are some number of events which are ``due''.
	 * The time to these is <= 0 and if negative represents the
	 * number of ticks which have passed since it was supposed to happen.
	 * The rest of the q elements (times > 0) are events yet to happen,
	 * where the time for each is given as a delta from the previous.
	 * Decrementing just the first of these serves to decrement the time
	 * to all events.
	 */
	p1 = calltodo.c_next;
	while (p1) {
		if (--p1->c_time > 0)
			break;
		needsoft = 1;
		if (p1->c_time == 0)
			break;
		p1 = p1->c_next;
	}

	/*
	 * Charge the time out based on the mode the cpu is in.
	 * Here again we fudge for the lack of proper interval timers
	 * assuming that the current state has been around at least
	 * one tick.
	 */
	if (USERMODE(ps)) {
		if (u.u_prof.pr_scale)
			needsoft = 1;
		/*
		 * CPU was in user state.  Increment
		 * user time counter, and process process-virtual time
		 * interval timer. 
		 */
		BUMPTIME(&u.u_ru.ru_utime, tick);
		if (timerisset(&u.u_timer[ITIMER_VIRTUAL].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_VIRTUAL], tick) == 0)
			psignal(u.u_procp, SIGVTALRM);
	} else {
		/*
		 * CPU was in system state.
		 */
		if (!noproc)
			BUMPTIME(&u.u_ru.ru_stime, tick);
	}

	/*
	 * If the cpu is currently scheduled to a process, then
	 * charge it with resource utilization for a tick, updating
	 * statistics which run in (user+system) virtual time,
	 * such as the cpu time limit and profiling timers.
	 * This assumes that the current process has been running
	 * the entire last tick.
	 */
	if (noproc == 0) {
		if ((u.u_ru.ru_utime.tv_sec+u.u_ru.ru_stime.tv_sec+1) >
		    u.u_rlimit[RLIMIT_CPU].rlim_cur) {
			psignal(u.u_procp, SIGXCPU);
			if (u.u_rlimit[RLIMIT_CPU].rlim_cur <
			    u.u_rlimit[RLIMIT_CPU].rlim_max)
				u.u_rlimit[RLIMIT_CPU].rlim_cur += 5;
		}
		if (timerisset(&u.u_timer[ITIMER_PROF].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_PROF], tick) == 0)
			psignal(u.u_procp, SIGPROF);
		s = u.u_procp->p_rssize;
		u.u_ru.ru_idrss += s;
#ifdef notdef
		u.u_ru.ru_isrss += 0;		/* XXX (haven't got this) */
#endif
		if (u.u_procp->p_textp) {
			register int xrss = u.u_procp->p_textp->x_rssize;

			s += xrss;
			u.u_ru.ru_ixrss += xrss;
		}
		if (s > u.u_ru.ru_maxrss)
			u.u_ru.ru_maxrss = s;
	}

	/*
	 * We adjust the priority of the current process.
	 * The priority of a process gets worse as it accumulates
	 * CPU time.  The cpu usage estimator (p_cpu) is increased here
	 * and the formula for computing priorities (in kern_synch.c)
	 * will compute a different value each time the p_cpu increases
	 * by 4.  The cpu usage estimator ramps up quite quickly when
	 * the process is running (linearly), and decays away exponentially,
	 * at a rate which is proportionally slower when the system is
	 * busy.  The basic principal is that the system will 90% forget
	 * that a process used a lot of CPU time in 5*loadav seconds.
	 * This causes the system to favor processes which haven't run
	 * much recently, and to round-robin among other processes.
	 */
	if (!noproc) {
		p = u.u_procp;
		p->p_cpticks++;
		if (++p->p_cpu == 0)
			p->p_cpu--;
		if ((p->p_cpu&3) == 0) {
			(void) setpri(p);
			if (p->p_pri >= PUSER)
				p->p_pri = p->p_usrpri;
		}
	}

	/*
	 * If the alternate clock has not made itself known then
	 * we must gather the statistics.
	 */
	if (phz == 0)
		gatherstats(pc, ps);

	/*
	 * Increment the time-of-day, and schedule
	 * processing of the callouts at a very low cpu priority,
	 * so we don't keep the relatively high clock interrupt
	 * priority any longer than necessary.
	 */
	if (timedelta == 0)
		BUMPTIME(&time, tick)
	else {
		register delta;

		if (timedelta < 0) {
			delta = tick - tickdelta;
			timedelta += tickdelta;
		} else {
			delta = tick + tickdelta;
			timedelta -= tickdelta;
		}
		BUMPTIME(&time, delta);
	}
	if (needsoft) {
		if (BASEPRI(ps)) {
			/*
			 * Save the overhead of a software interrupt;
			 * it will happen as soon as we return, so do it now.
			 */
			(void) splsoftclock();
			softclock(pc, ps);
		} else
			setsoftclock();
	}
}

int	dk_ndrive = DK_NDRIVE;
/*
 * Gather statistics on resource utilization.
 *
 * We make a gross assumption: that the system has been in the
 * state it is in (user state, kernel state, interrupt state,
 * or idle state) for the entire last time interval, and
 * update statistics accordingly.
 */
/*ARGSUSED*/
gatherstats(pc, ps)
	caddr_t pc;
	int ps;
{
	register int cpstate, s;

	/*
	 * Determine what state the cpu is in.
	 */
	if (USERMODE(ps)) {
		/*
		 * CPU was in user state.
		 */
		if (u.u_procp->p_nice > NZERO)
			cpstate = CP_NICE;
		else
			cpstate = CP_USER;
	} else {
		/*
		 * CPU was in system state.  If profiling kernel
		 * increment a counter.  If no process is running
		 * then this is a system tick if we were running
		 * at a non-zero IPL (in a driver).  If a process is running,
		 * then we charge it with system time even if we were
		 * at a non-zero IPL, since the system often runs
		 * this way during processing of system calls.
		 * This is approximate, but the lack of true interval
		 * timers makes doing anything else difficult.
		 */
		cpstate = CP_SYS;
		if (noproc && BASEPRI(ps))
			cpstate = CP_IDLE;
#ifdef GPROF
		s = pc - s_lowpc;
		if (profiling < 2 && s < s_textsize)
			kcount[s / (HISTFRACTION * sizeof (*kcount))]++;
#endif
	}
	/*
	 * We maintain statistics shown by user-level statistics
	 * programs:  the amount of time in each cpu state, and
	 * the amount of time each of DK_NDRIVE ``drives'' is busy.
	 */
	cp_time[cpstate]++;
	for (s = 0; s < DK_NDRIVE; s++)
		if (dk_busy&(1<<s))
			dk_time[s]++;
}

/*
 * Software priority level clock interrupt.
 * Run periodic events from timeout queue.
 */
/*ARGSUSED*/
softclock(pc, ps)
	caddr_t pc;
	int ps;
{

	for (;;) {
		register struct callout *p1;
		register caddr_t arg;
		register int (*func)();
		register int a, s;

		s = splhigh();
		if ((p1 = calltodo.c_next) == 0 || p1->c_time > 0) {
			splx(s);
			break;
		}
		arg = p1->c_arg; func = p1->c_func; a = p1->c_time;
		calltodo.c_next = p1->c_next;
		p1->c_next = callfree;
		callfree = p1;
		splx(s);
		(*func)(arg, a);
	}
	/*
	 * If trapped user-mode and profiling, give it
	 * a profiling tick.
	 */
	if (USERMODE(ps)) {
		register struct proc *p = u.u_procp;

		if (u.u_prof.pr_scale) {
			p->p_flag |= SOWEUPC;
			aston();
		}
		/*
		 * Check to see if process has accumulated
		 * more than 10 minutes of user time.  If so
		 * reduce priority to give others a chance.
		 */
		if (p->p_uid && p->p_nice == NZERO &&
		    u.u_ru.ru_utime.tv_sec > 10 * 60) {
			p->p_nice = NZERO+4;
			(void) setpri(p);
			p->p_pri = p->p_usrpri;
		}
	}
}

/*
 * Arrange that (*fun)(arg) is called in t/hz seconds.
 */
timeout(fun, arg, t)
	int (*fun)();
	caddr_t arg;
	register int t;
{
	register struct callout *p1, *p2, *pnew;
	register int s = splhigh();

	if (t <= 0)
		t = 1;
	pnew = callfree;
	if (pnew == NULL)
		panic("timeout table overflow");
	callfree = pnew->c_next;
	pnew->c_arg = arg;
	pnew->c_func = fun;
	for (p1 = &calltodo; (p2 = p1->c_next) && p2->c_time < t; p1 = p2)
		if (p2->c_time > 0)
			t -= p2->c_time;
	p1->c_next = pnew;
	pnew->c_next = p2;
	pnew->c_time = t;
	if (p2)
		p2->c_time -= t;
	splx(s);
}

/*
 * untimeout is called to remove a function timeout call
 * from the callout structure.
 */
untimeout(fun, arg)
	int (*fun)();
	caddr_t arg;
{
	register struct callout *p1, *p2;
	register int s;

	s = splhigh();
	for (p1 = &calltodo; (p2 = p1->c_next) != 0; p1 = p2) {
		if (p2->c_func == fun && p2->c_arg == arg) {
			if (p2->c_next && p2->c_time > 0)
				p2->c_next->c_time += p2->c_time;
			p1->c_next = p2->c_next;
			p2->c_next = callfree;
			callfree = p2;
			break;
		}
	}
	splx(s);
}

/*
 * Compute number of hz until specified time.
 * Used to compute third argument to timeout() from an
 * absolute time.
 */
hzto(tv)
	struct timeval *tv;
{
	register long ticks;
	register long sec;
	int s = splhigh();

	/*
	 * If number of milliseconds will fit in 32 bit arithmetic,
	 * then compute number of milliseconds to time and scale to
	 * ticks.  Otherwise just compute number of hz in time, rounding
	 * times greater than representible to maximum value.
	 *
	 * Delta times less than 25 days can be computed ``exactly''.
	 * Maximum value for any timeout in 10ms ticks is 250 days.
	 */
	sec = tv->tv_sec - time.tv_sec;
	if (sec <= 0x7fffffff / 1000 - 1000)
		ticks = ((tv->tv_sec - time.tv_sec) * 1000 +
			(tv->tv_usec - time.tv_usec) / 1000) / (tick / 1000);
	else if (sec <= 0x7fffffff / hz)
		ticks = sec * hz;
	else
		ticks = 0x7fffffff;
	splx(s);
	return (ticks);
}

profil()
{
	register struct a {
		short	*bufbase;
		unsigned bufsize;
		unsigned pcoffset;
		unsigned pcscale;
	} *uap = (struct a *)u.u_ap;
	register struct uprof *upp = &u.u_prof;

	upp->pr_base = uap->bufbase;
	upp->pr_size = uap->bufsize;
	upp->pr_off = uap->pcoffset;
	upp->pr_scale = uap->pcscale;
}

#ifdef COMPAT
opause()
{

	for (;;)
		sleep((caddr_t)&u, PSLEP);
}
#endif
