/*	kern_clock.c	4.50	83/01/15	*/

#include "../machine/reg.h"
#include "../machine/psl.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/callout.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/proc.h"
#include "../h/vm.h"
#include "../h/text.h"
#ifdef MUSH
#include "../h/quota.h"
#include "../h/share.h"
#endif

#ifdef vax
#include "../vax/mtpr.h"
#endif

#ifdef GPROF
#include "../h/gprof.h"
#endif

#
/*
 * Clock handling routines.
 *
 * This code is written for a machine with only one interval timer,
 * and does timing and resource utilization estimation statistically
 * based on the state of the machine hz times a second.  A machine
 * with proper clocks (running separately in user state, system state,
 * interrupt state and idle state) as well as a time-of-day clock
 * would allow a non-approximate implementation.
 */

/*
 * TODO:
 *	* Keep more accurate statistics by simulating good interval timers.
 *	* Use the time-of-day clock on the VAX to keep more accurate time
 *	  than is possible by repeated use of the interval timer.
 *	* Allocate more timeout table slots when table overflows.
 */

/* bump a timeval by a small number of usec's */
#define	bumptime(tp, usec) \
	(tp)->tv_usec += usec; \
	if ((tp)->tv_usec >= 1000000) { \
		(tp)->tv_usec -= 1000000; \
		(tp)->tv_sec++; \
	}

/*
 * The (single) hardware interval timer.
 * We update the events relating to real time, and then
 * make a gross assumption: that the system has been in the
 * state it is in (user state, kernel state, interrupt state,
 * or idle state) for the entire last time interval, and
 * update statistics accordingly.
 */
/*ARGSUSED*/
#ifdef vax
hardclock(pc, ps)
	caddr_t pc;
	int ps;
{
#endif
#ifdef sun
hardclock(regs)
	struct regs regs;
{
	int ps = regs.r_sr;
	caddr_t pc = (caddr_t)regs.r_pc;
#endif
	register struct callout *p1;
	register struct proc *p;
	register int s, cpstate;

#ifdef sun
	if (USERMODE(ps))		/* aston needs ar0 */
		u.u_ar0 = &regs.r_r0;
#endif
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
	for (p1 = calltodo.c_next; p1 && p1->c_time <= 0; p1 = p1->c_next)
		--p1->c_time;
	if (p1)
		--p1->c_time;

	/*
	 * Charge the time out based on the mode the cpu is in.
	 * Here again we fudge for the lack of proper interval timers
	 * assuming that the current state has been around at least
	 * one tick.
	 */
	if (USERMODE(ps)) {
		/*
		 * CPU was in user state.  Increment
		 * user time counter, and process process-virtual time
		 * interval timer. 
		 */
		bumptime(&u.u_ru.ru_utime, tick);
		if (timerisset(&u.u_timer[ITIMER_VIRTUAL].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_VIRTUAL], tick) == 0)
			psignal(u.u_procp, SIGVTALRM);
		if (u.u_procp->p_nice > NZERO)
			cpstate = CP_NICE;
		else
			cpstate = CP_USER;
		/*
		 * Charge it with resource utilization for a tick, updating
		 * statistics which run in (user+system) virtual time,
		 * such as the cpu time limit and profiling timers.
		 * This assumes that the current process has been running
		 * the entire last tick.
		 */
		if (!noproc) {
			s = u.u_procp->p_rssize;
			u.u_ru.ru_idrss += s; u.u_ru.ru_isrss += 0;	/* XXX */
			if (u.u_procp->p_textp) {
				register int xrss = u.u_procp->p_textp->x_rssize;

				s += xrss;
				u.u_ru.ru_ixrss += xrss;
			}
			if (s > u.u_ru.ru_maxrss)
				u.u_ru.ru_maxrss = s;
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
		}

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
#ifdef GPROF
		int k = pc - s_lowpc;
		if (profiling < 2 && k < s_textsize)
			kcount[k / (HISTFRACTION * sizeof (*kcount))]++;
#endif
		cpstate = CP_SYS;
		if (noproc) {
			if (BASEPRI(ps))
				cpstate = CP_IDLE;
		} else {
			bumptime(&u.u_ru.ru_stime, tick);
		}
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
#ifdef MUSH
		p->p_quota->q_cost += (p->p_nice > NZERO ?
		    (shconsts.sc_tic * ((2*NZERO)-p->p_nice)) / NZERO :
		    shconsts.sc_tic) * (((int)avenrun[0]+2)/3);
#endif
		if ((p->p_cpu&3) == 0) {
			(void) setpri(p);
			if (p->p_pri >= PUSER)
				p->p_pri = p->p_usrpri;
		}
	}

	/*
	 * Increment the time-of-day, and schedule
	 * processing of the callouts at a very low cpu priority,
	 * so we don't keep the relatively high clock interrupt
	 * priority any longer than necessary.
	 */
	bumptime(&time, tick);
	setsoftclock();
}

/*
 * Software priority level clock interrupt.
 * Run periodic events from timeout queue.
 */
/*ARGSUSED*/
#ifdef vax
softclock(pc, ps)
	caddr_t pc;
	int ps;
{
#endif
#ifdef sun
softclock()
{
	int ps = u.u_ar0[PS];
	caddr_t pc = (caddr_t)u.u_ar0[PC];
#endif

	for (;;) {
		register struct callout *p1;
		register caddr_t arg;
		register int (*func)();
		register int a, s;

		s = spl7();
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
	 * If trapped user-mode, give it a profiling tick.
	 */
	if (USERMODE(ps) && u.u_prof.pr_scale) {
		u.u_procp->p_flag |= SOWEUPC;
		aston();
	}
}

/*
 * Arrange that (*fun)(arg) is called in tim/hz seconds.
 */
timeout(fun, arg, tim)
	int (*fun)();
	caddr_t arg;
	int tim;
{
	register struct callout *p1, *p2, *pnew;
	register int t;
	int s;

	t = tim;
	s = spl7();
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

	s = spl7();
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
	int s = spl7();

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
