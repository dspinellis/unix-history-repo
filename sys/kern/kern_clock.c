/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
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
 *
 *	from: @(#)kern_clock.c	7.16 (Berkeley) 5/9/91
 *	$Id: kern_clock.c,v 1.12 1994/03/01 23:21:44 phk Exp $
 */

/* Portions of this software are covered by the following: */
/******************************************************************************
 *                                                                            *
 * Copyright (c) David L. Mills 1993, 1994                                    *
 *                                                                            *
 * Permission to use, copy, modify, and distribute this software and its      *
 * documentation for any purpose and without fee is hereby granted, provided  *
 * that the above copyright notice appears in all copies and that both the    *
 * copyright notice and this permission notice appear in supporting           *
 * documentation, and that the name University of Delaware not be used in     *
 * advertising or publicity pertaining to distribution of the software        *
 * without specific, written prior permission.  The University of Delaware    *
 * makes no representations about the suitability this software for any       *
 * purpose.  It is provided "as is" without express or implied warranty.      *
 *                                                                            *
 *****************************************************************************/


#include "param.h"
#include "systm.h"
#include "dkstat.h"
#include "callout.h"
#include "kernel.h"
#include "proc.h"
#include "signalvar.h"
#include "resourcevar.h"
#include "timex.h"

#include "machine/cpu.h"

#include "resource.h"
#include "vm/vm.h"

#ifdef GPROF
#include "gprof.h"
#endif

static void gatherstats(clockframe *);

/* From callout.h */
struct callout *callfree, *callout, calltodo;
int ncallout;

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
 * Phase-lock loop (PLL) definitions
 *
 * The following defines establish the performance envelope of the PLL.
 * They specify the maximum phase error (MAXPHASE), maximum frequency
 * error (MAXFREQ), minimum interval between updates (MINSEC) and
 * maximum interval between updates (MAXSEC). The intent of these bounds
 * is to force the PLL to operate within predefined limits in order to
 * satisfy correctness assertions. An excursion which exceeds these
 * bounds is clamped to the bound and operation proceeds accordingly. In
 * practice, this can occur only if something has failed or is operating
 * out of tolerance, but otherwise the PLL continues to operate in a
 * stable mode.
 *
 * MAXPHASE must be set greater than or equal to CLOCK.MAX (128 ms), as
 * defined in the NTP specification. CLOCK.MAX establishes the maximum
 * time offset allowed before the system time is reset, rather than
 * incrementally adjusted. Here, the maximum offset is clamped to
 * MAXPHASE only in order to prevent overflow errors due to defective
 * protocol implementations.
 *
 * MAXFREQ reflects the manufacturing frequency tolerance of the CPU
 * clock oscillator plus the maximum slew rate allowed by the protocol.
 * It should be set to at least the frequency tolerance of the
 * oscillator plus 100 ppm for vernier frequency adjustments. If the
 * kernel frequency discipline code is installed (PPS_SYNC), the CPU
 * oscillator frequency is disciplined to an external source, presumably
 * with negligible frequency error, and MAXFREQ can be reduced.
 */
#define MAXPHASE 512000L	/* max phase error (us) */
#ifdef PPS_SYNC
#define MAXFREQ (100L << SHIFT_USEC) /* max freq error (scaled ppm) */
#else
#define MAXFREQ (200L << SHIFT_USEC) /* max freq error (scaled ppm) */
#endif /* PPS_SYNC */
#define MINSEC 16L		/* min interval between updates (s) */
#define MAXSEC 1200L		/* max interval between updates (s) */

/*
 * The following variables are read and set by the ntp_adjtime() system
 * call. The ntp_pll.status variable defines the synchronization status of
 * the system clock, with codes defined in the timex.h header file. The
 * time_offset variable is used by the PLL to adjust the system time in
 * small increments. The time_constant variable determines the bandwidth
 * or "stiffness" of the PLL. The time_tolerance variable is the maximum
 * frequency error or tolerance of the CPU clock oscillator and is a
 * property of the architecture; however, in principle it could change
 * as result of the presence of external discipline signals, for
 * instance. The time_precision variable is usually equal to the kernel
 * tick variable; however, in cases where a precision clock counter or
 * external clock is available, the resolution can be much less than
 * this and depend on whether the external clock is working or not. The
 * time_maxerror variable is initialized by a ntp_adjtime() call and
 * increased by the kernel once each second to reflect the maximum error
 * bound growth. The time_esterror variable is set and read by the
 * ntp_adjtime() call, but otherwise not used by the kernel.
 */
/* - use appropriate fields in ntp_pll instead */
#if 0
int ntp_pll.status = TIME_BAD;	/* clock synchronization status */
long time_offset = 0;		/* time adjustment (us) */
long time_constant = 0;		/* pll time constant */
long time_tolerance = MAXFREQ;	/* frequency tolerance (scaled ppm) */
long time_precision = 1;	/* clock precision (us) */
long time_maxerror = MAXPHASE;	/* maximum error (us) */
long time_esterror = MAXPHASE;	/* estimated error (us) */
#endif

/*
 * The following variables establish the state of the PLL and the
 * residual time and frequency offset of the local clock. The time_phase
 * variable is the phase increment and the ntp_pll.frequency variable is the
 * frequency increment of the kernel time variable at each tick of the
 * clock. The ntp_pll.frequency variable is set via ntp_adjtime() from a value
 * stored in a file when the synchronization daemon is first started.
 * Its value is retrieved via ntp_adjtime() and written to the file
 * about once per hour by the daemon. The time_adj variable is the
 * adjustment added to the value of tick at each timer interrupt and is
 * recomputed at each timer interrupt. The time_reftime variable is the
 * second's portion of the system time on the last call to
 * ntp_adjtime(). It is used to adjust the ntp_pll.frequency variable and to
 * increase the time_maxerror as the time since last update increases.
 * The scale factors are defined in the timex.h header file.
 */
long time_phase = 0;		/* phase offset (scaled us) */
#if 0
long ntp_pll.frequency = 0;		/* frequency offset (scaled ppm) */
#endif
long time_adj = 0;		/* tick adjust (scaled 1 / hz) */
long time_reftime;	/* time at last adjustment (s) */

#ifdef PPS_SYNC
/*
 * The following defines and declarations are used only if a pulse-per-
 * second (PPS) signal is available and connected via a modem control
 * lead, such as produced by the optional ppsclock feature incorporated
 * in the asynch driver. They establish the design parameters of the PPS
 * frequency-lock loop used to discipline the CPU clock oscillator to
 * the PPS signal. PPS_AVG is the averaging factor for the frequency
 * loop. PPS_SHIFT and PPS_SHIFTMAX specify the minimum and maximum
 * intervals, respectively, in seconds as a power of two. The
 * PPS_DISPINC is the initial increment to pps_disp at each second.
 */
#define PPS_AVG 2		/* pps averaging constant (shift) */
#define PPS_SHIFT 2		/* min interval duration (s) (shift) */
#define PPS_SHIFTMAX 8		/* max interval duration (s) (shift) */
#define PPS_DISPINC 0L		/* dispersion increment (us/s) */

/*
 * The pps_time variable contains the time at each calibration as read
 * by microtime(). The pps_usec variable is latched from a high
 * resolution counter or external clock at pps_time. Here we want the
 * hardware counter contents only, not the contents plus the
 * time_tv.usec as usual. The pps_ybar variable is the current CPU
 * oscillator frequency offset estimate relative to the PPS signal. The
 * pps_disp variable is the current error estimate, which is increased
 * pps_dispinc once each second. Frequency updates are permitted only
 * when pps_disp is below the pps_dispmax threshold. The pps-mf[] array
 * is used as a median filter for the frequency estimate and to derive
 * the error estimate.
 */
struct timeval pps_time;	/* kernel time at last interval */
long pps_usec = 0;		/* usec counter at last interval */
#if 0
long pps_ybar = 0;		/* frequency estimate (scaled ppm) */
long pps_disp = MAXFREQ;	/* dispersion estimate (scaled ppm) */
#endif
long pps_dispmax = MAXFREQ / 2;	/* dispersion threshold */
long pps_dispinc = PPS_DISPINC;	/* pps dispersion increment/sec */
long pps_mf[] = {0, 0, 0};	/* pps median filter */

/*
 * The pps_count variable counts the seconds of the calibration
 * interval, the duration of which is pps_shift (s) in powers of two.
 * The pps_intcnt variable counts the calibration intervals for use in
 * the interval-adaptation algorithm. It's just too complicated for
 * words.
 */
int pps_count = 0;		/* calibration interval counter (s) */
#if 0
int pps_shift = PPS_SHIFT;	/* interval duration (s) (shift) */
#endif
int pps_intcnt = 0;		/* intervals at current duration */

/*
 * PPS signal quality monitors
 */
#if 0
long pps_calcnt;		/* calibration intervals */
long pps_jitcnt;		/* jitter limit exceeded */
long pps_discnt;		/* dispersion limit exceeded */
#endif
#endif /* PPS_SYNC */

struct timex ntp_pll = {
	0,			/* mode */
	0,			/* offset */
	0,			/* frequency */
	MAXPHASE,		/* maxerror */
	MAXPHASE,		/* esterror */
	TIME_BAD,		/* status */
	0,			/* time_constant */
	1,			/* precision */
	MAXFREQ,		/* tolerance */
	0,			/* ybar */
#ifdef PPS_SYNC
	MAXFREQ,		/* disp */
	PPS_SHIFT,		/* shift */
	0,			/* calcnt */
	0,			/* jitcnt */
	0			/* discnt */
#endif
};

/*
 * hardupdate() - local clock update
 *
 * This routine is called by ntp_adjtime() to update the local clock
 * phase and frequency. This is used to implement an adaptive-parameter,
 * first-order, type-II phase-lock loop. The code computes the time
 * since the last update and clamps to a maximum (for robustness). Then
 * it multiplies by the offset (sorry about the ugly multiply), scales
 * by the time constant, and adds to the frequency variable. Then, it
 * computes the phase variable as the offset scaled by the time
 * constant. Note that all shifts are assumed to be positive. Only
 * enough error checking is done to prevent bizarre behavior due to
 * overflow problems.
 *
 * For default SHIFT_UPDATE = 12, the offset is limited to +-512 ms, the
 * maximum interval between updates is 4096 s and the maximum frequency
 * offset is +-31.25 ms/s.
 */
void
hardupdate(offset)
	long offset;
{
	long mtemp;

	if (offset > MAXPHASE)
		ntp_pll.offset = MAXPHASE << SHIFT_UPDATE;
	else if (offset < -MAXPHASE)
		ntp_pll.offset = -(MAXPHASE << SHIFT_UPDATE);
	else
		ntp_pll.offset = offset << SHIFT_UPDATE;
	mtemp = time.tv_sec - time_reftime;
	time_reftime = time.tv_sec;
	if (mtemp > MAXSEC)
		mtemp = 0;

	/* ugly multiply should be replaced */
	if (offset < 0)
	  ntp_pll.frequency -= 
	    (-offset * mtemp) >> (ntp_pll.time_constant
				  + ntp_pll.time_constant
				  + SHIFT_KF 
				  - SHIFT_USEC);
	else
	  ntp_pll.frequency +=
	    (offset * mtemp) >> (ntp_pll.time_constant 
				 + ntp_pll.time_constant
				 + SHIFT_KF
				 - SHIFT_USEC);
	if (ntp_pll.frequency > ntp_pll.tolerance)
		ntp_pll.frequency = ntp_pll.tolerance;
	else if (ntp_pll.frequency < -ntp_pll.tolerance)
		ntp_pll.frequency = -ntp_pll.tolerance;
	if (ntp_pll.status == TIME_BAD)
		ntp_pll.status = TIME_OK;
}

/*
 * The hz hardware interval timer.
 * We update the events relating to real time.
 * If this timer is also being used to gather statistics,
 * we run through the statistics gathering routine as well.
 */
void
hardclock(frame)
	clockframe frame;
{
	register struct callout *p1;
	register struct proc *p = curproc;
	register struct pstats *pstats = 0;
	register struct rusage *ru;
	register struct vmspace *vm;
	register int s;
	int needsoft = 0;
	extern int tickdelta;
	extern long timedelta;
	long ltemp, time_update = 0;

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
	 * Curproc (now in p) is null if no process is running.
	 * We assume that curproc is set in user mode!
	 */
	if (p)
		pstats = p->p_stats;
	/*
	 * Charge the time out based on the mode the cpu is in.
	 * Here again we fudge for the lack of proper interval timers
	 * assuming that the current state has been around at least
	 * one tick.
	 */
	if (CLKF_USERMODE(&frame)) {
		if (pstats->p_prof.pr_scale)
			needsoft = 1;
		/*
		 * CPU was in user state.  Increment
		 * user time counter, and process process-virtual time
		 * interval timer. 
		 */
		BUMPTIME(&p->p_utime, tick);
		if (timerisset(&pstats->p_timer[ITIMER_VIRTUAL].it_value) &&
		    itimerdecr(&pstats->p_timer[ITIMER_VIRTUAL], tick) == 0)
			psignal(p, SIGVTALRM);
	} else {
		/*
		 * CPU was in system state.
		 */
		if (p)
			BUMPTIME(&p->p_stime, tick);
	}

	/* bump the resource usage of integral space use */
	if (p && pstats && (ru = &pstats->p_ru) && (vm = p->p_vmspace)) {
		ru->ru_ixrss += vm->vm_tsize * NBPG / 1024;
		ru->ru_idrss += vm->vm_dsize * NBPG / 1024;
		ru->ru_isrss += vm->vm_ssize * NBPG / 1024;
		if ((vm->vm_pmap.pm_stats.resident_count * NBPG / 1024) >
		    ru->ru_maxrss) {
			ru->ru_maxrss =
			    vm->vm_pmap.pm_stats.resident_count * NBPG / 1024;
		}
	}

	/*
	 * If the cpu is currently scheduled to a process, then
	 * charge it with resource utilization for a tick, updating
	 * statistics which run in (user+system) virtual time,
	 * such as the cpu time limit and profiling timers.
	 * This assumes that the current process has been running
	 * the entire last tick.
	 */
	if (p) {
		if ((p->p_utime.tv_sec+p->p_stime.tv_sec+1) >
		    p->p_rlimit[RLIMIT_CPU].rlim_cur) {
			psignal(p, SIGXCPU);
			if (p->p_rlimit[RLIMIT_CPU].rlim_cur <
			    p->p_rlimit[RLIMIT_CPU].rlim_max)
				p->p_rlimit[RLIMIT_CPU].rlim_cur += 5;
		}
		if (timerisset(&pstats->p_timer[ITIMER_PROF].it_value) &&
		    itimerdecr(&pstats->p_timer[ITIMER_PROF], tick) == 0)
			psignal(p, SIGPROF);

		/*
		 * We adjust the priority of the current process.
		 * The priority of a process gets worse as it accumulates
		 * CPU time.  The cpu usage estimator (p_cpu) is increased here
		 * and the formula for computing priorities (in kern_synch.c)
		 * will compute a different value each time the p_cpu increases
		 * by 4.  The cpu usage estimator ramps up quite quickly when
		 * the process is running (linearly), and decays away
		 * exponentially, * at a rate which is proportionally slower
		 * when the system is busy.  The basic principal is that the
		 * system will 90% forget that a process used a lot of CPU
		 * time in 5*loadav seconds.  This causes the system to favor
		 * processes which haven't run much recently, and to
		 * round-robin among other processes.
		 */
		p->p_cpticks++;
		if (++p->p_cpu == 0)
			p->p_cpu--;
		if ((p->p_cpu&3) == 0) {
			setpri(p);
			if (p->p_pri >= PUSER)
				p->p_pri = p->p_usrpri;
		}
	}

	/*
	 * If the alternate clock has not made itself known then
	 * we must gather the statistics.
	 */
	if (phz == 0)
		gatherstats(&frame);

	/*
	 * Increment the time-of-day, and schedule
	 * processing of the callouts at a very low cpu priority,
	 * so we don't keep the relatively high clock interrupt
	 * priority any longer than necessary.
	 */
	{
		int delta;
		if (timedelta == 0) {
		  delta = tick;
		} else {
			if (timedelta < 0) {
				delta = tick - tickdelta;
				timedelta += tickdelta;
			} else {
				delta = tick + tickdelta;
				timedelta -= tickdelta;
			}
		}
		/*
		 * Logic from ``Precision Time and Frequency Synchronization
		 * Using Modified Kernels'' by David L. Mills, University
		 * of Delaware.
		 */
		time_phase += time_adj;
		if(time_phase <= -FINEUSEC) {
			ltemp = -time_phase >> SHIFT_SCALE;
			time_phase += ltemp << SHIFT_SCALE;
			time_update -= ltemp;
		} else if(time_phase >= FINEUSEC) {
			ltemp = time_phase >> SHIFT_SCALE;
			time_phase -= ltemp << SHIFT_SCALE;
			time_update += ltemp;
		}

		time.tv_usec += delta + time_update;
		/*
		 * On rollover of the second the phase adjustment to be used for
		 * the next second is calculated. Also, the maximum error is
		 * increased by the tolerance. If the PPS frequency discipline
		 * code is present, the phase is increased to compensate for the
		 * CPU clock oscillator frequency error.
		 *
		 * With SHIFT_SCALE = 23, the maximum frequency adjustment is
		 * +-256 us per tick, or 25.6 ms/s at a clock frequency of 100
		 * Hz. The time contribution is shifted right a minimum of two
		 * bits, while the frequency contribution is a right shift.
		 * Thus, overflow is prevented if the frequency contribution is
		 * limited to half the maximum or 15.625 ms/s.
		 */
		if (time.tv_usec >= 1000000) {
			time.tv_usec -= 1000000;
			time.tv_sec++;
			ntp_pll.maxerror += ntp_pll.tolerance >> SHIFT_USEC;
			if (ntp_pll.offset < 0) {
				ltemp = -ntp_pll.offset >>
			    (SHIFT_KG + ntp_pll.time_constant);
				ntp_pll.offset += ltemp;
			time_adj = -ltemp <<
			  (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
			} else {
				ltemp = ntp_pll.offset >>
				  (SHIFT_KG + ntp_pll.time_constant);
				ntp_pll.offset -= ltemp;
				time_adj = ltemp <<
				  (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
			}
#ifdef PPS_SYNC
			/*
			 * Grow the pps error by pps_dispinc ppm and clamp to
			 * MAXFREQ. The hardpps() routine will pull it down as
			 * long as the PPS signal is good.
			 */
			ntp_pll.disp += pps_dispinc;
			if (ntp_pll.disp > MAXFREQ)
			  ntp_pll.disp = MAXFREQ;
			ltemp = ntp_pll.frequency + ntp_pll.ybar;
#else
			ltemp = ntp_pll.frequency;
#endif /* PPS_SYNC */
			if (ltemp < 0)
			  time_adj -= -ltemp >>
			    (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);
			else
			  time_adj += ltemp >>
			    (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);
#if 0
			time_adj += fixtick << (SHIFT_SCALE - SHIFT_HZ);
#endif

			/*
			 * When the CPU clock oscillator frequency is not a
			 * power of two in Hz, the SHIFT_HZ is only an
			 * approximate scale factor. In the SunOS kernel, this
			 * results in a PLL gain factor of 1/1.28 = 0.78 what it
			 * should be. In the following code the overall gain is
			 * increased by a factor of 1.25, which results in a
			 * residual error less than 3 percent.
			 */
			if (hz == 100) {
				if (time_adj < 0)
				  time_adj -= -time_adj >> 2;
				else
				  time_adj += time_adj >> 2;
			}
		}
	}

	if (needsoft) {
#if 0
/*
 * XXX - hardclock runs at splhigh, so the splsoftclock is useless and
 * softclock runs at splhigh as well if we do this.  It is not much of
 * an optimization, since the "software interrupt" is done with a call
 * from doreti, and the overhead of checking there is sometimes less
 * than checking here.  Moreover, the whole %$$%$^ frame is passed by
 * value here.
 */
		if (CLKF_BASEPRI(&frame)) {
			/*
			 * Save the overhead of a software interrupt;
			 * it will happen as soon as we return, so do it now.
			 */
			(void) splsoftclock();
			softclock(frame);
		} else
#endif
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
void
gatherstats(framep)
	clockframe *framep;
{
	register int cpstate, s;

	/*
	 * Determine what state the cpu is in.
	 */
	if (CLKF_USERMODE(framep)) {
		/*
		 * CPU was in user state.
		 */
		if (curproc->p_nice > NZERO)
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
		if (curproc == NULL && CLKF_BASEPRI(framep))
			cpstate = CP_IDLE;
#ifdef GPROF
		s = (u_long) CLKF_PC(framep) - (u_long) s_lowpc;
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
void
softclock(frame)
	clockframe frame;
{

	for (;;) {
		register struct callout *p1;
		register caddr_t arg;
		register timeout_func_t func;
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
	 * If no process to work with, we're finished.
	 */
	if (curproc == 0) return;

	/*
	 * If trapped user-mode and profiling, give it
	 * a profiling tick.
	 */
	if (CLKF_USERMODE(&frame)) {
		register struct proc *p = curproc;

		if (p->p_stats->p_prof.pr_scale)
			profile_tick(p, &frame);
		/*
		 * Check to see if process has accumulated
		 * more than 10 minutes of user time.  If so
		 * reduce priority to give others a chance.
		 */
		if (p->p_ucred->cr_uid && p->p_nice == NZERO &&
		    p->p_utime.tv_sec > 10 * 60) {
			p->p_nice = NZERO + 4;
			setpri(p);
			p->p_pri = p->p_usrpri;
		}
	}
}

/*
 * Arrange that (*func)(arg) is called in t/hz seconds.
 */
void
timeout(func, arg, t)
	timeout_func_t func;
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
	pnew->c_func = func;
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
void
untimeout(func, arg)
	timeout_func_t func;
	caddr_t arg;
{
	register struct callout *p1, *p2;
	register int s;

	s = splhigh();
	for (p1 = &calltodo; (p2 = p1->c_next) != 0; p1 = p2) {
		if (p2->c_func == func && p2->c_arg == arg) {
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

/* XXX clock_t */
u_long
hzto(tv)
	struct timeval *tv;
{
	register unsigned long ticks;
	register long sec;
	register long usec;
	int s;

	/*
	 * If the number of usecs in the whole seconds part of the time
	 * difference fits in a long, then the total number of usecs will
	 * fit in an unsigned long.  Compute the total and convert it to
	 * ticks, rounding up and adding 1 to allow for the current tick
	 * to expire.  Rounding also depends on unsigned long arithmetic
	 * to avoid overflow.
	 *
	 * Otherwise, if the number of ticks in the whole seconds part of
	 * the time difference fits in a long, then convert the parts to
	 * ticks separately and add, using similar rounding methods and
	 * overflow avoidance.  This method would work in the previous
	 * case but it is slightly slower and assumes that hz is integral.
	 *
	 * Otherwise, round the time difference down to the maximum
	 * representable value.
	 *
	 * Maximum value for any timeout in 10ms ticks is 248 days.
	 */
	s = splhigh();
	sec = tv->tv_sec - time.tv_sec;
	usec = tv->tv_usec - time.tv_usec;
	splx(s);
	if (usec < 0) {
		sec--;
		usec += 1000000;
	}
	if (sec < 0) {
#ifdef DIAGNOSTIC
		printf("hzto: negative time difference %ld sec %ld usec\n",
		       sec, usec);
#endif
		ticks = 1;
	} else if (sec <= LONG_MAX / 1000000)
		ticks = (sec * 1000000 + (unsigned long)usec + (tick - 1))
			/ tick + 1;
	else if (sec <= LONG_MAX / hz)
		ticks = sec * hz
			+ ((unsigned long)usec + (tick - 1)) / tick + 1;
	else
		ticks = LONG_MAX;
#define	CLOCK_T_MAX	INT_MAX	/* XXX should be ULONG_MAX */
	if (ticks > CLOCK_T_MAX)
		ticks = CLOCK_T_MAX;
	return (ticks);
}

#ifdef PPS_SYNC
/*
 * hardpps() - discipline CPU clock oscillator to external pps signal
 *
 * This routine is called at each PPS interrupt in order to discipline
 * the CPU clock oscillator to the PPS signal. It integrates successive
 * phase differences between the two oscillators and calculates the
 * frequency offset. This is used in hardclock() to discipline the CPU
 * clock oscillator so that intrinsic frequency error is cancelled out.
 * The code requires the caller to capture the time and hardware
 * counter value at the designated PPS signal transition.
 */
void
hardpps(tvp, usec)
	struct timeval *tvp;		/* time at PPS */
	long usec;			/* hardware counter at PPS */
{
	long u_usec, v_usec, bigtick;
	long cal_sec, cal_usec;

	/*
	 * During the calibration interval adjust the starting time when
	 * the tick overflows. At the end of the interval compute the
	 * duration of the interval and the difference of the hardware
	 * counters at the beginning and end of the interval. This code
	 * is deliciously complicated by the fact valid differences may
	 * exceed the value of tick when using long calibration
	 * intervals and small ticks. Note that the counter can be
	 * greater than tick if caught at just the wrong instant, but
	 * the values returned and used here are correct.
	 */
	bigtick = (long)tick << SHIFT_USEC;
	pps_usec -= ntp_pll.ybar;
	if (pps_usec >= bigtick)
		pps_usec -= bigtick;
	if (pps_usec < 0)
		pps_usec += bigtick;
	pps_time.tv_sec++;
	pps_count++;
	if (pps_count < (1 << pps_shift))
		return;
	pps_count = 0;
	ntp_pll.calcnt++;
	u_usec = usec << SHIFT_USEC;
	v_usec = pps_usec - u_usec;
	if (v_usec >= bigtick >> 1)
		v_usec -= bigtick;
	if (v_usec < -(bigtick >> 1))
		v_usec += bigtick;
	if (v_usec < 0)
		v_usec = -(-v_usec >> ntp_pll.shift);
	else
		v_usec = v_usec >> ntp_pll.shift;
	pps_usec = u_usec;
	cal_sec = tvp->tv_sec;
	cal_usec = tvp->tv_usec;
	cal_sec -= pps_time.tv_sec;
	cal_usec -= pps_time.tv_usec;
	if (cal_usec < 0) {
		cal_usec += 1000000;
		cal_sec--;
	}
	pps_time = *tvp;

	/*
	 * Check for lost interrupts, noise, excessive jitter and
	 * excessive frequency error. The number of timer ticks during
	 * the interval may vary +-1 tick. Add to this a margin of one
	 * tick for the PPS signal jitter and maximum frequency
	 * deviation. If the limits are exceeded, the calibration
	 * interval is reset to the minimum and we start over.
	 */
	u_usec = (long)tick << 1;
	if (!((cal_sec == -1 && cal_usec > (1000000 - u_usec))
	    || (cal_sec == 0 && cal_usec < u_usec))
	    || v_usec > ntp_pll.tolerance || v_usec < -ntp_pll.tolerance) {
		ntp_pll.jitcnt++;
		ntp_pll.shift = NTP_PLL.SHIFT;
		pps_dispinc = PPS_DISPINC;
		ntp_pll.intcnt = 0;
		return;
	}

	/*
	 * A three-stage median filter is used to help deglitch the pps
	 * signal. The median sample becomes the offset estimate; the
	 * difference between the other two samples becomes the
	 * dispersion estimate.
	 */
	pps_mf[2] = pps_mf[1];
	pps_mf[1] = pps_mf[0];
	pps_mf[0] = v_usec;
	if (pps_mf[0] > pps_mf[1]) {
		if (pps_mf[1] > pps_mf[2]) {
			u_usec = pps_mf[1];		/* 0 1 2 */
			v_usec = pps_mf[0] - pps_mf[2];
		} else if (pps_mf[2] > pps_mf[0]) {
			u_usec = pps_mf[0];		/* 2 0 1 */
			v_usec = pps_mf[2] - pps_mf[1];
		} else {
			u_usec = pps_mf[2];		/* 0 2 1 */
			v_usec = pps_mf[0] - pps_mf[1];
		}
	} else {
		if (pps_mf[1] < pps_mf[2]) {
			u_usec = pps_mf[1];		/* 2 1 0 */
			v_usec = pps_mf[2] - pps_mf[0];
		} else  if (pps_mf[2] < pps_mf[0]) {
			u_usec = pps_mf[0];		/* 1 0 2 */
			v_usec = pps_mf[1] - pps_mf[2];
		} else {
			u_usec = pps_mf[2];		/* 1 2 0 */
			v_usec = pps_mf[1] - pps_mf[0];
		}
	}

	/*
	 * Here the dispersion average is updated. If it is less than
	 * the threshold pps_dispmax, the frequency average is updated
	 * as well, but clamped to the tolerance.
	 */
	v_usec = (v_usec >> 1) - ntp_pll.disp;
	if (v_usec < 0)
		ntp_pll.disp -= -v_usec >> PPS_AVG;
	else
		ntp_pll.disp += v_usec >> PPS_AVG;
	if (ntp_pll.disp > pps_dispmax) {
		ntp_pll.discnt++;
		return;
	}
	if (u_usec < 0) {
		ntp_pll.ybar -= -u_usec >> PPS_AVG;
		if (ntp_pll.ybar < -ntp_pll.tolerance)
			ntp_pll.ybar = -ntp_pll.tolerance;
		u_usec = -u_usec;
	} else {
		ntp_pll.ybar += u_usec >> PPS_AVG;
		if (ntp_pll.ybar > ntp_pll.tolerance)
			ntp_pll.ybar = ntp_pll.tolerance;
	}

	/*
	 * Here the calibration interval is adjusted. If the maximum
	 * time difference is greater than tick/4, reduce the interval
	 * by half. If this is not the case for four consecutive
	 * intervals, double the interval.
	 */
	if (u_usec << ntp_pll.shift > bigtick >> 2) {
		ntp_pll.intcnt = 0;
		if (ntp_pll.shift > NTP_PLL.SHIFT) {
			ntp_pll.shift--;
			pps_dispinc <<= 1;
		}
	} else if (ntp_pll.intcnt >= 4) {
		ntp_pll.intcnt = 0;
		if (ntp_pll.shift < NTP_PLL.SHIFTMAX) {
			ntp_pll.shift++;
			pps_dispinc >>= 1;
		}
	} else
		ntp_pll.intcnt++;
}
#endif /* PPS_SYNC */
