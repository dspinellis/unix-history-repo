/*	kern_clock.c	4.16	81/03/09	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/callout.h"
#include "../h/seg.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/reg.h"
#include "../h/psl.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/text.h"
#include "../h/vlimit.h"
#include "../h/mtpr.h"
#include "../h/clock.h"
#include "../h/cpu.h"

#include "dh.h"
#include "dz.h"

/*
 * Hardclock is called straight from
 * the real time clock interrupt.
 * We limit the work we do at real clock interrupt time to:
 *	reloading clock
 *	decrementing time to callouts
 *	recording cpu time usage
 *	modifying priority of current process
 *	arrange for soft clock interrupt
 *	kernel pc profiling
 *
 * At software (softclock) interrupt time we:
 *	implement callouts
 *	maintain date
 *	lightning bolt wakeup (every second)
 *	alarm clock signals
 *	jab the scheduler
 *
 * On the vax softclock interrupts are implemented by
 * software interrupts.  Note that we may have multiple softclock
 * interrupts compressed into one (due to excessive interrupt load),
 * but that hardclock interrupts should never be lost.
 */

/*ARGSUSED*/
hardclock(pc, ps)
	caddr_t pc;
{
	register struct callout *p1;
	register struct proc *pp;
	register int s, cpstate;

	/*
	 * reprime clock
	 */
	clkreld();

	/*
	 * update callout times
	 */
	if (callout[0].c_func == NULL)
		goto out;
	p1 = &callout[0];
	while (p1->c_time<=0 && p1->c_func!=NULL)
		p1++;
	p1->c_time--;
out:

	/*
	 * Maintain iostat and per-process cpu statistics
	 */
	if (!noproc) {
		s = u.u_procp->p_rssize;
		u.u_vm.vm_idsrss += s;
		if (u.u_procp->p_textp) {
			register int xrss = u.u_procp->p_textp->x_rssize;

			s += xrss;
			u.u_vm.vm_ixrss += xrss;
		}
		if (s > u.u_vm.vm_maxrss)
			u.u_vm.vm_maxrss = s;
		if ((u.u_vm.vm_utime+u.u_vm.vm_stime+1)/hz > u.u_limit[LIM_CPU]) {
			psignal(u.u_procp, SIGXCPU);
			if (u.u_limit[LIM_CPU] < INFINITY - 5)
				u.u_limit[LIM_CPU] += 5;
		}
	}
	/*
	 * Update iostat information.
	 */
	if (USERMODE(ps)) {
		u.u_vm.vm_utime++;
		if(u.u_procp->p_nice > NZERO)
			cpstate = CP_NICE;
		else
			cpstate = CP_USER;
	} else {
		cpstate = CP_SYS;
		if (noproc)
			cpstate = CP_IDLE;
		else
			u.u_vm.vm_stime++;
	}
	cp_time[cpstate]++;
	for (s = 0; s < DK_NDRIVE; s++)
		if (dk_busy&(1<<s))
			dk_time[s]++;
	/*
	 * Adjust priority of current process.
	 */
	if (!noproc) {
		pp = u.u_procp;
		pp->p_cpticks++;
		if(++pp->p_cpu == 0)
			pp->p_cpu--;
		if(pp->p_cpu % 16 == 0) {
			(void) setpri(pp);
			if (pp->p_pri >= PUSER)
				pp->p_pri = pp->p_usrpri;
		}
	}
	/*
	 * Time moves on.
	 */
	++lbolt;
#if VAX780
	/*
	 * On 780's, impelement a fast UBA watcher,
	 * to make sure uba's don't get stuck.
	 */
	if (cpu == VAX_780 && panicstr == 0 && !BASEPRI(ps))
		unhang();
#endif
	/*
	 * Schedule a software interrupt for the rest
	 * of clock activities.
	 */
	setsoftclock();
}

/*
 * SCHMAG is the constant in the digital decay cpu
 * usage priority assignment.  Each second we multiply
 * the previous cpu usage estimate by SCHMAG.  At 9/10
 * it tends to decay away all knowledge of previous activity
 * in about 10 seconds.
 */
#define	SCHMAG	9/10

/*
 * Constant for decay filter for cpu usage field
 * in process table (used by ps au).
 */
double	ccpu = 0.95122942450071400909;		/* exp(-1/20) */

/*
 * Software clock interrupt.
 * This routine runs at lower priority than device interrupts.
 */
/*ARGSUSED*/
softclock(pc, ps)
	caddr_t pc;
{
	register struct callout *p1, *p2;
	register struct proc *pp;
	register int a, s;

	/*
	 * Perform callouts (but not after panic's!)
	 */
	if (panicstr == 0 && callout[0].c_time <= 0) {
		p1 = &callout[0];
		while (p1->c_func != 0 && p1->c_time <= 0) {
			(*p1->c_func)(p1->c_arg);
			p1++;
		}
		p2 = &callout[0];
		while (p2->c_func = p1->c_func) {
			p2->c_time = p1->c_time;
			p2->c_arg = p1->c_arg;
			p1++;
			p2++;
		}
	}

	/*
	 * Drain silos.
	 */
#if NDH > 0
	s = spl5(); dhtimer(); splx(s);
#endif
#if NDZ > 0
	s = spl5(); dztimer(); splx(s);
#endif

	/*
	 * If idling and processes are waiting to swap in,
	 * check on them.
	 */
	if (noproc && runin) {
		runin = 0;
		wakeup((caddr_t)&runin);
	}

	/*
	 * Run paging daemon and reschedule every 1/4 sec.
	 */
	if (lbolt % (hz/4) == 0) {
		vmpago();
		runrun++;
		aston();
	}

	/*
	 * Lightning bolt every second:
	 *	sleep timeouts
	 *	process priority recomputation
	 *	process %cpu averaging
	 *	virtual memory metering
	 *	kick swapper if processes want in
	 */
	if (lbolt >= hz) {
		/*
		 * This doesn't mean much on VAX since we run at
		 * software interrupt time... if hardclock()
		 * calls softclock() directly, it prevents
		 * this code from running when the priority
		 * was raised when the clock interrupt occurred.
		 */
		if (BASEPRI(ps))
			return;

		/*
		 * If we didn't run a few times because of
		 * long blockage at high ipl, we don't
		 * really want to run this code several times,
		 * so squish out all multiples of hz here.
		 */
		time += lbolt / hz;
		lbolt %= hz;

		/*
		 * Wakeup lightning bolt sleepers.
		 * Processes sleep on lbolt to wait
		 * for short amounts of time (e.g. 1 second).
		 */
		wakeup((caddr_t)&lbolt);

		/*
		 * Recompute process priority and process
		 * sleep() system calls as well as internal
		 * sleeps with timeouts (tsleep() kernel routine).
		 */
		for (pp = proc; pp < procNPROC; pp++)
		if (pp->p_stat && pp->p_stat!=SZOMB) {
			/*
			 * Increase resident time, to max of 127 seconds
			 * (it is kept in a character.)  For
			 * loaded processes this is time in core; for
			 * swapped processes, this is time on drum.
			 */
			if (pp->p_time != 127)
				pp->p_time++;
			/*
			 * If process has clock counting down, and it
			 * expires, set it running (if this is a tsleep()),
			 * or give it an SIGALRM (if the user process
			 * is using alarm signals.
			 */
			if (pp->p_clktim && --pp->p_clktim == 0)
				if (pp->p_flag & STIMO) {
					s = spl6();
					switch (pp->p_stat) {

					case SSLEEP:
						setrun(pp);
						break;

					case SSTOP:
						unsleep(pp);
						break;
					}
					pp->p_flag &= ~STIMO;
					splx(s);
				} else
					psignal(pp, SIGALRM);
			/*
			 * If process is blocked, increment computed
			 * time blocked.  This is used in swap scheduling.
			 */
			if (pp->p_stat==SSLEEP || pp->p_stat==SSTOP)
				if (pp->p_slptime != 127)
					pp->p_slptime++;
			/*
			 * Update digital filter estimation of process
			 * cpu utilization for loaded processes.
			 */
			if (pp->p_flag&SLOAD)
				pp->p_pctcpu = ccpu * pp->p_pctcpu +
				    (1.0 - ccpu) * (pp->p_cpticks/(float)hz);
			/*
			 * Recompute process priority.  The number p_cpu
			 * is a weighted estimate of cpu time consumed.
			 * A process which consumes cpu time has this
			 * increase regularly.  We here decrease it by
			 * a fraction (SCHMAG is 90%), giving a digital
			 * decay filter which damps out in about 10 seconds.
			 *
			 * If a process is niced, then the nice directly
			 * affects the new priority.  The final priority
			 * is in the range 0 to 255, to fit in a character.
			 */
			pp->p_cpticks = 0;
			a = (pp->p_cpu & 0377)*SCHMAG + pp->p_nice - NZERO;
			if (a < 0)
				a = 0;
			if (a > 255)
				a = 255;
			pp->p_cpu = a;
			(void) setpri(pp);
			/*
			 * Now have computed new process priority
			 * in p->p_usrpri.  Carefully change p->p_pri.
			 * A process is on a run queue associated with
			 * this priority, so we must block out process
			 * state changes during the transition.
			 */
			s = spl6();
			if (pp->p_pri >= PUSER) {
				if ((pp != u.u_procp || noproc) &&
				    pp->p_stat == SRUN &&
				    (pp->p_flag & SLOAD) &&
				    pp->p_pri != pp->p_usrpri) {
					remrq(pp);
					pp->p_pri = pp->p_usrpri;
					setrq(pp);
				} else
					pp->p_pri = pp->p_usrpri;
			}
			splx(s);
		}

		/*
		 * Perform virtual memory metering.
		 */
		vmmeter();

		/*
		 * If the swap process is trying to bring
		 * a process in, have it look again to see
		 * if it is possible now.
		 */
		if (runin!=0) {
			runin = 0;
			wakeup((caddr_t)&runin);
		}

		/*
		 * If there are pages that have been cleaned, 
		 * jolt the pageout daemon to process them.
		 * We do this here so that these pages will be
		 * freed if there is an abundance of memory and the
		 * daemon would not be awakened otherwise.
		 */
		if (bclnlist != NULL)
			wakeup((caddr_t)&proc[2]);

		/*
		 * If the trap occurred from usermode,
		 * then check to see if it has now been
		 * running more than 10 minutes of user time
		 * and should thus run with reduced priority
		 * to give other processes a chance.
		 */
		if (USERMODE(ps)) {
			pp = u.u_procp;
			if (pp->p_uid && pp->p_nice == NZERO &&
			    u.u_vm.vm_utime > 600 * hz)
				pp->p_nice = NZERO+4;
			(void) setpri(pp);
			pp->p_pri = pp->p_usrpri;
		}
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
 * Timeout is called to arrange that
 * fun(arg) is called in tim/hz seconds.
 * An entry is sorted into the callout
 * structure.  The time in each structure
 * entry is the number of hz's more
 * than the previous entry.
 * In this way, decrementing the
 * first entry has the effect of
 * updating all entries.
 *
 * The panic is there because there is nothing
 * intelligent to be done if an entry won't fit.
 */
timeout(fun, arg, tim)
	int (*fun)();
	caddr_t arg;
{
	register struct callout *p1, *p2, *p3;
	register int t;
	int s;

	t = tim;
	p1 = &callout[0];
	s = spl7();
	while (p1->c_func != 0 && p1->c_time <= t) {
		t -= p1->c_time;
		p1++;
	}
	p1->c_time -= t;
	p2 = p1;
	p3 = callout+(ncallout-2);
	while (p2->c_func != 0) {
		if (p2 >= p3)
			panic("timeout");
		p2++;
	}
	while (p2 >= p1) {
		(p2+1)->c_time = p2->c_time;
		(p2+1)->c_func = p2->c_func;
		(p2+1)->c_arg = p2->c_arg;
		p2--;
	}
	p1->c_time = t;
	p1->c_func = fun;
	p1->c_arg = arg;
	splx(s);
}
