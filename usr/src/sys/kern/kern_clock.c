/*	kern_clock.c	4.8	%G%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/callo.h"
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

#include "dh.h"
#include "dz.h"

#define	SCHMAG	9/10


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
 * At softclock interrupt time we:
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

hardclock(pc, ps)
	caddr_t pc;
{
	register struct callo *p1;
	register struct proc *pp;
	register long *ip;
	register int s, cpstate;

	/*
	 * reprime clock
	 */
	clkreld();

	/*
	 * update callout times
	 */
	if(callout[0].c_func == NULL)
		goto out;
	p1 = &callout[0];
	while(p1->c_time<=0 && p1->c_func!=NULL)
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
		if ((u.u_vm.vm_utime+u.u_vm.vm_stime+1)/HZ > u.u_limit[LIM_CPU]) {
			psignal(u.u_procp, SIGXCPU);
			if (u.u_limit[LIM_CPU] < INFINITY - 5)
				u.u_limit[LIM_CPU] += 5;
		}
	}
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
	++lbolt;
#if VAX==780
	if (!BASEPRI(ps))
		unhang();
#endif
	setsoftclock();
}

/*
 * Constant for decay filter for cpu usage.
 */
double	ccpu = 0.95122942450071400909;		/* exp(-1/20) */

/*
 * Software clock interrupt.
 * This routine is blocked by spl1(),
 * which doesn't block device interrupts!
 */
softclock(pc, ps)
	caddr_t pc;
{
	register struct callo *p1, *p2;
	register struct proc *pp;
	register int a, s;

	/*
	 * callout
	 */
	if(callout[0].c_time <= 0) {
		p1 = &callout[0];
		while(p1->c_func != 0 && p1->c_time <= 0) {
			(*p1->c_func)(p1->c_arg);
			p1++;
		}
		p2 = &callout[0];
		while(p2->c_func = p1->c_func) {
			p2->c_time = p1->c_time;
			p2->c_arg = p1->c_arg;
			p1++;
			p2++;
		}
	}

	/*
	 * Drain silos.
	 */
#if NDH11 > 0
	s = spl5(); dhtimer(); splx(s);
#endif
#if NDZ11 > 0
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
	if (lbolt % (HZ/4) == 0) {
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
	if (lbolt >= HZ) {
		if (BASEPRI(ps))
			return;
		lbolt -= HZ;
		++time;
		wakeup((caddr_t)&lbolt);
		for(pp = &proc[0]; pp < &proc[NPROC]; pp++)
		if (pp->p_stat && pp->p_stat!=SZOMB) {
			if(pp->p_time != 127)
				pp->p_time++;
			if(pp->p_clktim)
				if(--pp->p_clktim == 0)
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
			if(pp->p_stat==SSLEEP||pp->p_stat==SSTOP)
				if (pp->p_slptime != 127)
					pp->p_slptime++;
			if (pp->p_flag&SLOAD)
				pp->p_pctcpu = ccpu * pp->p_pctcpu +
				    (1.0 - ccpu) * (pp->p_cpticks/(float)HZ);
			pp->p_cpticks = 0;
			a = (pp->p_cpu & 0377)*SCHMAG + pp->p_nice - NZERO;
			if(a < 0)
				a = 0;
			if(a > 255)
				a = 255;
			pp->p_cpu = a;
			(void) setpri(pp);
			s = spl6();
			if(pp->p_pri >= PUSER) {
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
		vmmeter();
		if(runin!=0) {
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
		if (USERMODE(ps)) {
			pp = u.u_procp;
			if (pp->p_uid)
				if (pp->p_nice == NZERO && u.u_vm.vm_utime > 600 * HZ)
					pp->p_nice = NZERO+4;
			(void) setpri(pp);
			pp->p_pri = pp->p_usrpri;
		}
	}
	if (USERMODE(ps) && u.u_prof.pr_scale) {
		u.u_procp->p_flag |= SOWEUPC;
		aston();
	}
}

/*
 * timeout is called to arrange that
 * fun(arg) is called in tim/HZ seconds.
 * An entry is sorted into the callout
 * structure. The time in each structure
 * entry is the number of HZ's more
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
	register struct callo *p1, *p2, *p3;
	register int t;
	int s;

	t = tim;
	p1 = &callout[0];
	s = spl7();
	while(p1->c_func != 0 && p1->c_time <= t) {
		t -= p1->c_time;
		p1++;
	}
	p1->c_time -= t;
	p2 = p1;
	p3 = &callout[NCALL-2];
	while(p2->c_func != 0) {
		if (p2 >= p3)
			panic("timeout");
		p2++;
	}
	while(p2 >= p1) {
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
