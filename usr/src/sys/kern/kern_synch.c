/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_synch.c	6.10 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "file.h"
#include "inode.h"
#include "vm.h"
#include "kernel.h"
#include "buf.h"

#ifdef vax
#include "../vax/mtpr.h"	/* XXX */
#endif
/*
 * Force switch among equal priority processes every 100ms.
 */
roundrobin()
{

	runrun++;
	aston();
	timeout(roundrobin, (caddr_t)0, hz / 10);
}

/* fraction for digital decay to forget 90% of usage in 5*loadav sec */
#define	filter(loadav) ((2 * (loadav)) / (2 * (loadav) + 1))

double	ccpu = 0.95122942450071400909;		/* exp(-1/20) */

/*
 * Recompute process priorities, once a second
 */
schedcpu()
{
	register double ccpu1 = (1.0 - ccpu) / (double)hz;
	register struct proc *p;
	register int s, a;
	float scale = filter(avenrun[0]);

	wakeup((caddr_t)&lbolt);
	for (p = allproc; p != NULL; p = p->p_nxt) {
		if (p->p_time != 127)
			p->p_time++;
		if (p->p_stat==SSLEEP || p->p_stat==SSTOP)
			if (p->p_slptime != 127)
				p->p_slptime++;
		/*
		 * If the process has slept the entire second,
		 * stop recalculating its priority until it wakes up.
		 */
		if (p->p_slptime > 1) {
			p->p_pctcpu *= ccpu;
			continue;
		}
		/*
		 * p_pctcpu is only for ps.
		 */
		p->p_pctcpu = ccpu * p->p_pctcpu + ccpu1 * p->p_cpticks;
		p->p_cpticks = 0;
		a = (int) (scale * (p->p_cpu & 0377)) + p->p_nice;
		if (a < 0)
			a = 0;
		if (a > 255)
			a = 255;
		p->p_cpu = a;
		(void) setpri(p);
		s = splhigh();	/* prevent state changes */
		if (p->p_pri >= PUSER) {
#define	PPQ	(128 / NQS)
			if ((p != u.u_procp || noproc) &&
			    p->p_stat == SRUN &&
			    (p->p_flag & SLOAD) &&
			    (p->p_pri / PPQ) != (p->p_usrpri / PPQ)) {
				remrq(p);
				p->p_pri = p->p_usrpri;
				setrq(p);
			} else
				p->p_pri = p->p_usrpri;
		}
		splx(s);
	}
	vmmeter();
	if (runin!=0) {
		runin = 0;
		wakeup((caddr_t)&runin);
	}
	if (bclnlist != NULL)
		wakeup((caddr_t)&proc[2]);
	timeout(schedcpu, (caddr_t)0, hz);
}

/*
 * Recalculate the priority of a process after it has slept for a while.
 */
updatepri(p)
	register struct proc *p;
{
	register int a = p->p_cpu & 0377;
	float scale = filter(avenrun[0]);

	p->p_slptime--;		/* the first time was done in schedcpu */
	while (a && --p->p_slptime)
		a = (int) (scale * a) /* + p->p_nice */;
	if (a < 0)
		a = 0;
	if (a > 255)
		a = 255;
	p->p_cpu = a;
	(void) setpri(p);
}

#define SQSIZE 0100	/* Must be power of 2 */
#define HASH(x)	(( (int) x >> 5) & (SQSIZE-1))
struct slpque {
	struct proc *sq_head;
	struct proc **sq_tailp;
} slpque[SQSIZE];

/*
 * Give up the processor till a wakeup occurs
 * on chan, at which time the process
 * enters the scheduling queue at priority pri.
 * The most important effect of pri is that when
 * pri<=PZERO a signal cannot disturb the sleep;
 * if pri>PZERO signals will be processed.
 * Callers of this routine must be prepared for
 * premature return, and check that the reason for
 * sleeping has gone away.
 */
sleep(chan, pri)
	caddr_t chan;
	int pri;
{
	register struct proc *rp;
	register struct slpque *qp;
	register s;

	rp = u.u_procp;
	s = splhigh();
	if (panicstr) {
		/*
		 * After a panic, just give interrupts a chance,
		 * then just return; don't run any other procs 
		 * or panic below, in case this is the idle process
		 * and already asleep.
		 * The splnet should be spl0 if the network was being used
		 * by the filesystem, but for now avoid network interrupts
		 * that might cause another panic.
		 */
		(void) splnet();
		splx(s);
		return;
	}
	if (chan==0 || rp->p_stat != SRUN || rp->p_rlink)
		panic("sleep");
	rp->p_wchan = chan;
	rp->p_slptime = 0;
	rp->p_pri = pri;
	qp = &slpque[HASH(chan)];
	if (qp->sq_head == 0)
		qp->sq_head = rp;
	else
		*qp->sq_tailp = rp;
	*(qp->sq_tailp = &rp->p_link) = 0;
	if (pri > PZERO) {
		/*
		 * If we stop in issig(), wakeup may already have happened
		 * when we return (rp->p_wchan will then be 0).
		 */
		if (ISSIG(rp)) {
			if (rp->p_wchan)
				unsleep(rp);
			rp->p_stat = SRUN;
			(void) spl0();
			goto psig;
		}
		if (rp->p_wchan == 0)
			goto out;
		rp->p_stat = SSLEEP;
		(void) spl0();
		u.u_ru.ru_nvcsw++;
		swtch();
		if (ISSIG(rp))
			goto psig;
	} else {
		rp->p_stat = SSLEEP;
		(void) spl0();
		u.u_ru.ru_nvcsw++;
		swtch();
	}
	curpri = rp->p_usrpri;
out:
	splx(s);
	return;

	/*
	 * If priority was low (>PZERO) and
	 * there has been a signal, execute non-local goto through
	 * u.u_qsave, aborting the system call in progress (see trap.c)
	 */
psig:
	longjmp(&u.u_qsave);
	/*NOTREACHED*/
}

/*
 * Remove a process from its wait queue
 */
unsleep(p)
	register struct proc *p;
{
	register struct slpque *qp;
	register struct proc **hp;
	int s;

	s = splhigh();
	if (p->p_wchan) {
		hp = &(qp = &slpque[HASH(p->p_wchan)])->sq_head;
		while (*hp != p)
			hp = &(*hp)->p_link;
		*hp = p->p_link;
		if (qp->sq_tailp == &p->p_link)
			qp->sq_tailp = hp;
		p->p_wchan = 0;
	}
	splx(s);
}

/*
 * Wake up all processes sleeping on chan.
 */
wakeup(chan)
	register caddr_t chan;
{
	register struct slpque *qp;
	register struct proc *p, **q;
	int s;

	s = splhigh();
	qp = &slpque[HASH(chan)];
restart:
	for (q = &qp->sq_head; p = *q; ) {
		if (p->p_rlink || p->p_stat != SSLEEP && p->p_stat != SSTOP)
			panic("wakeup");
		if (p->p_wchan==chan) {
			p->p_wchan = 0;
			*q = p->p_link;
			if (qp->sq_tailp == &p->p_link)
				qp->sq_tailp = q;
			if (p->p_stat == SSLEEP) {
				/* OPTIMIZED INLINE EXPANSION OF setrun(p) */
				if (p->p_slptime > 1)
					updatepri(p);
				p->p_slptime = 0;
				p->p_stat = SRUN;
				if (p->p_flag & SLOAD)
					setrq(p);
				/*
				 * Since curpri is a usrpri,
				 * p->p_pri is always better than curpri.
				 */
				runrun++;
				aston();
				if ((p->p_flag&SLOAD) == 0) {
					if (runout != 0) {
						runout = 0;
						wakeup((caddr_t)&runout);
					}
					wantin++;
				}
				/* END INLINE EXPANSION */
				goto restart;
			}
			p->p_slptime = 0;
		} else
			q = &p->p_link;
	}
	splx(s);
}

/*
 * Initialize the (doubly-linked) run queues
 * to be empty.
 */
rqinit()
{
	register int i;

	for (i = 0; i < NQS; i++)
		qs[i].ph_link = qs[i].ph_rlink = (struct proc *)&qs[i];
}

/*
 * Set the process running;
 * arrange for it to be swapped in if necessary.
 */
setrun(p)
	register struct proc *p;
{
	register int s;

	s = splhigh();
	switch (p->p_stat) {

	case 0:
	case SWAIT:
	case SRUN:
	case SZOMB:
	default:
		panic("setrun");

	case SSTOP:
	case SSLEEP:
		unsleep(p);		/* e.g. when sending signals */
		break;

	case SIDL:
		break;
	}
	if (p->p_slptime > 1)
		updatepri(p);
	p->p_stat = SRUN;
	if (p->p_flag & SLOAD)
		setrq(p);
	splx(s);
	if (p->p_pri < curpri) {
		runrun++;
		aston();
	}
	if ((p->p_flag&SLOAD) == 0) {
		if (runout != 0) {
			runout = 0;
			wakeup((caddr_t)&runout);
		}
		wantin++;
	}
}

/*
 * Set user priority.
 * The rescheduling flag (runrun)
 * is set if the priority is better
 * than the currently running process.
 */
setpri(pp)
	register struct proc *pp;
{
	register int p;

	p = (pp->p_cpu & 0377)/4;
	p += PUSER + 2 * pp->p_nice;
	if (pp->p_rssize > pp->p_maxrss && freemem < desfree)
		p += 2*4;	/* effectively, nice(4) */
	if (p > 127)
		p = 127;
	if (p < curpri) {
		runrun++;
		aston();
	}
	pp->p_usrpri = p;
	return (p);
}
