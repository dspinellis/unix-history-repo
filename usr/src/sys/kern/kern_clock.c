/*	kern_clock.c	4.39	82/09/08	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/callout.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/proc.h"
#include "../h/psl.h"
#include "../h/vm.h"
#include "../h/text.h"
#ifdef MUSH
#include "../h/quota.h"
#include "../h/share.h"
#endif

#include "dh.h"
#include "dz.h"
#include "ps.h"

#ifdef GPROF
extern	int profiling;
extern	char *s_lowpc;
extern	u_long s_textsize;
extern	u_short *kcount;
#endif

#define	bumptime(tp)	\
	(tp)->tv_usec += tick; \
	if ((tp)->tv_usec >= 1000000) { \
		(tp)->tv_usec -= 1000000; \
		(tp)->tv_sec++; \
	}

/*ARGSUSED*/
hardclock(pc, ps)
	caddr_t pc;
{
	register struct callout *p1;
	register struct proc *p;
	register int s, cpstate;
	extern double avenrun[];

#if NPS > 0
	psextsync(pc, ps);
#endif

/* update callout times */
	for (p1 = calltodo.c_next; p1 && p1->c_time <= 0; p1 = p1->c_next)
		--p1->c_time;
	if (p1)
		--p1->c_time;

/* charge process for resource usage... statistically! */
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

/* charge for cpu */
	if (USERMODE(ps)) {
		bumptime(&u.u_ru.ru_utime);
		if (timerisset(&u.u_timer[ITIMER_VIRTUAL].it_value) &&
		    itimerdecr(&u.u_timer[ITIMER_VIRTUAL], tick) == 0)
			psignal(u.u_procp, SIGVTALRM);
		if (u.u_procp->p_nice > NZERO)
			cpstate = CP_NICE;
		else
			cpstate = CP_USER;
	} else {
#ifdef GPROF
		int k = pc - s_lowpc;
		if (profiling < 2 && k < s_textsize)
			kcount[k / sizeof (*kcount)]++;
#endif
		cpstate = CP_SYS;
		if (noproc) {
			if ((ps&PSL_IPL) != 0)
				cpstate = CP_IDLE;
		} else {
			bumptime(&u.u_ru.ru_stime);
		}
	}

/* iostat statistics */
	cp_time[cpstate]++;
	for (s = 0; s < DK_NDRIVE; s++)
		if (dk_busy&(1<<s))
			dk_time[s]++;

/* adjust priority of current process */
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
		if (p->p_cpu % 4 == 0) {
			(void) setpri(p);
			if (p->p_pri >= PUSER)
				p->p_pri = p->p_usrpri;
		}
	}
	bumptime(&time);
	setsoftclock();
}

/*ARGSUSED*/
softclock(pc, ps)
	caddr_t pc;
{
	register struct callout *p1;
	register int a, s;
	caddr_t arg;
	int (*func)();

	if (panicstr)
		goto nocallout;
	for (;;) {
		s = spl7();
		if ((p1 = calltodo.c_next) == 0 || p1->c_time > 0) {
			splx(s);
			break;
		}
		calltodo.c_next = p1->c_next;
		arg = p1->c_arg;
		func = p1->c_func;
		a = p1->c_time;
		p1->c_next = callfree;
		callfree = p1;
		(void) splx(s);
		(*func)(arg, a);
	}
nocallout:

#if NDH > 0
	s = spl5(); dhtimer(); splx(s);
#endif
#if NDZ > 0
	s = spl5(); dztimer(); splx(s);
#endif

/* if nothing to do, try swapin */
	if (noproc && runin) {
		runin = 0;
		wakeup((caddr_t)&runin);
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

hzto(tv)
	struct timeval *tv;
{
	register int ticks;
	int s = spl7();

	ticks = ((tv->tv_sec - time.tv_sec) * 1000 +
		(tv->tv_usec - time.tv_usec) / 1000) / (tick / 1000);
	splx(s);
	return (ticks);
}
