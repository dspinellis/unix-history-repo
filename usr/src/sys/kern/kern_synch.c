/*	kern_synch.c	4.19	82/09/04	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/vm.h"
#include "../h/pte.h"
#include "../h/inline.h"
#include "../h/mtpr.h"
#include "../h/quota.h"

#define SQSIZE 0100	/* Must be power of 2 */
#define HASH(x)	(( (int) x >> 5) & (SQSIZE-1))
struct proc *slpque[SQSIZE];

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
	register struct proc *rp, **hp;
	register s;

	rp = u.u_procp;
	s = spl6();
	if (chan==0 || rp->p_stat != SRUN || rp->p_rlink)
		panic("sleep");
	rp->p_wchan = chan;
	rp->p_slptime = 0;
	rp->p_pri = pri;
	hp = &slpque[HASH(chan)];
	rp->p_link = *hp;
	*hp = rp;
	if (pri > PZERO) {
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
out:
	splx(s);
	return;

	/*
	 * If priority was low (>PZERO) and
	 * there has been a signal, execute non-local goto through
	 * u.u_qsav, aborting the system call in progress (see trap.c)
	 * (or finishing a tsleep, see below)
	 */
psig:
	longjmp(u.u_qsav);
	/*NOTREACHED*/
}

/*
 * Sleep on chan at pri for at most a specified amount of time.
 * Return (TS_OK,TS_TIME,TS_SIG) on (normal,timeout,signal) condition.
 */
tsleep(chan, pri, tvp)
	caddr_t chan;
	int pri;
	struct timeval *tvp;
{
	register struct proc *p = u.u_procp;
	int s, rval;

	s = spl7();
	if (timercmp(tvp, &p->p_realtimer.itimer_value, >)) {
		/* alarm will occur first! */
		sleep(chan, pri);
		rval = TS_OK;		/* almost NOTREACHED modulo fuzz */
	} else {
		label_t lqsav;

		bcopy((caddr_t)u.u_qsav, (caddr_t)lqsav, sizeof (label_t));
		p->p_seltimer = *tvp;
		if (setjmp(u.u_qsav))
			rval = TS_SIG;
		else {
			sleep(chan, pri);
			rval = TS_OK;
		}
		timerclear(&p->p_seltimer);
		bcopy((caddr_t)lqsav, (caddr_t)u.u_qsav, sizeof (label_t));
	}
	splx(s);
	return (rval);
}

/*
 * Remove a process from its wait queue
 */
unsleep(p)
	register struct proc *p;
{
	register struct proc **hp;
	register s;

	s = spl6();
	if (p->p_wchan) {
		hp = &slpque[HASH(p->p_wchan)];
		while (*hp != p)
			hp = &(*hp)->p_link;
		*hp = p->p_link;
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
	register struct proc *p, **q, **h;
	int s;

	s = spl6();
	h = &slpque[HASH(chan)];
restart:
	for (q = h; p = *q; ) {
		if (p->p_rlink || p->p_stat != SSLEEP && p->p_stat != SSTOP)
			panic("wakeup");
		if (p->p_wchan==chan) {
			p->p_wchan = 0;
			*q = p->p_link;
			p->p_slptime = 0;
			if (p->p_stat == SSLEEP) {
				/* OPTIMIZED INLINE EXPANSION OF setrun(p) */
				p->p_stat = SRUN;
				if (p->p_flag & SLOAD)
					setrq(p);
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
				/* END INLINE EXPANSION */
				goto restart;
			}
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

	s = spl6();
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
	p += PUSER + 2*(pp->p_nice - NZERO);
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

/*
 * Create a new process-- the internal version of
 * sys fork.
 * It returns 1 in the new process, 0 in the old.
 */
newproc(isvfork)
	int isvfork;
{
	register struct proc *p;
	register struct proc *rpp, *rip;
	register int n;
	register struct file *fp;

	p = NULL;
	/*
	 * First, just locate a slot for a process
	 * and copy the useful info from this process into it.
	 * The panic "cannot happen" because fork has already
	 * checked for the existence of a slot.
	 */
retry:
	mpid++;
	if (mpid >= 30000) {
		mpid = 0;
		goto retry;
	}
	for (rpp = proc; rpp < procNPROC; rpp++) {
		if (rpp->p_stat == NULL && p==NULL)
			p = rpp;
		if (rpp->p_pid==mpid || rpp->p_pgrp==mpid)
			goto retry;
	}
	if ((rpp = p) == NULL)
		panic("no procs");

	/*
	 * Make a proc table entry for the new process.
	 */
	rip = u.u_procp;
#ifdef QUOTA
	(rpp->p_quota = rip->p_quota)->q_cnt++;
#endif
	rpp->p_stat = SIDL;
	timerclear(&rpp->p_realtimer.itimer_value);
	rpp->p_flag = SLOAD | (rip->p_flag & (SPAGI|SNUSIG));
	if (isvfork) {
		rpp->p_flag |= SVFORK;
		rpp->p_ndx = rip->p_ndx;
	} else
		rpp->p_ndx = rpp - proc;
	rpp->p_uid = rip->p_uid;
	rpp->p_pgrp = rip->p_pgrp;
	rpp->p_nice = rip->p_nice;
	rpp->p_textp = isvfork ? 0 : rip->p_textp;
	rpp->p_pid = mpid;
	rpp->p_ppid = rip->p_pid;
	rpp->p_pptr = rip;
	rpp->p_osptr = rip->p_cptr;
	if (rip->p_cptr)
		rip->p_cptr->p_ysptr = rpp;
	rpp->p_ysptr = NULL;
	rpp->p_cptr = NULL;
	rip->p_cptr = rpp;
	rpp->p_time = 0;
	rpp->p_cpu = 0;
	rpp->p_siga0 = rip->p_siga0;
	rpp->p_siga1 = rip->p_siga1;
	/* take along any pending signals, like stops? */
	if (isvfork) {
		rpp->p_tsize = rpp->p_dsize = rpp->p_ssize = 0;
		rpp->p_szpt = clrnd(ctopt(UPAGES));
		forkstat.cntvfork++;
		forkstat.sizvfork += rip->p_dsize + rip->p_ssize;
	} else {
		rpp->p_tsize = rip->p_tsize;
		rpp->p_dsize = rip->p_dsize;
		rpp->p_ssize = rip->p_ssize;
		rpp->p_szpt = rip->p_szpt;
		forkstat.cntfork++;
		forkstat.sizfork += rip->p_dsize + rip->p_ssize;
	}
	rpp->p_rssize = 0;
	rpp->p_maxrss = rip->p_maxrss;
	rpp->p_wchan = 0;
	rpp->p_slptime = 0;
	rpp->p_pctcpu = 0;
	rpp->p_cpticks = 0;
	n = PIDHASH(rpp->p_pid);
	p->p_idhash = pidhash[n];
	pidhash[n] = rpp - proc;
	multprog++;

	/*
	 * Increase reference counts on shared objects.
	 */
	for (n = 0; n < NOFILE; n++) {
		fp = u.u_ofile[n];
		if (fp == NULL)
			continue;
		fp->f_count++;
		if (u.u_pofile[n]&RDLOCK)
			fp->f_inode->i_rdlockc++;
		if (u.u_pofile[n]&WRLOCK)
			fp->f_inode->i_wrlockc++;
	}
	u.u_cdir->i_count++;
	if (u.u_rdir)
		u.u_rdir->i_count++;

	/*
	 * Partially simulate the environment
	 * of the new process so that when it is actually
	 * created (by copying) it will look right.
	 * This begins the section where we must prevent the parent
	 * from being swapped.
	 */
	rip->p_flag |= SKEEP;
	if (procdup(rpp, isvfork))
		return (1);

	/*
	 * Make child runnable and add to run queue.
	 */
	(void) spl6();
	rpp->p_stat = SRUN;
	setrq(rpp);
	(void) spl0();

	/*
	 * Cause child to take a non-local goto as soon as it runs.
	 * On older systems this was done with SSWAP bit in proc
	 * table; on VAX we use u.u_pcb.pcb_sswap so don't need
	 * to do rpp->p_flag |= SSWAP.  Actually do nothing here.
	 */
	/* rpp->p_flag |= SSWAP; */

	/*
	 * Now can be swapped.
	 */
	rip->p_flag &= ~SKEEP;

	/*
	 * If vfork make chain from parent process to child
	 * (where virtal memory is temporarily).  Wait for
	 * child to finish, steal virtual memory back,
	 * and wakeup child to let it die.
	 */
	if (isvfork) {
		u.u_procp->p_xlink = rpp;
		u.u_procp->p_flag |= SNOVM;
		while (rpp->p_flag & SVFORK)
			sleep((caddr_t)rpp, PZERO - 1);
		if ((rpp->p_flag & SLOAD) == 0)
			panic("newproc vfork");
		uaccess(rpp, Vfmap, &vfutl);
		u.u_procp->p_xlink = 0;
		vpassvm(rpp, u.u_procp, &vfutl, &u, Vfmap);
		u.u_procp->p_flag &= ~SNOVM;
		rpp->p_ndx = rpp - proc;
		rpp->p_flag |= SVFDONE;
		wakeup((caddr_t)rpp);
	}

	/*
	 * 0 return means parent.
	 */
	return (0);
}
