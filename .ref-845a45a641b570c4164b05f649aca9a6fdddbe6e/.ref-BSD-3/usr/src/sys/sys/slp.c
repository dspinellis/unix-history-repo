/*	slp.c	2.2	1/15/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/vm.h"
#include "../h/pte.h"


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
{
	register struct proc *rp;
	register s, h;

	rp = u.u_procp;
	s = spl6();
	if (chan==0)
		panic("zero wchan");
	rp->p_stat = SSLEEP;
	rp->p_wchan = chan;
	rp->p_slptime = 0;
	rp->p_pri = pri;
	h = HASH(chan);
	rp->p_link = slpque[h];
	slpque[h] = rp;
	if(pri > PZERO) {
		if(issig()) {
			rp->p_wchan = 0;
			rp->p_stat = SRUN;
			slpque[h] = rp->p_link;
			VOID spl0();
			goto psig;
		}
		VOID spl0();
		if(runin != 0) {
			runin = 0;
			wakeup((caddr_t)&runin);
		}
		swtch();
		if(issig())
			goto psig;
	} else {
		VOID spl0();
		swtch();
	}
	splx(s);
	return;

	/*
	 * If priority was low (>PZERO) and
	 * there has been a signal,
	 * execute non-local goto to
	 * the qsav location.
	 * (see trap1/trap.c)
	 */
psig:
	resume(u.u_procp->p_addr, u.u_qsav);
	/*NOTREACHED*/
}

/*
 * Wake up all processes sleeping on chan.
 */
wakeup(chan)
register caddr_t chan;
{
	register struct proc *p, *q;
	register i;
	int s;

	s = spl6();
	i = HASH(chan);
restart:
	p = slpque[i];
	q = NULL;
	while(p != NULL) {
		if(p->p_wchan==chan && p->p_stat!=SZOMB) {
			if (q == NULL)
				slpque[i] = p->p_link;
			else
				q->p_link = p->p_link;
			p->p_wchan = 0;
			p->p_slptime = 0;
			setrun(p);
			goto restart;
		}
		q = p;
		p = p->p_link;
	}
	splx(s);
}

/*
 * when you are sure that it
 * is impossible to get the
 * 'proc on q' diagnostic, the
 * diagnostic loop can be removed.
 */
setrq(p)
struct proc *p;
{
	register struct proc *q;
	register s;

	s = spl6();
	for(q=runq; q!=NULL; q=q->p_link)
		if(q == p) {
			printf("proc on q\n");
			goto out;
		}
	p->p_link = runq;
	runq = p;
out:
	splx(s);
}

/*
 * Set the process running;
 * arrange for it to be swapped in if necessary.
 */
setrun(p)
register struct proc *p;
{
	register caddr_t w;

	if (p->p_stat==0 || p->p_stat==SZOMB)
		panic("Running a dead proc");
	/*
	 * The assignment to w is necessary because of
	 * race conditions. (Interrupt between test and use)
	 */
	if (w = p->p_wchan) {
		wakeup(w);
		return;
	}
	p->p_stat = SRUN;
	setrq(p);
	if(p->p_pri < curpri)
		runrun++;
	if(runout != 0 && (p->p_flag&SLOAD) == 0) {
		runout = 0;
		wakeup((caddr_t)&runout);
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
	register p;

	p = (pp->p_cpu & 0377)/16;
	p += PUSER + pp->p_nice - NZERO;
	if(p > 127)
		p = 127;
	if(p < curpri)
		runrun++;
	pp->p_pri = p;
	return(p);
}

/*
 * put the current process on
 * the Q of running processes and
 * call the scheduler.
 */
qswtch()
{

	setrq(u.u_procp);
	swtch();
}

/*
 * This routine is called to reschedule the CPU.
 * if the calling process is not in RUN state,
 * arrangements for it to restart must have
 * been made elsewhere, usually by calling via sleep.
 * There is a race here. A process may become
 * ready after it has been examined.
 * In this case, idle() will be called and
 * will return in at most 1HZ time.
 * i.e. its not worth putting an spl() in.
 */
swtch()
{
	register n;
	register struct proc *p, *q, *pp, *pq;


	/*
	 * If not the idle process, resume the idle process.
	 */
	if (u.u_procp != &proc[0]) {
		if (save(u.u_rsav)) {
			return;
		}
		resume(proc[0].p_addr, u.u_qsav);
	}
	/*
	 * The first save returns nonzero when proc 0 is resumed
	 * by another process (above); then the second is not done
	 * and the process-search loop is entered.
	 *
	 * The first save returns 0 when swtch is called in proc 0
	 * from sched().  The second save returns 0 immediately, so
	 * in this case too the process-search loop is entered.
	 * Thus when proc 0 is awakened by being made runnable, it will
	 * find itself and resume itself at rsav, and return to sched().
	 */
	if (save(u.u_qsav)==0 && save(u.u_rsav))
		return;
loop:
	VOID spl6();
	runrun = 0;
	pp = NULL;
	q = NULL;
	n = 128;
	/*
	 * Search for highest-priority runnable process
	 */
	for(p=runq; p!=NULL; p=p->p_link) {
		if((p->p_stat==SRUN) && (p->p_flag&SLOAD)) {
			if(p->p_pri < n) {
				pp = p;
				pq = q;
				n = p->p_pri;
			}
		}
		q = p;
	}
	/*
	 * If no process is runnable, idle.
	 */
	p = pp;
	if(p == NULL) {
		idle();
		goto loop;
	}
	q = pq;
	if(q == NULL)
		runq = p->p_link;
	else
		q->p_link = p->p_link;
	curpri = n;
	VOID spl0();
	/*
	 * The rsav (ssav) contents are interpreted in the new address space
	 */
	n = p->p_flag&SSWAP;
	p->p_flag &= ~SSWAP;
	cnt.v_swtch++;
	resume(p->p_addr, n? u.u_ssav: u.u_rsav);
	/*NOTREACHED*/
}

/*
 * Create a new process-- the internal version of
 * sys fork.
 * It returns 1 in the new process, 0 in the old.
 */
newproc(isvfork)
{
	register struct proc *p;
	register struct proc *rpp, *rip;
	register int n;

	p = NULL;
	/*
	 * First, just locate a slot for a process
	 * and copy the useful info from this process into it.
	 * The panic "cannot happen" because fork has already
	 * checked for the existence of a slot.
	 */
retry:
	mpid++;
	if(mpid >= 30000) {
		mpid = 0;
		goto retry;
	}
	for(rpp = &proc[0]; rpp < &proc[NPROC]; rpp++) {
		if(rpp->p_stat == NULL && p==NULL)
			p = rpp;
		if (rpp->p_pid==mpid || rpp->p_pgrp==mpid)
			goto retry;
	}
	if ((rpp = p)==NULL)
		panic("no procs");

	/*
	 * make proc entry for new proc
	 */

	rip = u.u_procp;
	rpp->p_stat = SIDL;
	rpp->p_clktim = 0;
	rpp->p_flag = SLOAD | (rip->p_flag & SPAGI);
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
	rpp->p_time = 0;
	rpp->p_cpu = 0;
	if (isvfork) {
		rpp->p_tsize = rpp->p_dsize = rpp->p_ssize = 0;
		rpp->p_szpt = 0;
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
	rpp->p_wchan = 0;
	rpp->p_slptime = 0;
	rpp->p_aveflt = rip->p_aveflt;
	rate.v_pgin += rip->p_aveflt;
	rpp->p_faults = 0;

	/*
	 * make duplicate entries
	 * where needed
	 */

	multprog++;

	for(n=0; n<NOFILE; n++)
		if(u.u_ofile[n] != NULL) {
			u.u_ofile[n]->f_count++;
			if(!isvfork && u.u_vrpages[n])
				u.u_ofile[n]->f_inode->i_vfdcnt++;
		}

	u.u_cdir->i_count++;
	if (u.u_rdir)
		u.u_rdir->i_count++;
	/*
	 * Partially simulate the environment
	 * of the new process so that when it is actually
	 * created (by copying) it will look right.
	 */

	rip->p_flag |= SKEEP;	/* prevent parent from being swapped */

	/*
	 * When the resume is executed for the new process,
	 * here is where it will resume.
	 */

	if (save(u.u_ssav))
		return(1);

	procdup(rpp, isvfork);
	 
	rpp->p_stat = SRUN;
	rpp->p_flag |= SSWAP;
	setrq(rpp);
	rip->p_flag &= ~SKEEP;
	if (isvfork) {
		u.u_procp->p_xlink = rpp;
		u.u_procp->p_flag |= SNOVM;
		while (rpp->p_flag & SVFORK)
			sleep((caddr_t)rpp, PZERO - 1);
		if ((rpp->p_flag & SLOAD) == 0)
			panic("newproc vfork");
		uaccess(rpp, Vfmap, &vfutl);
		u.u_procp->p_xlink = 0;
		vpassvm(rpp, u.u_procp, &vfutl, &u);
		for (n = 0; n < NOFILE; n++)
			if (vfutl.u_vrpages[n]) {
				if ((u.u_vrpages[n] = vfutl.u_vrpages[n] - 1) == 0)
					if (--u.u_ofile[n]->f_inode->i_vfdcnt < 0)
						panic("newproc i_vfdcnt");
				vfutl.u_vrpages[n] = 0;
			}
		u.u_procp->p_flag &= ~SNOVM;
		rpp->p_ndx = rpp - proc;
		rpp->p_flag |= SVFDONE;
		wakeup((caddr_t)rpp);
	}
	return (0);
}
