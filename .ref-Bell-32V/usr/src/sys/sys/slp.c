#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/uba.h"
#include "../h/map.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/page.h"
#include "../h/mtpr.h"

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
	register s;

	rp = u.u_procp;
	s = spl6();
	rp->p_stat = SSLEEP;
	rp->p_wchan = chan;
	rp->p_pri = pri;
	if(pri > PZERO) {
		if(issig()) {
			rp->p_wchan = 0;
			rp->p_stat = SRUN;
			spl0();
			goto psig;
		}
		spl0();
		if(runin != 0) {
			runin = 0;
			wakeup((caddr_t)&runin);
		}
		swtch();
		if(issig())
			goto psig;
	} else {
		spl0();
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
}

/*
 * Wake up all processes sleeping on chan.
 */
wakeup(chan)
register caddr_t chan;
{
	register struct proc *p;
	register i;
	int s;

	p = &proc[0];
	i = NPROC;
	do {
		if(p->p_wchan==chan && p->p_stat!=SZOMB) {
			/*
			 * this code looks dumb, but
			 * there is a possible race due
			 * to interrupts.
			 */
			s = spl6();
			if(p->p_wchan == chan)
				setrun(p);
			splx(s);
		}
		p++;
	} while(--i);
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

	if (p->p_stat==0 || p->p_stat==SZOMB)
		panic("Running a dead proc");
	p->p_wchan = 0;
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
 * The main loop of the scheduling (swapping)
 * process.
 * The basic idea is:
 *  see if anyone wants to be swapped in;
 *  swap out processes until there is room;
 *  swap him in;
 *  repeat.
 * The runout flag is set whenever someone is swapped out.
 * Sched sleeps on it awaiting work.
 *
 * Sched sleeps on runin whenever it cannot find enough
 * core (by swapping out or otherwise) to fit the
 * selected swapped process.  It is awakened when the
 * core situation changes and in any case once per second.
 */
sched()
{
	register struct proc *rp, *p, *inp;
	register outage, inage,memneed,memleft;
	int maxsize;
	extern  int  freemem;

	/*
	 * find user to swap in;
	 * of users ready, select one out longest
	 */

loop:
	spl6();
	outage = -20000;
	for (rp = &proc[0]; rp < &proc[NPROC]; rp++)
	if (rp->p_stat==SRUN && (rp->p_flag&SLOAD)==0 &&
	    rp->p_time - (rp->p_nice-NZERO)*8 > outage) {
		p = rp;
		outage = rp->p_time - (rp->p_nice-NZERO)*8;
	}
	/*
	 * If there is no one there, wait.
	 */
	if (outage == -20000) {
		runout++;
		sleep((caddr_t)&runout, PSWP);
		goto loop;
	}
	spl0();

	/*
	 * See if there is core for that process;
	 * if so, swap it in.
	 */

	if (swapin(p))
		goto loop;

	/*
	 * none found.
	 * look around for core.
	 * Select the largest of those sleeping
	 * at bad priority; if none, select the oldest.
	 */

	spl6();
	inp = p;		/* we are trying to swap this guy in */
	p = NULL;
	maxsize = -1;
	inage = -1;
	for (rp = &proc[0]; rp < &proc[NPROC]; rp++) {
		if (rp->p_stat==SZOMB)
			continue;
		if (rp == inp)
			continue;
		if (rp->p_flag&SSPART) {	/* we have found one partially swapped */
			p = rp;
			maxsize = 0;
			break;
		}
		if ((rp->p_flag&(SSYS|SLOCK|SULOCK|SLOAD))!=SLOAD)
			continue;
		if (rp->p_textp && rp->p_textp->x_flag&XLOCK)
			continue;
		if (rp->p_stat==SSLEEP&&rp->p_pri>=PZERO || rp->p_stat==SSTOP) {
			if (maxsize < rp->p_size) {
				p = rp;
				maxsize = rp->p_size;
			}
		} else if (maxsize<0 && (rp->p_stat==SRUN||rp->p_stat==SSLEEP)) {
			if (rp->p_time+rp->p_nice-NZERO > inage) {
				p = rp;
				inage = rp->p_time+rp->p_nice-NZERO;
			}
		}
	}
	spl0();
	/*
	 * Swap found user out if sleeping at bad pri,
	 * or if he has spent at least 2 seconds in core and
	 * the swapped-out process has spent at least 3 seconds out.
	 * Otherwise wait a bit and try again.
	 */
	if (maxsize>=0 || (outage>=3 && inage>=2)) {
		p->p_flag &= ~SLOAD;
		if (inp->p_flag & SSPART)
			memneed = inp->p_swsize - UPAGES;
		else
			memneed = inp->p_size;
		if (inp->p_textp)
			if (inp->p_textp->x_ccount == 0)
				memneed += inp->p_textp->x_size;
		memneed -= freemem;		/* amount we must swap out */
		memneed += 11 + SWAPSIZE - (memneed+11)%SWAPSIZE;
		memleft = p->p_size;
		if (p->p_flag & SSPART)
			memleft -= p->p_swsize;	/* core left */
		if ((memleft - memneed) < 2*SWAPSIZE)
			memneed = memleft; 	/* take it all */
		xswap(p,-memneed,0,-1);
		goto loop;
	}
	spl6();
	runin++;
	sleep((caddr_t)&runin, PSWP);
	goto loop;
}

/*
 * Swap a process in.
 */
swapin(p)
register struct proc *p;
{
	extern int Swapmap[], swaputl[], Swap2map[], swap2utl[];
	register struct text *xp;
	register int i, *ip;
	int x, pt, tsize, tmem;
	struct proc *prc;

	if (xp = p->p_textp) {
		xlock(xp);
		tsize = xp->x_size;
		tmem = xp->x_ccount == 0 ? tsize : 0;
	} else {
		tsize = p->p_tsize;
		tmem = 0;
	}	
	if ((p->p_flag & SSPART) == 0) {
		pt = (p->p_size - UPAGES + tsize + 127)/128;	/* number of pages of page table */
		if (memall(Swapmap,UPAGES + pt) == 0)
			goto nomem;
		for (i=UPAGES+pt; --i>=0; ) {	/* map u-area & page tables */
			Swapmap[i] |= PG_V + PG_KW;
			mtpr(TBIS, swaputl + (128 * i));
		}
		if(memall(&swaputl[(UPAGES*128)+tsize-tmem],p->p_size-UPAGES+tmem)==0){
				memfree(Swapmap, UPAGES + pt);	/* free u-area & page tables */
				goto nomem;
		}
		ip = &swaputl[UPAGES*128];
		for (i=tmem; --i>=0;)
			*ip++ |= PG_V + PG_URKR;
		ip = &swaputl[(UPAGES*128) + tsize];
		for (i=p->p_size-UPAGES; --i>=0; )
			*ip++ |= PG_V + PG_UW;
		((struct user *)swaputl)->u_pcb.pcb_szpt = pt;	/* for swap I/O */
		x = p->p_swaddr;		/* disk address */
		for (i=0; i<UPAGES; i++)
			p->p_addr[i] = Swapmap[i];
		for (i=0; i<pt; i++)
			((struct user *)swaputl)->u_ptable[i] = (Swapmap+UPAGES)[i] & PG_PFNUM;
	} else {
		ptaccess(p, Swapmap, swaputl);
		pt = ((struct user *)swaputl)->u_pcb.pcb_szpt;
		if (memall(&swaputl[(UPAGES*128)+tsize-tmem], p->p_swsize-UPAGES
				+ tmem) == 0)
			goto nomem;
		ip = &swaputl[(UPAGES*128)];
		for (i=tmem; --i>=0; )
			*ip++ |= PG_V + PG_URKR;
		ip = &swaputl[(UPAGES*128)+tsize];
		for (i=p->p_swsize-UPAGES; --i>=0; )
			*ip++ |= PG_V + PG_UW;
		x = p->p_swaddr;
	}
	if (xp) {
		if (xp->x_ccount==0) {
			xp->x_caddr = p;	/* make link to loaded proc */
			if ((xp->x_flag&XLOAD)==0)
				swap(p,xp->x_daddr,0,xp->x_size,B_READ,0);
		} else {
			if (xp->x_caddr == 0)	/* must find the text */
				for (prc= &proc[0]; prc<&proc[NPROC]; prc++)
					if ((prc->p_flag&SLOAD) && (prc->p_textp==xp)
					  && (prc->p_flag&SLOCK)==0 && (prc!=p)){
						xp->x_caddr = prc;	/* found it */
						break;
					}
			if (xp->x_caddr == 0)
				panic("lost text");
			ptaccess(xp->x_caddr, Swap2map, swap2utl);
			bcopy(swap2utl + 128*UPAGES, swaputl + 128*UPAGES, 4*xp->x_size);
		}
		xp->x_ccount++;
		xunlock(xp);
	}
	if ((p->p_flag & SSPART) == 0)
		swap(p, x, tsize, p->p_size, B_READ, 1);
	else {
		swap(p, x+UPAGES, tsize, p->p_swsize-UPAGES, B_READ, 0);
		p->p_flag &= ~SSPART;
	}
	((struct user *)swaputl)->u_pcb.pcb_szpt = pt;   /* may have shrunk */
	((struct user *)swaputl)->u_pcb.pcb_p1br = (int)&u + UPAGES*512
					+ pt*512 - 0x800000;  /* if page table shrunk, its moved */
	mfree(swapmap, ctod(p->p_size), x);
	x = (UPAGES*128) + tsize +  p->p_size - UPAGES;
	for (i=1; i<=((struct user *)swaputl)->u_ssize; i++) {
		swaputl[((UPAGES+pt)*128) - i] = swaputl[x - i];
		if ( x != (UPAGES+pt)*128 )
			swaputl[x - i] = 0;
	}
	p->p_flag |= SLOAD;
	p->p_time = 0;
	return(1);

nomem:
	if (xp)
		xunlock(xp);
	return(0);
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
	spl6();
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
	spl0();
	/*
	 * The rsav (ssav) contents are interpreted in the new address space
	 */
	n = p->p_flag&SSWAP;
	p->p_flag &= ~SSWAP;
	resume(p->p_addr, n? u.u_ssav: u.u_rsav);
}

/*
 * Create a new process-- the internal version of
 * sys fork.
 * It returns 1 in the new process, 0 in the old.
 */
newproc()
{
	register struct proc *p, *up;
	register struct proc *rpp, *rip;
	register n;

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
	up = rip;
	rpp->p_stat = SRUN;
	rpp->p_clktim = 0;
	rpp->p_flag = SLOAD;
	rpp->p_uid = rip->p_uid;
	rpp->p_pgrp = rip->p_pgrp;
	rpp->p_nice = rip->p_nice;
	rpp->p_textp = rip->p_textp;
	rpp->p_pid = mpid;
	rpp->p_ppid = rip->p_pid;
	rpp->p_time = 0;
	rpp->p_cpu = 0;
	rpp->p_tsize = rip->p_tsize;

	/*
	 * make duplicate entries
	 * where needed
	 */

	for(n=0; n<NOFILE; n++)
		if(u.u_ofile[n] != NULL)
			u.u_ofile[n]->f_count++;
	if(up->p_textp != NULL) {
		up->p_textp->x_count++;
		up->p_textp->x_ccount++;
	}
	u.u_cdir->i_count++;
	if (u.u_rdir)
		u.u_rdir->i_count++;
	/*
	 * Partially simulate the environment
	 * of the new process so that when it is actually
	 * created (by copying) it will look right.
	 */
	rpp = p;
	u.u_procp = rpp;
	rip = up;
	rpp->p_size = rip->p_size;
	/*
	 * When the resume is executed for the new process,
	 * here's where it will resume.
	 */
	if (save(u.u_ssav)) {
		return(1);
	}
	/*
	 * If there is not enough core for the
	 * new process, swap out the current process to generate the
	 * copy.
	 */
	if (procdup(rpp) == NULL) {
		rip->p_stat = SIDL;
		for(n=UPAGES; --n>=0; )
			rpp->p_addr[n] = rip->p_addr[n];
		xswap(rpp, 0, 0, -1);
		rip->p_stat = SRUN;
	}
	u.u_procp = rip;
	setrq(rpp);
	rpp->p_flag |= SSWAP;
	return(0);
}

/*
 * Change the size of the data+stack regions of the process.
 * If the size is shrinking, it's easy-- just release the extra core.
 * If it's growing, and there is core, just allocate it
 * and copy the image, taking care to reset registers to account
 * for the fact that the system's stack has moved.
 * If there is no core, arrange for the process to be swapped
 * out after adjusting the size requirement-- when it comes
 * in, enough core will be allocated.
 *
 * After the expansion, the caller will take care of copying
 * the user's stack towards or away from the data area.
 */
expand(change,region)
{
	int n,p0,p1,oldss;
	register struct proc *p;
	register  new;
	register  struct  pt_entry  *base;

	p = u.u_procp;
	n = p->p_size;
	oldss = 0x200000 - u.u_pcb.pcb_p1lr;
	p->p_size += change;
	if (change == 0)
		return;
	p0 = mfpr(P0BR) + 4 * mfpr(P0LR);
	p1 = mfpr(P1BR) + 4 * mfpr(P1LR);

	/* see if we must add another page to the user page table */
	if ((new = (p0 - p1 + 4*change)/4) > 0) {
		new = ptexpand((new + 127)/128);
	}
	if (region == P0BR) {
		base = (struct pt_entry *)mfpr(P0BR);
		base += (p0 = mfpr(P0LR)) + (change > 0 ? 0 : change);
	} else {
		base = (struct pt_entry *)mfpr(P1BR);
		base += (p1 = mfpr(P1LR)) - (change > 0 ? change : 0);
	}
	if (change > 0) {
		new = memall(base,change);
	} else
		memfree(base,-change);
	if (region == P0BR)  {
		mtpr(P0LR,p0 + change);
		u.u_pcb.pcb_p0lr = p0 + change | 0x04000000;
	} else {
		mtpr(P1LR,p1 - change);
		u.u_pcb.pcb_p1lr = p1 - change;
	}
	if (change < 0 || new > 0) {
		new = 1;
		if (change < 0) {
			change = -change;
			new = 0;
		}
		while (--change >= 0) {
			*(int *)base |= PG_UW;
			base->pg_v = new;  
			base++;
		}
		return;
	}

	/*
	 * When the resume is executed for the new process
	 * here's where it will resume.
	 */
	if (save(u.u_ssav)) {
		return;
	}
	xswap(p, 1, n, oldss);
	p->p_flag |= SSWAP;
	qswtch();
	/* no return */
}
