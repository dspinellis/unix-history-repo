/*	kern_exit.c	6.8	85/05/22	*/

#include "../machine/reg.h"
#include "../machine/psl.h"

#include "param.h"
#include "systm.h"
#include "map.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "wait.h"
#include "vm.h"
#include "file.h"
#include "mbuf.h"
#include "inode.h"
#include "syslog.h"

/*
 * Exit system call: pass back caller's arg
 */
rexit()
{
	register struct a {
		int	rval;
	} *uap;

	uap = (struct a *)u.u_ap;
	exit((uap->rval & 0377) << 8);
}

/*
 * Release resources.
 * Save u. area for parent to look at.
 * Enter zombie state.
 * Wake up parent and init processes,
 * and dispose of children.
 */
exit(rv)
	int rv;
{
	register int i;
	register struct proc *p, *q, *nq;
	register int x;
	struct mbuf *m = m_getclr(M_WAIT, MT_ZOMBIE);

#ifdef PGINPROF
	vmsizmon();
#endif
	p = u.u_procp;
	p->p_flag &= ~(STRC|SULOCK);
	p->p_flag |= SWEXIT;
	p->p_sigignore = ~0;
	p->p_cpticks = 0;
	p->p_pctcpu = 0;
	for (i = 0; i < NSIG; i++)
		u.u_signal[i] = SIG_IGN;
	untimeout(realitexpire, (caddr_t)p);
	/*
	 * Release virtual memory.  If we resulted from
	 * a vfork(), instead give the resources back to
	 * the parent.
	 */
	if ((p->p_flag & SVFORK) == 0)
		vrelvm();
	else {
		p->p_flag &= ~SVFORK;
		wakeup((caddr_t)p);
		while ((p->p_flag & SVFDONE) == 0)
			sleep((caddr_t)p, PZERO - 1);
		p->p_flag &= ~SVFDONE;
	}
	for (i = 0; i < NOFILE; i++) {
		struct file *f;

		f = u.u_ofile[i];
		u.u_ofile[i] = NULL;
		u.u_pofile[i] = 0;
		closef(f);
	}
	ilock(u.u_cdir);
	iput(u.u_cdir);
	if (u.u_rdir) {
		ilock(u.u_rdir);
		iput(u.u_rdir);
	}
	u.u_rlimit[RLIMIT_FSIZE].rlim_cur = RLIM_INFINITY;
	acct();
#ifdef QUOTA
	qclean();
#endif
	vrelpt(u.u_procp);
	vrelu(u.u_procp, 0);
	(void) spl5();		/* hack for mem alloc race XXX */
	if (*p->p_prev = p->p_nxt)		/* off allproc queue */
		p->p_nxt->p_prev = p->p_prev;
	if (p->p_nxt = zombproc)		/* onto zombproc */
		p->p_nxt->p_prev = &p->p_nxt;
	p->p_prev = &zombproc;
	zombproc = p;
	multprog--;
	p->p_stat = SZOMB;
	noproc = 1;
	i = PIDHASH(p->p_pid);
	x = p - proc;
	if (pidhash[i] == x)
		pidhash[i] = p->p_idhash;
	else {
		for (i = pidhash[i]; i != 0; i = proc[i].p_idhash)
			if (proc[i].p_idhash == x) {
				proc[i].p_idhash = p->p_idhash;
				goto done;
			}
		panic("exit");
	}
	if (p->p_pid == 1)
		panic("init died");
done:
	p->p_xstat = rv;
	if (m) {
		p->p_ru = mtod(m, struct rusage *);
		*p->p_ru = u.u_ru;
		ruadd(p->p_ru, &u.u_cru);
	} else
		log(KERN_ALERT, "exit: pid %d: no mbuf", p->p_pid);
	if (p->p_cptr)		/* only need this if any child is S_ZOMB */
		wakeup((caddr_t)&proc[1]);
	for (q = p->p_cptr; q != NULL; q = nq) {
		nq = q->p_osptr;
		if (nq != NULL)
			nq->p_ysptr = NULL;
		if (proc[1].p_cptr)
			proc[1].p_cptr->p_ysptr = q;
		q->p_osptr = proc[1].p_cptr;
		q->p_ysptr = NULL;
		proc[1].p_cptr = q;

		q->p_pptr = &proc[1];
		q->p_ppid = 1;
		/*
		 * Traced processes are killed
		 * since their existence means someone is screwing up.
		 * Stopped processes are sent a hangup and a continue.
		 * This is designed to be ``safe'' for setuid
		 * processes since they must be willing to tolerate
		 * hangups anyways.
		 */
		if (q->p_flag&STRC) {
			q->p_flag &= ~STRC;
			psignal(q, SIGKILL);
		} else if (q->p_stat == SSTOP) {
			psignal(q, SIGHUP);
			psignal(q, SIGCONT);
		}
		/*
		 * Protect this process from future
		 * tty signals, clear TSTP/TTIN/TTOU if pending.
		 */
		(void) spgrp(q, -1);
	}
	p->p_cptr = NULL;
	psignal(p->p_pptr, SIGCHLD);
	wakeup((caddr_t)p->p_pptr);
	swtch();
}

wait()
{
	struct rusage ru, *rup;

	if ((u.u_ar0[PS] & PSL_ALLCC) != PSL_ALLCC) {
		u.u_error = wait1(0, (struct rusage *)0);
		return;
	}
	rup = (struct rusage *)u.u_ar0[R1];
	u.u_error = wait1(u.u_ar0[R0], &ru);
	if (u.u_error)
		return;
	if (rup != (struct rusage *)0)
		u.u_error = copyout((caddr_t)&ru, (caddr_t)rup,
		    sizeof (struct rusage));
}

/*
 * Wait system call.
 * Search for a terminated (zombie) child,
 * finally lay it to rest, and collect its status.
 * Look also for stopped (traced) children,
 * and pass back status from them.
 */
wait1(options, ru)
	register int options;
	struct rusage *ru;
{
	register f;
	register struct proc *p, *q;

	f = 0;
loop:
	q = u.u_procp;
	for (p = q->p_cptr; p; p = p->p_osptr) {
		f++;
		if (p->p_stat == SZOMB) {
			u.u_r.r_val1 = p->p_pid;
			u.u_r.r_val2 = p->p_xstat;
			p->p_xstat = 0;
			if (ru && p->p_ru)
				*ru = *p->p_ru;
			if (p->p_ru) {
				ruadd(&u.u_cru, p->p_ru);
				(void) m_free(dtom(p->p_ru));
				p->p_ru = 0;
			}
			p->p_stat = NULL;
			p->p_pid = 0;
			p->p_ppid = 0;
			if (*p->p_prev = p->p_nxt)	/* off zombproc */
				p->p_nxt->p_prev = p->p_prev;
			p->p_nxt = freeproc;		/* onto freeproc */
			freeproc = p;
			if (q = p->p_ysptr)
				q->p_osptr = p->p_osptr;
			if (q = p->p_osptr)
				q->p_ysptr = p->p_ysptr;
			if ((q = p->p_pptr)->p_cptr == p)
				q->p_cptr = p->p_osptr;
			p->p_pptr = 0;
			p->p_ysptr = 0;
			p->p_osptr = 0;
			p->p_cptr = 0;
			p->p_sig = 0;
			p->p_sigcatch = 0;
			p->p_sigignore = 0;
			p->p_sigmask = 0;
			p->p_pgrp = 0;
			p->p_flag = 0;
			p->p_wchan = 0;
			p->p_cursig = 0;
			return (0);
		}
		if (p->p_stat == SSTOP && (p->p_flag&SWTED)==0 &&
		    (p->p_flag&STRC || options&WUNTRACED)) {
			p->p_flag |= SWTED;
			u.u_r.r_val1 = p->p_pid;
			u.u_r.r_val2 = (p->p_cursig<<8) | WSTOPPED;
			return (0);
		}
	}
	if (f == 0)
		return (ECHILD);
	if (options&WNOHANG) {
		u.u_r.r_val1 = 0;
		return (0);
	}
	if (setjmp(&u.u_qsave)) {
		p = u.u_procp;
		if ((u.u_sigintr & sigmask(p->p_cursig)) != 0)
			return(EINTR);
		u.u_eosys = RESTARTSYS;
		return (0);
	}
	sleep((caddr_t)u.u_procp, PWAIT);
	goto loop;
}
