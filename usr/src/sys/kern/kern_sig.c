/*	kern_sig.c	5.9	82/10/17	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/text.h"
#include "../h/seg.h"
#include "../h/pte.h"
#include "../h/psl.h"
#include "../h/vm.h"
#include "../h/acct.h"
#include "../h/uio.h"
#include "../h/kernel.h"

/* KILL CODE SHOULDNT KNOW ABOUT PROCESS INTERNALS !?! */

sigvec()
{

}

sigblock()
{

}

sigsetmask()
{

}

sigpause()
{

}

sigstack()
{

}

kill()
{

}

killpg()
{

}

/* BEGIN DEFUNCT */
okill()
{
	register struct proc *p;
	register a, sig;
	register struct a {
		int	pid;
		int	signo;
	} *uap;
	int f, priv;

	uap = (struct a *)u.u_ap;
	f = 0;
	a = uap->pid;
	priv = 0;
	sig = uap->signo;
	if (sig < 0)
		/*
		 * A negative signal means send to process group.
		 */
		uap->signo = -uap->signo;
	if (uap->signo == 0 || uap->signo > NSIG) {
		u.u_error = EINVAL;
		return;
	}
	if (a > 0 && sig > 0) {
		p = pfind(a);
		if (p == 0 || u.u_uid && u.u_uid != p->p_uid) {
			u.u_error = ESRCH;
			return;
		}
		psignal(p, uap->signo);
		return;
	}
	if (a==-1 && u.u_uid==0) {
		priv++;
		a = 0;
		sig = -1;		/* like sending to pgrp */
	} else if (a==0) {
		/*
		 * Zero process id means send to my process group.
		 */
		sig = -1;
		a = u.u_procp->p_pgrp;
		if (a == 0) {
			u.u_error = EINVAL;
			return;
		}
	}
	for(p = proc; p < procNPROC; p++) {
		if (p->p_stat == NULL)
			continue;
		if (sig > 0) {
			if (p->p_pid != a)
				continue;
		} else if (p->p_pgrp!=a && priv==0 || p->p_ppid==0 ||
		    (p->p_flag&SSYS) || (priv && p==u.u_procp))
			continue;
		if (u.u_uid != 0 && u.u_uid != p->p_uid &&
		    (uap->signo != SIGCONT || !inferior(p)))
			continue;
		f++;
		psignal(p, uap->signo);
	}
	if (f == 0)
		u.u_error = ESRCH;
}

ossig()
{
	register int (*f)();
	struct a {
		int	signo;
		int	(*fun)();
	} *uap;
	register struct proc *p = u.u_procp;
	register a;
	long sigmask;

	uap = (struct a *)u.u_ap;
	a = uap->signo & SIGNUMMASK;
	f = uap->fun;
	if (a<=0 || a>=NSIG || a==SIGKILL || a==SIGSTOP ||
	    a==SIGCONT && (f == SIG_IGN || f == SIG_HOLD)) {
		u.u_error = EINVAL;
		return;
	}
	if ((uap->signo &~ SIGNUMMASK) || (f != SIG_DFL && f != SIG_IGN &&
	    SIGISDEFER(f)))
		u.u_procp->p_flag |= SNUSIG;
	/* 
	 * Don't clobber registers if we are to simulate
	 * a ret+rti.
	 */
	if ((uap->signo&SIGDORTI) == 0)
		u.u_r.r_val1 = (int)u.u_signal[a];
	/*
	 * Change setting atomically.
	 */
	(void) spl6();
	sigmask = 1L << (a-1);
	if (u.u_signal[a] == SIG_IGN)
		p->p_sig &= ~sigmask;		/* never to be seen again */
	u.u_signal[a] = f;
	if (f != SIG_DFL && f != SIG_IGN && f != SIG_HOLD)
		f = SIG_CATCH;
	if ((int)f & 1)
		p->p_siga0 |= sigmask;
	else
		p->p_siga0 &= ~sigmask;
	if ((int)f & 2)
		p->p_siga1 |= sigmask;
	else
		p->p_siga1 &= ~sigmask;
	(void) spl0();
	/*
	 * Now handle options.
	 */
	if (uap->signo & SIGDOPAUSE) {
		/*
		 * Simulate a PDP11 style wait instrution which
		 * atomically lowers priority, enables interrupts
		 * and hangs.
		 */
		opause();
		/*NOTREACHED*/
	}
	if (uap->signo & SIGDORTI)
		u.u_eosys = SIMULATERTI;
}

/*
 * Send the specified signal to
 * all processes with 'pgrp' as
 * process group.
 * Called by tty.c for quits and
 * interrupts.
 */
gsignal(pgrp, sig)
	register int pgrp;
{
	register struct proc *p;

	if (pgrp == 0)
		return;
	for(p = proc; p < procNPROC; p++)
		if (p->p_pgrp == pgrp)
			psignal(p, sig);
}

/*
 * Send the specified signal to
 * the specified process.
 */
psignal(p, sig)
	register struct proc *p;
	register int sig;
{
	register int s;
	register int (*action)();
	long sigmask;

	if ((unsigned)sig >= NSIG)
		return;
	sigmask = (1L << (sig-1));

	/*
	 * If proc is traced, always give parent a chance.
	 * Otherwise get the signal action from the bits in the proc table.
	 */
	if (p->p_flag & STRC)
		action = SIG_DFL;
	else {
		s = (p->p_siga1&sigmask) != 0;
		s <<= 1;
		s |= (p->p_siga0&sigmask) != 0;
		action = (int(*)())s;
		/*
		 * If the signal is ignored, we forget about it immediately.
		 */
		if (action == SIG_IGN)
			return;
	}
#define mask(sig)	(1<<(sig-1))
#define	stops	(mask(SIGSTOP)|mask(SIGTSTP)|mask(SIGTTIN)|mask(SIGTTOU))
	if (sig) {
		p->p_sig |= sigmask;
		switch (sig) {

		case SIGTERM:
			if ((p->p_flag&STRC) != 0 || action != SIG_DFL)
				break;
			/* fall into ... */

		case SIGKILL:
			if (p->p_nice > NZERO)
				p->p_nice = NZERO;
			break;

		case SIGCONT:
			p->p_sig &= ~stops;
			break;

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			p->p_sig &= ~mask(SIGCONT);
			break;
		}
	}
#undef mask
#undef stops
	/*
	 * Defer further processing for signals which are held.
	 */
	if (action == SIG_HOLD)
		return;
	s = spl6();
	switch (p->p_stat) {

	case SSLEEP:
		/*
		 * If process is sleeping at negative priority
		 * we can't interrupt the sleep... the signal will
		 * be noticed when the process returns through
		 * trap() or syscall().
		 */
		if (p->p_pri <= PZERO)
			goto out;
		/*
		 * Process is sleeping and traced... make it runnable
		 * so it can discover the signal in issig() and stop
		 * for the parent.
		 */
		if (p->p_flag&STRC)
			goto run;
		switch (sig) {

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			/*
			 * These are the signals which by default
			 * stop a process.
			 */
			if (action != SIG_DFL)
				goto run;
			/*
			 * Don't clog system with children of init
			 * stopped from the keyboard.
			 */
			if (sig != SIGSTOP && p->p_pptr == &proc[1]) {
				psignal(p, SIGKILL);
				p->p_sig &= ~sigmask;
				splx(s);
				return;
			}
			/*
			 * If a child in vfork(), stopping could
			 * cause deadlock.
			 */
			if (p->p_flag&SVFORK)
				goto out;
			p->p_sig &= ~sigmask;
			p->p_cursig = sig;
			stop(p);
			goto out;

		case SIGIO:
		case SIGURG:
		case SIGCHLD:
			/*
			 * These signals are special in that they
			 * don't get propogated... if the process
			 * isn't interested, forget it.
			 */
			if (action != SIG_DFL)
				goto run;
			p->p_sig &= ~sigmask;		/* take it away */
			goto out;

		default:
			/*
			 * All other signals cause the process to run
			 */
			goto run;
		}
		/*NOTREACHED*/

	case SSTOP:
		/*
		 * If traced process is already stopped,
		 * then no further action is necessary.
		 */
		if (p->p_flag&STRC)
			goto out;
		switch (sig) {

		case SIGKILL:
			/*
			 * Kill signal always sets processes running.
			 */
			goto run;

		case SIGCONT:
			/*
			 * If the process catches SIGCONT, let it handle
			 * the signal itself.  If it isn't waiting on
			 * an event, then it goes back to run state.
			 * Otherwise, process goes back to sleep state.
			 */
			if (action != SIG_DFL || p->p_wchan == 0)
				goto run;
			p->p_stat = SSLEEP;
			goto out;

		case SIGSTOP:
		case SIGTSTP:
		case SIGTTIN:
		case SIGTTOU:
			/*
			 * Already stopped, don't need to stop again.
			 * (If we did the shell could get confused.)
			 */
			p->p_sig &= ~sigmask;		/* take it away */
			goto out;

		default:
			/*
			 * If process is sleeping interruptibly, then
			 * unstick it so that when it is continued
			 * it can look at the signal.
			 * But don't setrun the process as its not to
			 * be unstopped by the signal alone.
			 */
			if (p->p_wchan && p->p_pri > PZERO)
				unsleep(p);
			goto out;
		}
		/*NOTREACHED*/

	default:
		/*
		 * SRUN, SIDL, SZOMB do nothing with the signal,
		 * other than kicking ourselves if we are running.
		 * It will either never be noticed, or noticed very soon.
		 */
		if (p == u.u_procp && !noproc)
#include "../vax/mtpr.h"
			aston();
		goto out;
	}
	/*NOTREACHED*/
run:
	/*
	 * Raise priority to at least PUSER.
	 */
	if (p->p_pri > PUSER)
		if ((p != u.u_procp || noproc) && p->p_stat == SRUN &&
		    (p->p_flag & SLOAD)) {
			remrq(p);
			p->p_pri = PUSER;
			setrq(p);
		} else
			p->p_pri = PUSER;
	setrun(p);
out:
	splx(s);
}

/*
 * Returns true if the current
 * process has a signal to process.
 * The signal to process is put in p_cursig.
 * This is asked at least once each time a process enters the
 * system (though this can usually be done without actually
 * calling issig by checking the pending signal masks.)
 * A signal does not do anything
 * directly to a process; it sets
 * a flag that asks the process to
 * do something to itself.
 */
issig()
{
	register struct proc *p;
	register int sig;
	long sigbits;
	long sigmask;

	p = u.u_procp;
	for (;;) {
		sigbits = p->p_sig;
		if ((p->p_flag&STRC) == 0)
			sigbits &= ~p->p_ignsig;
		if (p->p_flag&SVFORK)
#define bit(a) (1<<(a-1))
			sigbits &= ~(bit(SIGSTOP)|bit(SIGTSTP)|bit(SIGTTIN)|bit(SIGTTOU));
		if (sigbits == 0)
			break;
		sig = ffs((int)sigbits);
		sigmask = 1L << (sig-1);
		p->p_sig &= ~sigmask;		/* take the signal! */
		p->p_cursig = sig;
		if (p->p_flag&STRC && (p->p_flag&SVFORK)==0) {
			/*
			 * If traced, always stop, and stay
			 * stopped until released by the parent.
			 */
			do {
				stop(p);
				swtch();
			} while (!procxmt() && p->p_flag&STRC);

			/*
			 * If the traced bit got turned off,
			 * then put the signal taken above back into p_sig
			 * and go back up to the top to rescan signals.
			 * This ensures that siga0 and u_signal are consistent.
			 */
			if ((p->p_flag&STRC) == 0) {
				p->p_sig |= sigmask;
				continue;
			}

			/*
			 * If parent wants us to take the signal,
			 * then it will leave it in p->p_cursig;
			 * otherwise we just look for signals again.
			 */
			sig = p->p_cursig;
			if (sig == 0)
				continue;
		}
		switch (u.u_signal[sig]) {

		case SIG_DFL:
			/*
			 * Don't take default actions on system processes.
			 */
			if (p->p_ppid == 0)
				break;
			switch (sig) {

			case SIGTSTP:
			case SIGTTIN:
			case SIGTTOU:
				/*
				 * Children of init aren't allowed to stop
				 * on signals from the keyboard.
				 */
				if (p->p_pptr == &proc[1]) {
					psignal(p, SIGKILL);
					continue;
				}
				/* fall into ... */

			case SIGSTOP:
				if (p->p_flag&STRC)
					continue;
				stop(p);
				swtch();
				continue;

			case SIGCONT:
			case SIGCHLD:
				/*
				 * These signals are normally not
				 * sent if the action is the default.
				 */
				continue;		/* == ignore */

			default:
				goto send;
			}
			/*NOTREACHED*/

		case SIG_HOLD:
		case SIG_IGN:
			/*
			 * Masking above should prevent us
			 * ever trying to take action on a held
			 * or ignored signal, unless process is traced.
			 */
			if ((p->p_flag&STRC) == 0)
				printf("issig\n");
			continue;

		default:
			/*
			 * This signal has an action, let
			 * psig process it.
			 */
			goto send;
		}
		/*NOTREACHED*/
	}
	/*
	 * Didn't find a signal to send.
	 */
	p->p_cursig = 0;
	return (0);

send:
	/*
	 * Let psig process the signal.
	 */
	return (sig);
}

/*
 * Put the argument process into the stopped
 * state and notify the parent via wakeup and/or signal.
 */
stop(p)
	register struct proc *p;
{

	p->p_stat = SSTOP;
	p->p_flag &= ~SWTED;
	wakeup((caddr_t)p->p_pptr);
	/*
	 * Avoid sending signal to parent if process is traced
	 */
	if (p->p_flag&STRC)
		return;
	psignal(p->p_pptr, SIGCHLD);
}

/*
 * Perform the action specified by
 * the current signal.
 * The usual sequence is:
 *	if (issig())
 *		psig();
 * The signal bit has already been cleared by issig,
 * and the current signal number stored in p->p_cursig.
 */
psig()
{
	register struct proc *rp = u.u_procp;
	register int n = rp->p_cursig;
	long sigmask = 1L << (n-1);
	register int (*action)();

	if (rp->p_cursig == 0)
		panic("psig");
	action = u.u_signal[n];
	if (action != SIG_DFL) {
		if (action == SIG_IGN || action == SIG_HOLD)
			panic("psig action");
		u.u_error = 0;
		if (n != SIGILL && n != SIGTRAP)
			u.u_signal[n] = 0;
		/*
		 * If this catch value indicates automatic holding of
		 * subsequent signals, set the hold value.
		 */
		if (SIGISDEFER(action)) {
			(void) spl6();
			if ((int)SIG_HOLD & 1)
				rp->p_siga0 |= sigmask;
			else
				rp->p_siga0 &= ~sigmask;
			if ((int)SIG_HOLD & 2)
				rp->p_siga1 |= sigmask;
			else
				rp->p_siga1 &= ~sigmask;
			u.u_signal[n] = SIG_HOLD;
			(void) spl0();
			action = SIGUNDEFER(action);
		}
		u.u_ru.ru_nsignals++;
		sendsig(action, n);
		rp->p_cursig = 0;
		return;
	}
	u.u_acflag |= AXSIG;
	switch (n) {

	case SIGILL:
	case SIGIOT:
	case SIGBUS:
	case SIGQUIT:
	case SIGTRAP:
	case SIGEMT:
	case SIGFPE:
	case SIGSEGV:
	case SIGSYS:
		u.u_arg[0] = n;
		if (core())
			n += 0200;
	}
	exit(n);
}

#ifdef unneeded
int	corestop = 0;
#endif
/*
 * Create a core image on the file "core"
 * If you are looking for protection glitches,
 * there are probably a wealth of them here
 * when this occurs to a suid command.
 *
 * It writes UPAGES block of the
 * user.h area followed by the entire
 * data+stack segments.
 */
core()
{
	register struct inode *ip;
	extern schar();

#ifdef unneeded
	if (corestop) {
		int i;
		for (i = 0; i < 10; i++)
			if (u.u_comm[i])
				putchar(u.u_comm[i], 0);
		printf(", uid %d\n", u.u_uid);
		if (corestop&2)
			asm("halt");
	}
#endif
	if (u.u_uid != u.u_ruid)
		return (0);
	if (ctob(UPAGES+u.u_dsize+u.u_ssize) >=
	    u.u_rlimit[RLIMIT_CORE].rlim_cur)
		return (0);
	u.u_error = 0;
	u.u_dirp = "core";
	ip = namei(schar, 1, 1);
	if (ip == NULL) {
		if (u.u_error)
			return (0);
		ip = maknode(0666);
		if (ip==NULL)
			return (0);
	}
	if (access(ip, IWRITE) ||
	   (ip->i_mode&IFMT) != IFREG ||
	   ip->i_nlink != 1) {
		u.u_error = EFAULT;
		goto out;
	}
	itrunc(ip, 0);
	u.u_acflag |= ACORE;
	u.u_error = rdwri(UIO_WRITE, ip,
	    (caddr_t)&u, ctob(UPAGES),
	    0, 1, (int *)0);
	if (u.u_error == 0)
		u.u_error = rdwri(UIO_WRITE, ip,
		    (caddr_t)ctob(u.u_tsize), ctob(u.u_dsize),
		    ctob(UPAGES), 0, (int *)0);
	if (u.u_error == 0)
		u.u_error = rdwri(UIO_WRITE, ip,
		    (caddr_t)(USRSTACK-ctob(u.u_ssize)), ctob(u.u_ssize),
		    ctob(UPAGES)+ctob(u.u_dsize), 0, (int *)0);
out:
	iput(ip);
	return (u.u_error == 0);
}
