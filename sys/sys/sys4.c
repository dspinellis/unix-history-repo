/*	sys4.c	4.10	81/07/22	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/clock.h"
#include "../h/mtpr.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"

/*
 * Everything in this file is a routine implementing a system call.
 */

/*
 * return the current time (old-style entry)
 */
gtime()
{
	u.u_r.r_time = time;
	if (clkwrap())
		clkset();
}

/*
 * New time entry-- return TOD with milliseconds, timezone,
 * DST flag
 */
ftime()
{
	register struct a {
		struct	timeb	*tp;
	} *uap;
	struct timeb t;
	register unsigned ms;

	uap = (struct a *)u.u_ap;
	(void) spl7();
	t.time = time;
	ms = lbolt;
	(void) spl0();
	if (ms > hz) {
		ms -= hz;
		t.time++;
	}
	t.millitm = (1000*ms)/hz;
	t.timezone = timezone;
	t.dstflag = dstflag;
	if (copyout((caddr_t)&t, (caddr_t)uap->tp, sizeof(t)) < 0)
		u.u_error = EFAULT;
	if (clkwrap())
		clkset();
}

/*
 * Set the time
 */
stime()
{
	register struct a {
		time_t	time;
	} *uap;

	uap = (struct a *)u.u_ap;
	if(suser()) {
		bootime += uap->time - time;
		time = uap->time;
		clkset();
	}
}

setuid()
{
	register uid;
	register struct a {
		int	uid;
	} *uap;

	uap = (struct a *)u.u_ap;
	uid = uap->uid;
	if(u.u_ruid == uid || u.u_uid == uid || suser()) {
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_ruid = uid;
	}
}

getuid()
{

	u.u_r.r_val1 = u.u_ruid;
	u.u_r.r_val2 = u.u_uid;
}

setgid()
{
	register gid;
	register struct a {
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	gid = uap->gid;
	if(u.u_rgid == gid || u.u_gid == gid || suser()) {
		u.u_gid = gid;
		u.u_rgid = gid;
	}
}

getgid()
{

	u.u_r.r_val1 = u.u_rgid;
	u.u_r.r_val2 = u.u_gid;
}

getpid()
{
	u.u_r.r_val1 = u.u_procp->p_pid;
	u.u_r.r_val2 = u.u_procp->p_ppid;
}

sync()
{

	update();
}

nice()
{
	register n;
	register struct a {
		int	niceness;
	} *uap;

	uap = (struct a *)u.u_ap;
	n = uap->niceness + u.u_procp->p_nice;
	if(n >= 2*NZERO)
		n = 2*NZERO -1;
	if(n < 0)
		n = 0;
	if (n < u.u_procp->p_nice && !suser())
		return;
	u.u_procp->p_nice = n;
	(void) setpri(u.u_procp);
	/* IF RUNNING MAY HAVE WRONG p->p_pri, BUT WILL FIX ITSELF QUICKLY */
}

/*
 * Unlink system call.
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
unlink()
{
	register struct inode *ip, *pp;
	struct a {
		char	*fname;
	};

	pp = namei(uchar, 2);
	if(pp == NULL)
		return;
	/*
	 * Check for unlink(".")
	 * to avoid hanging on the iget
	 */
	if (pp->i_number == u.u_dent.d_ino) {
		ip = pp;
		ip->i_count++;
	} else
		ip = iget(pp->i_dev, u.u_dent.d_ino);
	if(ip == NULL)
		goto out1;
	if((ip->i_mode&IFMT)==IFDIR && !suser())
		goto out;
	/*
	 * Don't unlink a mounted file.
	 */
	if (ip->i_dev != pp->i_dev) {
		u.u_error = EBUSY;
		goto out;
	}
	if (ip->i_flag&ITEXT)
		xrele(ip);	/* try once to free text */
/*
	if ((ip->i_flag&ITEXT) && ip->i_nlink==1) {
 		u.u_error = ETXTBSY;
		goto out;
	}
*/
	u.u_offset -= sizeof(struct direct);
	u.u_base = (caddr_t)&u.u_dent;
	u.u_count = sizeof(struct direct);
	u.u_dent.d_ino = 0;
	writei(pp);
	ip->i_nlink--;
	ip->i_flag |= ICHG;

out:
	iput(ip);
out1:
	iput(pp);
}
chdir()
{
	chdirec(&u.u_cdir);
}

chroot()
{
	if (suser())
		chdirec(&u.u_rdir);
}

chdirec(ipp)
register struct inode **ipp;
{
	register struct inode *ip;
	struct a {
		char	*fname;
	};

	ip = namei(uchar, 0);
	if(ip == NULL)
		return;
	if((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto bad;
	}
	if(access(ip, IEXEC))
		goto bad;
	prele(ip);
	if (*ipp) {
		plock(*ipp);
		iput(*ipp);
	}
	*ipp = ip;
	return;

bad:
	iput(ip);
}

chmod()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	fmode;
	} *uap;

	uap = (struct a *)u.u_ap;
	if ((ip = owner()) == NULL)
		return;
	ip->i_mode &= ~07777;
	if (u.u_uid)
		uap->fmode &= ~ISVTX;
	ip->i_mode |= uap->fmode&07777;
	ip->i_flag |= ICHG;
	if (ip->i_flag&ITEXT && (ip->i_mode&ISVTX)==0)
		xrele(ip);
	iput(ip);
}

chown()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	uid;
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (!suser() || (ip = owner()) == NULL)
		return;
	ip->i_uid = uap->uid;
	ip->i_gid = uap->gid;
	ip->i_flag |= ICHG;
#ifndef INSECURE
	ip->i_mode &= ~(ISUID|ISGID);
#endif
	iput(ip);
}

ssig()
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
	if(a<=0 || a>=NSIG || a==SIGKILL || a==SIGSTOP ||
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
		pause();
		/*NOTREACHED*/
	}
	if (uap->signo & SIGDORTI)
		u.u_eosys = SIMULATERTI;
}

kill()
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
	} else if(a==0) {
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
		if(p->p_stat == NULL)
			continue;
		if(sig > 0) {
			if (p->p_pid != a)
				continue;
		} else if (p->p_pgrp!=a && priv==0 || p<=&proc[2] ||
		    (p->p_flag&SSYS) || (priv && p==u.u_procp))
			continue;
		if(u.u_uid != 0 && u.u_uid != p->p_uid &&
		    (uap->signo != SIGCONT || !inferior(p)))
			continue;
		f++;
		psignal(p, uap->signo);
	}
	if(f == 0)
		u.u_error = ESRCH;
}

times()
{
	register struct a {
		time_t	(*times)[4];
	} *uap;
	struct tms tms;

	tms.tms_utime = u.u_vm.vm_utime;
	tms.tms_stime = u.u_vm.vm_stime;
	tms.tms_cutime = u.u_cvm.vm_utime;
	tms.tms_cstime = u.u_cvm.vm_stime;
	uap = (struct a *)u.u_ap;
	if (copyout((caddr_t)&tms, (caddr_t)uap->times, sizeof(struct tms)) < 0)
		u.u_error = EFAULT;
}

profil()
{
	register struct a {
		short	*bufbase;
		unsigned bufsize;
		unsigned pcoffset;
		unsigned pcscale;
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_prof.pr_base = uap->bufbase;
	u.u_prof.pr_size = uap->bufsize;
	u.u_prof.pr_off = uap->pcoffset;
	u.u_prof.pr_scale = uap->pcscale;
}

/*
 * alarm clock signal
 */
alarm()
{
	register struct proc *p;
	register c;
	register struct a {
		int	deltat;
	} *uap;

	uap = (struct a *)u.u_ap;
	p = u.u_procp;
	c = p->p_clktim;
	p->p_clktim = uap->deltat;
	u.u_r.r_val1 = c;
}

/*
 * indefinite wait.
 * no one should wakeup(&u)
 */
pause()
{

	for(;;)
		sleep((caddr_t)&u, PSLEP);
}

/*
 * mode mask for creation of files
 */
umask()
{
	register struct a {
		int	mask;
	} *uap;
	register t;

	uap = (struct a *)u.u_ap;
	t = u.u_cmask;
	u.u_cmask = uap->mask & 0777;
	u.u_r.r_val1 = t;
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
utime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap;
	register struct inode *ip;
	time_t tv[2];

	uap = (struct a *)u.u_ap;
	if ((ip = owner()) == NULL)
		return;
	if (copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof(tv))) {
		u.u_error = EFAULT;
	} else {
		ip->i_flag |= IACC|IUPD|ICHG;
		iupdat(ip, &tv[0], &tv[1], 0);
	}
	iput(ip);
}

/*
 * Setpgrp on specified process and its descendants.
 * Pid of zero implies current process.
 * Pgrp -1 is getpgrp system call returning
 * current process group.
 */
setpgrp()
{
	register struct proc *top;
	register struct a {
		int	pid;
		int	pgrp;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (uap->pid == 0)
		top = u.u_procp;
	else {
		top = pfind(uap->pid);
		if (top == 0) {
			u.u_error = ESRCH;
			return;
		}
	}
	if (uap->pgrp <= 0) {
		u.u_r.r_val1 = top->p_pgrp;
		return;
	}
#ifdef notdef
	u.u_r.r_val1 = spgrp(top, uap->pgrp);
	if (u.u_r.r_val1 == 0)
		u.u_error = EPERM;
#else
	if (top->p_uid != u.u_uid && u.u_uid && !inferior(top))
		u.u_error = EPERM;
	else
		top->p_pgrp = uap->pgrp;
#endif
}

spgrp(top, npgrp)
register struct proc *top;
{
	register struct proc *pp, *p;
	int f = 0;

	for (p = top; npgrp == -1 || u.u_uid == p->p_uid ||
	    !u.u_uid || inferior(p); p = pp) {
		if (npgrp == -1) {
#define	bit(a)	(1<<(a-1))
			p->p_sig &= ~(bit(SIGTSTP)|bit(SIGTTIN)|bit(SIGTTOU));
			p->p_flag |= SDETACH;
		} else
			p->p_pgrp = npgrp;
		f++;
		/*
		 * Search for children.
		 */
		for (pp = proc; pp < procNPROC; pp++)
			if (pp->p_pptr == p)
				goto cont;
		/*
		 * Search for siblings.
		 */
		for (; p != top; p = p->p_pptr)
			for (pp = p + 1; pp < procNPROC; pp++)
				if (pp->p_pptr == p->p_pptr)
					goto cont;
		break;
	cont:
		;
	}
	return (f);
}

/*
 * Is p an inferior of the current process?
 */
inferior(p)
register struct proc *p;
{

	for (; p != u.u_procp; p = p->p_pptr)
		if (p <= &proc[2])
			return (0);
	return (1);
}

reboot()
{
	register struct a {
		int	opt;
	};

	if (suser())
		boot(RB_BOOT, ((struct a *)u.u_ap)->opt);
}

/*
 * lock user into core as much
 * as possible. swapping may still
 * occur if core grows.
 */
syslock()
{
	register struct proc *p;
	register struct a {
		int	flag;
	} *uap;

	uap = (struct a *)u.u_ap;
	if(suser()) {
		p = u.u_procp;
		p->p_flag &= ~SULOCK;
		if(uap->flag)
			p->p_flag |= SULOCK;
	}
}
