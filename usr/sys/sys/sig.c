#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/reg.h"
#include "../h/text.h"
#include "../h/seg.h"

/*
 * Priority for tracing
 */
#define	IPCPRI	PZERO

/*
 * Tracing variables.
 * Used to pass trace command from
 * parent to child being traced.
 * This data base cannot be
 * shared and is locked
 * per user.
 */
struct
{
	int	ip_lock;
	int	ip_req;
	int	*ip_addr;
	int	ip_data;
} ipc;

/*
 * Send the specified signal to
 * all processes with 'pgrp' as
 * process group.
 * Called by tty.c for quits and
 * interrupts.
 */
signal(pgrp, sig)
register pgrp;
{
	register struct proc *p;

	if(pgrp == 0)
		return;
	for(p = &proc[0]; p < &proc[NPROC]; p++)
		if(p->p_pgrp == pgrp)
			psignal(p, sig);
}

/*
 * Send the specified signal to
 * the specified process.
 */
psignal(p, sig)
register struct proc *p;
register sig;
{

	if((unsigned)sig >= NSIG)
		return;
	if(sig)
		p->p_sig |= 1<<(sig-1);
	if(p->p_pri > PUSER)
		p->p_pri = PUSER;
	if(p->p_stat == SSLEEP && p->p_pri > PZERO)
		setrun(p);
}

/*
 * Returns true if the current
 * process has a signal to process.
 * This is asked at least once
 * each time a process enters the
 * system.
 * A signal does not do anything
 * directly to a process; it sets
 * a flag that asks the process to
 * do something to itself.
 */
issig()
{
	register n;
	register struct proc *p;

	p = u.u_procp;
	while(p->p_sig) {
		n = fsig(p);
		if((u.u_signal[n]&1) == 0 || (p->p_flag&STRC))
			return(n);
		p->p_sig &= ~(1<<(n-1));
	}
	return(0);
}

/*
 * Enter the tracing STOP state.
 * In this state, the parent is
 * informed and the process is able to
 * receive commands from the parent.
 */
stop()
{
	register struct proc *pp, *cp;

loop:
	cp = u.u_procp;
	if(cp->p_ppid != 1)
	for (pp = &proc[0]; pp < &proc[NPROC]; pp++)
		if (pp->p_pid == cp->p_ppid) {
			wakeup((caddr_t)pp);
			cp->p_stat = SSTOP;
			swtch();
			if ((cp->p_flag&STRC)==0 || procxmt())
				return;
			goto loop;
		}
	exit(fsig(u.u_procp));
}

/*
 * Perform the action specified by
 * the current signal.
 * The usual sequence is:
 *	if(issig())
 *		psig();
 */
psig()
{
	register n, p;
	register struct proc *rp;

	rp = u.u_procp;
	if (u.u_fpsaved==0) {
		savfp(&u.u_fps);
		u.u_fpsaved = 1;
	}
	if (rp->p_flag&STRC)
		stop();
	n = fsig(rp);
	if (n==0)
		return;
	rp->p_sig &= ~(1<<(n-1));
	if((p=u.u_signal[n]) != 0) {
		u.u_error = 0;
		if(n != SIGINS && n != SIGTRC)
			u.u_signal[n] = 0;
		sendsig((caddr_t)p, n);
		return;
	}
	switch(n) {

	case SIGQUIT:
	case SIGINS:
	case SIGTRC:
	case SIGIOT:
	case SIGEMT:
	case SIGFPT:
	case SIGBUS:
	case SIGSEG:
	case SIGSYS:
		if(core())
			n += 0200;
	}
	exit(n);
}

/*
 * find the signal in bit-position
 * representation in p_sig.
 */
fsig(p)
struct proc *p;
{
	register n, i;

	n = p->p_sig;
	for(i=1; i<NSIG; i++) {
		if(n & 1)
			return(i);
		n >>= 1;
	}
	return(0);
}

/*
 * Create a core image on the file "core"
 * If you are looking for protection glitches,
 * there are probably a wealth of them here
 * when this occurs to a suid command.
 *
 * It writes USIZE block of the
 * user.h area followed by the entire
 * data+stack segments.
 */
core()
{
	register struct inode *ip;
	register unsigned s;
	extern schar();

	u.u_error = 0;
	u.u_dirp = "core";
	ip = namei(schar, 1);
	if(ip == NULL) {
		if(u.u_error)
			return(0);
		ip = maknode(0666);
		if (ip==NULL)
			return(0);
	}
	if(!access(ip, IWRITE) &&
	   (ip->i_mode&IFMT) == IFREG &&
	   u.u_uid == u.u_ruid) {
		itrunc(ip);
		u.u_offset = 0;
		u.u_base = (caddr_t)&u;
		u.u_count = ctob(USIZE);
		u.u_segflg = 1;
		writei(ip);
		s = u.u_procp->p_size - USIZE;
		estabur((unsigned)0, s, (unsigned)0, 0, RO);
		u.u_base = 0;
		u.u_count = ctob(s);
		u.u_segflg = 0;
		writei(ip);
	}
	iput(ip);
	return(u.u_error==0);
}

/*
 * grow the stack to include the SP
 * true return if successful.
 */

grow(sp)
unsigned sp;
{
	register si, i;
	register struct proc *p;
	register a;

	if(sp >= -ctob(u.u_ssize))
		return(0);
	si = (-sp)/64 - u.u_ssize + SINCR;
	if(si <= 0)
		return(0);
	if(estabur(u.u_tsize, u.u_dsize, u.u_ssize+si, u.u_sep, RO))
		return(0);
	p = u.u_procp;
	expand(p->p_size+si);
	a = p->p_addr + p->p_size;
	for(i=u.u_ssize; i; i--) {
		a--;
		copyseg(a-si, a);
	}
	for(i=si; i; i--)
		clearseg(--a);
	u.u_ssize += si;
	return(1);
}

/*
 * sys-trace system call.
 */
ptrace()
{
	register struct proc *p;
	register struct a {
		int	data;
		int	pid;
		int	*addr;
		int	req;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (uap->req <= 0) {
		u.u_procp->p_flag |= STRC;
		return;
	}
	for (p=proc; p < &proc[NPROC]; p++) 
		if (p->p_stat==SSTOP
		 && p->p_pid==uap->pid
		 && p->p_ppid==u.u_procp->p_pid)
			goto found;
	u.u_error = ESRCH;
	return;

    found:
	while (ipc.ip_lock)
		sleep((caddr_t)&ipc, IPCPRI);
	ipc.ip_lock = p->p_pid;
	ipc.ip_data = uap->data;
	ipc.ip_addr = uap->addr;
	ipc.ip_req = uap->req;
	p->p_flag &= ~SWTED;
	setrun(p);
	while (ipc.ip_req > 0)
		sleep((caddr_t)&ipc, IPCPRI);
	u.u_r.r_val1 = ipc.ip_data;
	if (ipc.ip_req < 0)
		u.u_error = EIO;
	ipc.ip_lock = 0;
	wakeup((caddr_t)&ipc);
}

/*
 * Code that the child process
 * executes to implement the command
 * of the parent process in tracing.
 */
procxmt()
{
	register int i;
	register *p;
	register struct text *xp;

	if (ipc.ip_lock != u.u_procp->p_pid)
		return(0);
	i = ipc.ip_req;
	ipc.ip_req = 0;
	wakeup((caddr_t)&ipc);
	switch (i) {

	/* read user I */
	case 1:
		if (fuibyte((caddr_t)ipc.ip_addr) == -1)
			goto error;
		ipc.ip_data = fuiword((caddr_t)ipc.ip_addr);
		break;

	/* read user D */
	case 2:
		if (fubyte((caddr_t)ipc.ip_addr) == -1)
			goto error;
		ipc.ip_data = fuword((caddr_t)ipc.ip_addr);
		break;

	/* read u */
	case 3:
		i = (int)ipc.ip_addr;
		if (i<0 || i >= ctob(USIZE))
			goto error;
		ipc.ip_data = ((physadr)&u)->r[i>>1];
		break;

	/* write user I */
	/* Must set up to allow writing */
	case 4:
		/*
		 * If text, must assure exclusive use
		 */
		if (xp = u.u_procp->p_textp) {
			if (xp->x_count!=1 || xp->x_iptr->i_mode&ISVTX)
				goto error;
			xp->x_iptr->i_flag &= ~ITEXT;
		}
		estabur(u.u_tsize, u.u_dsize, u.u_ssize, u.u_sep, RW);
		i = suiword((caddr_t)ipc.ip_addr, 0);
		suiword((caddr_t)ipc.ip_addr, ipc.ip_data);
		estabur(u.u_tsize, u.u_dsize, u.u_ssize, u.u_sep, RO);
		if (i<0)
			goto error;
		if (xp)
			xp->x_flag |= XWRIT;
		break;

	/* write user D */
	case 5:
		if (suword((caddr_t)ipc.ip_addr, 0) < 0)
			goto error;
		suword((caddr_t)ipc.ip_addr, ipc.ip_data);
		break;

	/* write u */
	case 6:
		i = (int)ipc.ip_addr;
		p = (int *)&((physadr)&u)->r[i>>1];
		if (p >= (int *)&u.u_fps && p < (int *)&u.u_fps.u_fpregs[6])
			goto ok;
		for (i=0; i<8; i++)
			if (p == &u.u_ar0[regloc[i]])
				goto ok;
		if (p == &u.u_ar0[RPS]) {
			ipc.ip_data |= 0170000;	/* assure user space */
			ipc.ip_data &= ~0340;	/* priority 0 */
			goto ok;
		}
		goto error;

	ok:
		*p = ipc.ip_data;
		break;

	/* set signal and continue */
	/*  one version causes a trace-trap */
	case 9:
		u.u_ar0[RPS] |= TBIT;
	case 7:
		if ((int)ipc.ip_addr != 1)
			u.u_ar0[PC] = (int)ipc.ip_addr;
		u.u_procp->p_sig = 0;
		if (ipc.ip_data)
			psignal(u.u_procp, ipc.ip_data);
		return(1);

	/* force exit */
	case 8:
		exit(fsig(u.u_procp));

	default:
	error:
		ipc.ip_req = -1;
	}
	return(0);
}
