#
/*
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../proc.h"
#include "../inode.h"
#include "../reg.h"

/*
 * Priority for tracing
 */
#define	IPCPRI	(-1)

/*
 * Structure to access an array of integers.
 */
struct
{
	int	inta[];
};

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
	int	ip_addr;
	int	ip_data;
} ipc;

/*
 * Send the specified signal to
 * all processes with 'tp' as its
 * controlling teletype.
 * Called by tty.c for quits and
 * interrupts.
 */
signal(tp, sig)
{
	register struct proc *p;

	for(p = &proc[0]; p < &proc[NPROC]; p++)
		if(p->p_ttyp == tp)
			psignal(p, sig);
}

/*
 * Send the specified signal to
 * the specified process.
 */
psignal(p, sig)
int *p;
{
	register *rp;

	if(sig >= NSIG)
		return;
	rp = p;
	if(rp->p_sig != SIGKIL)
		rp->p_sig = sig;
	if(rp->p_stat > PUSER)
		rp->p_stat = PUSER;
	if(rp->p_stat == SWAIT)
		setrun(rp);
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
	if(n = p->p_sig) {
		if (p->p_flag&STRC) {
			stop();
			if ((n = p->p_sig) == 0)
				return(0);
		}
		if((u.u_signal[n]&1) == 0)
			return(n);
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
			wakeup(pp);
			cp->p_stat = SSTOP;
			swtch();
			if ((cp->p_flag&STRC)==0 || procxmt())
				return;
			goto loop;
		}
	exit();
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
	register *rp;

	rp = u.u_procp;
	n = rp->p_sig;
	rp->p_sig = 0;
	if((p=u.u_signal[n]) != 0) {
		u.u_error = 0;
		if(n != SIGINS && n != SIGTRC)
			u.u_signal[n] = 0;
		n = u.u_ar0[R6] - 4;
		grow(n);
		suword(n+2, u.u_ar0[RPS]);
		suword(n, u.u_ar0[R7]);
		u.u_ar0[R6] = n;
		u.u_ar0[RPS] =& ~TBIT;
		u.u_ar0[R7] = p;
		return;
	}
	switch(n) {

	case SIGQIT:
	case SIGINS:
	case SIGTRC:
	case SIGIOT:
	case SIGEMT:
	case SIGFPT:
	case SIGBUS:
	case SIGSEG:
	case SIGSYS:
		u.u_arg[0] = n;
		if(core())
			n =+ 0200;
	}
	u.u_arg[0] = (u.u_ar0[R0]<<8) | n;
	exit();
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
	register s, *ip;
	extern schar;

	u.u_error = 0;
	u.u_dirp = "core";
	ip = namei(&schar, 1);
	if(ip == NULL) {
		if(u.u_error)
			return(0);
		ip = maknode(0666);
		if(ip == NULL)
			return(0);
	}
	if(!access(ip, IWRITE) &&
	   (ip->i_mode&IFMT) == 0 &&
	   u.u_uid == u.u_ruid) {
		itrunc(ip);
		u.u_offset[0] = 0;
		u.u_offset[1] = 0;
		u.u_base = &u;
		u.u_count = USIZE*64;
		u.u_segflg = 1;
		writei(ip);
		s = u.u_procp->p_size - USIZE;
		estabur(0, s, 0, 0);
		u.u_base = 0;
		u.u_count = s*64;
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
char *sp;
{
	register a, si, i;

	if(sp >= -u.u_ssize*64)
		return(0);
	si = ldiv(-sp, 64) - u.u_ssize + SINCR;
	if(si <= 0)
		return(0);
	if(estabur(u.u_tsize, u.u_dsize, u.u_ssize+si, u.u_sep))
		return(0);
	expand(u.u_procp->p_size+si);
	a = u.u_procp->p_addr + u.u_procp->p_size;
	for(i=u.u_ssize; i; i--) {
		a--;
		copyseg(a-si, a);
	}
	for(i=si; i; i--)
		clearseg(--a);
	u.u_ssize =+ si;
	return(1);
}

/*
 * sys-trace system call.
 */
ptrace()
{
	register struct proc *p;

	if (u.u_arg[2] <= 0) {
		u.u_procp->p_flag =| STRC;
		return;
	}
	for (p=proc; p < &proc[NPROC]; p++) 
		if (p->p_stat==SSTOP
		 && p->p_pid==u.u_arg[0]
		 && p->p_ppid==u.u_procp->p_pid)
			goto found;
	u.u_error = ESRCH;
	return;

    found:
	while (ipc.ip_lock)
		sleep(&ipc, IPCPRI);
	ipc.ip_lock = p->p_pid;
	ipc.ip_data = u.u_ar0[R0];
	ipc.ip_addr = u.u_arg[1] & ~01;
	ipc.ip_req = u.u_arg[2];
	p->p_flag =& ~SWTED;
	setrun(p);
	while (ipc.ip_req > 0)
		sleep(&ipc, IPCPRI);
	u.u_ar0[R0] = ipc.ip_data;
	if (ipc.ip_req < 0)
		u.u_error = EIO;
	ipc.ip_lock = 0;
	wakeup(&ipc);
}

/*
 * Code that the child process
 * executes to implement the command
 * of the parent process in tracing.
 */
procxmt()
{
	register int i;
	register int *p;

	if (ipc.ip_lock != u.u_procp->p_pid)
		return(0);
	i = ipc.ip_req;
	ipc.ip_req = 0;
	wakeup(&ipc);
	switch (i) {

	/* read user I */
	case 1:
		if (fuibyte(ipc.ip_addr) == -1)
			goto error;
		ipc.ip_data = fuiword(ipc.ip_addr);
		break;

	/* read user D */
	case 2:
		if (fubyte(ipc.ip_addr) == -1)
			goto error;
		ipc.ip_data = fuword(ipc.ip_addr);
		break;

	/* read u */
	case 3:
		i = ipc.ip_addr;
		if (i<0 || i >= (USIZE<<6))
			goto error;
		ipc.ip_data = u.inta[i>>1];
		break;

	/* write user I (for now, always an error) */
	case 4:
		if (suiword(ipc.ip_addr, 0) < 0)
			goto error;
		suiword(ipc.ip_addr, ipc.ip_data);
		break;

	/* write user D */
	case 5:
		if (suword(ipc.ip_addr, 0) < 0)
			goto error;
		suword(ipc.ip_addr, ipc.ip_data);
		break;

	/* write u */
	case 6:
		p = &u.inta[ipc.ip_addr>>1];
		if (p >= u.u_fsav && p < &u.u_fsav[25])
			goto ok;
		for (i=0; i<9; i++)
			if (p == &u.u_ar0[regloc[i]])
				goto ok;
		goto error;
	ok:
		if (p == &u.u_ar0[RPS]) {
			ipc.ip_data =| 0170000;	/* assure user space */
			ipc.ip_data =& ~0340;	/* priority 0 */
		}
		*p = ipc.ip_data;
		break;

	/* set signal and continue */
	case 7:
		u.u_procp->p_sig = ipc.ip_data;
		return(1);

	/* force exit */
	case 8:
		exit();

	default:
	error:
		ipc.ip_req = -1;
	}
	return(0);
}
