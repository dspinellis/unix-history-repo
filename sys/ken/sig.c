#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/reg.h"

signal(tp, sig)
{
	int i;
	struct proc *p;

	p = &proc[0];
	for(i=0; i<NPROC; i++) {
		if(p->p_ttyp==tp)
			psignal(p, sig);
		p++;
	}
}

psignal(p, sig)
int *p;
{
	register *rp;

	rp = p;
	rp->p_sig = sig;
	if(rp->p_stat == SWAIT) {
		rp->p_wchan = 0;
		rp->p_stat = SRUN;
		if(runout) {
			runout = 0;
			wakeup(&runout);
		}
	}
}

issig()
{
	register n;

	if(n = u.u_procp->p_sig)
		if((u.u_signal[n]&1) == 0)
			return(n);
	return(0);
}

psig()
{
	int n, p;
	register *rp;

	rp = u.u_procp;
	n = rp->p_sig;
	rp->p_sig = 0;
	if((p=u.u_signal[n]) != 0) {
		u.u_error = 0;
		u.u_signal[n] = 0;
		suword(u.u_ar0[R6]=-2, u.u_ar0[RPS]);
		suword(u.u_ar0[R6]=-2, u.u_ar0[R7]);
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
		if(core())
			n =+ 0200;
	}
	u.u_arg[0] = (u.u_ar0[R0]<<8) | n;
	exit();
}

core()
{
	int s, *ip;
	extern schar;

	u.u_error = 0;
	u.u_dirp = "core";
	ip = namei(&schar, 1);
	if(ip == NULL) {
		if(u.u_error)
			return(0);
		ip = maknode(0666);
	}
	if(!access(ip, IWRITE)) {
		itrunc(ip);
		u.u_offset[0] = 0;
		u.u_offset[1] = 0;
		u.u_base = &u;
		u.u_count = USIZE*64;
		u.u_segflg = 1;
		writei(ip);
		s = u.u_procp->p_size - USIZE;
		estabur(0, s, 0);
		u.u_base = 0;
		u.u_count = s*64;
		u.u_segflg = 0;
		writei(ip);
	}
	iput(ip);
	return(u.u_error==0);
}
