#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/proc.h"

quit()
{
	if(u.u_arg[0]==0 || u.u_arg[0]==1)
		u.u_arg[0]--;
	u.u_signal[SIGQIT] = u.u_arg[0];
}

intr()
{
	if(u.u_arg[0]==0 || u.u_arg[0]==1)
		u.u_arg[0]--;
	u.u_signal[SIGINT] = u.u_arg[0];
}

cemt()
{
	u.u_signal[SIGEMT] = u.u_arg[0];
}

ilgins()
{
	u.u_signal[SIGINS] = u.u_arg[0];
}

fpe()
{
	u.u_signal[SIGFPT] = u.u_arg[0];
}

ssig()
{
	if(u.u_arg[0]<=0 || u.u_arg[0]>=NSIG) {
		u.u_error = EINVAL;
		return;
	}
	u.u_ar0[R0] = u.u_signal[u.u_arg[0]];
	u.u_signal[9] = 0;		/* kill not allowed */
	u.u_signal[u.u_arg[0]] = u.u_arg[1];
	if(u.u_procp->p_sig == u.u_arg[0])
		u.u_procp->p_sig = 0;
}

kill()
{
	int i;
	struct proc *p;

	p = &proc[0];
	for(i=0; i<NPROC; i++) {
		if(p->p_pid == u.u_ar0[R0])
			goto found;
		p++;
	}
	u.u_error = ESRCH;
	return;

found:
	if(p->p_ttyp != u.u_procp->p_ttyp)
		if(!suser())
			return;
		psignal(p, SIGKIL);
}

times()
{
	int i, *p;

	p = &u.u_utime;
	for(i=0; i<6; i++) {
		suword(u.u_arg[0], *p);
		p++;
		u.u_arg[0] =+ 2;
	}
}
