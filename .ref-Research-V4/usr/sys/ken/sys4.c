#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../user.h"
#include "../reg.h"
#include "../proc.h"

ssig()
{
	register a;

	a = u.u_arg[0];
	if(a<=0 || a>=NSIG) {
		u.u_error = EINVAL;
		return;
	}
	u.u_ar0[R0] = u.u_signal[a];
	u.u_signal[a] = u.u_arg[1];
	u.u_signal[9] = 0;		/* kill not allowed */
	if(u.u_procp->p_sig == a)
		u.u_procp->p_sig = 0;
}

kill()
{
	register struct proc *p;

	for(p = &proc[0]; p < &proc[NPROC]; p++)
		if(p->p_pid == u.u_ar0[R0])
			goto found;
	u.u_error = ESRCH;
	return;

found:
	if(p->p_ttyp != u.u_procp->p_ttyp)
		if(!suser())
			return;
		psignal(p, u.u_arg[0]);
}

times()
{
	register *p;

	for(p = &u.u_utime; p  < &u.u_utime+6;) {
		suword(u.u_arg[0], *p++);
		u.u_arg[0] =+ 2;
	}
}

profil()
{

	u.u_prof[0] = u.u_arg[0] & ~1;	/* base of sample buf */
	u.u_prof[1] = u.u_arg[1];	/* size of same */
	u.u_prof[2] = u.u_arg[2];	/* pc offset */
	u.u_prof[3] = (u.u_arg[3]>>1) & 077777; /* pc scale */
}
