#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/reg.h"
#include "../h/seg.h"
#include "../h/trap.h"
#include "../h/psl.h"

#define	USER	040		/* user-mode flag added to type */

struct	sysent	sysent[64];

/*
 * Called from the trap handler when a processor trap occurs.
 */
trap(params, r0, r1, r2, r3, r4, r5 ,r6, r7, r8, r9, r10,
	r11, r12, r13, sp, type, code, pc, psl)
caddr_t params;
{
	register i, a;
	register struct sysent *callp;
	register caddr_t errorp;
	register time_t syst;
	register int *locr0;

	locr0 = &r0;
	syst = u.u_stime;
	u.u_ar0 = locr0;
	if (USERMODE(locr0[PS]))
		type |= USER;
	u.u_ar0 = locr0;
	switch (type) {

	/*
	 * Trap not expected.
	 * Usually a kernel mode bus error.
	 */
	default:
		printf("user = ");
		for(i=0; i<UPAGES; i++)
			printf("%x ", u.u_procp->p_addr[i]);
		printf("\n");
		printf("ps = %x\n", locr0[PS]);
		printf("pc = %x\n", locr0[PC]);
		printf("trap type %x\n", type);
		printf("code = %x\n", code);
		panic("trap");

	case PROTFLT + USER:	/* protection fault */
		i = SIGBUS;
		break;

	case PRIVINFLT + USER:	/* privileged instruction fault */
	case RESADFLT + USER:	/* reserved addressing fault */
	case RESOPFLT + USER:	/* resereved operand fault */
		i = SIGINS;
		break;

	case RESCHED + USER:	/* Allow process switch */
		goto out;

	case SYSCALL + USER: /* sys call */
		params += NBPW;		/* skip word with param count */
		u.u_error = 0;
		callp = &sysent[code&077];
		if (callp == sysent) { /* indirect */
			a = fuword(params);
			params += NBPW;
			callp = &sysent[a&077];
		}
		for(i=0; i<callp->sy_narg; i++) {
			u.u_arg[i] = fuword(params);
			params += NBPW;
		}
		u.u_ap = u.u_arg;
		locr0[PS] &= ~PSL_C;
		u.u_dirp = (caddr_t)u.u_arg[0];
		u.u_r.r_val1 = 0;
		u.u_r.r_val2 = locr0[R1];
		if(save(u.u_qsav)){
			if(u.u_error==0)
				u.u_error = EINTR;
		} else {
			(*(callp->sy_call))();
		}
		if(u.u_error) {
			locr0[R0] = u.u_error;
			locr0[PS] |= PSL_C;	/* carry bit */
		 } else {
			locr0[R0] = u.u_r.r_val1;
			locr0[R1] = u.u_r.r_val2;
		}
		goto out;

	case ARITHTRAP + USER:
		i = SIGFPT;
		break;

	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case SEGFLT + USER: /* segmentation exception */
		if(grow(locr0[SP]) || grow(code))
			goto out;
		i = SIGSEG;
		break;

	case BPTFLT + USER:	/* bpt instruction fault */
	case TRCTRAP + USER:	/* trace trap */
		locr0[PS] &= ~PSL_T;	/* turn off trace bit */
		i = SIGTRC;
		break;

	case XFCFLT + USER:	/* xfc instruction fault */
		i = SIGEMT;
		break;

	case COMPATFLT + USER: /* compatibility mode fault */
				/* so far, just send a SIGINS signal */
		i = SIGINS;
		break;

	}
	psignal(u.u_procp, i);

out:
	if(issig())
		psig();
	curpri = setpri(u.u_procp);
	if (runrun)
		qswtch();
	if(u.u_prof.pr_scale)
		addupc((caddr_t)locr0[PC], &u.u_prof, (int)(u.u_stime-syst));
	return;
}

/*
 * nonexistent system call-- set fatal error code.
 */
nosys()
{
	u.u_error = 100;
}

/*
 * Ignored system call
 */
nullsys()
{
}

random(dev, stat, ps, pc, ccb)
{
	printf("Random interrupt, device %x\n", dev);
}
