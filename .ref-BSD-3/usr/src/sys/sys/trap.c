/*	trap.c	2.1	1/5/80	*/

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
struct	sysent	vmsysent[64];

int	vmindir();

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(params, r0, r1, r2, r3, r4, r5 ,r6, r7, r8, r9, r10,
	r11, r12, r13, sp, type, code, pc, psl)
unsigned code;
caddr_t params;
{
	register i, a;
	register struct sysent *callp;
	register time_t syst;
	register int *locr0;
	int errorsave, *ar0save;

	locr0 = &r0;
	ar0save = u.u_ar0;
	syst = u.u_stime;
	u.u_ar0 = locr0;
	if (USERMODE(locr0[PS]))
		type |= USER;
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
		/*
		 * In order to allow programs which call vfork() and
		 * vtimes() to work on non-virtual systems, we map codes
		 * in the range 64 to 127 to vm system calls; thus code 66
		 * is a vfork() call on a virtual system, but just a fork()
		 * call on a non-virtual system (similarly for vtimes/times).
		 * In fact, all virtual system calls could be called by giving
		 * calls in the range 64-127, but some of these don't degenerate
		 * safely to non-virtual calls, so as long as the virtual
		 * system is not the only system ever run, it is not safe
		 * to run this way.
		 */
		if (code > 077 && code <= 0177)
			callp = &vmsysent[code&077];
		else
			callp = &sysent[code&077];
		if (callp == sysent || callp->sy_call == vmindir) {
			a = fuword(params);
			params += NBPW;
			if (callp == sysent)
				callp = &sysent[a&077];
			else
				callp = &vmsysent[a&077];
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
		if(grow((unsigned)locr0[SP]) || grow(code))
			goto out;
		i = SIGSEG;
		break;

	case TABLEFLT:		/* allow page table faults in kernel mode */
	case TABLEFLT + USER:   /* page table fault */
		panic("page table fault");

	case PAGEFLT:		/* allow page faults in kernel mode */
	case PAGEFLT + USER:	/* page fault */
		errorsave = u.u_error;
		pagein(code);   /* bring in page containing virtual addr */
		u.u_error = errorsave;
		goto out1;

	case BPTFLT + USER:	/* bpt instruction fault */
	case TRCTRAP + USER:	/* trace trap */
		locr0[PS] &= ~PSL_T;	/* turn off trace bit */
		i = SIGTRC;
		break;

	case XFCFLT + USER:	/* xfc instruction fault */
		i = SIGEMT;
		break;

	case COMPATFLT + USER:	/* compatibility mode fault */
			/* for now, just send SIGINS signal */
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

out1:
	/*
	 * Restore in case recursive trap call
	 * due to virtual memory faults.
	 */
	u.u_ar0 = ar0save;
}

/*
 * vm indirect system call... we can only get here if vmindir is executed
 * indirectly, since it normally indirects itself in the code above.
 * Thus this is a fatal error.
 */
vmindir()
{

	nosys();
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

/*ARGSUSED*/
/*
random(dev, stat, ps, pc, ccb)
{

	printf("Random interrupt, device %x\n", dev);
}
*/
