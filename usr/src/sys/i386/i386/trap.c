/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the University of Utah, and William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)trap.c	5.5 (Berkeley) %G%
 */

/*
 * Copyright (c) 1989, 1990 William F. Jolitz
 */

/*
 * 386 Trap and System call handleing
 */

#include "../i386/psl.h"
#include "../i386/reg.h"
#include "../i386/pte.h"
#include "../i386/segments.h"
#include "../i386/frame.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "seg.h"
#include "acct.h"
#include "kernel.h"
#include "vm.h"
#include "cmap.h"

#include "../i386/trap.h"

#define	USER	0x100		/* user-mode flag added to type */

struct	sysent sysent[];
int	nsysent;
/*
 * Called from the trap handler when a processor trap occurs.
 */
unsigned *rcr2(), Sysbase;
extern short cpl;
/*ARGSUSED*/
trap(frame)
	struct trapframe frame;
#define type frame.tf_trapno
#define code frame.tf_err
#define pc frame.tf_eip
{
	register int *locr0 = ((int *)&frame);
	register int i;
	register struct proc *p;
	struct timeval syst;
	extern int nofault;


	locr0[tEFLAGS] &= ~PSL_NT;	/* clear nested trap XXX */
	if(nofault && frame.tf_trapno != 0xc) {
		locr0[tEIP] = nofault; return;
	}

	syst = u.u_ru.ru_stime;
	if (ISPL(locr0[tCS]) == SEL_UPL) {
		type |= USER;
		u.u_ar0 = locr0;
	}
	switch (type) {

	default:
bit_sucker:
#ifdef KDB
		if (kdb_trap(&psl))
			return;
#endif
		printf("trap type %d code %x pc %x cs %x eflags %x\n",
			type, code, pc, frame.tf_cs, frame.tf_eflags);
		type &= ~USER;
		panic("trap");
		/*NOTREACHED*/

	case T_SEGNPFLT + USER:
	case T_PROTFLT + USER:		/* protection fault */
		u.u_code = code + BUS_SEGM_FAULT ;
		i = SIGBUS;
		break;

	case T_PRIVINFLT + USER:	/* privileged instruction fault */
	case T_RESADFLT + USER:		/* reserved addressing fault */
	case T_RESOPFLT + USER:		/* reserved operand fault */
	case T_FPOPFLT + USER:		/* coprocessor operand fault */
		u.u_code = type &~ USER;
		i = SIGILL;
		break;

	case T_ASTFLT + USER:		/* Allow process switch */
	case T_ASTFLT:
		astoff();
		if ((u.u_procp->p_flag & SOWEUPC) && u.u_prof.pr_scale) {
			addupc(pc, &u.u_prof, 1);
			u.u_procp->p_flag &= ~SOWEUPC;
		}
		goto out;

	case T_DNA + USER:
		u.u_code = FPE_FPU_NP_TRAP;
		i = SIGFPE;
		break;

	case T_BOUND + USER:
		u.u_code = FPE_SUBRNG_TRAP;
		i = SIGFPE;
		break;

	case T_OFLOW + USER:
		u.u_code = FPE_INTOVF_TRAP;
		i = SIGFPE;
		break;

	case T_DIVIDE + USER:
		u.u_code = FPE_INTDIV_TRAP;
		i = SIGFPE;
		break;

	case T_ARITHTRAP + USER:
		u.u_code = code;
		i = SIGFPE;
		break;
#ifdef notdef
	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case T_STKFLT + USER:
	case T_SEGFLT + USER:
		if (grow((unsigned)locr0[tESP]))
			goto out;
		u.u_code = code;
		i = SIGSEGV;
		break;

	case T_TABLEFLT:		/* allow page table faults in kernel */
	case T_TABLEFLT + USER:		/* page table fault */
		panic("ptable fault");
#endif

	case T_PAGEFLT:			/* allow page faults in kernel mode */
		if (code & PGEX_P) goto bit_sucker;
		/* fall into */
	case T_PAGEFLT + USER:		/* page fault */
		{	register u_int vp;
			u_int ea;

			ea = (u_int)rcr2();

			/* out of bounds reference */
			if (ea >= &Sysbase || code & PGEX_P) {
				u.u_code = code + BUS_PAGE_FAULT;
				i = SIGBUS;
				break;
			}

			/* stack reference to the running process? */
			vp = btop(ea);
			if (vp >= dptov(u.u_procp, u.u_procp->p_dsize)
			&& vp < sptov(u.u_procp, u.u_procp->p_ssize-1)){
				/* attempt to grow stack */
				if (grow((unsigned)locr0[tESP]) || grow(ea)) {
					if (type == T_PAGEFLT) return;
					goto out;
				} else	if (nofault) {
					locr0[tEIP] = nofault;
					return;
				}
				i = SIGSEGV;
				break;
			}

			i = u.u_error;
			pagein(ea, 0);
			u.u_error = i;
			if (type == T_PAGEFLT) return;
			goto out;
		}

	case T_TRCTRAP:	 /* trace trap -- someone single stepping lcall's */
		locr0[tEFLAGS] &= ~PSL_T;
			/* Q: how do we turn it on again? */
		return;
	
	case T_BPTFLT + USER:		/* bpt instruction fault */
	case T_TRCTRAP + USER:		/* trace trap */
		locr0[tEFLAGS] &= ~PSL_T;
		i = SIGTRAP;
		break;

#ifdef notdef
	/*
	 * For T_KSPNOTVAL and T_BUSERR, can not allow spl to
	 * drop to 0 as clock could go off and we would end up
	 * doing an rei to the interrupt stack at ipl 0 (a
	 * reserved operand fault).  Instead, we allow psignal
	 * to post an ast, then return to user mode where we
	 * will reenter the kernel on the kernel's stack and
	 * can then service the signal.
	 */
	case T_KSPNOTVAL:
		if (noproc)
			panic("ksp not valid");
		/* fall thru... */
	case T_KSPNOTVAL + USER:
		printf("pid %d: ksp not valid\n", u.u_procp->p_pid);
		/* must insure valid kernel stack pointer? */
		psignal(u.u_procp, SIGKILL);
		return;

	case T_BUSERR + USER:
		u.u_code = code;
		psignal(u.u_procp, SIGBUS);
		return;
#endif

#include "isa.h"
#if	NISA > 0
	case T_NMI:
	case T_NMI + USER:
		if(isa_nmi(code) == 0) return;
		else goto bit_sucker;
#endif
	}
	psignal(u.u_procp, i);
out:
	p = u.u_procp;

	if (p->p_cursig || ISSIG(p))
		psig(1);
	p->p_pri = p->p_usrpri;
	if (runrun) {
		/*
		 * Since we are u.u_procp, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) splclock();
		setrq(p);
		u.u_ru.ru_nivcsw++;
		swtch();
		(void)spl0();
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(pc, &u.u_prof, ticks);
	}
	curpri = p->p_pri;
#undef type
#undef code
#undef pc
}

/*
 * Called from locore when a system call occurs
 */
/*ARGSUSED*/
syscall(frame)
	struct syscframe frame;
#define code frame.sf_eax	/* note: written over! */
#define pc frame.sf_eip
{
	register int *locr0 = ((int *)&frame);
	register caddr_t params;
	register int i;
	register struct sysent *callp;
	register struct proc *p;
	struct timeval syst;
	int opc;

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	syst = u.u_ru.ru_stime;
	if (ISPL(locr0[sCS]) != SEL_UPL)
		panic("syscall");
	u.u_ar0 = locr0;
	params = (caddr_t)locr0[sESP] + NBPW ;
	u.u_error = 0;
	/*
	 * Reconstruct pc, assuming lcall $X,y is 7 bytes, as it is always.
	 */
	opc = pc - 7;
	callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	if (callp == sysent) {
		i = fuword(params);
		params += NBPW;
		callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	}
	if ((i = callp->sy_narg * sizeof (int)) &&
	    (u.u_error = copyin(params, (caddr_t)u.u_arg, (u_int)i)) != 0) {
		locr0[sEAX] = u.u_error;
		locr0[sEFLAGS] |= PSL_C;	/* carry bit */
		goto done;
	}
	u.u_r.r_val1 = 0;
	u.u_r.r_val2 = locr0[sEDX];
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0 && u.u_eosys != RESTARTSYS)
			u.u_error = EINTR;
	} else {
		u.u_eosys = NORMALRETURN;
		(*callp->sy_call)();
	}
	if (u.u_eosys == NORMALRETURN) {
		if (u.u_error) {
			locr0[sEAX] = u.u_error;
			locr0[sEFLAGS] |= PSL_C;	/* carry bit */
		} else {
			locr0[sEFLAGS] &= ~PSL_C;	/* clear carry bit */
			locr0[sEAX] = u.u_r.r_val1;
			locr0[sEDX] = u.u_r.r_val2;
		}
	} else if (u.u_eosys == RESTARTSYS)
		pc = opc;
	/* else if (u.u_eosys == JUSTRETURN) */
		/* nothing to do */
done:
	p = u.u_procp;
	if (p->p_cursig || ISSIG(p))
		psig(0);
	p->p_pri = p->p_usrpri;
	if (runrun) {
		/*
		 * Since we are u.u_procp, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) splclock();
		setrq(p);
		u.u_ru.ru_nivcsw++;
		swtch();
		(void)spl0();
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(opc, &u.u_prof, ticks);
	}
	curpri = p->p_pri;
}

/*
 * nonexistent system call-- signal process (may want to handle it)
 * flag error if process won't see signal immediately
 * Q: should we do that all the time ??
 */
nosys()
{

	if (u.u_signal[SIGSYS] == SIG_IGN || u.u_signal[SIGSYS] == SIG_HOLD)
		u.u_error = EINVAL;
	psignal(u.u_procp, SIGSYS);
}

/*
 * Ignored system call
 */
nullsys()
{
}
