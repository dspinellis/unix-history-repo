/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.c	7.10 (Berkeley) 6/25/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "seg.h"
#include "acct.h"
#include "kernel.h"

#include "psl.h"
#include "reg.h"
#include "pte.h"
#include "mtpr.h"
#ifdef KTRACE
#include "ktrace.h"
#endif

#include "../tahoe/trap.h"

#define	USER	040		/* user-mode flag added to type */

struct	sysent sysent[];
int	nsysent;

char	*trap_type[] = {
	"Reserved addressing mode",		/* T_RESADFLT */
	"Privileged instruction",		/* T_PRIVINFLT */
	"Reserved operand",			/* T_RESOPFLT */
	"Breakpoint",				/* T_BPTFLT */
	0,
	"Kernel call",				/* T_SYSCALL */
	"Arithmetic trap",			/* T_ARITHTRAP */
	"System forced exception",		/* T_ASTFLT */
	"Segmentation fault",			/* T_SEGFLT */
	"Protection fault",			/* T_PROTFLT */
	"Trace trap",				/* T_TRCTRAP */
	0,
	"Page fault",				/* T_PAGEFLT */
	"Page table fault",			/* T_TABLEFLT */
	"Alignment fault",			/* T_ALIGNFLT */
	"Kernel stack not valid",		/* T_KSPNOTVAL */
	"Bus error",				/* T_BUSERR */
	"Kernel debugger request",		/* T_KDBTRAP */
};
int	TRAP_TYPES = sizeof (trap_type) / sizeof (trap_type[0]);

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(sp, type, hfs, accmst, acclst, dbl, code, pc, psl)
	unsigned type, code;	/* kdb assumes these are *not* registers */
{
	int r0, r1;		/* must reserve space */
	register int *locr0 = ((int *)&psl)-PS;
	register int i;
	unsigned ucode = code;
	register struct proc *p;
	struct timeval syst;

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	syst = u.u_ru.ru_stime;
	if (USERMODE(locr0[PS])) {
		type |= USER;
		u.u_ar0 = locr0;
	}
	switch (type) {

	default:
#ifdef KADB
		if (kdb_trap(&psl))
			return;
#endif
		printf("trap type %d, code = %x, pc = %x\n", type, code, pc);
		type &= ~USER;
		if (type < TRAP_TYPES && trap_type[type])
			panic(trap_type[type]);
		else
			panic("trap");
		/*NOTREACHED*/

	case T_PROTFLT + USER:		/* protection fault */
		i = SIGBUS;
		break;

	case T_PRIVINFLT + USER:	/* privileged instruction fault */
	case T_RESADFLT + USER:		/* reserved addressing fault */
	case T_RESOPFLT + USER:		/* resereved operand fault */
	case T_ALIGNFLT + USER:		/* unaligned data fault */
		ucode = type &~ USER;
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

	case T_ARITHTRAP + USER:
		i = SIGFPE;
		break;

	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case T_SEGFLT + USER:
		if (grow((unsigned)locr0[SP]) || grow(code))
			goto out;
		i = SIGSEGV;
		break;

	case T_TABLEFLT:		/* allow page table faults in kernel */
	case T_TABLEFLT + USER:		/* page table fault */
		panic("ptable fault");

	case T_PAGEFLT:			/* allow page faults in kernel mode */
	case T_PAGEFLT + USER:		/* page fault */
		pagein(code, 0);
		if (type == T_PAGEFLT)
			return;
		goto out;

	case T_BPTFLT + USER:		/* bpt instruction fault */
	case T_TRCTRAP + USER:		/* trace trap */
		locr0[PS] &= ~PSL_T;
		i = SIGTRAP;
		break;

#ifdef notdef
	/* THIS CODE IS BOGUS- delete? (KSP not valid is unrecoverable)
	   And what does KSPNOTVAL in user-mode mean? */
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
panic("ksp not valid - 2");
		/* must insure valid kernel stack pointer? */
		psignal(u.u_procp, SIGKILL);
		return;
#endif

	case T_BUSERR + USER:
		i = SIGBUS;
		break;
	}
	trapsignal(i, ucode);
out:
	p = u.u_procp;
	if (i = CURSIG(p))
		psig(i);
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
		if (i = CURSIG(p))
			psig(i);
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(locr0[PC], &u.u_prof, ticks);
	}
	curpri = p->p_pri;
}

/*
 * Called from locore when a system call occurs
 */
/*ARGSUSED*/
syscall(sp, type, hfs, accmst, acclst, dbl, code, pc, psl)
	unsigned code;
{
	int r0, r1;			/* must reserve space */
	register int *locr0 = ((int *)&psl)-PS;
	register caddr_t params;
	register int i;
	register struct sysent *callp;
	register struct proc *p = u.u_procp;
	struct timeval syst;
	int error, opc;
	struct args {
		int i[8];
	} args;
	int rval[2];

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	syst = u.u_ru.ru_stime;
	if (!USERMODE(locr0[PS]))
		panic("syscall");
	u.u_ar0 = locr0;
	params = (caddr_t)locr0[FP] + NBPW;
/* BEGIN GROT */
	/*
	 * Try to reconstruct pc, assuming code
	 * is an immediate constant
	 */
	opc = pc - 2;		/* short literal */
	if (code > 0x3f) {
		opc--;				/* byte immediate */
		if (code > 0x7f) {
			opc--;			/* word immediate */
			if (code > 0x7fff)
				opc -= 2;	/* long immediate */
		}
	}
/* END GROT */
	if (code == 0) {			/* indir */
		code = fuword(params);
		params += NBPW;
	}
	if (code >= nsysent)
		callp = &sysent[0];		/* indir (illegal) */
	else
		callp = &sysent[code];
	if ((i = callp->sy_narg * sizeof (int)) &&
	    (error = copyin(params, (caddr_t)&args, (u_int)i)) != 0) {
		locr0[R0] = error;
		locr0[PS] |= PSL_C;	/* carry bit */
#ifdef KTRACE
		if (KTRPOINT(p, KTR_SYSCALL))
			ktrsyscall(p->p_tracep, code, callp->sy_narg, args.i);
#endif
		goto done;
	}
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSCALL))
		ktrsyscall(p->p_tracep, code, callp->sy_narg, args.i);
#endif
	rval[0] = 0;
	rval[1] = locr0[R1];
	error = (*callp->sy_call)(u.u_procp, &args, rval);
	if (error == ERESTART)
		pc = opc;
	else if (error != EJUSTRETURN) {
		if (error) {
			locr0[R0] = error;
			locr0[PS] |= PSL_C;	/* carry bit */
		} else {
			locr0[PS] &= ~PSL_C;	/* clear carry bit */
			locr0[R0] = rval[0];
			locr0[R1] = rval[1];
		}
	}
	/* else if (error == EJUSTRETURN) */
		/* nothing to do */
done:
	/*
	 * Reinitialize proc pointer `p' as it may be different
	 * if this is a child returning from fork syscall.
	 */
	p = u.u_procp;
	if (i = CURSIG(p))
		psig(i);
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
		if (i = CURSIG(p))
			psig(i);
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(locr0[PC], &u.u_prof, ticks);
	}
	curpri = p->p_pri;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSRET))
		ktrsysret(p->p_tracep, code, error, rval[0]);
#endif
}
