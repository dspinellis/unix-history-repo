/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.c	7.10 (Berkeley) 6/25/90
 */

#include "psl.h"
#include "reg.h"
#include "pte.h"

#include "param.h"
#include "systm.h"
#include "user.h"
#include "assym.s"
#include "proc.h"
#include "seg.h"
#include "trap.h"
#include "acct.h"
#include "kernel.h"
#ifdef KTRACE
#include "ktrace.h"
#endif

#include "mtpr.h"

#define	USER	040		/* user-mode flag added to type */

struct	sysent	sysent[];
int	nsysent;

char	*trap_type[] = {
	"Reserved addressing mode",
	"Privileged instruction",
	"Reserved operand",
	"Breakpoint",
	"Xfc trap",
	"Syscall trap",
	"Arithmetic fault",
	"Ast trap",
	"Segmentation fault",
	"Protection fault",
	"Trace trap",
	"Compatibility mode trap",
	"Page fault",
	"Page table fault",
	"Kernel debugger trap",
};
int	TRAP_TYPES = (sizeof trap_type / sizeof trap_type[0]);

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(sp, type, code, pc, psl)
	int sp, type;
	unsigned code;
	int pc, psl;
{
	register int *locr0 = ((int *)&psl)-PS;
	register int i;
	unsigned ucode = code;
	register struct proc *p;
	struct timeval syst;

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
		if ((unsigned)type < TRAP_TYPES)
			panic(trap_type[type]);
		panic("trap");

	case T_PROTFLT+USER:	/* protection fault */
		i = SIGBUS;
		break;

	case T_PRIVINFLT+USER:	/* privileged instruction fault */
	case T_RESADFLT+USER:	/* reserved addressing fault */
	case T_RESOPFLT+USER:	/* reserved operand fault */
		ucode = type &~ USER;
		i = SIGILL;
		break;

	case T_ASTFLT+USER:
		astoff();
		if ((u.u_procp->p_flag & SOWEUPC) && u.u_prof.pr_scale) {
			addupc(pc, &u.u_prof, 1);
			u.u_procp->p_flag &= ~SOWEUPC;
		}
		goto out;

	case T_ARITHTRAP+USER:
		i = SIGFPE;
		break;

	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case T_SEGFLT+USER:
		if (grow((unsigned)locr0[SP]) || grow(code))
			goto out;
		i = SIGSEGV;
		break;

	case T_TABLEFLT:	/* allow page table faults in kernel mode */
	case T_TABLEFLT+USER:   /* page table fault */
		panic("ptable fault");

	case T_PAGEFLT:		/* allow page faults in kernel mode */
	case T_PAGEFLT+USER:	/* page fault */
		pagein(code, 0);
		if (type == T_PAGEFLT)
			return;
		goto out;

	case T_BPTFLT+USER:	/* bpt instruction fault */
	case T_TRCTRAP+USER:	/* trace trap */
		locr0[PS] &= ~PSL_T;
		i = SIGTRAP;
		break;

	case T_XFCFLT+USER:	/* xfc instruction fault */
		i = SIGEMT;
		break;

	case T_COMPATFLT+USER:	/* compatibility mode fault */
		u.u_acflag |= ACOMPAT;
		i = SIGILL;
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
 * Called from the trap handler when a system call occurs
 */
/*ARGSUSED*/
syscall(sp, type, code, pc, psl)
	unsigned code;
{
	register int *locr0 = ((int *)&psl)-PS;
	register caddr_t params;		/* known to be r10 below */
	register int i;				/* known to be r9 below */
	register struct sysent *callp;
	register struct proc *p = u.u_procp;
	int error, opc;
	struct args {
		int i[8];
	} args;
	int rval[2];
	struct timeval syst;

	syst = u.u_ru.ru_stime;
	if (!USERMODE(locr0[PS]))
		panic("syscall");
	u.u_ar0 = locr0;
	params = (caddr_t)locr0[AP] + NBPW;
	opc = pc - 2;
	if (code > 63)
		opc -= 2;
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
			locr0[R0] = rval[0];
			locr0[R1] = rval[1];
			locr0[PS] &= ~PSL_C;
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
