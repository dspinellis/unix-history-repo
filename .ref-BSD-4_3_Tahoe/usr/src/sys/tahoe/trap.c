/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.c	7.1 (Berkeley) 5/21/88
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "seg.h"
#include "acct.h"
#include "kernel.h"

#include "psl.h"
#include "reg.h"
#include "pte.h"
#include "mtpr.h"

#define	SYSCALLTRACE
#ifdef SYSCALLTRACE
#include "../sys/syscalls.c"
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
	"Kernel debugger trap",			/* T_KDBTRAP */
};
int	TRAP_TYPES = sizeof (trap_type) / sizeof (trap_type[0]);

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(sp, type, hfs, accmst, acclst, dbl, code, pc, psl)
	unsigned type, code;
{
	int r0, r1;		/* must reserve space */
	register int *locr0 = ((int *)&psl)-PS;
	register int i;
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

	case T_ARITHTRAP + USER:
		u.u_code = code;
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
		i = u.u_error;
		pagein(code, 0);
		u.u_error = i;
		if (type == T_PAGEFLT)
			return;
		goto out;

	case T_BPTFLT + USER:		/* bpt instruction fault */
	case T_TRCTRAP + USER:		/* trace trap */
		locr0[PS] &= ~PSL_T;
		i = SIGTRAP;
		break;

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
	}
	psignal(u.u_procp, i);
out:
	p = u.u_procp;
	if (p->p_cursig || ISSIG(p))
		psig();
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

#ifdef SYSCALLTRACE
int	syscalltrace = 0;
#endif

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
	register struct proc *p;
	struct timeval syst;
	int opc;

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	syst = u.u_ru.ru_stime;
	if (!USERMODE(locr0[PS]))
		panic("syscall");
	u.u_ar0 = locr0;
	if (code == 139) {			/* 4.2 COMPATIBILTY XXX */
		osigcleanup();			/* 4.2 COMPATIBILTY XXX */
		goto done;			/* 4.2 COMPATIBILTY XXX */
	}
	params = (caddr_t)locr0[FP] + NBPW;
	u.u_error = 0;
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
	callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	if (callp == sysent) {
		i = fuword(params);
		params += NBPW;
		callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	}
	if ((i = callp->sy_narg * sizeof (int)) &&
	    (u.u_error = copyin(params, (caddr_t)u.u_arg, (u_int)i)) != 0) {
		locr0[R0] = u.u_error;
		locr0[PS] |= PSL_C;	/* carry bit */
		goto done;
	}
	u.u_r.r_val1 = 0;
	u.u_r.r_val2 = locr0[R1];
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0 && u.u_eosys != RESTARTSYS)
			u.u_error = EINTR;
	} else {
		u.u_eosys = NORMALRETURN;
#ifdef SYSCALLTRACE
		if (syscalltrace) {
			register int a;
			char *cp;

			if (code >= nsysent)
				printf("0x%x", code);
			else
				printf("%s", syscallnames[code]);
			cp = "(";
			for (a = 0; a < callp->sy_narg; a++) {
				printf("%s%x", cp, u.u_arg[a]);
				cp = ", ";
			}
			if (a)
				printf(")");
			printf("\n");
		}
#endif
		(*callp->sy_call)();
	}
	if (u.u_eosys == NORMALRETURN) {
		if (u.u_error) {
			locr0[R0] = u.u_error;
			locr0[PS] |= PSL_C;	/* carry bit */
		} else {
			locr0[PS] &= ~PSL_C;	/* clear carry bit */
			locr0[R0] = u.u_r.r_val1;
			locr0[R1] = u.u_r.r_val2;
		}
	} else if (u.u_eosys == RESTARTSYS)
		pc = opc;
	/* else if (u.u_eosys == JUSTRETURN) */
		/* nothing to do */
done:
	p = u.u_procp;
	if (p->p_cursig || ISSIG(p))
		psig();
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

#ifdef notdef
/*
 * Ignored system call
 */
nullsys()
{

}
#endif
