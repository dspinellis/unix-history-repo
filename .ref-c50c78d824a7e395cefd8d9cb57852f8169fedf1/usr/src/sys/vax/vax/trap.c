/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trap.c	7.3 (Berkeley) %G%
 */

#include "psl.h"
#include "reg.h"
#include "pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "assym.s"
#include "proc.h"
#include "seg.h"
#include "trap.h"
#include "acct.h"
#include "kernel.h"
#ifdef SYSCALLTRACE
#include "../sys/syscalls.c"
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
	case T_RESOPFLT+USER:	/* resereved operand fault */
		u.u_code = type &~ USER;
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
		u.u_code = code;
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
		i = u.u_error;
		pagein(code, 0);
		u.u_error = i;
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
		u.u_code = code;
		i = SIGILL;
		break;
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
int syscalltrace = 0;
#endif
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
	register struct proc *p;
	int opc;
	struct timeval syst;

	syst = u.u_ru.ru_stime;
	if (!USERMODE(locr0[PS]))
		panic("syscall");
	u.u_ar0 = locr0;
	if (code == 139) {			/* XXX 4.2 COMPATIBILITY */
		osigcleanup();			/* XXX 4.2 COMPATIBILITY */
		goto done;			/* XXX 4.2 COMPATIBILITY */
	}					/* XXX 4.2 COMPATIBILITY */
	params = (caddr_t)locr0[AP] + NBPW;
	u.u_error = 0;
	opc = pc - 2;
	if (code > 63)
		opc -= 2;
	if (code >= nsysent)
		callp = &sysent[0];		/* indir (illegal) */
	else {
		callp = &sysent[code];
		if (callp == sysent) {		/* indir */
			i = fuword(params);
			params += NBPW;
			if ((unsigned)i >= nsysent)
				callp = &sysent[0];
			else
				callp = &sysent[i];
		}
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
			register int i;
			char *cp;

			if (code >= nsysent)
				printf("0x%x", code);
			else
				printf("%s", syscallnames[code]);
			cp = "(";
			for (i= 0; i < callp->sy_narg; i++) {
				printf("%s%x", cp, u.u_arg[i]);
				cp = ", ";
			}
			if (i)
				putchar(')', 0);
			putchar('\n', 0);
		}
#endif
		(*(callp->sy_call))();
	}
	if (u.u_eosys == NORMALRETURN) {
		if (u.u_error) {
			locr0[R0] = u.u_error;
			locr0[PS] |= PSL_C;	/* carry bit */
		} else {
			locr0[R0] = u.u_r.r_val1;
			locr0[R1] = u.u_r.r_val2;
			locr0[PS] &= ~PSL_C;
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
