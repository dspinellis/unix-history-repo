/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the University of Utah, and William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)trap.c	5.7 (Berkeley) %G%
 */


/*
 * 386 Trap and System call handleing
 */

#include "machine/psl.h"
#include "machine/reg.h"
#include "machine/pte.h"
#include "machine/segments.h"
#include "machine/frame.h"

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "seg.h"
#include "acct.h"
#include "kernel.h"
#include "vm.h"
#include "cmap.h"
#ifdef KTRACE
#include "ktrace.h"
#endif

#include "machine/trap.h"

#define	USER	0x40		/* user-mode flag added to type */
#define	FRMTRAP	0x100		/* distinguish trap from syscall */

struct	sysent sysent[];
int	nsysent;
#include "dbg.h"
/*
 * Called from the trap handler when a processor trap occurs.
 */
unsigned rcr2(), Sysbase;
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
	int ucode;
	int oar0;

#ifdef DEBUG
dprintf(DALLTRAPS, "\n%d. trap",u.u_procp->p_pid);
dprintf(DALLTRAPS, " pc:%x cs:%x ds:%x eflags:%x isp %x\n",
		frame.tf_eip, frame.tf_cs, frame.tf_ds, frame.tf_eflags,
		frame.tf_isp);
dprintf(DALLTRAPS, "edi %x esi %x ebp %x ebx %x esp %x\n",
		frame.tf_edi, frame.tf_esi, frame.tf_ebp,
		frame.tf_ebx, frame.tf_esp);
dprintf(DALLTRAPS, "edx %x ecx %x eax %x\n",
		frame.tf_edx, frame.tf_ecx, frame.tf_eax);
p=u.u_procp;
dprintf(DALLTRAPS, "sig %x %x %x \n",
		p->p_sigignore, p->p_sigcatch, p->p_sigmask);
dprintf(DALLTRAPS, " ec %x type %x cpl %x ",
		frame.tf_err&0xffff, frame.tf_trapno, cpl);
#endif

	locr0[tEFLAGS] &= ~PSL_NT;	/* clear nested trap XXX */
if(nofault && frame.tf_trapno != 0xc)
	{ locr0[tEIP] = nofault; return;}

	syst = u.u_ru.ru_stime;
oar0= u.u_ar0;
	if (ISPL(locr0[tCS]) == SEL_UPL) {
		type |= USER;
		u.u_ar0 = locr0;
	}
	ucode=0;
	switch (type) {

	default:
bit_sucker:
#ifdef KDB
		if (kdb_trap(&psl))
			return;
#endif

splhigh();
printf("cr2 %x cpl %x ", rcr2(), cpl);
		printf("trap type %d, code = %x, pc = %x cs = %x, eflags = %x\n", type, code, pc, frame.tf_cs, frame.tf_eflags);
		type &= ~USER;
pg("panic");
		panic("trap");
		/*NOTREACHED*/

	case T_SEGNPFLT + USER:
	case T_PROTFLT + USER:		/* protection fault */
		ucode = code + BUS_SEGM_FAULT ;
		i = SIGBUS;
		break;

	case T_PRIVINFLT + USER:	/* privileged instruction fault */
	case T_RESADFLT + USER:		/* reserved addressing fault */
	case T_RESOPFLT + USER:		/* reserved operand fault */
	case T_FPOPFLT + USER:		/* coprocessor operand fault */
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

	case T_DNA + USER:
#ifdef	NPX
		if (npxdna()) return;
#endif
		ucode = FPE_FPU_NP_TRAP;
		i = SIGFPE;
		break;

	case T_BOUND + USER:
		ucode = FPE_SUBRNG_TRAP;
		i = SIGFPE;
		break;

	case T_OFLOW + USER:
		ucode = FPE_INTOVF_TRAP;
		i = SIGFPE;
		break;

	case T_DIVIDE + USER:
		ucode = FPE_INTDIV_TRAP;
		i = SIGFPE;
		break;

	case T_ARITHTRAP + USER:
		ucode = code;
		i = SIGFPE;
		break;

#ifdef notdef
	/*
	 * If the user SP is above the stack segment,
	 * grow the stack automatically.
	 */
	case T_STKFLT + USER:
	case T_SEGFLT + USER:
		if (grow((unsigned)locr0[tESP]) /*|| grow(code)*/)
			goto out;
		ucode = code;
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

#ifdef DEBUG
dprintf(DPAGIN|DALLTRAPS, "pf code %x pc %x usp %x cr2 %x |",
		code, frame.tf_eip, frame.tf_esp, rcr2());
#endif
			ea = (u_int)rcr2();

			/* out of bounds reference */
			if (ea >= (u_int)&Sysbase || code & PGEX_P) {
				ucode = code + BUS_PAGE_FAULT;
				i = SIGBUS;
				break;
			}

			/* stack reference to the running process? */
			vp = btop(ea);
			if (vp >= dptov(u.u_procp, u.u_procp->p_dsize)
			&& vp < sptov(u.u_procp, u.u_procp->p_ssize-1)){
				/* attempt to grow stack */
				if (grow((unsigned)locr0[tESP]) || grow(ea)) {
					if (type == T_PAGEFLT)
{
u.u_ar0 = oar0;
return;
}
					goto out;
				} else	if (nofault) {
u.u_ar0 = oar0;
					locr0[tEIP] = nofault;
					return;
				}
				i = SIGSEGV;
				ucode = code + BUS_PAGE_FAULT;
				break;
			}

			pagein(ea, 0, code);
			if (type == T_PAGEFLT) return;
			goto out;
		}

	case T_TRCTRAP:	 /* trace trap -- someone single stepping lcall's */
		locr0[tEFLAGS] &= ~PSL_T;
			/* Q: how do we turn it on again? */
u.u_ar0 = oar0;
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
		trapsignal(SIGKILL,0|FRMTRAP);
u.u_ar0 = oar0;
		return;

	case T_BUSERR + USER:
		trapsignal(SIGBUS, code|FRMTRAP);
u.u_ar0 = oar0;
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
	trapsignal(i, ucode|FRMTRAP);
	if ((type & USER) == 0)
{
u.u_ar0 = oar0;
		return;
}
out:
	p = u.u_procp;
	if (i = CURSIG(p))
		psig(i,FRMTRAP);
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
			psig(i,FRMTRAP);
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
u.u_ar0 = oar0;
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
	register int *locr0 = ((int *)&frame)/*-PS*/;
	register caddr_t params;
	register int i;
	register struct sysent *callp;
	register struct proc *p;
	struct timeval syst;
	int error, opc;
	int args[8], rval[2];

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	syst = u.u_ru.ru_stime;
	p = u.u_procp;
	if (ISPL(locr0[sCS]) != SEL_UPL)
{
printf("\npc:%x cs:%x eflags:%x\n",
		frame.sf_eip, frame.sf_cs, frame.sf_eflags);
printf("edi %x esi %x ebp %x ebx %x esp %x\n",
		frame.sf_edi, frame.sf_esi, frame.sf_ebp,
		frame.sf_ebx, frame.sf_esp);
printf("edx %x ecx %x eax %x\n", frame.sf_edx, frame.sf_ecx, frame.sf_eax);
printf("cr0 %x cr2 %x cpl %x \n", rcr0(), rcr2(), cpl);
		panic("syscall");
}
	u.u_ar0 = locr0;
	params = (caddr_t)locr0[sESP] + NBPW ;

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
/*dprintf(DALLSYSC,"%d. call %d ", p->p_pid, code);*/
	if ((i = callp->sy_narg * sizeof (int)) &&
	    (error = copyin(params, (caddr_t)args, (u_int)i))) {
		locr0[sEAX] = (u_char) error;
		locr0[sEFLAGS] |= PSL_C;	/* carry bit */
#ifdef KTRACE
		if (KTRPOINT(p, KTR_SYSCALL))
			ktrsyscall(p->p_tracep, code, callp->sy_narg, &args);
#endif
		goto done;
	}
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSCALL))
		ktrsyscall(p->p_tracep, code, callp->sy_narg, &args);
#endif
	rval[0] = 0;
	rval[1] = locr0[sEDX];
	error = (*callp->sy_call)(p, args, rval);
	if (error == ERESTART)
		pc = opc;
	else if (error != EJUSTRETURN) {
		if (error) {
			locr0[sEAX] = (u_char) error;
			locr0[sEFLAGS] |= PSL_C;	/* carry bit */
		} else {
			locr0[sEAX] = rval[0];
			locr0[sEDX] = rval[1];
			locr0[sEFLAGS] &= ~PSL_C;	/* carry bit */
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
	/*
	 * XXX the check for sigreturn ensures that we don't
	 * attempt to set up a call to a signal handler (sendsig) before
	 * we have cleaned up the stack from the last call (sigreturn).
	 * Allowing this seems to lock up the machine in certain scenarios.
	 * What should really be done is to clean up the signal handling
	 * so that this is not a problem.
	 */
#include "syscall.h"
	if (code != SYS_sigreturn && (i = CURSIG(p)))
		psig(i,0);
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
		if (code != SYS_sigreturn && (i = CURSIG(p)))
			psig(i,0);
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks) {
#ifdef PROFTIMER
			extern int profscale;
			addupc(pc, &u.u_prof, ticks * profscale);
#else
			addupc(pc, &u.u_prof, ticks);
#endif
		}
	}
	curpri = p->p_pri;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSRET))
		ktrsysret(p->p_tracep, code, error, rval[0]);
#endif
}

#ifdef notdef
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
#endif

/*
 * Ignored system call
 */
nullsys()
{

}
