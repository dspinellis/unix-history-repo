/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: trap.c 1.28 89/09/25$
 *
 *	@(#)trap.c	7.9 (Berkeley) %G%
 */

#include "../include/cpu.h"
#include "../include/psl.h"
#include "../include/reg.h"
#include "../include/mtpr.h"

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/seg.h"
#include "../include/trap.h"
#include "sys/acct.h"
#include "sys/kernel.h"
#include "sys/syslog.h"
#ifdef KTRACE
#include "sys/ktrace.h"
#endif

#include "vm/vm_param.h"
#include "vm/pmap.h"
#include "vm/vm_map.h"
#include "sys/vmmeter.h"

#ifdef HPUXCOMPAT
#include "../hpux/hpux.h"
#endif

#define	USER	040		/* user-mode flag added to type */

struct	sysent	sysent[];
int	nsysent;

char	*trap_type[] = {
	"Bus error",
	"Address error",
	"Illegal instruction",
	"Zero divide",
	"CHK instruction",
	"TRAPV instruction",
	"Privilege violation",
	"Trace trap",
	"MMU fault",
	"SSIR trap",
	"Format error",
	"68881 exception",
	"Coprocessor violation",
	"Async system trap"
};
#define	TRAP_TYPES	(sizeof trap_type / sizeof trap_type[0])

#ifdef DEBUG
int mmudebug = 0;
#endif

/*
 * Called from the trap handler when a processor trap occurs.
 */
/*ARGSUSED*/
trap(type, code, v, frame)
	int type;
	unsigned code;
	register unsigned v;
	struct frame frame;
{
	register int i;
	unsigned ucode = 0;
	register struct proc *p = u.u_procp;
	struct timeval syst;
	unsigned ncode;

	cnt.v_trap++;
	syst = u.u_ru.ru_stime;
	if (USERMODE(frame.f_sr)) {
		type |= USER;
		u.u_ar0 = frame.f_regs;
	}
	switch (type) {

	default:
dopanic:
#ifdef KGDB
		if (!panicstr && kgdb_trap(type, code, v, &frame))
			return;
#endif
		printf("trap type %d, code = %x, v = %x\n", type, code, v);
		regdump(frame.f_regs, 128);
		type &= ~USER;
		if ((unsigned)type < TRAP_TYPES)
			panic(trap_type[type]);
		panic("trap");

	case T_BUSERR:		/* kernel bus error */
		if (!u.u_pcb.pcb_onfault)
			goto dopanic;
		/*
		 * If we have arranged to catch this fault in any of the
		 * copy to/from user space routines, set PC to return to
		 * indicated location and set flag informing buserror code
		 * that it may need to clean up stack frame.
		 */
copyfault:
		frame.f_pc = (int) u.u_pcb.pcb_onfault;
		frame.f_stackadj = -1;
		return;

	case T_BUSERR+USER:	/* bus error */
	case T_ADDRERR+USER:	/* address error */
		i = SIGBUS;
		break;

#ifdef FPCOPROC
	case T_COPERR:		/* kernel coprocessor violation */
#endif
	case T_FMTERR:		/* kernel format error */
	/*
	 * The user has most likely trashed the RTE or FP state info
	 * in the stack frame of a signal handler.
	 */
		type |= USER;
		printf("pid %d: kernel %s exception\n", u.u_procp->p_pid,
		       type==T_COPERR ? "coprocessor" : "format");
		u.u_signal[SIGILL] = SIG_DFL;
		i = sigmask(SIGILL);
		p->p_sigignore &= ~i;
		p->p_sigcatch &= ~i;
		p->p_sigmask &= ~i;
		i = SIGILL;
		ucode = frame.f_format;	/* XXX was ILL_RESAD_FAULT */
		break;

#ifdef FPCOPROC
	case T_COPERR+USER:	/* user coprocessor violation */
	/* What is a proper response here? */
		ucode = 0;
		i = SIGFPE;
		break;

	case T_FPERR+USER:		/* 68881 exceptions */
	/*
	 * We pass along the 68881 status register which locore stashed
	 * in code for us.  Note that there is a possibility that the
	 * bit pattern of this register will conflict with one of the
	 * FPE_* codes defined in signal.h.  Fortunately for us, the
	 * only such codes we use are all in the range 1-7 and the low
	 * 3 bits of the status register are defined as 0 so there is
	 * no clash.
	 */
		ucode = code;
		i = SIGFPE;
		break;
#endif

	case T_ILLINST+USER:	/* illegal instruction fault */
#ifdef HPUXCOMPAT
		if (u.u_procp->p_flag & SHPUX) {
			ucode = HPUX_ILL_ILLINST_TRAP;
			i = SIGILL;
			break;
		}
		/* fall through */
#endif
	case T_PRIVINST+USER:	/* privileged instruction fault */
#ifdef HPUXCOMPAT
		if (u.u_procp->p_flag & SHPUX)
			ucode = HPUX_ILL_PRIV_TRAP;
		else
#endif
		ucode = frame.f_format;	/* XXX was ILL_PRIVIN_FAULT */
		i = SIGILL;
		break;

	case T_ZERODIV+USER:	/* Divide by zero */
#ifdef HPUXCOMPAT
		if (u.u_procp->p_flag & SHPUX)
			ucode = HPUX_FPE_INTDIV_TRAP;
		else
#endif
		ucode = frame.f_format;	/* XXX was FPE_INTDIV_TRAP */
		i = SIGFPE;
		break;

	case T_CHKINST+USER:	/* CHK instruction trap */
#ifdef HPUXCOMPAT
		if (u.u_procp->p_flag & SHPUX) {
			/* handled differently under hp-ux */
			i = SIGILL;
			ucode = HPUX_ILL_CHK_TRAP;
			break;
		}
#endif
		ucode = frame.f_format;	/* XXX was FPE_SUBRNG_TRAP */
		i = SIGFPE;
		break;

	case T_TRAPVINST+USER:	/* TRAPV instruction trap */
#ifdef HPUXCOMPAT
		if (u.u_procp->p_flag & SHPUX) {
			/* handled differently under hp-ux */
			i = SIGILL;
			ucode = HPUX_ILL_TRAPV_TRAP;
			break;
		}
#endif
		ucode = frame.f_format;	/* XXX was FPE_INTOVF_TRAP */
		i = SIGFPE;
		break;

	/*
	 * XXX: Trace traps are a nightmare.
	 *
	 *	HP-UX uses trap #1 for breakpoints,
	 *	HPBSD uses trap #2,
	 *	SUN 3.x uses trap #15,
	 *	KGDB uses trap #15 (for kernel breakpoints).
	 *
	 * HPBSD and HP-UX traps both get mapped by locore.s into T_TRACE.
	 * SUN 3.x traps get passed through as T_TRAP15 and are not really
	 * supported yet.  KGDB traps are also passed through as T_TRAP15
	 * and are not used yet.
	 */
	case T_TRACE:		/* kernel trace trap */
	case T_TRAP15:		/* SUN (or KGDB) kernel trace trap */
#ifdef KGDB
		if (kgdb_trap(type, code, v, &frame))
			return;
#endif
		frame.f_sr &= ~PSL_T;
		i = SIGTRAP;
		break;

	case T_TRACE+USER:	/* user trace trap */
	case T_TRAP15+USER:	/* SUN user trace trap */
		frame.f_sr &= ~PSL_T;
		i = SIGTRAP;
		break;

	case T_ASTFLT:		/* system async trap, cannot happen */
		goto dopanic;

	case T_ASTFLT+USER:	/* user async trap */
		astoff();
		/*
		 * We check for software interrupts first.  This is because
		 * they are at a higher level than ASTs, and on a VAX would
		 * interrupt the AST.  We assume that if we are processing
		 * an AST that we must be at IPL0 so we don't bother to
		 * check.  Note that we ensure that we are at least at SIR
		 * IPL while processing the SIR.
		 */
		spl1();
		/* fall into... */

	case T_SSIR:		/* software interrupt */
	case T_SSIR+USER:
		if (ssir & SIR_NET) {
			siroff(SIR_NET);
			cnt.v_soft++;
			netintr();
		}
		if (ssir & SIR_CLOCK) {
			siroff(SIR_CLOCK);
			cnt.v_soft++;
			softclock((caddr_t)frame.f_pc, (int)frame.f_sr);
		}
		/*
		 * If this was not an AST trap, we are all done.
		 */
		if (type != T_ASTFLT+USER) {
			cnt.v_trap--;
			return;
		}
		spl0();
#ifndef PROFTIMER
		if ((u.u_procp->p_flag&SOWEUPC) && u.u_prof.pr_scale) {
			addupc(frame.f_pc, &u.u_prof, 1);
			u.u_procp->p_flag &= ~SOWEUPC;
		}
#endif
		goto out;

	case T_MMUFLT:		/* kernel mode page fault */
		/* fall into ... */

	case T_MMUFLT+USER:	/* page fault */
	    {
		register vm_offset_t va;
		register vm_map_t map;
		int rv;
		vm_prot_t ftype;
		extern vm_map_t kernel_map;
		unsigned nss;

		/*
		 * It is only a kernel address space fault iff:
		 * 	1. (type & USER) == 0  and
		 * 	2. pcb_onfault not set or
		 *	3. pcb_onfault set but supervisor space data fault
		 * The last can occur during an exec() copyin where the
		 * argument space is lazy-allocated.
		 */
		if (type == T_MMUFLT &&
		    (!u.u_pcb.pcb_onfault ||
		     (code & (SSW_DF|FC_SUPERD)) == (SSW_DF|FC_SUPERD)))
			map = kernel_map;
		else
			map = u.u_procp->p_map;
		if ((code & (SSW_DF|SSW_RW)) == SSW_DF)	/* what about RMW? */
			ftype = VM_PROT_READ | VM_PROT_WRITE;
		else
			ftype = VM_PROT_READ;
		va = trunc_page((vm_offset_t)v);
#ifdef DEBUG
		if (map == kernel_map && va == 0) {
			printf("trap: bad kernel access at %x\n", v);
			goto dopanic;
		}
#endif
		/*
		 * XXX: rude hack to make stack limits "work"
		 */
		nss = 0;
		if ((caddr_t)va >= u.u_maxsaddr && map != kernel_map) {
			nss = clrnd(btoc(USRSTACK-(unsigned)va));
			if (nss > btoc(u.u_rlimit[RLIMIT_STACK].rlim_cur)) {
				rv = KERN_FAILURE;
				goto nogo;
			}
		}
		rv = vm_fault(map, va, ftype, FALSE);
		if (rv == KERN_SUCCESS) {
			/*
			 * XXX: continuation of rude stack hack
			 */
			if (nss > u.u_ssize)
				u.u_ssize = nss;
			if (type == T_MMUFLT)
				return;
			goto out;
		}
nogo:
		if (type == T_MMUFLT) {
			if (u.u_pcb.pcb_onfault)
				goto copyfault;
			printf("vm_fault(%x, %x, %x, 0) -> %x\n",
			       map, va, ftype, rv);
			printf("  type %x, code [mmu,,ssw]: %x\n",
			       type, code);
			goto dopanic;
		}
		i = (rv == KERN_PROTECTION_FAILURE) ? SIGBUS : SIGSEGV;
		break;
	    }
	}
	trapsignal(i, ucode);
	if ((type & USER) == 0)
		return;
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
		if (ticks) {
#ifdef PROFTIMER
			extern int profscale;
			addupc(frame.f_pc, &u.u_prof, ticks * profscale);
#else
			addupc(frame.f_pc, &u.u_prof, ticks);
#endif
		}
	}
	curpri = p->p_pri;
}

/*
 * Called from the trap handler when a system call occurs
 */
/*ARGSUSED*/
syscall(code, frame)
	volatile int code;
	struct frame frame;
{
	register caddr_t params;
	register int i;
	register struct sysent *callp;
	register struct proc *p = u.u_procp;
	int error, opc, numsys;
	struct args {
		int i[8];
	} args;
	int rval[2];
	struct timeval syst;
	struct sysent *systab;
#ifdef HPUXCOMPAT
	extern struct sysent hpuxsysent[];
	extern int hpuxnsysent, notimp();
#endif

	cnt.v_syscall++;
	syst = u.u_ru.ru_stime;
	if (!USERMODE(frame.f_sr))
		panic("syscall");
	u.u_ar0 = frame.f_regs;
	opc = frame.f_pc - 2;
	systab = sysent;
	numsys = nsysent;
#ifdef HPUXCOMPAT
	if (p->p_flag & SHPUX) {
		systab = hpuxsysent;
		numsys = hpuxnsysent;
	}
#endif
	params = (caddr_t)frame.f_regs[SP] + NBPW;
	if (code == 0) {			/* indir */
		code = fuword(params);
		params += NBPW;
	}
	if (code >= numsys)
		callp = &systab[0];		/* indir (illegal) */
	else
		callp = &systab[code];
	if ((i = callp->sy_narg * sizeof (int)) &&
	    (error = copyin(params, (caddr_t)&args, (u_int)i))) {
#ifdef HPUXCOMPAT
		if (p->p_flag & SHPUX)
			error = bsdtohpuxerrno(error);
#endif
		frame.f_regs[D0] = (u_char) error;
		frame.f_sr |= PSL_C;	/* carry bit */
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
	rval[1] = frame.f_regs[D1];
#ifdef HPUXCOMPAT
	/* debug kludge */
	if (callp->sy_call == notimp)
		error = notimp(u.u_procp, args.i, rval, code, callp->sy_narg);
	else
#endif
	error = (*callp->sy_call)(u.u_procp, &args, rval);
	if (error == ERESTART)
		frame.f_pc = opc;
	else if (error != EJUSTRETURN) {
		if (error) {
#ifdef HPUXCOMPAT
			if (p->p_flag & SHPUX)
				error = bsdtohpuxerrno(error);
#endif
			frame.f_regs[D0] = (u_char) error;
			frame.f_sr |= PSL_C;	/* carry bit */
		} else {
			frame.f_regs[D0] = rval[0];
			frame.f_regs[D1] = rval[1];
			frame.f_sr &= ~PSL_C;
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
#include "sys/syscall.h"
	if (code != SYS_sigreturn && (i = CURSIG(p)))
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
		if (code != SYS_sigreturn && (i = CURSIG(p)))
			psig(i);
	}
	if (u.u_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &u.u_ru.ru_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks) {
#ifdef PROFTIMER
			extern int profscale;
			addupc(frame.f_pc, &u.u_prof, ticks * profscale);
#else
			addupc(frame.f_pc, &u.u_prof, ticks);
#endif
		}
	}
	curpri = p->p_pri;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSRET))
		ktrsysret(p->p_tracep, code, error, rval[0]);
#endif
}
