/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: trap.c 1.28 89/09/25$
 *
 *	@(#)trap.c	7.7 (Berkeley) 6/25/90
 */

#include "cpu.h"
#include "psl.h"
#include "reg.h"
#include "pte.h"
#include "mtpr.h"

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "seg.h"
#include "trap.h"
#include "acct.h"
#include "kernel.h"
#include "vm.h"
#include "cmap.h"
#include "syslog.h"
#ifdef KTRACE
#include "ktrace.h"
#endif

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
		/*
		 * Could be caused by a page fault in one of the copy to/from
		 * user space routines.  If so, we will have a catch address.
		 */
		if (!u.u_pcb.pcb_onfault)
			goto dopanic;
		/* fall into ... */

	case T_MMUFLT+USER:	/* page fault */
/*
		printf("trap: T_MMUFLT pid %d, code %x, v %x, pc %x, ps %x\n",
		       p->p_pid, code, v, frame.f_pc, frame.f_sr);
*/
		if (v >= USRSTACK) {
			if (type == T_MMUFLT)
				goto copyfault;
			i = SIGSEGV;
			break;
		}
		ncode = code >> 16;
#if defined(HP330) || defined(HP360) || defined(HP370)
		/*
		 * Crudely map PMMU faults into HP MMU faults.
		 */
		if (mmutype != MMU_HP) {
			int ocode = ncode;
			ncode = 0;
			if (ocode & PMMU_WP)
				ncode |= MMU_WPF;
			else if (ocode & PMMU_INV) {
				if ((ocode & PMMU_LVLMASK) == 2)
					ncode |= MMU_PF;
				else 
					ncode |= MMU_PTF;
			}
			/*
			 * RMW cycle, must load ATC by hand
			 */
			else if ((code & (SSW_DF|SSW_RM)) == (SSW_DF|SSW_RM)) {
#ifdef DEBUG
				log(LOG_WARNING,
				    "RMW fault at %x: MMUSR %x SSW %x\n",
				    v, ocode, code & 0xFFFF);
#endif
				ploadw((caddr_t)v);
				return;
			}
			/*
			 * Fault with no fault bits, should indicate bad
			 * hardware but we see this on 340s using starbase
			 * sometimes (faults accessing catseye registers)
			 */
			else {
				log(LOG_WARNING,
				    "Bad PMMU fault at %x: MMUSR %x SSW %x\n",
				    v, ocode, code & 0xFFFF);
				return;
			}
#ifdef DEBUG
			if (mmudebug && mmudebug == p->p_pid)
				printf("MMU %d: v%x, os%x, ns%x\n",
				       p->p_pid, v, ocode, ncode);
#endif
		}
#endif
#ifdef DEBUG
		if ((ncode & (MMU_PTF|MMU_PF|MMU_WPF|MMU_FPE)) == 0) {
			printf("T_MMUFLT with no fault bits\n");
			goto dopanic;
		}
#endif
		if (ncode & MMU_PTF) {
#ifdef DEBUG
			/*
			 * NOTE: we use a u_int instead of an ste since the
			 * current compiler generates bogus code for some
			 * bitfield operations (i.e. attempts to access last
			 * word of a page as a longword causing fault).
			 */
			extern struct ste *vtoste();
			u_int *ste = (u_int *)vtoste(p, v);

			if (*ste & SG_V) {
				if (ncode & MMU_WPF) {
					printf("PTF|WPF...\n");
					if (type == T_MMUFLT)
						goto copyfault;
					i = SIGBUS;
					break;
				}
				printf("MMU_PTF with sg_v, ste@%x = %x\n",
				       ste, *ste);
				goto dopanic;
			}
#endif
#ifdef HPUXCOMPAT
			if (ISHPMMADDR(v)) {
				extern struct ste *vtoste();
				u_int *bste, *nste;

				bste = (u_int *)vtoste(p, HPMMBASEADDR(v));
				nste = (u_int *)vtoste(p, v);
				if ((*bste & SG_V) && *nste == SG_NV) {
					*nste = *bste;
					TBIAU();
					return;
				}
			}
#endif
		growit:
			if (type == T_MMUFLT)
				goto copyfault;
			if (grow((unsigned)frame.f_regs[SP]) || grow(v))
				goto out;
			i = SIGSEGV;
			break;
		}
#ifdef HPUXCOMPAT
		if (ISHPMMADDR(v)) {
			TBIS(v);
			v = HPMMBASEADDR(v);
		}
#endif
		/*
		 * NOTE: WPF without PG_V is possible
		 * (e.g. attempt to write shared text which is paged out)
		 */
		if (ncode & MMU_WPF) {
#ifdef DEBUG
			extern struct ste *vtoste();
			u_int *ste = (u_int *)vtoste(p, v);

			if (!(*ste & SG_V)) {
				printf("MMU_WPF without sg_v, ste@%x = %x\n",
				       ste, *ste);
				goto dopanic;
			}
#endif
			if (type == T_MMUFLT)
				goto copyfault;
			i = SIGBUS;
			break;
		}
		if (ncode & MMU_PF) {
			register u_int vp;
#ifdef DEBUG
			extern struct ste *vtoste();
			u_int *ste = (u_int *)vtoste(p, v);
			struct pte *pte;

			if (!(*ste & SG_V)) {
				printf("MMU_PF without sg_v, ste@%x = %x\n",
				       ste, *ste);
				goto dopanic;
			}
#endif
			vp = btop(v);
			if (vp >= dptov(p, p->p_dsize) &&
			    vp < sptov(p, p->p_ssize-1))
				goto growit;
#ifdef DEBUG
			pte = vtopte(p, vp);
			if (*(u_int *)pte & PG_V) {
				printf("MMU_PF with pg_v, pte = %x\n",
				       *(u_int *)pte);
				goto dopanic;
			}
#endif
			pagein(v, 0);
			if (type == T_MMUFLT)
				return;
			goto out;
		}
#ifdef DEBUG
		printf("T_MMUFLT: unrecognized scenerio\n");
		goto dopanic;
#endif
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
#include "syscall.h"
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
