/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trap.c	7.5 (Berkeley) %G%
 *
 * from: $Header: trap.c,v 1.34 93/05/28 04:34:50 torek Exp $
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/kernel.h>
#include <sys/malloc.h>
#include <sys/resource.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/syslog.h>
#ifdef KTRACE
#include <sys/ktrace.h>
#endif

#include <vm/vm_kern.h>

#include <machine/cpu.h>
#include <machine/ctlreg.h>
#include <machine/frame.h>
#include <machine/trap.h>

#define	offsetof(s, f) ((int)&((s *)0)->f)

extern int cold;

int	rwindow_debug = 0;

/*
 * Initial FPU state is all registers == all 1s, everything else == all 0s.
 * This makes every floating point register a signalling NaN, with sign bit
 * set, no matter how it is interpreted.  Appendix N of the Sparc V8 document
 * seems to imply that we should do this, and it does make sense.
 */
struct	fpstate initfpstate = {
	{ ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0,
	  ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0, ~0 }
};

/*
 * There are more than 100 trap types, but most are unused.
 *
 * Trap type 0 is taken over as an `Asynchronous System Trap'.
 * This is left-over Vax emulation crap that should be fixed.
 */
static const char T[] = "trap";
const char *trap_type[] = {
	/* non-user vectors */
	"ast",			/* 0 */
	"text fault",		/* 1 */
	"illegal instruction",	/* 2 */
	"privileged instruction",/*3 */
	"fp disabled",		/* 4 */
	"window overflow",	/* 5 */
	"window underflow",	/* 6 */
	"alignment fault",	/* 7 */
	"fp exception",		/* 8 */
	"data fault",		/* 9 */
	"tag overflow",		/* 0a */
	T, T, T, T, T, T,	/* 0b..10 */
	"level 1 int",		/* 11 */
	"level 2 int",		/* 12 */
	"level 3 int",		/* 13 */
	"level 4 int",		/* 14 */
	"level 5 int",		/* 15 */
	"level 6 int",		/* 16 */
	"level 7 int",		/* 17 */
	"level 8 int",		/* 18 */
	"level 9 int",		/* 19 */
	"level 10 int",		/* 1a */
	"level 11 int",		/* 1b */
	"level 12 int",		/* 1c */
	"level 13 int",		/* 1d */
	"level 14 int",		/* 1e */
	"level 15 int",		/* 1f */
	T, T, T, T, T, T, T, T,	/* 20..27 */
	T, T, T, T, T, T, T, T,	/* 28..2f */
	T, T, T, T, T, T,	/* 30..35 */
	"cp disabled",		/* 36 */
	T,			/* 37 */
	T, T, T, T, T, T, T, T,	/* 38..3f */
	"cp exception",		/* 40 */
	T, T, T, T, T, T, T,	/* 41..47 */
	T, T, T, T, T, T, T, T,	/* 48..4f */
	T, T, T, T, T, T, T, T,	/* 50..57 */
	T, T, T, T, T, T, T, T,	/* 58..5f */
	T, T, T, T, T, T, T, T,	/* 60..67 */
	T, T, T, T, T, T, T, T,	/* 68..6f */
	T, T, T, T, T, T, T, T,	/* 70..77 */
	T, T, T, T, T, T, T, T,	/* 78..7f */

	/* user (software trap) vectors */
	"sun syscall",		/* 80 */
	"breakpoint",		/* 81 */
	"zero divide",		/* 82 */
	"flush windows",	/* 83 */
	"clean windows",	/* 84 */
	"range check",		/* 85 */
	"fix align",		/* 86 */
	"integer overflow",	/* 87 */
	"kgdb exec",		/* 88 */
	"syscall"		/* 89 */
};

#define	N_TRAP_TYPES	(sizeof trap_type / sizeof *trap_type)

/*
 * Define the code needed before returning to user mode, for
 * trap, mem_access_fault, and syscall.
 */
static inline void
userret(struct proc *p, int pc, u_quad_t oticks)
{
	int sig;

	/* take pending signals */
	while ((sig = CURSIG(p)) != 0)
		psig(sig);
	p->p_pri = p->p_usrpri;
	if (want_ast) {
		want_ast = 0;
		if (p->p_flag & SOWEUPC) {
			p->p_flag &= ~SOWEUPC;
			ADDUPROF(p);
		}
	}
	if (want_resched) {
		/*
		 * Since we are curproc, a clock interrupt could
		 * change our priority without changing run queues
		 * (the running process is not kept on a run queue).
		 * If this happened after we setrq ourselves but
		 * before we swtch()'ed, we might not be on the queue
		 * indicated by our priority.
		 */
		(void) splstatclock();
		setrq(p);
		p->p_stats->p_ru.ru_nivcsw++;
		swtch();
		(void) spl0();
		while ((sig = CURSIG(p)) != 0)
			psig(sig);
	}

	/*
	 * If profiling, charge recent system time to the trapped pc.
	 */
	if (p->p_flag & SPROFIL)
		addupc_task(p, pc, (int)(p->p_sticks - oticks));

	curpri = p->p_pri;
}

/*
 * If someone stole the FPU while we were away, do not enable it
 * on return.  This is not done in userret() above as it must follow
 * the ktrsysret() in syscall().  Actually, it is likely that the
 * ktrsysret should occur before the call to userret.
 */
static inline void share_fpu(struct proc *p, struct trapframe *tf) {
	if ((tf->tf_psr & PSR_EF) != 0 && fpproc != p)
		tf->tf_psr &= ~PSR_EF;
}

/*
 * Called from locore.s trap handling, for non-MMU-related traps.
 * (MMU-related traps go through mem_access_fault, below.)
 */
trap(type, psr, pc, tf)
	register unsigned type;
	register int psr, pc;
	register struct trapframe *tf;
{
	register struct proc *p;
	register struct pcb *pcb;
	register int n;
	u_quad_t sticks;

	/* This steps the PC over the trap. */
#define	ADVANCE (n = tf->tf_npc, tf->tf_pc = n, tf->tf_npc = n + 4)

	cnt.v_trap++;
	/*
	 * Generally, kernel traps cause a panic.  Any exceptions are
	 * handled early here.
	 */
	if (psr & PSR_PS) {
		/*
		 * Storing %fsr in cpu_attach will cause this trap
		 * even though the fpu has been enabled, if and only
		 * if there is no FPU.
		 */
		if (type == T_FPDISABLED && cold) {
			ADVANCE;
			return;
		}
		goto dopanic;
	}
	if ((p = curproc) == NULL)
		p = &proc0;
	sticks = p->p_sticks;
	pcb = &p->p_addr->u_pcb;
	p->p_md.md_tf = tf;	/* for ptrace/signals */

	switch (type) {

	default:
		if (type < 0x80) {
dopanic:
			printf("trap type 0x%x: pc=%x npc=%x psr=%b\n",
			    type, pc, tf->tf_npc, psr, PSR_BITS);
			panic(type < N_TRAP_TYPES ? trap_type[type] : T);
			/* NOTREACHED */
		}
		/* the following message is gratuitous */
		/* ... but leave it in until we find anything */
		printf("%s[%d]: unimplemented software trap 0x%x\n",
		    p->p_comm, p->p_pid, type);
		trapsignal(p, SIGILL, type);
		break;

	case T_AST:
		break;	/* the work is all in userret() */

	case T_ILLINST:
		trapsignal(p, SIGILL, 0);	/* XXX code?? */
		break;

	case T_PRIVINST:
		trapsignal(p, SIGILL, 0);	/* XXX code?? */
		break;

	case T_FPDISABLED: {
		register struct fpstate *fs = p->p_md.md_fpstate;

		if (fs == NULL) {
			fs = malloc(sizeof *fs, M_SUBPROC, M_WAITOK);
			*fs = initfpstate;
			p->p_md.md_fpstate = fs;
		}
		/*
		 * If we have not found an FPU, we have to emulate it.
		 */
		if (!foundfpu) {
#ifdef notyet
			fpu_emulate(p, tf, fs);
			break;
#else
			trapsignal(p, SIGFPE, 0);	/* XXX code?? */
			break;
#endif
		}
		/*
		 * We may have more FPEs stored up and/or ops queued.
		 * If they exist, handle them and get out.  Otherwise,
		 * resolve the FPU state, turn it on, and try again.
		 */
		if (fs->fs_qsize) {
			fpu_cleanup(p, fs);
			break;
		}
		if (fpproc != p) {		/* we do not have it */
			if (fpproc != NULL)	/* someone else had it */
				savefpstate(fpproc->p_md.md_fpstate);
			loadfpstate(fs);
			fpproc = p;		/* now we do have it */
		}
		tf->tf_psr |= PSR_EF;
		break;
	}

	case T_WINOF:
		if (rwindow_save(p))
			sigexit(p, SIGILL);
		break;

#define read_rw(src, dst) \
	copyin((caddr_t)(src), (caddr_t)(dst), sizeof(struct rwindow))

	case T_RWRET:
		/*
		 * T_RWRET is a window load needed in order to rett.
		 * It simply needs the window to which tf->tf_out[6]
		 * (%sp) points.  There are no user or saved windows now.
		 * Copy the one from %sp into pcb->pcb_rw[0] and set
		 * nsaved to -1.  If we decide to deliver a signal on
		 * our way out, we will clear nsaved.
		 */
if (pcb->pcb_uw || pcb->pcb_nsaved) panic("trap T_RWRET 1");
if (rwindow_debug)
printf("%s[%d]: rwindow: pcb<-stack: %x\n", p->p_comm, p->p_pid, tf->tf_out[6]);
		if (read_rw(tf->tf_out[6], &pcb->pcb_rw[0]))
			sigexit(p, SIGILL);
if (pcb->pcb_nsaved) panic("trap T_RWRET 2");
		pcb->pcb_nsaved = -1;		/* mark success */
		break;

	case T_WINUF:
		/*
		 * T_WINUF is a real window underflow, from a restore
		 * instruction.  It needs to have the contents of two
		 * windows---the one belonging to the restore instruction
		 * itself, which is at its %sp, and the one belonging to
		 * the window above, which is at its %fp or %i6---both
		 * in the pcb.  The restore's window may still be in
		 * the cpu; we need to force it out to the stack.
		 */
if (rwindow_debug)
printf("%s[%d]: rwindow: T_WINUF 0: pcb<-stack: %x\n",
p->p_comm, p->p_pid, tf->tf_out[6]);
		write_user_windows();
		if (rwindow_save(p) || read_rw(tf->tf_out[6], &pcb->pcb_rw[0]))
			sigexit(p, SIGILL);
if (rwindow_debug)
printf("%s[%d]: rwindow: T_WINUF 1: pcb<-stack: %x\n",
p->p_comm, p->p_pid, pcb->pcb_rw[0].rw_in[6]);
		if (read_rw(pcb->pcb_rw[0].rw_in[6], &pcb->pcb_rw[1]))
			sigexit(p, SIGILL);
if (pcb->pcb_nsaved) panic("trap T_WINUF");
		pcb->pcb_nsaved = -1;		/* mark success */
		break;

	case T_ALIGN:
		trapsignal(p, SIGBUS, 0);	/* XXX code?? */
		break;

	case T_FPE:
		/*
		 * Clean up after a floating point exception.
		 * fpu_cleanup can (and usually does) modify the
		 * state we save here, so we must `give up' the FPU
		 * chip context.  (The software and hardware states
		 * will not match once fpu_cleanup does its job, so
		 * we must not save again later.)
		 */
		if (p != fpproc)
			panic("fpe without being the FP user");
		savefpstate(p->p_md.md_fpstate);
		fpproc = NULL;
		/* tf->tf_psr &= ~PSR_EF; */	/* share_fpu will do this */
		fpu_cleanup(p, p->p_md.md_fpstate);
		/* fpu_cleanup posts signals if needed */
#if 0		/* ??? really never??? */
		ADVANCE;
#endif
		break;

	case T_TAGOF:
		trapsignal(p, SIGEMT, 0);	/* XXX code?? */
		break;

	case T_CPDISABLED:
		uprintf("coprocessor instruction\n");	/* XXX */
		trapsignal(p, SIGILL, 0);	/* XXX code?? */
		break;

	case T_BREAKPOINT:
		trapsignal(p, SIGTRAP, 0);
		break;

	case T_DIV0:
		ADVANCE;
		trapsignal(p, SIGFPE, FPE_INTDIV_TRAP);
		break;

	case T_FLUSHWIN:
		write_user_windows();
#ifdef probably_slower_since_this_is_usually_false
		if (pcb->pcb_nsaved && rwindow_save(p))
			sigexit(p, SIGILL);
#endif
		ADVANCE;
		break;

	case T_CLEANWIN:
		uprintf("T_CLEANWIN\n");	/* XXX */
		ADVANCE;
		break;

	case T_RANGECHECK:
		uprintf("T_RANGECHECK\n");	/* XXX */
		ADVANCE;
		trapsignal(p, SIGILL, 0);	/* XXX code?? */
		break;

	case T_FIXALIGN:
		uprintf("T_FIXALIGN\n");	/* XXX */
		ADVANCE;
		break;

	case T_INTOF:
		uprintf("T_INTOF\n");		/* XXX */
		ADVANCE;
		trapsignal(p, SIGFPE, FPE_INTOVF_TRAP);
		break;
	}
	userret(p, pc, sticks);
	share_fpu(p, tf);
#undef ADVANCE
}

/*
 * Save windows from PCB into user stack, and return 0.  This is used on
 * window overflow pseudo-traps (from locore.s, just before returning to
 * user mode) and when ptrace or sendsig needs a consistent state.
 * As a side effect, rwindow_save() always sets pcb_nsaved to 0,
 * clobbering the `underflow restore' indicator if it was -1.
 *
 * If the windows cannot be saved, pcb_nsaved is restored and we return -1.
 */
int
rwindow_save(p)
	register struct proc *p;
{
	register struct pcb *pcb = &p->p_addr->u_pcb;
	register struct rwindow *rw = &pcb->pcb_rw[0];
	register int i;

	i = pcb->pcb_nsaved;
	if (i < 0) {
		pcb->pcb_nsaved = 0;
		return (0);
	}
	if (i == 0)
		return (0);
if(rwindow_debug)
printf("%s[%d]: rwindow: pcb->stack:", p->p_comm, p->p_pid);
	do {
if(rwindow_debug)
printf(" %x", rw[1].rw_in[6]);
		if (copyout((caddr_t)rw, (caddr_t)rw[1].rw_in[6],
		    sizeof *rw))
			return (-1);
		rw++;
	} while (--i > 0);
if(rwindow_debug)
printf("\n");
	pcb->pcb_nsaved = 0;
	return (0);
}

/*
 * Kill user windows (before exec) by writing back to stack or pcb
 * and then erasing any pcb tracks.  Otherwise we might try to write
 * the registers into the new process after the exec.
 */
kill_user_windows(p)
	struct proc *p;
{

	write_user_windows();
	p->p_addr->u_pcb.pcb_nsaved = 0;
}

/*
 * Called from locore.s trap handling, for synchronous memory faults.
 *
 * This duplicates a lot of logic in trap() and perhaps should be
 * moved there; but the bus-error-register parameters are unique to
 * this routine.
 *
 * Since synchronous errors accumulate during prefetch, we can have
 * more than one `cause'.  But we do not care what the cause, here;
 * we just want to page in the page and try again.
 */
mem_access_fault(type, ser, v, pc, psr, tf)
	register unsigned type;
	register int ser;
	register u_int v;
	register int pc, psr;
	register struct trapframe *tf;
{
	register struct proc *p;
	register struct vmspace *vm;
	register vm_offset_t va;
	register int i, rv, sig = SIGBUS;
	vm_prot_t ftype;
	int onfault, mmucode;
	u_quad_t sticks;

	cnt.v_trap++;
	if ((p = curproc) == NULL)	/* safety check */
		p = &proc0;
	sticks = p->p_sticks;

	/*
	 * Figure out what to pass the VM code, and ignore the sva register
	 * value in v on text faults (text faults are always at pc).
	 * Kernel faults are somewhat different: text faults are always
	 * illegal, and data faults are extra complex.  User faults must
	 * set p->p_md.md_tf, in case we decide to deliver a signal.  Check
	 * for illegal virtual addresses early since those can induce more
	 * faults.
	 */
	if (type == T_TEXTFAULT)
		v = pc;
	i = (int)v >> PG_VSHIFT;
	if (i != 0 && i != -1)
		goto fault;
	ftype = ser & SER_WRITE ? VM_PROT_READ|VM_PROT_WRITE : VM_PROT_READ;
	va = trunc_page(v);
	if (psr & PSR_PS) {
		extern char Lfsbail[];
		if (type == T_TEXTFAULT) {
			(void) splhigh();
			printf("text fault: pc=%x ser=%b\n", pc, ser, SER_BITS);
			panic("kernel fault");
			/* NOTREACHED */
		}
		/*
		 * If this was an access that we shouldn't try to page in,
		 * resume at the fault handler without any action.
		 */
		if (p->p_addr && p->p_addr->u_pcb.pcb_onfault == Lfsbail)
			goto kfault;

		/*
		 * During autoconfiguration, faults are never OK unless
		 * pcb_onfault is set.  Once running normally we must allow
		 * exec() to cause copy-on-write faults to kernel addresses.
		 */
		if (cold)
			goto kfault;
		if (va >= KERNBASE) {
			if (vm_fault(kernel_map, va, ftype, 0) == KERN_SUCCESS)
				return;
			goto kfault;
		}
	} else
		p->p_md.md_tf = tf;

	/*
	 * mmu_pagein returns -1 if the page is already valid, in which
	 * case we have a hard fault; it returns 1 if it loads a segment
	 * that got bumped out via LRU replacement.
	 */
	vm = p->p_vmspace;
	rv = mmu_pagein(&vm->vm_pmap, va, ser & SER_WRITE ? PG_V|PG_W : PG_V);
	if (rv < 0)
		goto fault;
	if (rv > 0)
		goto out;

	/* alas! must call the horrible vm code */
	rv = vm_fault(&vm->vm_map, (vm_offset_t)va, ftype, FALSE);

	/*
	 * If this was a stack access we keep track of the maximum
	 * accessed stack size.  Also, if vm_fault gets a protection
	 * failure it is due to accessing the stack region outside
	 * the current limit and we need to reflect that as an access
	 * error.
	 */
	if ((caddr_t)va >= vm->vm_maxsaddr) {
		if (rv == KERN_SUCCESS) {
			unsigned nss = clrnd(btoc(USRSTACK - va));
			if (nss > vm->vm_ssize)
				vm->vm_ssize = nss;
		} else if (rv == KERN_PROTECTION_FAILURE)
			rv = KERN_INVALID_ADDRESS;
	}
	if (rv == KERN_SUCCESS) {
		/*
		 * pmap_enter() does not enter all requests made from
		 * vm_fault into the MMU (as that causes unnecessary
		 * entries for `wired' pages).  Instead, we call
		 * mmu_pagein here to make sure the new PTE gets installed.
		 */
		(void) mmu_pagein(&vm->vm_pmap, va, 0);
	} else {
		/*
		 * Pagein failed.  If doing copyin/out, return to onfault
		 * address.  Any other page fault in kernel, die; if user
		 * fault, deliver SIGBUS or SIGSEGV.
		 */
		if (rv != KERN_PROTECTION_FAILURE)
			sig = SIGSEGV;
fault:
		if (psr & PSR_PS) {
kfault:
			onfault = p->p_addr ?
			    (int)p->p_addr->u_pcb.pcb_onfault : 0;
			if (!onfault) {
				(void) splhigh();
				printf("data fault: pc=%x addr=%x ser=%b\n",
				    pc, v, ser, SER_BITS);
				panic("kernel fault");
				/* NOTREACHED */
			}
			tf->tf_pc = onfault;
			tf->tf_npc = onfault + 4;
			return;
		}
		trapsignal(p, sig, (u_int)v);
	}
out:
	if ((psr & PSR_PS) == 0) {
		userret(p, pc, sticks);
		share_fpu(p, tf);
	}
}

/*
 * System calls.  `pc' is just a copy of tf->tf_pc.
 *
 * Note that the things labelled `out' registers in the trapframe were the
 * `in' registers within the syscall trap code (because of the automatic
 * `save' effect of each trap).  They are, however, the %o registers of the
 * thing that made the system call, and are named that way here.
 *
 * The `suncompat' parameter actually only exists if COMPAT_SUNOS is defined.
 */
syscall(code, tf, pc, suncompat)
	register u_int code;
	register struct trapframe *tf;
	int pc, suncompat;
{
	register int i, nsys, *ap, nap;
	register struct sysent *callp;
	register struct proc *p;
	int error, new;
	struct args {
		int i[8];
	} args;
	int rval[2];
	u_quad_t sticks;
	extern int nsysent;
	extern struct pcb *cpcb;

	cnt.v_syscall++;
	p = curproc;
#ifdef DIAGNOSTIC
	if (tf->tf_psr & PSR_PS)
		panic("syscall");
	if (cpcb != &p->p_addr->u_pcb)
		panic("syscall cpcb/ppcb");
	if (tf != (struct trapframe *)((caddr_t)cpcb + UPAGES * NBPG) - 1)
		panic("syscall trapframe");
#endif
	sticks = p->p_sticks;
	p->p_md.md_tf = tf;
	new = code & (SYSCALL_G7RFLAG | SYSCALL_G2RFLAG);
	code &= ~(SYSCALL_G7RFLAG | SYSCALL_G2RFLAG);
#ifdef COMPAT_SUNOS
	if (suncompat) {
		extern int nsunsys;
		extern struct sysent sunsys[];

		callp = sunsys, nsys = nsunsys;
	} else
#endif
		callp = sysent, nsys = nsysent;

	/*
	 * The first six system call arguments are in the six %o registers.
	 * Any arguments beyond that are in the `argument extension' area
	 * of the user's stack frame (see <machine/frame.h>).
	 *
	 * Check for ``special'' codes that alter this, namely indir and
	 * __indir.  The latter takes a quad syscall number, so that other
	 * arguments are at their natural alignments.  Adjust the number
	 * of ``easy'' arguments as appropriate; we will copy the hard
	 * ones later as needed.
	 */
	ap = &tf->tf_out[0];
	nap = 6;
	switch (code) {

	case SYS_indir:
		code = *ap++;
		nap--;
		break;

	case SYS___indir:
#ifdef COMPAT_SUNOS
		if (suncompat)
			break;
#endif
		code = ap[_QUAD_LOWWORD];
		ap += 2;
		nap -= 2;
		break;

	}
	/* Callp currently points to indir, which returns ENOSYS. */
	if (code < nsys) {
		callp += code;
		i = callp->sy_narg;
		if (i > nap) {	/* usually false */
			if (i > 8)
				panic("syscall nargs");
			error = copyin((caddr_t)tf->tf_out[6] +
				    offsetof(struct frame, fr_argx),
			    (caddr_t)&args.i[nap], (i - nap) * sizeof(int));
			if (error) {
#ifdef KTRACE
				if (KTRPOINT(p, KTR_SYSCALL))
					ktrsyscall(p->p_tracep, code,
					    callp->sy_narg, args.i);
#endif
				goto bad;
			}
			i = nap;
		}
		copywords(ap, args.i, i * 4);
	}
	rval[0] = 0;
	rval[1] = tf->tf_out[1];
	error = (*callp->sy_call)(p, &args, rval);
	if (error == 0) {
		/*
		 * If fork succeeded and we are the child, our stack
		 * has moved and the pointer tf is no longer valid,
		 * and p is wrong.  Compute the new trapframe pointer.
		 * (The trap frame invariably resides at the
		 * tippity-top of the u. area.)
		 */
		p = curproc;
		tf = (struct trapframe *)
		    ((caddr_t)p->p_addr + UPAGES * NBPG - sizeof(*tf));
/* this is done earlier: */
/*		p->p_md.md_tf = tf; */
		tf->tf_out[0] = rval[0];
		tf->tf_out[1] = rval[1];
		if (new) {
			/* jmp %g2 (or %g7, deprecated) on success */
			i = tf->tf_global[new & SYSCALL_G2RFLAG ? 2 : 7];
			if (i & 3) {
				error = EINVAL;
				goto bad;
			}
		} else {
			/* old system call convention: clear C on success */
			tf->tf_psr &= ~PSR_C;	/* success */
			i = tf->tf_npc;
		}
		tf->tf_pc = i;
		tf->tf_npc = i + 4;
	} else if (error > 0 /*error != ERESTART && error != EJUSTRETURN*/) {
bad:
		tf->tf_out[0] = error;
		tf->tf_psr |= PSR_C;	/* fail */
		i = tf->tf_npc;
		tf->tf_pc = i;
		tf->tf_npc = i + 4;
	}
	/* else if (error == ERESTART || error == EJUSTRETURN) */
		/* nothing to do */
	userret(p, pc, sticks);
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSRET))
		ktrsysret(p->p_tracep, code, error, rval[0]);
#endif
	share_fpu(p, tf);
}
