/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department, Ralph Campbell, Sony Corp. and Kazumasa Utashiro
 * of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: trap.c 1.32 91/04/06$
 *
 *	@(#)trap.c	7.5 (Berkeley) %G%
 */

#include <machine/fix_machine_type.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/kernel.h>
#include <sys/signalvar.h>
#include <sys/syscall.h>
#include <sys/user.h>
#include <sys/buf.h>
#ifdef KTRACE
#include <sys/ktrace.h>
#endif
#include <net/netisr.h>

#include <machine/trap.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/cpu.h>
#include <machine/pte.h>
#include <machine/mips_opcode.h>
#include <machine/adrsmap.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <vm/vm_page.h>

#include "lp.h"
#include "bm.h"
#include "ms.h"
#include "en.h"
#include <news3400/hbdev/dmac_0448.h>
#include <news3400/sio/scc.h>

/*
 * This is a kludge to allow X windows to work.
 */
#undef X_KLUGE

#ifdef X_KLUGE
#define USER_MAP_ADDR	0x4000
#define NPTES 300
static pt_entry_t UserMapPtes[NPTES];
static unsigned nUserMapPtes;
static pid_t UserMapPid;
#endif

struct	proc *machFPCurProcPtr;		/* pointer to last proc to use FP */

extern void MachKernGenException();
extern void MachUserGenException();
extern void MachKernIntr();
extern void MachUserIntr();
extern void MachTLBModException();
extern void MachTLBMissException();
static void MemErrorInterrupt();
extern unsigned MachEmulateBranch();

void (*machExceptionTable[])() = {
/*
 * The kernel exception handlers.
 */
	MachKernIntr,			/* external interrupt */
	MachKernGenException,		/* TLB modification */
	MachTLBMissException,		/* TLB miss (load or instr. fetch) */
	MachTLBMissException,		/* TLB miss (store) */
	MachKernGenException,		/* address error (load or I-fetch) */
	MachKernGenException,		/* address error (store) */
	MachKernGenException,		/* bus error (I-fetch) */
	MachKernGenException,		/* bus error (load or store) */
	MachKernGenException,		/* system call */
	MachKernGenException,		/* breakpoint */
	MachKernGenException,		/* reserved instruction */
	MachKernGenException,		/* coprocessor unusable */
	MachKernGenException,		/* arithmetic overflow */
	MachKernGenException,		/* reserved */
	MachKernGenException,		/* reserved */
	MachKernGenException,		/* reserved */
/*
 * The user exception handlers.
 */
	MachUserIntr,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
	MachUserGenException,
};

char	*trap_type[] = {
	"external interrupt",
	"TLB modification",
	"TLB miss (load or instr. fetch)",
	"TLB miss (store)",
	"address error (load or I-fetch)",
	"address error (store)",
	"bus error (I-fetch)",
	"bus error (load or store)",
	"system call",
	"breakpoint",
	"reserved instruction",
	"coprocessor unusable",
	"arithmetic overflow",
	"reserved 13",
	"reserved 14",
	"reserved 15",
};

#ifdef DEBUG
#define TRAPSIZE	10
struct trapdebug {		/* trap history buffer for debugging */
	u_int	status;
	u_int	cause;
	u_int	vadr;
	u_int	pc;
	u_int	ra;
	u_int	code;
} trapdebug[TRAPSIZE], *trp = trapdebug;
#endif

/*
 * Handle an exception.
 * Called from MachKernGenException() or MachUserGenException()
 * when a processor trap occurs.
 * In the case of a kernel trap, we return the pc where to resume if
 * ((struct pcb *)UADDR)->pcb_onfault is set, otherwise, return old pc.
 */
unsigned
trap(statusReg, causeReg, vadr, pc, args)
	unsigned statusReg;	/* status register at time of the exception */
	unsigned causeReg;	/* cause register at time of exception */
	unsigned vadr;		/* address (if any) the fault occured on */
	unsigned pc;		/* program counter where to continue */
{
	register int type, i;
	unsigned ucode = 0;
	register struct proc *p = curproc;
	u_quad_t sticks;
	vm_prot_t ftype;
	extern unsigned onfault_table[];

#ifdef DEBUG
	trp->status = statusReg;
	trp->cause = causeReg;
	trp->vadr = vadr;
	trp->pc = pc;
	trp->ra = !USERMODE(statusReg) ? ((int *)&args)[19] :
		p->p_md.md_regs[RA];
	trp->code = 0;
	if (++trp == &trapdebug[TRAPSIZE])
		trp = trapdebug;
#endif

	cnt.v_trap++;
	type = (causeReg & MACH_CR_EXC_CODE) >> MACH_CR_EXC_CODE_SHIFT;
	if (USERMODE(statusReg)) {
		type |= T_USER;
		sticks = p->p_sticks;
	}

	/*
	 * Enable hardware interrupts if they were on before.
	 * We only respond to software interrupts when returning to user mode.
	 */
	if (statusReg & MACH_SR_INT_ENA_PREV)
		splx((statusReg & MACH_HARD_INT_MASK) | MACH_SR_INT_ENA_CUR);

	switch (type) {
	case T_TLB_MOD:
		/* check for kernel address */
		if ((int)vadr < 0) {
			register pt_entry_t *pte;
			register unsigned entry;
#ifndef ATTR
			register vm_offset_t pa;
#endif

			pte = kvtopte(vadr);
			entry = pte->pt_entry;
			if (entry & PG_RO) {
				/* write to read only page in the kernel */
				ftype = VM_PROT_WRITE;
				goto kernel_fault;
			}
			entry |= PG_M;
			pte->pt_entry = entry;
			vadr &= PG_FRAME;
			printf("trap: TLBupdate hi %x lo %x i %x\n", vadr,
				entry, MachTLBUpdate(vadr, entry)); /* XXX */
#ifdef ATTR
			pmap_attributes[atop(entry - KERNBASE)] |= PMAP_ATTR_MOD;
#else
			pa = entry & PG_FRAME;
			if (!IS_VM_PHYSADDR(pa))
				panic("trap: kmod");
			PHYS_TO_VM_PAGE(pa)->clean = FALSE;
#endif
			return (pc);
		}
		/* FALLTHROUGH */

	case T_TLB_MOD+T_USER:
	    {
		pmap_hash_t hp;
#ifndef ATTR
		vm_offset_t pa;
#endif
#ifdef DIAGNOSTIC
		extern pmap_hash_t zero_pmap_hash;
		extern pmap_t cur_pmap;

		if (cur_pmap->pm_hash == zero_pmap_hash)
			panic("tlbmod");
#endif
		hp = &((pmap_hash_t)PMAP_HASH_UADDR)[PMAP_HASH(vadr)];
		if (((hp->pmh_pte[0].high ^ vadr) & ~PGOFSET) == 0)
			i = 0;
		else if (((hp->pmh_pte[1].high ^ vadr) & ~PGOFSET) == 0)
			i = 1;
		else
			panic("trap: tlb umod not found");
		if (hp->pmh_pte[i].low & PG_RO) {
			ftype = VM_PROT_WRITE;
			goto dofault;
		}
		hp->pmh_pte[i].low |= PG_M;
		printf("trap: TLBupdate hi %x lo %x i %x\n",
			hp->pmh_pte[i].high, hp->pmh_pte[i].low,
			MachTLBUpdate(hp->pmh_pte[i].high, hp->pmh_pte[i].low)); /* XXX */
#ifdef ATTR
		pmap_attributes[atop(hp->pmh_pte[i].low - KERNBASE)] |=
			PMAP_ATTR_MOD;
#else
		pa = hp->pmh_pte[i].low & PG_FRAME;
		if (!IS_VM_PHYSADDR(pa))
			panic("trap: umod");
		PHYS_TO_VM_PAGE(pa)->clean = FALSE;
#endif
		if (!USERMODE(statusReg))
			return (pc);
		goto out;
	    }

	case T_TLB_LD_MISS:
	case T_TLB_ST_MISS:
		ftype = (type == T_TLB_ST_MISS) ? VM_PROT_WRITE : VM_PROT_READ;
		/* check for kernel address */
		if ((int)vadr < 0) {
			register vm_offset_t va;
			int rv;

		kernel_fault:
			va = trunc_page((vm_offset_t)vadr);
			rv = vm_fault(kernel_map, va, ftype, FALSE);
			if (rv == KERN_SUCCESS)
				return (pc);
			if (i = ((struct pcb *)UADDR)->pcb_onfault) {
				((struct pcb *)UADDR)->pcb_onfault = 0;
				return (onfault_table[i]);
			}
			goto err;
		}
		/* check for fuswintr() or suswintr() getting a page fault */
		if ((i = ((struct pcb *)UADDR)->pcb_onfault) == 4)
			return (onfault_table[i]);
		goto dofault;

	case T_TLB_LD_MISS+T_USER:
		ftype = VM_PROT_READ;
		goto dofault;

	case T_TLB_ST_MISS+T_USER:
		ftype = VM_PROT_WRITE;
	dofault:
	    {
		register vm_offset_t va;
		register struct vmspace *vm = p->p_vmspace;
		register vm_map_t map = &vm->vm_map;
		int rv;

#ifdef X_KLUGE
		if (p->p_pid == UserMapPid &&
		    (va = pmax_btop(vadr - USER_MAP_ADDR)) < nUserMapPtes) {
			register pt_entry_t *pte;

			pte = &UserMapPtes[va];
			MachTLBWriteRandom((vadr & PG_FRAME) |
				(vm->vm_pmap.pm_tlbpid << VMMACH_TLB_PID_SHIFT),
				pte->pt_entry);
			return (pc);
		}
#endif
		va = trunc_page((vm_offset_t)vadr);
		rv = vm_fault(map, va, ftype, FALSE);
		if (rv != KERN_SUCCESS) {
			printf("vm_fault(%x, %x, %x, 0) -> %x ADR %x PC %x RA %x\n",
				map, va, ftype, rv, vadr, pc,
				!USERMODE(statusReg) ? ((int *)&args)[19] :
					p->p_md.md_regs[RA]); /* XXX */
			printf("\tpid %d %s PC %x RA %x\n", p->p_pid,
				p->p_comm, p->p_md.md_regs[PC],
				p->p_md.md_regs[RA]); /* XXX */
#ifdef DEBUG
			trapDump("vm_fault");
#endif
		}
		/*
		 * If this was a stack access we keep track of the maximum
		 * accessed stack size.  Also, if vm_fault gets a protection
		 * failure it is due to accessing the stack region outside
		 * the current limit and we need to reflect that as an access
		 * error.
		 */
		if ((caddr_t)va >= vm->vm_maxsaddr) {
			if (rv == KERN_SUCCESS) {
				unsigned nss;

				nss = clrnd(btoc(USRSTACK-(unsigned)va));
				if (nss > vm->vm_ssize)
					vm->vm_ssize = nss;
			} else if (rv == KERN_PROTECTION_FAILURE)
				rv = KERN_INVALID_ADDRESS;
		}
		if (rv == KERN_SUCCESS) {
			if (!USERMODE(statusReg))
				return (pc);
			goto out;
		}
		if (!USERMODE(statusReg)) {
			if (i = ((struct pcb *)UADDR)->pcb_onfault) {
				((struct pcb *)UADDR)->pcb_onfault = 0;
				return (onfault_table[i]);
			}
			goto err;
		}
		ucode = vadr;
		i = (rv == KERN_PROTECTION_FAILURE) ? SIGBUS : SIGSEGV;
		break;
	    }

	case T_ADDR_ERR_LD+T_USER:	/* misaligned or kseg access */
		if (vadr == KERNBASE) {
			struct args {
				int	i[1];
			} args;
			int rval[2];

			/*
			 * Assume a signal handler is trying to return
			 * (see sendsig() and sigreturn()). We have to
			 * pop the sigframe struct to get the address of
			 * the sigcontext.
			 */
			args.i[0] = p->p_md.md_regs[SP] + 4 * sizeof(int);
			(void) sigreturn(curproc, &args, rval);
			goto out;
		}
		/* FALLTHROUGH */

	case T_ADDR_ERR_ST+T_USER:	/* misaligned or kseg access */
	case T_BUS_ERR_IFETCH+T_USER:	/* BERR asserted to cpu */
	case T_BUS_ERR_LD_ST+T_USER:	/* BERR asserted to cpu */
		i = SIGSEGV;
		break;

	case T_SYSCALL+T_USER:
	    {
		register int *locr0 = p->p_md.md_regs;
		register struct sysent *callp;
		unsigned int code;
		int numsys;
		struct args {
			int i[8];
		} args;
		int rval[2];
		struct sysent *systab;
		extern int nsysent;

		cnt.v_syscall++;
		/* compute next PC after syscall instruction */
		if ((int)causeReg < 0)
			locr0[PC] = MachEmulateBranch(locr0, pc, 0, 0);
		else
			locr0[PC] += 4;
		systab = sysent;
		numsys = nsysent;
		code = locr0[V0];
#ifdef COMPAT_NEWSOS
		if (code >= 1000)
			code -= 1000;			/* too easy */
#endif
		switch (code) {
		case SYS_indir:
			/*
			 * Code is first argument, followed by actual args.
			 */
			code = locr0[A0];
#ifdef COMPAT_NEWSOS
			if (code >= 1000)
				code -= 1000;		/* too easy */
#endif
			if (code >= numsys)
				callp = &systab[SYS_indir]; /* (illegal) */
			else
				callp = &systab[code];
			i = callp->sy_narg;
			args.i[0] = locr0[A1];
			args.i[1] = locr0[A2];
			args.i[2] = locr0[A3];
			if (i > 3) {
				i = copyin((caddr_t)(locr0[SP] +
						4 * sizeof(int)),
					(caddr_t)&args.i[3],
					(u_int)(i - 3) * sizeof(int));
				if (i) {
					locr0[V0] = i;
					locr0[A3] = 1;
#ifdef KTRACE
					if (KTRPOINT(p, KTR_SYSCALL))
						ktrsyscall(p->p_tracep, code,
							callp->sy_narg, args.i);
#endif
					goto done;
				}
			}
			break;

		case SYS___indir:
			/*
			 * Like indir, but code is a quad, so as to maintain
			 * quad alignment for the rest of the arguments.
			 */
			code = locr0[A0 + _QUAD_LOWWORD];
			if (code >= numsys)
				callp = &systab[SYS_indir]; /* (illegal) */
			else
				callp = &systab[code];
			i = callp->sy_narg;
			args.i[0] = locr0[A2];
			args.i[1] = locr0[A3];
			if (i > 2) {
				i = copyin((caddr_t)(locr0[SP] +
						4 * sizeof(int)),
					(caddr_t)&args.i[2],
					(u_int)(i - 2) * sizeof(int));
				if (i) {
					locr0[V0] = i;
					locr0[A3] = 1;
#ifdef KTRACE
					if (KTRPOINT(p, KTR_SYSCALL))
						ktrsyscall(p->p_tracep, code,
							callp->sy_narg, args.i);
#endif
					goto done;
				}
			}
			break;

		default:
			if (code >= numsys)
				callp = &systab[SYS_indir]; /* (illegal) */
			else
				callp = &systab[code];
			i = callp->sy_narg;
			args.i[0] = locr0[A0];
			args.i[1] = locr0[A1];
			args.i[2] = locr0[A2];
			args.i[3] = locr0[A3];
			if (i > 4) {
				i = copyin((caddr_t)(locr0[SP] +
						4 * sizeof(int)),
					(caddr_t)&args.i[4],
					(u_int)(i - 4) * sizeof(int));
				if (i) {
					locr0[V0] = i;
					locr0[A3] = 1;
#ifdef KTRACE
					if (KTRPOINT(p, KTR_SYSCALL))
						ktrsyscall(p->p_tracep, code,
							callp->sy_narg, args.i);
#endif
					goto done;
				}
			}
		}
#ifdef KTRACE
		if (KTRPOINT(p, KTR_SYSCALL))
			ktrsyscall(p->p_tracep, code, callp->sy_narg, args.i);
#endif
		rval[0] = 0;
		rval[1] = locr0[V1];
#ifdef DEBUG
		if (trp == trapdebug)
			trapdebug[TRAPSIZE - 1].code = code;
		else
			trp[-1].code = code;
#endif
#ifdef COMPAT_NEWSOS
	/* 151 = setenvp, 152 = sysnews, 162 = getdomainname KU:XXX */
	if (code == 151 || code == 152 || code == 162)
		i = 0;
	else
#endif
		i = (*callp->sy_call)(p, &args, rval);
if(i==EINVAL)
printf("EINVAL: pid=%d, code=%d\n", p->p_pid, code);
		/*
		 * Reinitialize proc pointer `p' as it may be different
		 * if this is a child returning from fork syscall.
		 */
		p = curproc;
		locr0 = p->p_md.md_regs;
#ifdef DEBUG
		{ int s;
		s = splhigh();
		trp->status = statusReg;
		trp->cause = causeReg;
		trp->vadr = locr0[SP];
		trp->pc = locr0[PC];
		trp->ra = locr0[RA];
		trp->code = -code;
		if (++trp == &trapdebug[TRAPSIZE])
			trp = trapdebug;
		splx(s);
		}
#endif
		if (i == ERESTART)
			locr0[PC] = pc;
		else if (i != EJUSTRETURN) {
			if (i) {
				locr0[V0] = i;
				locr0[A3] = 1;
			} else {
				locr0[V0] = rval[0];
				locr0[V1] = rval[1];
				locr0[A3] = 0;
			}
		}
		/* else if (i == EJUSTRETURN) */
			/* nothing to do */
	done:
#ifdef KTRACE
		if (KTRPOINT(p, KTR_SYSRET))
			ktrsysret(p->p_tracep, code, i, rval[0]);
#endif

		goto out;
	    }

	case T_BREAK+T_USER:
	    {
		register unsigned va, instr;

		/* compute address of break instruction */
		va = pc;
		if ((int)causeReg < 0)
			va += 4;

		/* read break instruction */
		instr = fuiword((caddr_t)va);
#ifdef KADB
		if (instr == MACH_BREAK_BRKPT || instr == MACH_BREAK_SSTEP)
			goto err;
#endif
		if (p->p_md.md_ss_addr != va || instr != MACH_BREAK_SSTEP) {
			i = SIGTRAP;
			break;
		}

		/* restore original instruction and clear BP  */
		i = suiword((caddr_t)va, p->p_md.md_ss_instr);
		if (i < 0) {
			vm_offset_t sa, ea;
			int rv;

			sa = trunc_page((vm_offset_t)va);
			ea = round_page((vm_offset_t)va+sizeof(int)-1);
			rv = vm_map_protect(&p->p_vmspace->vm_map, sa, ea,
				VM_PROT_DEFAULT, FALSE);
			if (rv == KERN_SUCCESS) {
				i = suiword((caddr_t)va, p->p_md.md_ss_instr);
				(void) vm_map_protect(&p->p_vmspace->vm_map,
					sa, ea, VM_PROT_READ|VM_PROT_EXECUTE,
					FALSE);
			}
		}
		if (i < 0) {
			i = SIGTRAP;
			break;
		}
		p->p_md.md_ss_addr = 0;
		goto out;
	    }

	case T_RES_INST+T_USER:
		i = SIGILL;
		break;

	case T_COP_UNUSABLE+T_USER:
		if ((causeReg & MACH_CR_COP_ERR) != 0x10000000) {
			i = SIGILL;	/* only FPU instructions allowed */
			break;
		}
		MachSwitchFPState(machFPCurProcPtr, p->p_md.md_regs);
		machFPCurProcPtr = p;
		p->p_md.md_regs[PS] |= MACH_SR_COP_1_BIT;
		p->p_md.md_flags |= MDP_FPUSED;
		goto out;

	case T_OVFLOW+T_USER:
		i = SIGFPE;
		break;

	case T_ADDR_ERR_LD:	/* misaligned access */
	case T_ADDR_ERR_ST:	/* misaligned access */
	case T_BUS_ERR_LD_ST:	/* BERR asserted to cpu */
		if (i = ((struct pcb *)UADDR)->pcb_onfault) {
			((struct pcb *)UADDR)->pcb_onfault = 0;
			return (onfault_table[i]);
		}
		/* FALLTHROUGH */

	default:
	err:
#ifdef KADB
	    {
		extern struct pcb kdbpcb;

		if (USERMODE(statusReg))
			kdbpcb = p->p_addr->u_pcb;
		else {
			kdbpcb.pcb_regs[ZERO] = 0;
			kdbpcb.pcb_regs[AST] = ((int *)&args)[2];
			kdbpcb.pcb_regs[V0] = ((int *)&args)[3];
			kdbpcb.pcb_regs[V1] = ((int *)&args)[4];
			kdbpcb.pcb_regs[A0] = ((int *)&args)[5];
			kdbpcb.pcb_regs[A1] = ((int *)&args)[6];
			kdbpcb.pcb_regs[A2] = ((int *)&args)[7];
			kdbpcb.pcb_regs[A3] = ((int *)&args)[8];
			kdbpcb.pcb_regs[T0] = ((int *)&args)[9];
			kdbpcb.pcb_regs[T1] = ((int *)&args)[10];
			kdbpcb.pcb_regs[T2] = ((int *)&args)[11];
			kdbpcb.pcb_regs[T3] = ((int *)&args)[12];
			kdbpcb.pcb_regs[T4] = ((int *)&args)[13];
			kdbpcb.pcb_regs[T5] = ((int *)&args)[14];
			kdbpcb.pcb_regs[T6] = ((int *)&args)[15];
			kdbpcb.pcb_regs[T7] = ((int *)&args)[16];
			kdbpcb.pcb_regs[T8] = ((int *)&args)[17];
			kdbpcb.pcb_regs[T9] = ((int *)&args)[18];
			kdbpcb.pcb_regs[RA] = ((int *)&args)[19];
			kdbpcb.pcb_regs[MULLO] = ((int *)&args)[21];
			kdbpcb.pcb_regs[MULHI] = ((int *)&args)[22];
			kdbpcb.pcb_regs[PC] = pc;
			kdbpcb.pcb_regs[SR] = statusReg;
			bzero((caddr_t)&kdbpcb.pcb_regs[F0], 33 * sizeof(int));
		}
		if (kdb(causeReg, vadr, p, !USERMODE(statusReg)))
			return (kdbpcb.pcb_regs[PC]);
	    }
#else
#ifdef DEBUG
		trapDump("trap");
#endif
#endif
		panic("trap");
	}
	printf("trap: pid %d %s sig %d adr %x pc %x ra %x\n", p->p_pid,
		p->p_comm, i, vadr, pc, p->p_md.md_regs[RA]); /* XXX */
	trapsignal(p, i, ucode);
out:
	/*
	 * Note: we should only get here if returning to user mode.
	 */
	/* take pending signals */
	while ((i = CURSIG(p)) != 0)
		psig(i);
	p->p_pri = p->p_usrpri;
	astpending = 0;
	if (want_resched) {
		int s;

		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		s = splstatclock();
		setrq(p);
		p->p_stats->p_ru.ru_nivcsw++;
		swtch();
		splx(s);
		while ((i = CURSIG(p)) != 0)
			psig(i);
	}
	/*
	 * If profiling, charge system time to the trapped pc.
	 */
	if (p->p_flag & SPROFIL)
		addupc_task(p, pc, (int)(p->p_sticks - sticks));
	curpri = p->p_pri;
	return (pc);
}

/*
 * Handle an interrupt.
 * Called from MachKernIntr() or MachUserIntr()
 * Note: curproc might be NULL.
 */
interrupt(statusReg, causeReg, pc)
	unsigned statusReg;	/* status register at time of the exception */
	unsigned causeReg;	/* cause register at time of exception */
	unsigned pc;		/* program counter where to continue */
{
	register unsigned mask;
	struct clockframe cf;

#ifdef DEBUG
	trp->status = statusReg;
	trp->cause = causeReg;
	trp->vadr = 0;
	trp->pc = pc;
	trp->ra = 0;
	trp->code = 0;
	if (++trp == &trapdebug[TRAPSIZE])
		trp = trapdebug;
#endif

	mask = causeReg & statusReg;	/* pending interrupts & enable mask */
#ifndef NOPRIORITY
	if (mask & MACH_INT_MASK_5) {		/* level 5 interrupt */
		splx((MACH_SPL_MASK_8 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		printf("level 5 interrupt: PC %x CR %x SR %x\n",
			pc, causeReg, statusReg);
		causeReg &= ~MACH_INT_MASK_5;
	}
	if (mask & MACH_INT_MASK_4) {		/* level 4 interrupt */
		/*
		 * asynchronous bus error
		 */
		splx((MACH_SPL_MASK_7 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		printf("level 4 interrupt: PC %x CR %x SR %x\n",
			pc, causeReg, statusReg);
		*(char *)INTCLR0 = INTCLR0_BERR;
		causeReg &= ~MACH_INT_MASK_4;
	}
	if (mask & MACH_INT_MASK_3) {		/* level 3 interrupt */
		/*
		 * fp error
		 */
		splx((MACH_SPL_MASK_6 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		if (!USERMODE(statusReg)) {
#ifdef DEBUG
			trapDump("fpintr");
#else
			printf("FPU interrupt: PC %x CR %x SR %x\n",
				pc, causeReg, statusReg);
#endif
		} else
			MachFPInterrupt(statusReg, causeReg, pc);
		causeReg &= ~MACH_INT_MASK_3;
	}
	if (mask & MACH_INT_MASK_2) {		/* level 2 interrupt */
		register int stat;

		splx((MACH_SPL_MASK_5 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		stat = *(volatile u_char *)INTST0;
		if (stat & INTST0_TIMINT) {	/* timer */
			static int led_count = 0;

			*(volatile u_char *)INTCLR0 = INTCLR0_TIMINT;
			cf.pc = pc;
			cf.sr = statusReg;
			hardclock(&cf);
			if (++led_count > hz) {
				led_count = 0;
				*(volatile u_char *)DEBUG_PORT ^= DP_LED1;
			}
		}
#if NBM > 0
		if (stat & INTST0_KBDINT)	/* keyboard */
			kbm_rint(SCC_KEYBOARD);
#endif
#if NMS > 0
		if (stat & INTST0_MSINT)	/* mouse */
			kbm_rint(SCC_MOUSE);
#endif
		causeReg &= ~MACH_INT_MASK_2;
	}
	if (mask & MACH_INT_MASK_1) {		/* level 1 interrupt */
		splx((MACH_SPL_MASK_4 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		level1_intr();
		causeReg &= ~MACH_INT_MASK_1;
	}
	if (mask & MACH_INT_MASK_0) {		/* level 0 interrupt */
		splx((MACH_SPL_MASK_3 & ~causeReg) | MACH_SR_INT_ENA_CUR);
		level0_intr();
		causeReg &= ~MACH_INT_MASK_0;
	}
	splx((MACH_SPL_MASK_3 & ~causeReg) | MACH_SR_INT_ENA_CUR);
#else /* NOPRIORITY */
	/* handle clock interrupts ASAP */
	if (mask & MACH_INT_MASK_2) {		/* level 2 interrupt */
		register int stat;

		stat = *(volatile u_char *)INTST0;
		if (stat & INTST0_TIMINT) {	/* timer */
			static int led_count = 0;

			*(volatile u_char *)INTCLR0 = INTCLR0_TIMINT;
			cf.pc = pc;
			cf.sr = statusReg;
			hardclock(&cf);
			if (++led_count > hz) {
				led_count = 0;
				*(volatile u_char *)DEBUG_PORT ^= DP_LED1;
			}
		}
#if NBM > 0
		if (stat & INTST0_KBDINT)	/* keyboard */
			kbm_rint(SCC_KEYBOARD);
#endif
#if NMS > 0
		if (stat & INTST0_MSINT)	/* mouse */
			kbm_rint(SCC_MOUSE);
#endif
		causeReg &= ~MACH_INT_MASK_2;	/* reenable clock interrupts */
	}
	/*
	 * Enable hardware interrupts which were enabled but not pending.
	 * We only respond to software interrupts when returning to spl0.
	 */
	splx((statusReg & ~causeReg & MACH_HARD_INT_MASK)|MACH_SR_INT_ENA_CUR);

	if (mask & MACH_INT_MASK_5) {		/* level 5 interrupt */
		printf("level 5 interrupt: PC %x CR %x SR %x\n",
			pc, causeReg, statusReg);
		;
	}
	if (mask & MACH_INT_MASK_4) {		/* level 4 interrupt */
		/*
		 * asynchronous bus error
		 */
		printf("level 4 interrupt: PC %x CR %x SR %x\n",
			pc, causeReg, statusReg);
		*(char *)INTCLR0 = INTCLR0_BERR;
	}
	if (mask & MACH_INT_MASK_3) {		/* level 3 interrupt */
		/*
		 * fp error
		 */
		if (!USERMODE(statusReg)) {
#ifdef DEBUG
			trapDump("fpintr");
#else
			printf("FPU interrupt: PC %x CR %x SR %x\n",
				pc, causeReg, statusReg);
#endif
		} else
			MachFPInterrupt(statusReg, causeReg, pc);
	}
	if (mask & MACH_INT_MASK_1)		/* level 1 interrupt */
		level1_intr();
	if (mask & MACH_INT_MASK_0)		/* level 0 interrupt */
		level0_intr();
#endif /* NOPRIORITY */
	if (mask & MACH_SOFT_INT_MASK_0) {
		struct clockframe cf;

		clearsoftclock();
		cnt.v_soft++;
		cf.pc = pc;
		cf.sr = statusReg;
		softclock();
	}
	/* process network interrupt if we trapped or will very soon */
	if ((mask & MACH_SOFT_INT_MASK_1) ||
	    netisr && (statusReg & MACH_SOFT_INT_MASK_1)) {
		clearsoftnet();
		cnt.v_soft++;
#ifdef INET
		if (netisr & (1 << NETISR_ARP)) {
			netisr &= ~(1 << NETISR_ARP);
			arpintr();
		}
		if (netisr & (1 << NETISR_IP)) {
			netisr &= ~(1 << NETISR_IP);
			ipintr();
		}
#endif
#ifdef NS
		if (netisr & (1 << NETISR_NS)) {
			netisr &= ~(1 << NETISR_NS);
			nsintr();
		}
#endif
#ifdef ISO
		if (netisr & (1 << NETISR_ISO)) {
			netisr &= ~(1 << NETISR_ISO);
			clnlintr();
		}
#endif
	}
}

/*
 * This is called from MachUserIntr() if astpending is set.
 * This is very similar to the tail of trap().
 */
softintr(statusReg, pc)
	unsigned statusReg;	/* status register at time of the exception */
	unsigned pc;		/* program counter where to continue */
{
	register struct proc *p = curproc;
	int sig;

	cnt.v_soft++;
	/* take pending signals */
	while ((sig = CURSIG(p)) != 0)
		psig(sig);
	p->p_pri = p->p_usrpri;
	astpending = 0;
	if (p->p_flag & SOWEUPC) {
		p->p_flag &= ~SOWEUPC;
		ADDUPROF(p);
	}
	if (want_resched) {
		int s;

		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		s = splstatclock();
		setrq(p);
		p->p_stats->p_ru.ru_nivcsw++;
		swtch();
		splx(s);
		while ((sig = CURSIG(p)) != 0)
			psig(sig);
	}
	curpri = p->p_pri;
}

#ifdef DEBUG
trapDump(msg)
	char *msg;
{
	register int i;
	int s;

	s = splhigh();
	printf("trapDump(%s)\n", msg);
	for (i = 0; i < TRAPSIZE; i++) {
		if (trp == trapdebug)
			trp = &trapdebug[TRAPSIZE - 1];
		else
			trp--;
		if (trp->cause == 0)
			break;
		printf("%s: ADR %x PC %x CR %x SR %x\n",
			trap_type[(trp->cause & MACH_CR_EXC_CODE) >>
				MACH_CR_EXC_CODE_SHIFT],
			trp->vadr, trp->pc, trp->cause, trp->status);
		printf("   RA %x code %d\n", trp-> ra, trp->code);
	}
	bzero(trapdebug, sizeof(trapdebug));
	trp = trapdebug;
	splx(s);
}
#endif

#ifdef X_KLUGE
/*
 * This is a kludge to allow X windows to work.
 */
caddr_t
vmUserMap(size, pa)
	int size;
	unsigned pa;
{
	register caddr_t v;
	unsigned off, entry;

	if (nUserMapPtes == 0)
		UserMapPid = curproc->p_pid;
	else if (UserMapPid != curproc->p_pid)
		return ((caddr_t)0);
	off = pa & PGOFSET;
	size = btoc(off + size);
	if (nUserMapPtes + size > NPTES)
		return ((caddr_t)0);
	v = (caddr_t)(USER_MAP_ADDR + pmax_ptob(nUserMapPtes) + off);
	entry = (pa & 0x9ffff000) | PG_V | PG_M;
	if (pa >= MACH_UNCACHED_MEMORY_ADDR)
		entry |= PG_N;
	while (size > 0) {
		UserMapPtes[nUserMapPtes].pt_entry = entry;
		entry += NBPG;
		nUserMapPtes++;
		size--;
	}
	return (v);
}

vmUserUnmap()
{
	int id;

	nUserMapPtes = 0;
	if (UserMapPid == curproc->p_pid) {
		id = curproc->p_vmspace->vm_pmap.pm_tlbpid;
		if (id >= 0)
			MachTLBFlushPID(id);
	}
	UserMapPid = 0;
}
#endif

/*
 *----------------------------------------------------------------------
 *
 * MemErrorInterrupt --
 *
 *	Handler an interrupt for the control register.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
#ifdef NOTDEF
static void
MemErrorInterrupt()
{

}
#endif

/*
 * Return the resulting PC as if the branch was executed.
 */
unsigned
MachEmulateBranch(regsPtr, instPC, fpcCSR, allowNonBranch)
	unsigned *regsPtr;
	unsigned instPC;
	unsigned fpcCSR;
	int allowNonBranch;
{
	InstFmt inst;
	unsigned retAddr;
	int condition;
	extern unsigned GetBranchDest();

#if 0
	printf("regsPtr=%x PC=%x Inst=%x fpcCsr=%x\n", regsPtr, instPC,
		*(unsigned *)instPC, fpcCSR);
#endif

	inst = *(InstFmt *)instPC;
	switch ((int)inst.JType.op) {
	case OP_SPECIAL:
		switch ((int)inst.RType.func) {
		case OP_JR:
		case OP_JALR:
			retAddr = regsPtr[inst.RType.rs];
			break;

		default:
			if (!allowNonBranch)
				panic("MachEmulateBranch: Non-branch");
			retAddr = instPC + 4;
			break;
		}
		break;

	case OP_BCOND:
		switch ((int)inst.IType.rt) {
		case OP_BLTZ:
		case OP_BLTZAL:
			if ((int)(regsPtr[inst.RType.rs]) < 0)
				retAddr = GetBranchDest((InstFmt *)instPC);
			else
				retAddr = instPC + 8;
			break;

		case OP_BGEZAL:
		case OP_BGEZ:
			if ((int)(regsPtr[inst.RType.rs]) >= 0)
				retAddr = GetBranchDest((InstFmt *)instPC);
			else
				retAddr = instPC + 8;
			break;

		default:
			panic("MachEmulateBranch: Bad branch cond");
		}
		break;

	case OP_J:
	case OP_JAL:
		retAddr = (inst.JType.target << 2) | 
			((unsigned)instPC & 0xF0000000);
		break;

	case OP_BEQ:
		if (regsPtr[inst.RType.rs] == regsPtr[inst.RType.rt])
			retAddr = GetBranchDest((InstFmt *)instPC);
		else
			retAddr = instPC + 8;
		break;

	case OP_BNE:
		if (regsPtr[inst.RType.rs] != regsPtr[inst.RType.rt])
			retAddr = GetBranchDest((InstFmt *)instPC);
		else
			retAddr = instPC + 8;
		break;

	case OP_BLEZ:
		if ((int)(regsPtr[inst.RType.rs]) <= 0)
			retAddr = GetBranchDest((InstFmt *)instPC);
		else
			retAddr = instPC + 8;
		break;

	case OP_BGTZ:
		if ((int)(regsPtr[inst.RType.rs]) > 0)
			retAddr = GetBranchDest((InstFmt *)instPC);
		else
			retAddr = instPC + 8;
		break;

	case OP_COP1:
		switch (inst.RType.rs) {
		case OP_BCx:
		case OP_BCy:
			if ((inst.RType.rt & COPz_BC_TF_MASK) == COPz_BC_TRUE)
				condition = fpcCSR & MACH_FPC_COND_BIT;
			else
				condition = !(fpcCSR & MACH_FPC_COND_BIT);
			if (condition)
				retAddr = GetBranchDest((InstFmt *)instPC);
			else
				retAddr = instPC + 8;
			break;

		default:
			if (!allowNonBranch)
				panic("MachEmulateBranch: Bad coproc branch instruction");
			retAddr = instPC + 4;
		}
		break;

	default:
		if (!allowNonBranch)
			panic("MachEmulateBranch: Non-branch instruction");
		retAddr = instPC + 4;
	}
#if 0
	printf("Target addr=%x\n", retAddr);
#endif
	return (retAddr);
}

unsigned
GetBranchDest(InstPtr)
	InstFmt *InstPtr;
{
	return ((unsigned)InstPtr + 4 + ((short)InstPtr->IType.imm << 2));
}

/*
 * This routine is called by procxmt() to single step one instruction.
 * We do this by storing a break instruction after the current instruction,
 * resuming execution, and then restoring the old instruction.
 */
cpu_singlestep(p)
	register struct proc *p;
{
	register unsigned va;
	register int *locr0 = p->p_md.md_regs;
	int i;

	/* compute next address after current location */
	va = MachEmulateBranch(locr0, locr0[PC], 0, 1);
	if (p->p_md.md_ss_addr || p->p_md.md_ss_addr == va ||
	    !useracc((caddr_t)va, 4, B_READ)) {
		printf("SS %s (%d): breakpoint already set at %x (va %x)\n",
			p->p_comm, p->p_pid, p->p_md.md_ss_addr, va); /* XXX */
		return (EFAULT);
	}
	p->p_md.md_ss_addr = va;
	p->p_md.md_ss_instr = fuiword((caddr_t)va);
	i = suiword((caddr_t)va, MACH_BREAK_SSTEP);
	if (i < 0) {
		vm_offset_t sa, ea;
		int rv;

		sa = trunc_page((vm_offset_t)va);
		ea = round_page((vm_offset_t)va+sizeof(int)-1);
		rv = vm_map_protect(&p->p_vmspace->vm_map, sa, ea,
			VM_PROT_DEFAULT, FALSE);
		if (rv == KERN_SUCCESS) {
			i = suiword((caddr_t)va, MACH_BREAK_SSTEP);
			(void) vm_map_protect(&p->p_vmspace->vm_map,
				sa, ea, VM_PROT_READ|VM_PROT_EXECUTE, FALSE);
		}
	}
	if (i < 0)
		return (EFAULT);
	printf("SS %s (%d): breakpoint set at %x: %x (pc %x)\n",
		p->p_comm, p->p_pid, p->p_md.md_ss_addr,
		p->p_md.md_ss_instr, locr0[PC]); /* XXX */
	return (0);
}

/*
 * news3400 - INT0 service routine.
 *
 * INTST0 bit	4:	dma
 *		3:	slot #1
 *		2:	slot #3
 *		1:	external #1
 *		0:	external #3
 */

#define	LEVEL0_MASK	\
	(INTST1_DMA|INTST1_SLOT1|INTST1_SLOT3|INTST1_EXT1|INTST1_EXT3)

level0_intr()
{
	register int stat;

	stat = *(volatile u_char *)INTST1 & LEVEL0_MASK;
	*(u_char *)INTCLR1 = stat;

	if (stat & INTST1_DMA)
		dma_intr();
	if (stat & INTST1_SLOT1)
		exec_hb_intr2();
#if NEN > 0
	if (stat & INTST1_SLOT3) {
		int s, t;

		s = splimp();
		t = lance_intr();
		(void) splx(s);
		if (t == 0)
			exec_hb_intr4();
	}
#endif
#if NLE > 0
	if (stat & INTST1_SLOT3) {
		int s;

		s = splimp();
		leintr(0);
		(void) splx(s);
	}
#endif
	if (stat & INTST1_EXT1)
		print_int_stat("EXT #1");
	if (stat & INTST1_EXT3)
		print_int_stat("EXT #3");
}

/*
 * news3400 - INT1 service routine.
 *
 * INTST0 bit	1:	centro fault
 *		0:	centro busy
 * INTST1 bit	7:	beep
 *		6:	scc
 *		5:	lance
 */

#define LEVEL1_MASK2	(INTST0_CFLT|INTST0_CBSY)
#define LEVEL1_MASK1	(INTST1_BEEP|INTST1_SCC|INTST1_LANCE)

level1_intr(pc)
	unsigned pc;
{
	register int stat;
	register u_int saved_inten1 = *(u_char *)INTEN1;

	*(u_char *)INTEN1 = 0;		/* disable intr: beep, lance, scc */

	stat = *(volatile u_char *)INTST1 & LEVEL1_MASK1;
	*(u_char *)INTCLR1 = stat;

	stat &= saved_inten1;

	if (stat & INTST1_BEEP) {
		*(volatile u_char *)INTCLR1 = INTCLR1_BEEP;
		print_int_stat("BEEP");
	}
	if (stat & INTST1_SCC) {
		scc_intr();
		if (saved_inten1 & *(u_char *)INTST1 & INTST1_SCC)
			scc_intr();
	}
#if NEN > 0
	if (stat & INTST1_LANCE)
		lance_intr();
#endif
#if NLE > 0
	if (stat & INTST1_LANCE)
		leintr(0);
#endif

	*(u_char *)INTEN1 = saved_inten1;

#if NLP > 0
	/*
	 * The PARK2 cannot find centro interrupt correctly.
	 * We must check it by reading the cause register of cpu
	 * while other interrupts are disabled.
	 */
	{
		register int causereg;
		int s = splhigh();

		causereg = get_causereg();
		(void) splx(s);

		if ((causereg & CAUSE_IP4) == 0)
			return;
	}
#endif

	stat = (int)(*(u_char *)INTST0) & LEVEL1_MASK2;
	*(u_char *)INTCLR0 = stat;

	if (stat & INTST0_CBSY)		/* centro busy */
#if NLP > 0
		lpxint(0);
#else
		printf("stray intr: CBSY\n");
#endif
}

/*
 * DMA interrupt service routine.
 */
dma_intr()
{
        register volatile u_char *gsp = (u_char *)DMAC_GSTAT;
        register u_int gstat = *gsp;
        register int mrqb, i;

	/*
	 * when DMA intrrupt occurs there remain some untransferred data.
	 * wait data transfer completion.
	 */
	mrqb = (gstat & (CH0_INT|CH1_INT|CH2_INT|CH3_INT)) << 1;
	if (gstat & mrqb) {
		/*
		 * SHOULD USE DELAY()
		 */
		for (i = 0; i < 50; i++)
			;
		if (*gsp & mrqb)
			printf("dma_intr: MRQ\n");
	}

	/* SCSI Dispatch */
	if (gstat & CH_INT(CH_SCSI))
		scintr();

#include "fd.h"
#if NFD > 0
        /* FDC Interrupt Dispatch */
	if (gstat & CH_INT(CH_FDC))
		fdc_intr(0);
#endif /* NFD > 0 */

#include "sb.h"
#if NSB > 0
        /* Audio Interface Dispatch */
	sbintr(0);
#endif /* NSB > 0 */

        /* Video I/F Dispatch */
	if (gstat & CH_INT(CH_VIDEO))
		;
}

/*
 * SCC vector interrupt service routine.
 */
scc_intr()
{
	int vec;
	extern int scc_xint(), scc_sint(), scc_rint(), scc_cint();
	static int (*func[])() = {
		scc_xint,
		scc_sint,
		scc_rint,
		scc_cint
	};

	vec = *(volatile u_char *)SCCVECT;
	(*func[(vec & SCC_INT_MASK) >> 1])(vec);
}

print_int_stat(msg)
	char *msg;
{
	int s0 = *(volatile u_char *)INTST0;
	int s1 = *(volatile u_char *)INTST1;

	if (msg)
		printf("%s: ", msg);
	else
		printf("intr: ");
	printf("INTST0=0x%x, INTST1=0x%x.\n", s0, s1);
}
