/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: trap.c 1.32 91/04/06$
 *
 *	@(#)trap.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "proc.h"
#include "kernel.h"
#include "signalvar.h"
#include "user.h"
#ifdef KTRACE
#include "ktrace.h"
#endif
#include "net/netisr.h"

#include "../include/trap.h"
#include "../include/psl.h"
#include "../include/reg.h"
#include "../include/cpu.h"
#include "pte.h"
#include "clockreg.h"

#include "vm/vm.h"
#include "vm/vm_kern.h"
#include "vm/vm_page.h"

/*
 * This is a kludge to allow X windows to work.
 */
#define X_KLUGE

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
extern void MemErrorInterrupt();

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
trap(statusReg, causeReg, vadr, pc, args /* XXX */)
	unsigned statusReg;	/* status register at time of the exception */
	unsigned causeReg;	/* cause register at time of exception */
	unsigned vadr;		/* address (if any) the fault occured on */
	unsigned pc;		/* program counter where to continue */
{
	register int type, i;
	unsigned ucode = 0;
	register struct proc *p = curproc;
	struct timeval syst;
	vm_prot_t ftype;
	extern unsigned onfault_table[];

#ifdef DEBUG
	trp->status = statusReg;
	trp->cause = causeReg;
	trp->vadr = vadr;
	trp->pc = pc;
	trp->ra = !USERMODE(statusReg) ? ((int *)&args)[19] : p->p_regs[RA];
	trp->code = 0;
	if (++trp == &trapdebug[TRAPSIZE])
		trp = trapdebug;
#endif

	cnt.v_trap++;
	type = (causeReg & MACH_CR_EXC_CODE) >> MACH_CR_EXC_CODE_SHIFT;
	if (USERMODE(statusReg)) {
		type |= T_USER;
		syst = p->p_stime;
	}

	/*
	 * Enable hardware interrupts if they were on before.
	 * We only respond to software interrupts when returning to user mode.
	 */
	if (statusReg & MACH_SR_INT_ENA_PREV)
		splx((statusReg & MACH_HARD_INT_MASK) | MACH_SR_INT_ENA_CUR);

	switch (type) {
	case T_TLB_MOD:
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
		if (hp->low & PG_RO) {
			ftype = VM_PROT_WRITE;
			goto dofault;
		}
		hp->low |= PG_M;
		printf("trap: TLBupdate hi %x lo %x i %x\n", hp->high, hp->low,
			MachTLBUpdate(hp->high, hp->low)); /* XXX */
#ifdef ATTR
		pmap_attributes[atop(hp->low - KERNBASE)] |= PMAP_ATTR_MOD;
#else
		pa = hp->low & PG_FRAME;
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
					p->p_regs[RA]); /* XXX */
			printf("\tpid %d %s PC %x RA %x\n", p->p_pid,
				p->p_comm, p->p_regs[PC], p->p_regs[RA]); /* XXX */
			trapDump("vm_fault");
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
			args.i[0] = p->p_regs[SP] + 4 * sizeof(int);
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
		register int *locr0 = p->p_regs;
		register struct sysent *callp;
		int code, numsys;
		struct args {
			int i[8];
		} args;
		int rval[2];
		struct sysent *systab;
		extern unsigned MachEmulateBranch();
		extern int nsysent;
#ifdef ULTRIXCOMPAT
		extern struct sysent ultrixsysent[];
		extern int ultrixnsysent;
#endif

		cnt.v_syscall++;
		/* compute next PC after syscall instruction */
		if ((int)causeReg < 0)
			locr0[PC] = MachEmulateBranch(locr0, pc, 0, 0);
		else
			locr0[PC] += 4;
		systab = sysent;
		numsys = nsysent;
#ifdef ULTRIXCOMPAT
		if (p->p_md.md_flags & MDP_ULTRIX) {
			systab = ultrixsysent;
			numsys = ultrixnsysent;
		}
#endif
		code = locr0[V0];
		if (code == 0) {			/* indir */
			code = locr0[A0];
			if (code >= numsys)
				callp = &systab[0];	/* indir (illegal) */
			else
				callp = &systab[code];
			i = callp->sy_narg;
			args.i[0] = locr0[A1];
			args.i[1] = locr0[A2];
			args.i[2] = locr0[A3];
			if (i > 3) {
				i = copyin((caddr_t)(locr0[SP] +
						3 * sizeof(int)),
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
		} else {
			if (code >= numsys)
				callp = &systab[0];	/* indir (illegal) */
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
		i = (*callp->sy_call)(p, &args, rval);
		/*
		 * Reinitialize proc pointer `p' as it may be different
		 * if this is a child returning from fork syscall.
		 */
		p = curproc;
		locr0 = p->p_regs;
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
		i = SIGTRAP;
		break;

	case T_RES_INST+T_USER:
		i = SIGILL;
		break;

	case T_COP_UNUSABLE+T_USER:
		if ((causeReg & MACH_CR_COP_ERR) != 0x10000000) {
			i = SIGILL;	/* only FPU instructions allowed */
			break;
		}
		MachSwitchFPState(machFPCurProcPtr, p->p_regs);
		machFPCurProcPtr = p;
		p->p_regs[PS] |= MACH_SR_COP_1_BIT;
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
		panic("trap");
	}
	printf("trap: pid %d %s sig %d adr %x pc %x ra %x\n", p->p_pid,
		p->p_comm, i, vadr, pc, p->p_regs[RA]); /* XXX */
	trapsignal(p, i, ucode);
out:
	/*
	 * Note: we should only get here if returning to user mode.
	 */
	astpending = 0;
	while (i = CURSIG(p))
		psig(i);
	p->p_pri = p->p_usrpri;
	if (want_resched) {
		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) splclock();
		setrq(p);
		p->p_stats->p_ru.ru_nivcsw++;
		swtch();
		while (i = CURSIG(p))
			psig(i);
	}
	if (p->p_stats->p_prof.pr_scale) {
		int ticks;
		struct timeval *tv = &p->p_stime;

		ticks = ((tv->tv_sec - syst.tv_sec) * 1000 +
			(tv->tv_usec - syst.tv_usec) / 1000) / (tick / 1000);
		if (ticks)
			addupc(pc, &p->p_stats->p_prof, ticks);
	}
	curpri = p->p_pri;
	return (pc);
}

int temp; /*XXX*/

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
	register int i;
	register unsigned mask;
	clockframe cf;

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

	cnt.v_intr++;
	mask = causeReg & statusReg;	/* pending interrupts & enable mask */
	/*
	 * Enable hardware interrupts which were enabled but not pending.
	 * We only respond to software interrupts when returning to spl0.
	 */
	splx((statusReg & ~causeReg & MACH_HARD_INT_MASK) |
		MACH_SR_INT_ENA_CUR);
	/*
	 * The first three tests should probably use
	 * some kind of table generated by 'config'.
	 */
	if (mask & MACH_INT_MASK_0)
		siiintr();
	if (mask & MACH_INT_MASK_1)
		leintr();
	if (mask & MACH_INT_MASK_2)
		dcintr();
	if (mask & MACH_INT_MASK_3) {
		register volatile struct chiptime *c =
			(volatile struct chiptime *)MACH_CLOCK_ADDR;

		temp = c->regc;	/* clear interrupt bits */
		cf.pc = pc;
		cf.ps = statusReg;
		hardclock(cf);
	}
	if (mask & MACH_INT_MASK_4)
		MemErrorInterrupt();
	if (mask & MACH_INT_MASK_5) {
		printf("FPU interrupt: PC %x CR %x SR %x\n",
			pc, causeReg, statusReg); /* XXX */
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
	if (mask & MACH_SOFT_INT_MASK_0) {
		clockframe cf;

		clearsoftclock();
		cf.pc = pc;
		cf.ps = statusReg;
		softclock(cf);
	}
	/* process network interrupt if we trapped or will very soon */
	if ((mask & MACH_SOFT_INT_MASK_1) ||
	    netisr && (statusReg & MACH_SOFT_INT_MASK_1)) {
		clearsoftnet();
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
	register int i;

	cnt.v_soft++;
	astpending = 0;
	while (i = CURSIG(p))
		psig(i);
	p->p_pri = p->p_usrpri;
	if (want_resched) {
		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we setrq ourselves but before we
		 * swtch()'ed, we might not be on the queue indicated by
		 * our priority.
		 */
		(void) splclock();
		setrq(p);
		p->p_stats->p_ru.ru_nivcsw++;
		swtch();
		while (i = CURSIG(p))
			psig(i);
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
static void
MemErrorInterrupt()
{
	volatile u_short *sysCSRPtr = (u_short *)MACH_SYS_CSR_ADDR;
	u_short csr;

	csr = *sysCSRPtr;

	if (csr & MACH_CSR_MEM_ERR) {
		printf("Memory error at 0x%x\n",
			*(unsigned *)MACH_WRITE_ERROR_ADDR);
		panic("Mem error interrupt");
	}
	*sysCSRPtr = (csr & ~MACH_CSR_MBZ) | 0xff;
}

/* machDis.c -
 *
 *     	This contains the routine which disassembles an instruction to find
 *	the target.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.  
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 */

#ifndef lint
static char rcsid[] = "$Header: /sprite/src/kernel/mach/ds3100.md/RCS/machDis.c,v 1.1 89/07/11 17:55:43 nelson Exp $ SPRITE (Berkeley)";
#endif not lint

/*
 * Define the instruction formats.
 */
typedef union {
	unsigned word;

	struct {
		unsigned imm: 16;
		unsigned f2: 5;
		unsigned f1: 5;
		unsigned op: 6;
	} IType;

	struct {
		unsigned target: 26;
		unsigned op: 6;
	} JType;

	struct {
		unsigned funct: 6;
		unsigned f4: 5;
		unsigned f3: 5;
		unsigned f2: 5;
		unsigned f1: 5;
		unsigned op: 6;
	} RType;

	struct {
		unsigned funct: 6;
		unsigned fd: 5;
		unsigned fs: 5;
		unsigned ft: 5;
		unsigned fmt: 4;
		unsigned : 1;		/* always '1' */
		unsigned op: 6;		/* always '0x11' */
	} FRType;
} InstFmt;

/*
 * Opcodes of the branch instructions.
 */
#define OP_SPECIAL	0x00
#define OP_BCOND	0x01
#define OP_J		0x02
#define	OP_JAL		0x03
#define OP_BEQ		0x04
#define OP_BNE		0x05
#define OP_BLEZ		0x06
#define OP_BGTZ		0x07

/*
 * Branch subops of the special opcode.
 */
#define OP_JR		0x08
#define OP_JALR		0x09

/*
 * Sub-ops for OP_BCOND code.
 */
#define OP_BLTZ		0x00
#define OP_BGEZ		0x01
#define OP_BLTZAL	0x10
#define OP_BGEZAL	0x11

/*
 * Coprocessor branch masks.
 */
#define COPz_BC_MASK	0x1a
#define COPz_BC		0x08
#define COPz_BC_TF_MASK	0x01
#define COPz_BC_TRUE	0x01
#define COPz_BC_FALSE	0x00

/*
 * Coprocessor 1 operation.
 */
#define OP_COP_1	0x11

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
	InstFmt *instPtr;
	unsigned retAddr;
	int condition;
	extern unsigned GetBranchDest();

#ifdef notdef
	printf("regsPtr=%x PC=%x Inst=%x fpcCsr=%x\n", regsPtr, instPC,
		*instPC, fpcCSR);
#endif

	instPtr = (InstFmt *)instPC;
	switch ((int)instPtr->JType.op) {
	case OP_SPECIAL:
		switch ((int)instPtr->RType.funct) {
		case OP_JR:
		case OP_JALR:
			retAddr = regsPtr[instPtr->RType.f1];
			break;

		default:
			if (!allowNonBranch)
				panic("MachEmulateBranch: Non-branch");
			retAddr = instPC + 4;
			break;
		}
		break;

	case OP_BCOND:
		switch ((int)instPtr->IType.f2) {
		case OP_BLTZ:
		case OP_BLTZAL:
			if ((int)(regsPtr[instPtr->RType.f1]) < 0)
				retAddr = GetBranchDest(instPtr);
			else
				retAddr = instPC + 8;
			break;

		case OP_BGEZAL:
		case OP_BGEZ:
			if ((int)(regsPtr[instPtr->RType.f1]) >= 0)
				retAddr = GetBranchDest(instPtr);
			else
				retAddr = instPC + 8;
			break;

		default:
			panic("MachEmulateBranch: Bad branch cond");
		}
		break;

	case OP_J:
	case OP_JAL:
		retAddr = (instPtr->JType.target << 2) | 
			((unsigned)instPC & 0xF0000000);
		break;

	case OP_BEQ:
		if (regsPtr[instPtr->RType.f1] == regsPtr[instPtr->RType.f2])
			retAddr = GetBranchDest(instPtr);
		else
			retAddr = instPC + 8;
		break;

	case OP_BNE:
		if (regsPtr[instPtr->RType.f1] != regsPtr[instPtr->RType.f2])
			retAddr = GetBranchDest(instPtr);
		else
			retAddr = instPC + 8;
		break;

	case OP_BLEZ:
		if ((int)(regsPtr[instPtr->RType.f1]) <= 0)
			retAddr = GetBranchDest(instPtr);
		else
			retAddr = instPC + 8;
		break;

	case OP_BGTZ:
		if ((int)(regsPtr[instPtr->RType.f1]) > 0)
			retAddr = GetBranchDest(instPtr);
		else
			retAddr = instPC + 8;
		break;

	case OP_COP_1:
		if ((instPtr->RType.f1 & COPz_BC_MASK) == COPz_BC) {
			if ((instPtr->RType.f2 & COPz_BC_TF_MASK) ==
			    COPz_BC_TRUE)
				condition = fpcCSR & MACH_FPC_COND_BIT;
			else
				condition = !(fpcCSR & MACH_FPC_COND_BIT);
			if (condition)
				retAddr = GetBranchDest(instPtr);
			else
				retAddr = instPC + 8;
		} else if (allowNonBranch)
			retAddr = instPC + 4;
		else
			panic("MachEmulateBranch: Bad coproc branch instruction");
		break;

	default:
		if (!allowNonBranch)
			panic("MachEmulateBranch: Non-branch instruction");
		retAddr = instPC + 4;
	}
#ifdef notdef
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
