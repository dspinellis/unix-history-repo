/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the University of Utah, and William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)trap.c	8.4 (Berkeley) 9/23/93
 */

/*
 * 386 Trap and System call handleing
 */

#include <machine/cpu.h>
#include <machine/psl.h>
#include <machine/reg.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/acct.h>
#include <sys/kernel.h>
#ifdef KTRACE
#include <sys/ktrace.h>
#endif

#include <vm/vm_param.h>
#include <vm/pmap.h>
#include <vm/vm_map.h>

#include <machine/trap.h>
#include <machine/dbg.h>


struct	sysent sysent[];
int	nsysent;
unsigned rcr2();
extern short cpl;


/*
 * trap(frame):
 *	Exception, fault, and trap interface to BSD kernel. This
 * common code is called from assembly language IDT gate entry
 * routines that prepare a suitable stack frame, and restore this
 * frame after the exception has been processed. Note that the
 * effect is as if the arguments were passed call by reference.
 */

/*ARGSUSED*/
trap(frame)
	struct trapframe frame;
{
	register int i;
	register struct proc *p = curproc;
	u_quad_t sticks;
	int ucode, type, code, eva;
	extern int cold;

if(cold) goto we_re_toast;
	frame.tf_eflags &= ~PSL_NT;	/* clear nested trap XXX */
	type = frame.tf_trapno;
	
	if (curpcb && curpcb->pcb_onfault && frame.tf_trapno != 0xc) {
copyfault:	frame.tf_eip = (int)curpcb->pcb_onfault;
		return;
	}

	if (ISPL(frame.tf_cs) == SEL_UPL) {
		type |= T_USER;
		p->p_md.md_regs = (int *)&frame;
		curpcb->pcb_flags |= FM_TRAP;	/* used by sendsig */
		sticks = p->p_sticks;
	}

	ucode=0;
	eva = rcr2();
	code = frame.tf_err;
	switch (type) {

	default:
	we_re_toast:
#ifdef KDB
		if (kdb_trap(&psl))
			return;
#endif

		printf("trap type %d code = %x eip = %x cs = %x eflags = %x ",
			frame.tf_trapno, frame.tf_err, frame.tf_eip,
			frame.tf_cs, frame.tf_eflags);
		printf("cr2 %x cpl %x\n", eva, cpl);
		type &= ~T_USER;
		panic("trap");
		/*NOTREACHED*/

	case T_SEGNPFLT|T_USER:
	case T_STKFLT|T_USER:		/* 386bsd */
	case T_PROTFLT|T_USER:		/* protection fault */
		ucode = code + BUS_SEGM_FAULT ;
		i = SIGBUS;
		break;

	case T_PRIVINFLT|T_USER:	/* privileged instruction fault */
	case T_RESADFLT|T_USER:		/* reserved addressing fault */
	case T_RESOPFLT|T_USER:		/* reserved operand fault */
	case T_FPOPFLT|T_USER:		/* coprocessor operand fault */
		ucode = type &~ T_USER;
		i = SIGILL;
		break;

	case T_ASTFLT|T_USER:		/* Allow process switch */
	case T_ASTFLT:
		astoff();
		if ((p->p_flag & P_OWEUPC) && p->p_stats->p_prof.pr_scale) {
			addupc(frame.tf_eip, &p->p_stats->p_prof, 1);
			p->p_flag &= ~P_OWEUPC;
		}
		goto out;

	case T_DNA|T_USER:
#include "npx.h"
#if NNPX > 0
		/* if a transparent fault (due to context switch "late") */
		if (npxdna()) return;
#endif
		ucode = FPE_FPU_NP_TRAP;
		i = SIGFPE;
		break;

	case T_BOUND|T_USER:
		ucode = FPE_SUBRNG_TRAP;
		i = SIGFPE;
		break;

	case T_OFLOW|T_USER:
		ucode = FPE_INTOVF_TRAP;
		i = SIGFPE;
		break;

	case T_DIVIDE|T_USER:
		ucode = FPE_INTDIV_TRAP;
		i = SIGFPE;
		break;

	case T_ARITHTRAP|T_USER:
		ucode = code;
		i = SIGFPE;
		break;

	case T_PAGEFLT:			/* allow page faults in kernel mode */
		if (code & PGEX_P) goto we_re_toast;

		/* fall into */
	case T_PAGEFLT|T_USER:		/* page fault */
	    {
		register vm_offset_t va;
		register struct vmspace *vm = p->p_vmspace;
		register vm_map_t map;
		int rv;
		vm_prot_t ftype;
		extern vm_map_t kernel_map;

		va = trunc_page((vm_offset_t)eva);
		/*
		 * It is only a kernel address space fault iff:
		 * 	1. (type & T_USER) == 0  and
		 * 	2. pcb_onfault not set or
		 *	3. pcb_onfault set but supervisor space fault
		 * The last can occur during an exec() copyin where the
		 * argument space is lazy-allocated.
		 */
		if (type == T_PAGEFLT && va >= 0xfe000000)
			map = kernel_map;
		else
			map = &vm->vm_map;
		if (code & PGEX_W)
			ftype = VM_PROT_READ | VM_PROT_WRITE;
		else
			ftype = VM_PROT_READ;

		rv = user_page_fault(p, map, va, ftype, type);

		if (rv == KERN_SUCCESS) {
			if (type == T_PAGEFLT)
				return;
			goto out;
		}

		if (type == T_PAGEFLT) {
			if (curpcb->pcb_onfault)
				goto copyfault;
			printf("vm_fault(%x, %x, %x, 0) -> %x\n",
			       map, va, ftype, rv);
			printf("  type %x, code %x\n",
			       type, code);
			goto we_re_toast;
		}
		i = (rv == KERN_PROTECTION_FAILURE) ? SIGBUS : SIGSEGV;
		break;
	    }

	case T_TRCTRAP:	 /* trace trap -- someone single stepping lcall's */
		frame.tf_eflags &= ~PSL_T;

			/* Q: how do we turn it on again? */
		return;
	
	case T_BPTFLT|T_USER:		/* bpt instruction fault */
	case T_TRCTRAP|T_USER:		/* trace trap */
		frame.tf_eflags &= ~PSL_T;
		i = SIGTRAP;
		break;

#include "isa.h"
#if	NISA > 0
	case T_NMI:
	case T_NMI|T_USER:
		/* machine/parity/power fail/"kitchen sink" faults */
		if(isa_nmi(code) == 0) return;
		else goto we_re_toast;
#endif
	}

	trapsignal(p, i, ucode);
	if ((type & T_USER) == 0)
		return;
out:
	while (i = CURSIG(p))
		postsig(i);
	p->p_priority = p->p_usrpri;
	if (want_resched) {
		int pl;

		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we put ourselves on the run queue
		 * but before we switched, we might not be on the queue
		 * indicated by our priority.
		 */
		pl = splclock();
		setrunqueue(p);
		p->p_stats->p_ru.ru_nivcsw++;
		mi_switch();
		splx(pl);
		while (i = CURSIG(p))
			postsig(i);
	}
	if (p->p_stats->p_prof.pr_scale) {
		u_quad_t ticks = p->p_sticks - sticks;

		if (ticks) {
#ifdef PROFTIMER
			extern int profscale;
			addupc(frame.tf_eip, &p->p_stats->p_prof,
			    ticks * profscale);
#else
			addupc(frame.tf_eip, &p->p_stats->p_prof, ticks);
#endif
		}
	}
	curpriority = p->p_priority;
	curpcb->pcb_flags &= ~FM_TRAP;	/* used by sendsig */
}

/*
 * syscall(frame):
 *	System call request from POSIX system call gate interface to kernel.
 * Like trap(), argument is call by reference.
 */
/*ARGSUSED*/
syscall(frame)
	volatile struct syscframe frame;
{
	register int *locr0 = ((int *)&frame);
	register caddr_t params;
	register int i;
	register struct sysent *callp;
	register struct proc *p = curproc;
	u_quad_t sticks;
	int error, opc;
	int args[8], rval[2];
	unsigned int code;

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	sticks = p->p_sticks;
	if (ISPL(frame.sf_cs) != SEL_UPL)
		panic("syscall");

	code = frame.sf_eax;
	p->p_md.md_regs = (int *)&frame;
	curpcb->pcb_flags &= ~FM_TRAP;	/* used by sendsig */
	params = (caddr_t)frame.sf_esp + sizeof (int) ;

	/*
	 * Reconstruct pc, assuming lcall $X,y is 7 bytes, as it is always.
	 */
	opc = frame.sf_eip - 7;
	callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	if (callp == sysent) {
		code = fuword(params);
		params += sizeof (int);
		callp = (code >= nsysent) ? &sysent[63] : &sysent[code];
	}

	if ((i = callp->sy_narg * sizeof (int)) &&
	    (error = copyin(params, (caddr_t)args, (u_int)i))) {
		frame.sf_eax = error;
		frame.sf_eflags |= PSL_C;	/* carry bit */
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
	rval[1] = frame.sf_edx;
	error = (*callp->sy_call)(p, args, rval);
	if (error == ERESTART)
		frame.sf_eip = opc;
	else if (error != EJUSTRETURN) {
		if (error) {
			frame.sf_eax = error;
			frame.sf_eflags |= PSL_C;	/* carry bit */
		} else {
			frame.sf_eax = rval[0];
			frame.sf_edx = rval[1];
			frame.sf_eflags &= ~PSL_C;	/* carry bit */
		}
	}
	/* else if (error == EJUSTRETURN) */
		/* nothing to do */
done:
	/*
	 * Reinitialize proc pointer `p' as it may be different
	 * if this is a child returning from fork syscall.
	 */
	p = curproc;
	while (i = CURSIG(p))
		postsig(i);
	p->p_priority = p->p_usrpri;
	if (want_resched) {
		int pl;

		/*
		 * Since we are curproc, clock will normally just change
		 * our priority without moving us from one queue to another
		 * (since the running process is not on a queue.)
		 * If that happened after we put ourselves on the run queue
		 * but before we switched, we might not be on the queue
		 * indicated by our priority.
		 */
		pl = splclock();
		setrunqueue(p);
		p->p_stats->p_ru.ru_nivcsw++;
		mi_switch();
		splx(pl);
		while (i = CURSIG(p))
			postsig(i);
	}
	if (p->p_stats->p_prof.pr_scale) {
		u_quad_t ticks = p->p_sticks - sticks;

		if (ticks) {
#ifdef PROFTIMER
			extern int profscale;
			addupc(frame.sf_eip, &p->p_stats->p_prof,
			    ticks * profscale);
#else
			addupc(frame.sf_eip, &p->p_stats->p_prof, ticks);
#endif
		}
	}
	curpriority = p->p_priority;
#ifdef KTRACE
	if (KTRPOINT(p, KTR_SYSRET))
		ktrsysret(p->p_tracep, code, error, rval[0]);
#endif
}

int
user_page_fault (p, map, addr, ftype, type)
struct proc *p;
vm_map_t map;
caddr_t addr;
vm_prot_t ftype;
int type;
{
	struct vmspace *vm;
	vm_offset_t va;
	int rv;
	extern vm_map_t kernel_map;
	unsigned nss, v;

	vm = p->p_vmspace;

	va = trunc_page((vm_offset_t)addr);

	/*
	 * XXX: rude hack to make stack limits "work"
	 */
	nss = 0;
	if ((caddr_t)va >= vm->vm_maxsaddr && map != kernel_map) {
		nss = clrnd(btoc(USRSTACK - (unsigned)va));
		if (nss > btoc(p->p_rlimit[RLIMIT_STACK].rlim_cur))
			return (KERN_FAILURE);
	}

	/* check if page table is mapped, if not, fault it first */
#define pde_v(v) (PTD[((v)>>PD_SHIFT)&1023].pd_v)
	if (!pde_v(va)) {
		v = trunc_page(vtopte(va));
		if ((rv = vm_fault(map, v, ftype, FALSE)) != KERN_SUCCESS)
			return (rv);
		/* check if page table fault, increment wiring */
		vm_map_pageable(map, v, round_page(v+1), FALSE);
	} else
		v = 0;

	if ((rv = vm_fault(map, va, ftype, FALSE)) != KERN_SUCCESS)
		return (rv);

	/*
	 * XXX: continuation of rude stack hack
	 */
	if (nss > vm->vm_ssize)
		vm->vm_ssize = nss;
	va = trunc_page(vtopte(va));
	/*
	 * for page table, increment wiring
	 * as long as not a page table fault as well
	 */
	if (!v && type != T_PAGEFLT)
		vm_map_pageable(map, va, round_page(va+1), FALSE);
	return (KERN_SUCCESS);
}

int
user_write_fault (addr)
void *addr;
{
	if (user_page_fault (curproc, &curproc->p_vmspace->vm_map,
			     addr, VM_PROT_READ | VM_PROT_WRITE,
			     T_PAGEFLT) == KERN_SUCCESS)
		return (0);
	else
		return (EFAULT);
}

int
copyout (from, to, len)
void *from;
void *to;
u_int len;
{
	u_int *pte, *pde;
	int rest_of_page;
	int thistime;
	int err;

	/* be very careful not to overflow doing this check */
	if (to >= (void *)USRSTACK || (void *)USRSTACK - to < len)
		return (EFAULT);

	pte = (u_int *)vtopte (to);
	pde = (u_int *)vtopte (pte);

	rest_of_page = PAGE_SIZE - ((int)to & (PAGE_SIZE - 1));

	while (1) {
		thistime = len;
		if (thistime > rest_of_page)
			thistime = rest_of_page;

		if ((*pde & PG_V) == 0
		    || (*pte & (PG_V | PG_UW)) != (PG_V | PG_UW))
			if (err = user_write_fault (to))
				return (err);

		bcopy (from, to, thistime);

		len -= thistime;

		/*
		 * Break out as soon as possible in the common case
		 * that the whole transfer is containted in one page.
		 */
		if (len == 0)
			break;

		from += thistime;
		to += thistime;
		pte++;
		pde = (u_int *)vtopte (pte);
		rest_of_page = PAGE_SIZE;
	}

	return (0);
}
