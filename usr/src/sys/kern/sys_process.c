/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sys_process.c	7.17 (Berkeley) %G%
 */

#define IPCREG
#include "param.h"
#include "proc.h"
#include "vnode.h"
#include "seg.h"
#include "buf.h"
#include "ptrace.h"

#include "machine/reg.h"
#include "machine/psl.h"
#include "vm/vm.h"
#include "vm/vm_page.h"

#include "user.h"

/*
 * Priority for tracing
 */
#define	IPCPRI	PZERO

/*
 * Tracing variables.
 * Used to pass trace command from
 * parent to child being traced.
 * This data base cannot be
 * shared and is locked
 * per user.
 */
struct {
	int	ip_lock;
	int	ip_req;
	int	*ip_addr;
	int	ip_data;
} ipc;

/*
 * sys-trace system call.
 */
ptrace(curp, uap, retval)
	struct proc *curp;
	register struct args {
		int	req;
		int	pid;
		int	*addr;
		int	data;
	} *uap;
	int *retval;
{
	register struct proc *p;

	if (uap->req <= 0) {
		curp->p_flag |= STRC;
		return (0);
	}
	p = pfind(uap->pid);
	if (p == 0 || p->p_stat != SSTOP || p->p_pptr != curp ||
	    !(p->p_flag & STRC))
		return (ESRCH);
	while (ipc.ip_lock)
		sleep((caddr_t)&ipc, IPCPRI);
	ipc.ip_lock = p->p_pid;
	ipc.ip_data = uap->data;
	ipc.ip_addr = uap->addr;
	ipc.ip_req = uap->req;
	p->p_flag &= ~SWTED;
	while (ipc.ip_req > 0) {
		if (p->p_stat==SSTOP)
			setrun(p);
		sleep((caddr_t)&ipc, IPCPRI);
	}
	*retval = ipc.ip_data;
	ipc.ip_lock = 0;
	wakeup((caddr_t)&ipc);
	if (ipc.ip_req < 0)
		return (EIO);
	return (0);
}

#define	PHYSOFF(p, o) \
	((physadr)(p)+((o)/sizeof(((physadr)0)->r[0])))
#if defined(i386)
#undef        PC
#undef        SP
#undef        PS
#undef        R0
#undef        R1

#define       PC      tEIP
#define       SP      tESP
#define       PS      tEFLAGS
#define       R0      tEDX
#define       R1      tECX
#endif

/*
 * Code that the child process
 * executes to implement the command
 * of the parent process in tracing.
 */
procxmt(p)
	register struct proc *p;
{
	register int i, *poff;

	if (ipc.ip_lock != p->p_pid)
		return (0);
	p->p_slptime = 0;
	u.u_kproc.kp_proc.p_regs = p->p_regs;	/* u.u_ar0 */
	i = ipc.ip_req;
	ipc.ip_req = 0;
	switch (i) {

	case PT_READ_I:			/* read the child's text space */
		if (!useracc((caddr_t)ipc.ip_addr, 4, B_READ))
			goto error;
		ipc.ip_data = fuiword((caddr_t)ipc.ip_addr);
		break;

	case PT_READ_D:			/* read the child's data space */
		if (!useracc((caddr_t)ipc.ip_addr, 4, B_READ))
			goto error;
		ipc.ip_data = fuword((caddr_t)ipc.ip_addr);
		break;

	case PT_READ_U:			/* read the child's u. */
#ifdef HPUXCOMPAT
		if (u.u_pcb.pcb_flags & PCB_HPUXTRACE)
			i = hpuxtobsduoff(ipc.ip_addr);
		else
#endif
		i = (int)ipc.ip_addr;
		if (i<0 || i > ctob(UPAGES)-sizeof(int))
			goto error;
		ipc.ip_data = *(int *)PHYSOFF(&u, i);
		break;

	case PT_WRITE_I:		/* write the child's text space */
		if ((i = suiword((caddr_t)ipc.ip_addr, ipc.ip_data)) < 0) {
			vm_offset_t sa, ea;
			int rv;

			sa = trunc_page((vm_offset_t)ipc.ip_addr);
			ea = round_page((vm_offset_t)ipc.ip_addr+sizeof(int)-1);
			rv = vm_map_protect(&p->p_vmspace->vm_map, sa, ea,
					VM_PROT_DEFAULT, FALSE);
			if (rv == KERN_SUCCESS) {
				i = suiword((caddr_t)ipc.ip_addr, ipc.ip_data);
				(void) vm_map_protect(&p->p_vmspace->vm_map,
					sa, ea, VM_PROT_READ|VM_PROT_EXECUTE,
					FALSE);
			}
		}
		if (i < 0)
			goto error;
		break;

	case PT_WRITE_D:		/* write the child's data space */
		if (suword((caddr_t)ipc.ip_addr, 0) < 0)
			goto error;
		(void) suword((caddr_t)ipc.ip_addr, ipc.ip_data);
		break;

	case PT_WRITE_U:		/* write the child's u. */
#ifdef HPUXCOMPAT
		if (u.u_pcb.pcb_flags & PCB_HPUXTRACE)
			i = hpuxtobsduoff(ipc.ip_addr);
		else
#endif
		i = (int)ipc.ip_addr;
		poff = (int *)PHYSOFF(&u, i);
		for (i=0; i<NIPCREG; i++)
			if (poff == &p->p_regs[ipcreg[i]])
				goto ok;
		if (poff == &p->p_regs[PS]) {
			ipc.ip_data |= PSL_USERSET;
			ipc.ip_data &= ~PSL_USERCLR;
#ifdef PSL_CM_CLR
			if (ipc.ip_data & PSL_CM)
				ipc.ip_data &= ~PSL_CM_CLR;
#endif
			goto ok;
		}
#if defined(hp300)
#ifdef FPCOPROC
		if (poff >= (int *)u.u_pcb.pcb_fpregs.fpf_regs &&
		    poff <= (int *)&u.u_pcb.pcb_fpregs.fpf_fpiar)
			goto ok;
#endif
#endif
		goto error;

	ok:
		*poff = ipc.ip_data;
		break;

	case PT_STEP:			/* single step the child */
	case PT_CONTINUE:		/* continue the child */
		if ((int)ipc.ip_addr != 1)
			p->p_regs[PC] = (int)ipc.ip_addr;
		if ((unsigned)ipc.ip_data > NSIG)
			goto error;
		p->p_xstat = ipc.ip_data;	/* see issig */
		if (i == PT_STEP) 
			p->p_regs[PS] |= PSL_T;
		wakeup((caddr_t)&ipc);
		return (1);

	case PT_KILL:			/* kill the child process */
		wakeup((caddr_t)&ipc);
		exit(p, (int)p->p_xstat);

	default:
	error:
		ipc.ip_req = -1;
	}
	wakeup((caddr_t)&ipc);
	return (0);
}
