/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sys_process.c	7.12 (Berkeley) 6/28/90
 */

#define IPCREG
#include "param.h"
#include "user.h"
#include "proc.h"
#include "vnode.h"
#include "text.h"
#include "seg.h"
#include "buf.h"
#include "ptrace.h"

#include "machine/reg.h"
#include "machine/psl.h"
#include "machine/pte.h"

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
	if (p == 0 || p->p_stat != SSTOP || p->p_ppid != curp->p_pid ||
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

/*
 * Code that the child process
 * executes to implement the command
 * of the parent process in tracing.
 */
procxmt(p)
	register struct proc *p;
{
	register int i, *poff;
	register struct text *xp;
	struct vattr vattr;
	struct vnode *vp;

	if (ipc.ip_lock != p->p_pid)
		return (0);
	p->p_slptime = 0;
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
		/*
		 * If text, must assure exclusive use
		 */
		if (xp = p->p_textp) {
			vp = xp->x_vptr;
			VOP_GETATTR(vp, &vattr, u.u_cred);
			if (xp->x_count!=1 || (vattr.va_mode & VSVTX))
				goto error;
			xp->x_flag |= XTRC;
		}
		i = -1;
		if ((i = suiword((caddr_t)ipc.ip_addr, ipc.ip_data)) < 0) {
			if (!chgprot((caddr_t)ipc.ip_addr, RW) &&
			    !chgprot((caddr_t)ipc.ip_addr+(sizeof(int)-1), RW))
				i = suiword((caddr_t)ipc.ip_addr, ipc.ip_data);
			(void) chgprot((caddr_t)ipc.ip_addr, RO);
			(void) chgprot((caddr_t)ipc.ip_addr+(sizeof(int)-1), RO);
		}
		if (i < 0)
			goto error;
#if defined(tahoe)
		/* make sure the old value is not in cache */
		ckeyrelease(p->p_ckey);
		p->p_ckey = getcodekey();
#endif
		if (xp) {
			xp->x_flag |= XWRIT;
#if defined(tahoe)
			xp->x_ckey = p->p_ckey;
#endif
		}
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
			if (poff == &u.u_ar0[ipcreg[i]])
				goto ok;
		if (poff == &u.u_ar0[PS]) {
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
			u.u_ar0[PC] = (int)ipc.ip_addr;
		if ((unsigned)ipc.ip_data > NSIG)
			goto error;
		p->p_xstat = ipc.ip_data;	/* see issig */
		if (i == PT_STEP) 
			u.u_ar0[PS] |= PSL_T;
		wakeup((caddr_t)&ipc);
		return (1);

	case PT_KILL:			/* kill the child process */
		wakeup((caddr_t)&ipc);
		exit(p, p->p_xstat);

	default:
	error:
		ipc.ip_req = -1;
	}
	wakeup((caddr_t)&ipc);
	return (0);
}
