/*	vmsys.c	4.11	81/04/23	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/reg.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/tty.h"
#include "../h/mtpr.h"
#include "../h/vlimit.h"
#include "../h/trace.h"
#include <vadvise.h>

vfork()
{

	fork1(1);
}

vread()
{

	vrdwr(FREAD);
}

vwrite()
{

	vrdwr(FWRITE);
}

resuba()
{

	if (suser())
		ubareset(u.u_arg[0]);
}

futz()
{

}

segalloc()
{

	u.u_error = EIO;
}

segfree()
{

	u.u_error = EIO;
}

segsync()
{

	u.u_error = EIO;
}

int	both;

vadvise()
{
	register struct a {
		int	anom;
	} *uap;
	register struct proc *rp = u.u_procp;
	int oanom = rp->p_flag & SUANOM;
	register struct pte *pte;
	register struct cmap *c;
	register int i;

	uap = (struct a *)u.u_ap;
#ifdef TRACE
	trace(TR_VADVISE, uap->anom, u.u_procp->p_pid);
#endif
	rp->p_flag &= ~(SSEQL|SUANOM);
	switch (uap->anom) {

	case VA_ANOM:
		rp->p_flag |= SUANOM;
		break;

	case VA_SEQL:
		rp->p_flag |= SSEQL;
		break;
	}
	if (both || (oanom && (rp->p_flag & SUANOM) == 0)) {
		for (i = 0; i < rp->p_dsize; i += CLSIZE) {
			pte = dptopte(rp, i);
			if (pte->pg_v) {
				c = &cmap[pgtocm(pte->pg_pfnum)];
				if (c->c_lock)
					continue;
				pte->pg_v = 0;
				if (anycl(pte, pg_m))
					pte->pg_m = 1;
				distcl(pte);
			}
		}
		mtpr(TBIA, 0);
	}
}

vtimes()
{
	register struct a {
		struct	vtimes *par_vm;
		struct	vtimes *ch_vm;
	} *uap = (struct a *)u.u_ap;

	if (uap->par_vm == 0)
		goto onlych;
	if (copyout((caddr_t)&u.u_vm, (caddr_t)uap->par_vm,
	    sizeof(struct vtimes)) < 0)
		u.u_error = EFAULT;
onlych:
	if (uap->ch_vm == 0)
		return;
	if (copyout((caddr_t)&u.u_cvm, (caddr_t)uap->ch_vm,
	    sizeof(struct vtimes)) < 0)
		u.u_error = EFAULT;
}

vmsadd(vp, wp)
	register struct vtimes *vp, *wp;
{

	vp->vm_utime += wp->vm_utime;
	vp->vm_stime += wp->vm_stime;
	vp->vm_nswap += wp->vm_nswap;
	vp->vm_idsrss += wp->vm_idsrss;
	vp->vm_ixrss += wp->vm_ixrss;
	if (vp->vm_maxrss < wp->vm_maxrss)
		vp->vm_maxrss = wp->vm_maxrss;
	vp->vm_majflt += wp->vm_majflt;
	vp->vm_minflt += wp->vm_minflt;
	vp->vm_inblk += wp->vm_inblk;
	vp->vm_oublk += wp->vm_oublk;
}

/*
 * Revoke access the current tty by all processes.
 * Used only by the super-user in init
 * to give ``clean'' terminals at login.
 */
vhangup()
{

	if (!suser())
		return;
	if (u.u_ttyp == NULL)
		return;
	forceclose(u.u_ttyd);
	if ((u.u_ttyp->t_state) & ISOPEN)
		gsignal(u.u_ttyp->t_pgrp, SIGHUP);
}

forceclose(dev)
	dev_t dev;
{
	register struct file *fp;
	register struct inode *ip;
	register int n = 0;

	for (fp = &file[0]; fp < fileNFILE; fp++) {
		if (fp->f_count==0)
			continue;
		ip = fp->f_inode;
		if ((ip->i_mode & IFMT) != IFCHR)
			continue;
		if (ip->i_un.i_rdev != dev)
			continue;
		fp->f_flag &= ~(FREAD|FWRITE);
		n++;
	}
	return (n);
}

/*
 * Affect per-process limits.
 * To just return old limit, specify negative new limit.
 */
vlimit()
{
	register struct a {
		unsigned which;
		int	limit;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (uap->which > NLIMITS) {
		u.u_error = EINVAL;
		return;
	}
	u.u_r.r_val1 = u.u_limit[uap->which];
	if (uap->limit < 0)
		return;
	switch (uap->which) {

	case LIM_DATA:
		if (uap->limit > ctob(MAXDSIZ))
			uap->limit = ctob(MAXDSIZ);
		break;

	case LIM_STACK:
		if (uap->limit > ctob(MAXSSIZ))
			uap->limit = ctob(MAXSSIZ);
		break;
	}
	if (u.u_limit[LIM_NORAISE] && uap->limit > u.u_limit[uap->which] &&
	    !suser()) {
		u.u_error = EACCES;
		return;
	}
	u.u_limit[uap->which] = uap->limit;
	if (uap->which == LIM_MAXRSS)
		u.u_procp->p_maxrss = uap->limit/NBPG;
}

#ifdef TRACE
int	nvualarm;

vtrace()
{
	register struct a {
		int	request;
		int	value;
	} *uap;
	int vdoualarm();

	uap = (struct a *)u.u_ap;
	switch (uap->request) {

	case VTR_DISABLE:		/* disable a trace point */
	case VTR_ENABLE:		/* enable a trace point */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			u.u_error = EINVAL;
		else {
			u.u_r.r_val1 = traceflags[uap->value];
			traceflags[uap->value] = uap->request;
		}
		break;

	case VTR_VALUE:		/* return a trace point setting */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			u.u_error = EINVAL;
		else
			u.u_r.r_val1 = traceflags[uap->value];
		break;

	case VTR_UALARM:	/* set a real-time ualarm, less than 1 min */
		if (uap->value <= 0 || uap->value > 60 * hz ||
		    nvualarm > 5)
			u.u_error = EINVAL;
		else {
			nvualarm++;
			timeout(vdoualarm, (caddr_t)u.u_procp->p_pid,
			    uap->value);
		}
		break;

	case VTR_STAMP:
		trace(TR_STAMP, uap->value, 0);
		break;
	}
}

vdoualarm(arg)
	int arg;
{
	register struct proc *p;

	p = pfind(arg);
	if (p)
		psignal(p, 16);
	nvualarm--;
}
#endif
