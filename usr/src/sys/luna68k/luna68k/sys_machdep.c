/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * from: hp300/hp300/sys_machdep.c	7.11 (Berkeley) 12/27/92
 *
 *	@(#)sys_machdep.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/mtio.h>
#include <sys/buf.h>
#include <sys/trace.h>

#include <vm/vm.h>

#ifdef TRACE
int	nvualarm;

struct vtrace_args {
	int	request;
	int	value;
};
vtrace(p, uap, retval)
	struct proc *p;
	register struct vtrace_args *uap;
	int *retval;
{
	int vdoualarm();

	switch (uap->request) {

	case VTR_DISABLE:		/* disable a trace point */
	case VTR_ENABLE:		/* enable a trace point */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			return (EINVAL);
		*retval = traceflags[uap->value];
		traceflags[uap->value] = uap->request;
		break;

	case VTR_VALUE:		/* return a trace point setting */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			return (EINVAL);
		*retval = traceflags[uap->value];
		break;

	case VTR_UALARM:	/* set a real-time ualarm, less than 1 min */
		if (uap->value <= 0 || uap->value > 60 * hz || nvualarm > 5)
			return (EINVAL);
		nvualarm++;
		timeout(vdoualarm, (caddr_t)p->p_pid, uap->value);
		break;

	case VTR_STAMP:
		trace(TR_STAMP, uap->value, p->p_pid);
		break;
	}
	return (0);
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

#include <machine/cpu.h>

/* XXX should be in an include file somewhere */
#define CC_PURGE	1
#define CC_FLUSH	2
#define CC_IPURGE	4
#define CC_EXTPURGE	0x80000000
/* XXX end should be */

/*
 * Note that what we do here for a 68040 is different than HP-UX.
 *
 * In 'pux they either act on a line (len == 16), a page (len == NBPG)
 * or the whole cache (len == anything else).
 *
 * In BSD we attempt to be more optimal when acting on "odd" sizes.
 * For lengths up to 1024 we do all affected lines, up to 2*NBPG we
 * do pages, above that we do the entire cache.
 */
/*ARGSUSED1*/
cachectl(req, addr, len)
	int req;
	caddr_t	addr;
	int len;
{
	int error = 0;

	switch (req) {
	case CC_EXTPURGE|CC_PURGE:
	case CC_EXTPURGE|CC_FLUSH:
	case CC_PURGE:
	case CC_FLUSH:
		break;
	case CC_EXTPURGE|CC_IPURGE:
	case CC_IPURGE:
		ICIA();
		break;
	default:
		error = EINVAL;
		break;
	}
	return(error);
}
