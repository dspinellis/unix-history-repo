/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sys_machdep.c	7.7 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/user.h"
#include "sys/ioctl.h"
#include "sys/file.h"
#include "sys/proc.h"
#include "sys/kernel.h"
#include "sys/mtio.h"
#include "sys/buf.h"
#include "sys/trace.h"

#include "../include/pte.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

/* ARGSUSED */
resuba(p, uap, retval)
	struct proc *p;
	struct args {
		int	value;
	} *uap;
	int *retval;
{
	int error;

	if (error = suser(u.u_cred, &u.u_acflag))
		return (error);
	if (uap->value < numuba)
		ubareset(uap->value);
	return (0);
}

#ifdef TRACE
int	nvualarm;

vtrace(p, uap, retval)
	struct proc *p;
	register struct args {
		int	request;
		int	value;
	} *uap;
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

#ifdef COMPAT
/*
 * Note: these tables are sorted by
 * ioctl "code" (in ascending order).
 */
int dctls[] = { DKIOCHDR, 0 };
int fctls[] = { FIOCLEX, FIONCLEX, FIOASYNC, FIONBIO, FIONREAD, 0 };
int mctls[] = { MTIOCTOP, MTIOCGET, 0 };
int tctls[] = {
	TIOCGETD, TIOCSETD, TIOCHPCL, TIOCMODG, TIOCMODS,
	TIOCGETP, TIOCSETP, TIOCSETN, TIOCEXCL, TIOCNXCL,
	TIOCFLUSH,TIOCSETC, TIOCGETC, TIOCREMOTE,TIOCMGET,
	TIOCMBIC, TIOCMBIS, TIOCMSET, TIOCSTART,TIOCSTOP,
	TIOCPKT,  TIOCNOTTY,TIOCSTI,  TIOCOUTQ, TIOCGLTC,
	TIOCSLTC, TIOCSPGRP,TIOCGPGRP,TIOCCDTR, TIOCSDTR,
	TIOCCBRK, TIOCSBRK, TIOCLGET, TIOCLSET, TIOCLBIC,
	TIOCLBIS, 0
};

/*
 * Map an old style ioctl command to new.
 */
mapioctl(cmd)
	int cmd;
{
	register int *map, c;

	switch ((cmd >> 8) & 0xff) {

	case 'd':
		map = dctls;
		break;

	case 'f':
		map = fctls;
		break;

	case 'm':
		map = mctls;
		break;

	case 't':
		map = tctls;
		break;

	default:
		return (0);
	}
	while ((c = *map) && (c&0xff) < (cmd&0xff))
		map++;
	if (c && (c&0xff) == (cmd&0xff))
		return (c);
	return (0);
}
#endif
