/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_sysctl.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "user.h"
#include "proc.h"
#include "kinfo.h"
#include "vm.h"
#include "ioctl.h"
#include "tty.h"
#include "buf.h"

/* TODO - gather stats on average and max time spent */
getkinfo()
{
	register struct a {
		int	op;
		char	*where;
		int	*size;
		int	arg;
	} *uap = (struct a *)u.u_ap;
	int wanted;

	switch (ki_type(uap->op)) {

	case KINFO_PROC:
		u.u_error = kinfo_proc(uap->op, (char *)uap->where, 
				       (int *)uap->size, uap->arg, &wanted);
		if (!u.u_error)
			u.u_r.r_val1 = wanted;
		break;

	default:
		u.u_error = EINVAL;
	}

	return;
}

/* 
 * try over estimating by 5 procs 
 */
#define KINFO_PROCSLOP	(5 * sizeof (struct kinfo_proc))

int kinfo_proc_userfailed;
int kinfo_proc_wefailed;

kinfo_proc(op, where, asize, arg, awanted)
	char *where;
	int *asize, *awanted;
{
	int	bufsize,	/* max size of users buffer */
		copysize, 	/* size copied */
		needed;	
	int locked;
	int error;

	if (error = kinfo_doprocs(op, NULL, NULL, arg, &needed))
		return (error);
	if (where == NULL || asize == NULL) {
		*awanted = needed;
		return (0);
	}
	if (error = copyin((caddr_t)asize, (caddr_t)&bufsize, sizeof (bufsize)))
		return (error);
	needed += KINFO_PROCSLOP;
	locked = copysize = MIN(needed, bufsize);
	if (!useracc(where, copysize, B_WRITE))
		return (EFAULT);
	/*
	 * lock down target pages - NEED DEADLOCK AVOIDANCE
	 */
	if (copysize > ((int)ptob(freemem) - (20 * 1024))) 	/* XXX */
		return (ENOMEM);
	vslock(where, copysize);
	error = kinfo_doprocs(op, where, &copysize, arg, &needed);
	vsunlock(where, locked, B_WRITE);
	if (error)
		return (error);
	*awanted = needed;
	if (error = copyout((caddr_t)&copysize, (caddr_t)asize,
	    sizeof (copysize)))
		return (error);
	
	return (0);
}

kinfo_doprocs(op, where, acopysize, arg, aneeded)
	char *where;
	int *acopysize, *aneeded;
{
	register struct proc *p;
	register caddr_t dp = (caddr_t)where;
	register needed = 0;
	int buflen;
	int doingzomb;
	struct eproc eproc;
	struct tty *tp;
	int error = 0;

	if (where != NULL)
		buflen = *acopysize;

	p = allproc;
	doingzomb = 0;
again:
	for (; p != NULL; p = p->p_nxt) {
		/* 
		 * TODO - make more efficient (see notes below).
		 * do by session. 
		 */
		switch (ki_op(op)) {

		case KINFO_PROC_PID:
			/* could do this with just a lookup */
			if (p->p_pid != (pid_t)arg)
				continue;
			break;

		case KINFO_PROC_PGRP:
			/* could do this by traversing pgrp */
			if (p->p_pgrp->pg_id != (pid_t)arg)
				continue;
			break;

		case KINFO_PROC_TTY:
			if ((p->p_flag&SCTTY) == 0 || 
			    p->p_session->s_ttyp == NULL ||
			    p->p_session->s_ttyp->t_dev != (dev_t)arg)
				continue;
			break;

		case KINFO_PROC_UID:
			if (p->p_uid != (uid_t)arg)
				continue;
			break;

		case KINFO_PROC_RUID:
			if (p->p_ruid != (uid_t)arg)
				continue;
			break;
		}
		if (where != NULL && buflen >= sizeof (struct kinfo_proc)) {
			if (error = copyout((caddr_t)p, dp, 
			    sizeof (struct proc)))
				return (error);
			dp += sizeof (struct proc);
			eproc.kp_paddr = p;
			eproc.kp_sess = p->p_pgrp->pg_session;
			eproc.kp_pgid = p->p_pgrp->pg_id;
			eproc.kp_jobc = p->p_pgrp->pg_jobc;
			tp = p->p_pgrp->pg_session->s_ttyp;
			if ((p->p_flag&SCTTY) && tp != NULL) {
				eproc.kp_tdev = tp->t_dev;
				eproc.kp_tpgid = tp->t_pgrp ? 
					tp->t_pgrp->pg_id : -1;
				eproc.kp_tsess = tp->t_session;
			} else
				eproc.kp_tdev = NODEV;
			if (error = copyout((caddr_t)&eproc, dp, 
			    sizeof (eproc)))
				return (error);
			dp += sizeof (eproc);
			buflen -= sizeof (struct kinfo_proc);
		}
		needed += sizeof (struct kinfo_proc);
	}
	if (doingzomb == 0) {
		p = zombproc;
		doingzomb++;
		goto again;
	}
	if (where != NULL)
		*acopysize = dp - where;
	*aneeded = needed;

	return (0);
}
