/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_kinfo.c	7.11 (Berkeley) 6/28/90
 */

#include "param.h"
#include "user.h"
#include "proc.h"
#include "text.h"
#include "kinfo.h"
#include "vm.h"
#include "ioctl.h"
#include "tty.h"
#include "buf.h"


#define snderr(e) { error = (e); goto release;}
extern int kinfo_doproc(), kinfo_rtable(), kinfo_vnode();
struct kinfo_lock kinfo_lock;

/* ARGSUSED */
getkerninfo(p, uap, retval)
	struct proc *p;
	register struct args {
		int	op;
		char	*where;
		int	*size;
		int	arg;
	} *uap;
	int *retval;
{

	int	bufsize,	/* max size of users buffer */
		needed,	locked, (*server)(), error = 0;

	if (error = copyin((caddr_t)uap->size,
				(caddr_t)&bufsize, sizeof (bufsize)))
		goto done;

	switch (ki_type(uap->op)) {

	case KINFO_PROC:
		server = kinfo_doproc;
		break;

	case KINFO_RT:
		server = kinfo_rtable;
		break;

	case KINFO_VNODE:
		server = kinfo_vnode;
		break;

	default:
		error = EINVAL;
		goto done;
	}
	if (uap->where == NULL || uap->size == NULL) {
		error = (*server)(uap->op, NULL, NULL, uap->arg, &needed);
		goto done;
	}
	while (kinfo_lock.kl_lock) {
		kinfo_lock.kl_want++;
		sleep(&kinfo_lock, PRIBIO+1);
		kinfo_lock.kl_want--;
		kinfo_lock.kl_locked++;
	}
	kinfo_lock.kl_lock++;

	if (!useracc(uap->where, bufsize, B_WRITE))
		snderr(EFAULT);
	/*
	 * lock down target pages - NEED DEADLOCK AVOIDANCE
	 */
	if (bufsize > ((int)ptob(freemem) - (20 * 1024))) 	/* XXX */
		snderr(ENOMEM);
	if (server != kinfo_vnode)	/* XXX */
		vslock(uap->where, bufsize);
	locked = bufsize;
	error = (*server)(uap->op, uap->where, &bufsize, uap->arg, &needed);
	if (server != kinfo_vnode)	/* XXX */
		vsunlock(uap->where, locked, B_WRITE);
	if (error == 0)
		error = copyout((caddr_t)&bufsize,
				(caddr_t)uap->size, sizeof (bufsize));
release:
	kinfo_lock.kl_lock--;
	if (kinfo_lock.kl_want)
		wakeup(&kinfo_lock);
done:
	if (!error)
		*retval = needed;
	return (error);
}

/* 
 * try over estimating by 5 procs 
 */
#define KINFO_PROCSLOP	(5 * sizeof (struct kinfo_proc))

kinfo_doproc(op, where, acopysize, arg, aneeded)
	char *where;
	int *acopysize, *aneeded;
{
	register struct proc *p;
	register struct kinfo_proc *dp = (struct kinfo_proc *)where;
	register needed = 0;
	int buflen;
	int doingzomb;
	struct eproc eproc;
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
			register struct text *txt;
			register struct tty *tp;

			if (error = copyout((caddr_t)p, &dp->kp_proc, 
			    sizeof (struct proc)))
				return (error);
			eproc.e_paddr = p;
			eproc.e_sess = p->p_pgrp->pg_session;
			eproc.e_pgid = p->p_pgrp->pg_id;
			eproc.e_jobc = p->p_pgrp->pg_jobc;
			if ((p->p_flag&SCTTY) && 
			     (tp = eproc.e_sess->s_ttyp)) {
				eproc.e_tdev = tp->t_dev;
				eproc.e_tpgid = tp->t_pgrp ? 
					tp->t_pgrp->pg_id : -1;
				eproc.e_tsess = tp->t_session;
			} else
				eproc.e_tdev = NODEV;
			eproc.e_flag = eproc.e_sess->s_ttyvp ? EPROC_CTTY : 0;
			if (SESS_LEADER(p))
				eproc.e_flag |= EPROC_SLEADER;
			if (p->p_wmesg)
				strncpy(eproc.e_wmesg, p->p_wmesg, WMESGLEN);
			if (txt = p->p_textp) {
				eproc.e_xsize = txt->x_size;
				eproc.e_xrssize = txt->x_rssize;
				eproc.e_xccount = txt->x_ccount;
				eproc.e_xswrss = txt->x_swrss;
			} else {
				eproc.e_xsize = eproc.e_xrssize =
				  eproc.e_xccount =  eproc.e_xswrss = 0;
			}
			if (error = copyout((caddr_t)&eproc, &dp->kp_eproc, 
			    sizeof (eproc)))
				return (error);
			dp++;
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
		*acopysize = (caddr_t)dp - where;
	else
		needed += KINFO_PROCSLOP;
	*aneeded = needed;

	return (0);
}
