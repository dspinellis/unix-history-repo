/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_tty.c	7.12 (Berkeley) %G%
 */

/*
 * Indirect driver for controlling tty.
 */
#include "param.h"
#include "systm.h"
#include "conf.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "vnode.h"
#include "file.h"

#define cttyvp(p) ((p)->p_flag&SCTTY ? (p)->p_session->s_ttyvp : NULL)

/*ARGSUSED*/
cttyopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	struct vnode *ttyvp = cttyvp(p);
	int error;

	if (ttyvp == NULL)
		return (ENXIO);
	VOP_LOCK(ttyvp);
	error = VOP_ACCESS(ttyvp,
	  (flag&FREAD ? VREAD : 0) | (flag&FWRITE ? VWRITE : 0), p->p_ucred, p);
	VOP_UNLOCK(ttyvp);
	if (error)
		return (error);
	return (VOP_OPEN(ttyvp, flag, NOCRED, p));
}

/*ARGSUSED*/
cttyread(dev, uio, flag, p)
	dev_t dev;
	struct uio *uio;
	struct proc *p;
{
	register struct vnode *ttyvp = cttyvp(p);
	int error;

	if (ttyvp == NULL)
		return (EIO);
	VOP_LOCK(ttyvp);
	error = VOP_READ(ttyvp, uio, flag, NOCRED);
	VOP_UNLOCK(ttyvp);
	return (error);
}

/*ARGSUSED*/
cttywrite(dev, uio, flag, p)
	dev_t dev;
	struct uio *uio;
	struct proc *p;
{
	register struct vnode *ttyvp = cttyvp(p);
	int error;

	if (ttyvp == NULL)
		return (EIO);
	VOP_LOCK(ttyvp);
	error = VOP_WRITE(ttyvp, uio, flag, NOCRED);
	VOP_UNLOCK(ttyvp);
	return (error);
}

/*ARGSUSED*/
cttyioctl(dev, cmd, addr, flag, p)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
	struct proc *p;
{
	struct vnode *ttyvp = cttyvp(p);

	if (ttyvp == NULL)
		return (EIO);
	if (cmd == TIOCNOTTY) {
		if (!SESS_LEADER(p)) {
			p->p_flag &= ~SCTTY;
			return (0);
		} else
			return (EINVAL);
	}
	return (VOP_IOCTL(ttyvp, cmd, addr, flag, NOCRED, p));
}

/*ARGSUSED*/
cttyselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{
	struct vnode *ttyvp = cttyvp(p);

	if (ttyvp == NULL)
		return (1);	/* try operation to get EOF/failure */
	return (VOP_SELECT(ttyvp, flag, FREAD|FWRITE, NOCRED, p));
}
