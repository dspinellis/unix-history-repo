/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_tty.c	7.9 (Berkeley) %G%
 */

/*
 * Indirect driver for controlling tty.
 */
#include "param.h"
#include "systm.h"
#include "conf.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "vnode.h"
#include "file.h"
#include "uio.h"

#define cttyvp(p) ((p)->p_flag&SCTTY ? (p)->p_session->s_ttyvp : NULL)

/*ARGSUSED*/
syopen(dev, flag)
	dev_t dev;
	int flag;
{
	struct vnode *ttyvp = cttyvp(u.u_procp);
	int error;

	if (ttyvp == NULL)
		return (ENXIO);
	VOP_LOCK(ttyvp);
	error = VOP_ACCESS(ttyvp,
	   (flag&FREAD ? VREAD : 0) | (flag&FWRITE ? VWRITE : 0), u.u_cred);
	VOP_UNLOCK(ttyvp);
	if (error)
		return (error);
	return (VOP_OPEN(ttyvp, flag, NOCRED));
}

/*ARGSUSED*/
syread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct vnode *ttyvp = cttyvp(u.u_procp);
	int error;

	if (ttyvp == NULL)
		return (ENXIO);
	VOP_LOCK(ttyvp);
	error = VOP_READ(ttyvp, uio, flag, NOCRED);
	VOP_UNLOCK(ttyvp);
	return (error);
}

/*ARGSUSED*/
sywrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct vnode *ttyvp = cttyvp(u.u_procp);
	int error;

	if (ttyvp == NULL)
		return (ENXIO);
	VOP_LOCK(ttyvp);
	error = VOP_WRITE(ttyvp, uio, flag, NOCRED);
	VOP_UNLOCK(ttyvp);
	return (error);
}

/*ARGSUSED*/
syioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{
	struct vnode *ttyvp = cttyvp(u.u_procp);

	if (ttyvp == NULL)
		return (ENXIO);
	if (cmd == TIOCNOTTY) {
		if (!SESS_LEADER(u.u_procp)) {
			u.u_procp->p_flag &= ~SCTTY;
			return (0);
		} else
			return (EINVAL);
	}
	return (VOP_IOCTL(ttyvp, cmd, addr, flag, NOCRED));
}

/*ARGSUSED*/
syselect(dev, flag)
	dev_t dev;
	int flag;
{
	struct vnode *ttyvp = cttyvp(u.u_procp);

	if (ttyvp == NULL)
		return (1);	/* try operation to get EOF/failure */
	return (VOP_SELECT(ttyvp, flag, FREAD|FWRITE, NOCRED));
}
