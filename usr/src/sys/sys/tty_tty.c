/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_tty.c	7.1 (Berkeley) 6/5/86
 */

/*
 * Indirect driver for controlling tty.
 *
 * THIS IS GARBAGE: MUST SOON BE DONE WITH struct inode * IN PROC TABLE.
 */
#include "param.h"
#include "systm.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "uio.h"

/*ARGSUSED*/
syopen(dev, flag)
	dev_t dev;
	int flag;
{

	if (u.u_ttyp == NULL)
		return (ENXIO);
	return ((*cdevsw[major(u.u_ttyd)].d_open)(u.u_ttyd, flag));
}

/*ARGSUSED*/
syread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	if (u.u_ttyp == NULL)
		return (ENXIO);
	return ((*cdevsw[major(u.u_ttyd)].d_read)(u.u_ttyd, uio));
}

/*ARGSUSED*/
sywrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	if (u.u_ttyp == NULL)
		return (ENXIO);
	return ((*cdevsw[major(u.u_ttyd)].d_write)(u.u_ttyd, uio));
}

/*ARGSUSED*/
syioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{

	if (cmd == TIOCNOTTY) {
		u.u_ttyp = 0;
		u.u_ttyd = 0;
		u.u_procp->p_pgrp = 0;
		return (0);
	}
	if (u.u_ttyp == NULL)
		return (ENXIO);
	return ((*cdevsw[major(u.u_ttyd)].d_ioctl)(u.u_ttyd, cmd, addr, flag));
}

/*ARGSUSED*/
syselect(dev, flag)
	dev_t dev;
	int flag;
{

	if (u.u_ttyp == NULL) {
		u.u_error = ENXIO;
		return (0);
	}
	return ((*cdevsw[major(u.u_ttyd)].d_select)(u.u_ttyd, flag));
}
