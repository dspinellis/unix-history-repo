/*	tty_tty.c	4.5	82/01/13	*/

/*
 * Indirect driver for controlling tty.
 *
 * THIS IS GARBAGE: MUST SOON BE DONE WITH struct inode * IN PROC TABLE.
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"

/*ARGSUSED*/
syopen(dev, flag)
{

	if(u.u_ttyp == NULL || (u.u_procp->p_flag&SDETACH)) {
		u.u_error = ENXIO;
		return;
	}
	(*cdevsw[major(u.u_ttyd)].d_open)(u.u_ttyd, flag);
}

/*ARGSUSED*/
syread(dev)
{

	if (u.u_ttyp == NULL || (u.u_procp->p_flag&SDETACH)) {
		u.u_error = ENXIO;
		return;
	}
	(*cdevsw[major(u.u_ttyd)].d_read)(u.u_ttyd);
}

/*ARGSUSED*/
sywrite(dev)
{

	if (u.u_ttyp == NULL || (u.u_procp->p_flag&SDETACH)) {
		u.u_error = ENXIO;
		return;
	}
	(*cdevsw[major(u.u_ttyd)].d_write)(u.u_ttyd);
}

/*ARGSUSED*/
syioctl(dev, cmd, addr, flag)
caddr_t addr;
{

	if (cmd == TIOCNOTTY) {
		u.u_ttyp = 0;
		u.u_ttyd = 0;
		u.u_procp->p_pgrp = 0;
		return;
	}
	if (u.u_ttyp == NULL || (u.u_procp->p_flag&SDETACH)) {
		u.u_error = ENXIO;
		return;
	}
	(*cdevsw[major(u.u_ttyd)].d_ioctl)(u.u_ttyd, cmd, addr, flag);
}

syselect(dev, flag)
{

	if (u.u_ttyp == NULL || (u.u_procp->p_flag&SDETACH)) {
		u.u_error = ENXIO;
		return (0);
	}
	return ((*cdevsw[major(u.u_ttyd)].d_select)(dev, flag));
}
