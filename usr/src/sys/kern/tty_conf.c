/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tty_conf.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	enodev();
int	nullop();

int	ttyopen(),ttylclose(),ttread(),ttwrite(),nullioctl(),ttstart();
int	ttymodem(), nullmodem(), ttyinput();

#include "tb.h"
#if NTB > 0
int	tbopen(),tbclose(),tbread(),tbinput(),tbioctl();
#endif

#include "sl.h"
#if NSL > 0
int	slopen(),slclose(),slinput(),sltioctl(),slstart();
#endif


struct	linesw linesw[] =
{
	ttyopen, ttylclose, ttread, ttwrite, nullioctl,
	ttyinput, enodev, nullop, ttstart, ttymodem,	/* 0- termios */

	enodev, enodev, enodev, enodev, enodev,		/* 1- defunct */
	enodev, enodev, enodev, enodev, enodev,

	enodev, enodev, enodev, enodev, enodev,		/* 2- defunct */
	enodev, enodev, enodev, enodev, enodev,
#if NTB > 0
	tbopen, tbclose, tbread, enodev, tbioctl,
	tbinput, enodev, nullop, ttstart, nullmodem,	/* 3- TABLDISC */
#else
	enodev, enodev, enodev, enodev, enodev,
	enodev, enodev, enodev, enodev, enodev,
#endif
#if NSL > 0
	slopen, slclose, enodev, enodev, sltioctl,
	slinput, enodev, nullop, slstart, nullmodem,	/* 4- SLIPDISC */
#else
	enodev, enodev, enodev, enodev, enodev,
	enodev, enodev, enodev, enodev, enodev,
#endif
};

int	nldisp = sizeof (linesw) / sizeof (linesw[0]);

/*
 * Do nothing specific version of line
 * discipline specific ioctl command.
 */
/*ARGSUSED*/
nullioctl(tp, cmd, data, flags)
	struct tty *tp;
	char *data;
	int flags;
{

#ifdef lint
	tp = tp; data = data; flags = flags;
#endif
	return (-1);
}
