/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_conf.c	6.5 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	nodev();
int	nulldev();

int	ttyopen(),ttyclose(),ttread(),ttwrite(),nullioctl(),ttstart();
int	ttyinput();

#include "bk.h"
#if NBK > 0
int	bkopen(),bkclose(),bkread(),bkinput(),bkioctl();
#endif

#include "tb.h"
#if NTB > 0
int	tbopen(),tbclose(),tbread(),tbinput(),tbioctl();
#endif

struct	linesw linesw[] =
{
	ttyopen, nulldev, ttread, ttwrite, nullioctl,
	ttyinput, nodev, nulldev, ttstart, nulldev,
#if NBK > 0
	bkopen, bkclose, bkread, ttwrite, bkioctl,
	bkinput, nodev, nulldev, ttstart, nulldev,
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
	ttyopen, ttyclose, ttread, ttwrite, nullioctl,
	ttyinput, nodev, nulldev, ttstart, nulldev,
#if NTB > 0
	tbopen, tbclose, tbread, nodev, tbioctl,
	tbinput, nodev, nulldev, ttstart, nulldev,		/* 3 */
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
#if NTB > 0
	tbopen, tbclose, tbread, nodev, tbioctl,
	tbinput, nodev, nulldev, ttstart, nulldev,		/* 4 */
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
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
