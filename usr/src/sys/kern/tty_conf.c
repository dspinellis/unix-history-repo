/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tty_conf.c	7.9 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/conf.h>

#define	ttynodisc ((int (*) __P((dev_t, struct tty *)))enodev)
#define	ttyerrclose ((int (*) __P((struct tty *, int flags)))enodev)
#define	ttyerrio ((int (*) __P((struct tty *, struct uio *, int)))enodev)
#define	ttyerrinput ((int (*) __P((int c, struct tty *)))enodev)
#define	ttyerrstart ((int (*) __P((struct tty *)))enodev)

int	ttyopen __P((dev_t dev, struct tty *tp));
int	ttylclose __P((struct tty *tp, int flags));
int	ttread __P((struct tty *, struct uio *, int flags));
int	ttwrite __P((struct tty *, struct uio *, int flags));
int	nullioctl __P((struct tty *tp, int cmd, caddr_t data,
			int flag, struct proc *p));
int	ttyinput __P((int c, struct tty *tp));
int	ttstart __P((struct tty *tp));
int	ttymodem __P((struct tty *tp, int flags));
int	nullmodem __P((struct tty *tp, int flags));

#include "tb.h"
#if NTB > 0
int	tbopen __P((dev_t dev, struct tty *tp));
int	tbclose __P((struct tty *tp, int flags));
int	tbread __P((struct tty *, struct uio *, int flags));
int	tbioctl __P((struct tty *tp, int cmd, caddr_t data,
			int flag, struct proc *p));
int	tbinput __P((int c, struct tty *tp));
#endif

#include "sl.h"
#if NSL > 0
int	slopen __P((dev_t dev, struct tty *tp));
int	slclose __P((struct tty *tp, int flags));
int	sltioctl __P((struct tty *tp, int cmd, caddr_t data,
			int flag, struct proc *p));
int	slinput __P((int c, struct tty *tp));
int	slstart __P((struct tty *tp));
#endif


struct	linesw linesw[] =
{
	ttyopen, ttylclose, ttread, ttwrite, nullioctl,
	ttyinput, ttstart, ttymodem,			/* 0- termios */

	ttynodisc, ttyerrclose, ttyerrio, ttyerrio, nullioctl,
	ttyerrinput, ttyerrstart, nullmodem,		/* 1- defunct */

	ttynodisc, ttyerrclose, ttyerrio, ttyerrio, nullioctl,
	ttyerrinput, ttyerrstart, nullmodem,		/* 2- defunct */

#if NTB > 0
	tbopen, tbclose, tbread, enodev, tbioctl,
	tbinput, ttstart, nullmodem,			/* 3- TABLDISC */
#else
	ttynodisc, ttyerrclose, ttyerrio, ttyerrio, nullioctl,
	ttyerrinput, ttyerrstart, nullmodem,
#endif

#if NSL > 0
	slopen, slclose, ttyerrio, ttyerrio, sltioctl,
	slinput, slstart, nullmodem,			/* 4- SLIPDISC */
#else
	ttynodisc, ttyerrclose, ttyerrio, ttyerrio, nullioctl,
	ttyerrinput, ttyerrstart, nullmodem,
#endif
};

int	nldisp = sizeof (linesw) / sizeof (linesw[0]);

/*
 * Do nothing specific version of line
 * discipline specific ioctl command.
 */
/*ARGSUSED*/
nullioctl(tp, cmd, data, flags, p)
	struct tty *tp;
	int cmd;
	char *data;
	int flags;
	struct proc *p;
{

#ifdef lint
	tp = tp; data = data; flags = flags; p = p;
#endif
	return (-1);
}
