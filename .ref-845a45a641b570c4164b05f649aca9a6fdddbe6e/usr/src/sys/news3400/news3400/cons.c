/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cons.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/file.h>
#include <sys/conf.h>

#include "bm.h"

dev_t consdev = (dev_t)NULL;	/* initialized by consinit() */
struct tty *constty = 0;

vcopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_open)(dev, flag, mode, p));
}

vcclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_close)(dev, flag, mode, p));
}
 
vcread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_read)(dev, uio, flag));
}
 
vcwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_write)(dev, uio, flag));
}
 
vcstop(tp, flag)
	struct tty *tp;
	int flag;
{
	dev_t dev;

	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_stop)(tp, flag));
}

vcioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	if ((dev = consdev) == NULL)
		return 0;
	return ((*cdevsw[major(dev)].d_ioctl)(dev, cmd, data, flag, p));
}

/*ARGSUSED*/
vcselect(dev, rw, p)
	dev_t dev;
	int rw;
	struct proc *p;
{
	if ((dev = consdev) == NULL)
		return 1;
	return (ttselect(dev, rw, p));
}

/*
 * Get character from console.
 */
cngetc()
{
	/* notyet */
	return(0);
}

#define	SCC_CONSOLE	0
/*
 * Print a character on console.
 */
cnputc(c)
	int c;
{
	int s;
	int (*putc)(), scccons_putc(), bmcons_putc();

	if (consdev == NULL)
		return 0;

#if NBM > 0
	if (consdev == makedev(1, 0))
		putc = scccons_putc;
	else
		putc = bmcons_putc;
#else
	putc = scccons_putc;
#endif

	/* KU: should be much more efficient */
	s = splhigh();
	putc(c);
	if (c == '\n')
		putc('\r');
	splx(s);
}

scccons_putc(c)
	int c;
{
	char cnbuf[1];

	cnbuf[0] = (char)c;
	scc_error_write(SCC_CONSOLE, cnbuf, 1);
}

#if NBM > 0
bmcons_putc(c)
	int c;
{
	char cnbuf[1];

	cnbuf[0] = (char)c;
	vt100_write(0, cnbuf, 1);
}
#endif
