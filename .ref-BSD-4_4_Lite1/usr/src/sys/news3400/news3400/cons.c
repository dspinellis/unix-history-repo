/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)cons.c	8.1 (Berkeley) 6/11/93
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
