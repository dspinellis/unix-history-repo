/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)fb.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: fb.c,v 1.7 92/11/26 01:12:48 torek Exp $
 */

/*
 * /dev/fb (indirect frame buffer driver).  This is gross; we should
 * just build cdevsw[] dynamically.
 */

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/device.h>
#include <sys/proc.h>
#include <sys/fbio.h>

#include <machine/fbvar.h>

static struct fbdevice *devfb;

void
fb_unblank()
{

	if (devfb)
		(*devfb->fb_driver->fbd_unblank)(devfb->fb_device);
}

void
fb_attach(fb)
	struct fbdevice *fb;
{

if (devfb) panic("multiple /dev/fb declarers");
	devfb = fb;
}

int
fbopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{

	if (devfb == NULL)
		return (ENXIO);
	return (cdevsw[devfb->fb_major].d_open(dev, flags, mode, p));
}

int
fbclose(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{

	return (cdevsw[devfb->fb_major].d_close(dev, flags, mode, p));
}

int
fbioctl(dev, cmd, data, flags, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flags;
	struct proc *p;
{

	return (cdevsw[devfb->fb_major].d_ioctl(dev, cmd, data, flags, p));
}

int
fbmap(dev, off, prot)
	dev_t dev;
	int off, prot;
{
	int (*map)() = cdevsw[devfb->fb_major].d_mmap;

	if (map == NULL)
		return (-1);
	return (map(dev, off, prot));
}
