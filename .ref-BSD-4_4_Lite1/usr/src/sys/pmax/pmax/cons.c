/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
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
 * from: Utah $Hdr: cons.c 1.1 90/07/09$
 *
 *	@(#)cons.c	8.2 (Berkeley) 1/11/94
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/file.h>
#include <sys/conf.h>

#include <pmax/stand/dec_prom.h>

/*
 * Console I/O is redirected to the appropriate device, either a screen and
 * keyboard, a serial port, or the "virtual" console.
 */
#include <pmax/pmax/cons.h>

extern struct tty *constty;	/* virtual console output device */

struct consdev cn_tab = {
	1,
	1,
	NODEV,
	(struct pmax_fb *)0,
	(int (*)())0,
	(int (*)())0,
	(void (*)())0,
	(struct tty *)0,
};

cnopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	if (cn_tab.cn_dev == NODEV)
		return (0);
	dev = cn_tab.cn_dev;
	return ((*cdevsw[major(dev)].d_open)(dev, flag, mode, p));
}
 
cnclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	if (cn_tab.cn_dev == NODEV)
		return (0);
	dev = cn_tab.cn_dev;
	return ((*cdevsw[major(dev)].d_close)(dev, flag, mode, p));
}
 
cnread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	if (cn_tab.cn_dev == NODEV)
		return (0);
	dev = cn_tab.cn_dev;
	return ((*cdevsw[major(dev)].d_read)(dev, uio, flag));
}
 
cnwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	if (constty)
		return ((*linesw[constty->t_line].l_write)(constty, uio, flag));
	if (cn_tab.cn_dev == NODEV)
		return (0);
	dev = cn_tab.cn_dev;
	return ((*cdevsw[major(dev)].d_write)(dev, uio, flag));
}
 
cnioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	int error;

	/*
	 * Superuser can always use this to wrest control of console
	 * output from the "virtual" console.
	 */
	if (cmd == TIOCCONS && constty) {
		error = suser(p->p_ucred, (u_short *) NULL);
		if (error)
			return (error);
		constty = NULL;
		return (0);
	}
#if 0
	if (constty) {
		error = (*linesw[constty->t_line].l_ioctl)
			(constty, cmd, data, flag, p);
		if (error >= 0)
			return (error);
	}
#endif
	if (cn_tab.cn_dev == NODEV)
		return (0);
	dev = cn_tab.cn_dev;
	return ((*cdevsw[major(dev)].d_ioctl)(dev, cmd, data, flag, p));
}

/*ARGSUSED*/
cnselect(dev, rw, p)
	dev_t dev;
	int rw;
	struct proc *p;
{
	if (cn_tab.cn_dev == NODEV)
		return (1);
	return (ttselect(cn_tab.cn_dev, rw, p));
}

/*
 * Get character from console.
 */
cngetc()
{

	/* check to be sure device has been initialized */
	if (cn_tab.cn_dev == NODEV || cn_tab.cn_disabled)
		return ((*callv->getchar)());
	return ((*cn_tab.cn_getc)(cn_tab.cn_dev));
}

/*
 * Print a character on console.
 */
cnputc(c)
	register int c;
{
	int s;

	if (cn_tab.cn_dev == NODEV || cn_tab.cn_disabled) {
		s = splhigh();
		(*callv->printf)("%c", c);
		splx(s);
	} else if (c) {
		if (c == '\n')
			(*cn_tab.cn_putc)(cn_tab.cn_dev, '\r');
		(*cn_tab.cn_putc)(cn_tab.cn_dev, c);
	}
}
