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
 *	@(#)sun_ioctl.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: sun_ioctl.c,v 1.7 93/05/28 04:40:43 torek Exp $
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/filedesc.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <sys/tty.h>

/*
 * SunOS ioctl calls.
 * This file is something of a hodge-podge.
 * Support gets added as things turn up....
 */

struct sun_ttysize {
	int	ts_row;
	int	ts_col;
};

struct sun_termio {
	u_short	c_iflag;
	u_short	c_oflag;
	u_short	c_cflag;
	u_short	c_lflag;
	char	c_line;
	unsigned char c_cc[8];
};

struct sun_ioctl_args {
	int	fd;
	int	cmd;
	caddr_t	data;
};
sun_ioctl(p, uap, retval)
	register struct proc *p;
	register struct sun_ioctl_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	register int (*ctl)();
	int error;

	if ((unsigned)uap->fd >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[uap->fd]) == NULL)
		return (EBADF);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0)
		return (EBADF);
	ctl = fp->f_ops->fo_ioctl;

	switch (uap->cmd) {

	case _IOR('t', 0, int):
		uap->cmd = TIOCGETD;
		break;

	case _IOW('t', 1, int):
		uap->cmd = TIOCSETD;
		break;

	case _IO('t', 36): {		/* sun TIOCCONS, no parameters */
		int on = 1;
		return ((*ctl)(fp, TIOCCONS, (caddr_t)&on, p));
	    }

	case _IOW('t', 37, struct sun_ttysize): {
		struct winsize ws;
		if ((error = (*ctl)(fp, TIOCGWINSZ, (caddr_t)&ws, p)) != 0)
			return (error);
		ws.ws_row = ((struct sun_ttysize *)uap->data)->ts_row;
		ws.ws_col = ((struct sun_ttysize *)uap->data)->ts_col;
		return ((*ctl)(fp, TIOCSWINSZ, (caddr_t)&ws, p));
	    }

	case _IOW('t', 38, struct sun_ttysize): {
		struct winsize ws;
		if ((error = (*ctl)(fp, TIOCGWINSZ, (caddr_t)&ws, p)) != 0)
			return (error);
		((struct sun_ttysize *)uap->data)->ts_row = ws.ws_row;
		((struct sun_ttysize *)uap->data)->ts_col = ws.ws_col;
		return (0);
	    }

	case _IOR('t', 130, int):
		uap->cmd = TIOCSPGRP;
		break;

	case _IOR('t', 131, int):
		uap->cmd = TIOCGPGRP;
		break;

	case _IO('t', 132):
		uap->cmd = TIOCSCTTY;
		break;

	case _IOR('T', 1, struct sun_termio): {
		struct termios bt;
		struct sun_termio st;
		int speed;
		static struct speedtab sptab[] = {
			{ 0, 0 }, { 50, 1 }, { 75, 2 }, { 110, 3 },
			{ 134, 4 }, { 135, 4 }, { 150, 5 }, { 200, 6 },
			{ 300, 7 }, { 600, 8 }, { 1200, 9 }, { 1800, 10 },
			{ 2400, 11 }, { 4800, 12 }, { 9600, 13 },
			{ 19200, 14 }, { 38400, 15 }, { -1, -1 }
		};

		if ((error = (*ctl)(fp, TIOCGETA, (caddr_t)&bt, p)) != 0)
			return (error);
		/* most bits match */
		st.c_iflag = bt.c_iflag & (IGNBRK|BRKINT|IGNPAR|PARMRK|INPCK|
		    ISTRIP|INLCR|IGNCR|ICRNL|IXANY|IMAXBEL);
		if (bt.c_iflag & IXON)
			st.c_iflag |= 0x0400;
		if (bt.c_iflag & IXOFF)
			st.c_iflag |= 0x1000;
		st.c_oflag = bt.c_oflag & OPOST;
		if (bt.c_oflag & ONLCR)
			st.c_oflag |= 0x0004;
		if (bt.c_oflag & OXTABS)
			st.c_oflag |= 0x1800;
		speed = ttspeedtab(bt.c_ospeed, sptab);
		st.c_cflag = speed >= 0 ? speed : 0;
		st.c_cflag |= (bt.c_cflag & CSIZE) >> 4;
		if (bt.c_cflag & CSTOPB)
			st.c_cflag |= 0x40;
		if (bt.c_cflag & PARENB)
			st.c_cflag |= 0x100;
		if (bt.c_cflag & PARODD)
			st.c_cflag |= 0x200;
		if (bt.c_cflag & HUPCL)
			st.c_cflag |= 0x400;
		if (bt.c_cflag & CLOCAL)
			st.c_cflag |= 0x800;
		st.c_lflag = 0;
		if (bt.c_lflag & (ECHOKE|ECHOE|ECHOK))
			st.c_lflag |= 0x0800;
		if (bt.c_lflag & ECHO)
			st.c_lflag |= 0x0008;
		if (bt.c_lflag & ECHONL)
			st.c_lflag |= 0x0040;
		if (bt.c_lflag & ECHOPRT)
			st.c_lflag |= 0x0400;
		if (bt.c_lflag & ECHOCTL)
			st.c_lflag |= 0x0200;
		if (bt.c_lflag & ISIG)
			st.c_lflag |= 0x0001;
		if (bt.c_lflag & ICANON)
			st.c_lflag |= 0x0002;
		if (bt.c_lflag & IEXTEN)
			st.c_lflag |= 0x8000;
		if (bt.c_lflag & NOFLSH)
			st.c_lflag |= 0x0080;
#define mapcc(x) ((x) == _POSIX_VDISABLE ? 0 : (x))
		st.c_cc[0] = mapcc(bt.c_cc[VINTR]);
		st.c_cc[1] = mapcc(bt.c_cc[VQUIT]);
		st.c_cc[2] = mapcc(bt.c_cc[VERASE]);
		st.c_cc[3] = mapcc(bt.c_cc[VKILL]);
		st.c_cc[4] = mapcc(bt.c_cc[VEOF]);
		st.c_cc[5] = mapcc(bt.c_cc[VEOL]);
		st.c_cc[6] = mapcc(bt.c_cc[VEOL2]);
		st.c_cc[7] = 0;
		return (copyout((caddr_t)&st, uap->data, sizeof(st)));
	    }
	}
	return (ioctl(p, uap, retval));
}
