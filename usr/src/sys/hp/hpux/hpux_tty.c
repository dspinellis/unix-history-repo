/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: hpux_tty.c 1.1 90/07/09$
 *
 *	@(#)hpux_tty.c	7.6 (Berkeley) %G%
 */

/*
 * stty/gtty/termio emulation stuff
 */
#ifdef HPUXCOMPAT

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/user.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/proc.h"
#include "sys/file.h"
#include "sys/conf.h"
#include "sys/buf.h"
#include "sys/uio.h"
#include "sys/kernel.h"

#include "hpux.h"
#include "hpux_termio.h"

/*
 * Map BSD/POSIX style termios info to and from SYS5 style termio stuff.
 */
hpuxtermio(fp, com, data)
	struct file *fp;
	caddr_t data;
{
	struct termios tios;
	int line, error, (*ioctlrout)();
	register struct hpuxtermio *tiop;

	ioctlrout = fp->f_ops->fo_ioctl;
	tiop = (struct hpuxtermio *)data;
	switch (com) {
	case HPUXTCGETA:
		/*
		 * Get BSD terminal state
		 */
		bzero(data, sizeof(struct hpuxtermio));
		if (error = (*ioctlrout)(fp, TIOCGETA, (caddr_t)&tios))
			break;
		/*
		 * Set iflag.
		 * Same through ICRNL, no BSD equivs for IUCLC, IENQAK
		 */
		tiop->c_iflag = tios.c_iflag & 0x1ff;
		if (tios.c_iflag & IXON)
			tiop->c_iflag |= TIO_IXON;
		if (tios.c_iflag & IXOFF)
			tiop->c_iflag |= TIO_IXOFF;
		if (tios.c_iflag & IXANY)
			tiop->c_iflag |= TIO_IXANY;
		/*
		 * Set oflag.
		 * No BSD equivs for OLCUC/OCRNL/ONOCR/ONLRET/OFILL/OFDEL
		 * or any of the delays.
		 */
		if (tios.c_oflag & OPOST)
			tiop->c_oflag |= TIO_OPOST;
		if (tios.c_oflag & ONLCR)
			tiop->c_oflag |= TIO_ONLCR;
		if (tios.c_oflag & OXTABS)
			tiop->c_oflag |= TIO_TAB3;
		/*
		 * Set cflag.
		 * Baud from ospeed, rest from cflag.
		 */
		tiop->c_cflag = bsdtohpuxbaud(tios.c_ospeed);
		switch (tios.c_cflag & CSIZE) {
		case CS5:
			tiop->c_cflag |= TIO_CS5; break;
		case CS6:
			tiop->c_cflag |= TIO_CS6; break;
		case CS7:
			tiop->c_cflag |= TIO_CS7; break;
		case CS8:
			tiop->c_cflag |= TIO_CS8; break;
		}
		if (tios.c_cflag & CSTOPB)
			tiop->c_cflag |= TIO_CSTOPB;
		if (tios.c_cflag & CREAD)
			tiop->c_cflag |= TIO_CREAD;
		if (tios.c_cflag & PARENB)
			tiop->c_cflag |= TIO_PARENB;
		if (tios.c_cflag & PARODD)
			tiop->c_cflag |= TIO_PARODD;
		if (tios.c_cflag & HUPCL)
			tiop->c_cflag |= TIO_HUPCL;
		if (tios.c_cflag & CLOCAL)
			tiop->c_cflag |= TIO_CLOCAL;
		/*
		 * Set lflag.
		 * No BSD equiv for XCASE.
		 */
		if (tios.c_lflag & ECHOE)
			tiop->c_lflag |= TIO_ECHOE;
		if (tios.c_lflag & ECHOK)
			tiop->c_lflag |= TIO_ECHOK;
		if (tios.c_lflag & ECHO)
			tiop->c_lflag |= TIO_ECHO;
		if (tios.c_lflag & ECHONL)
			tiop->c_lflag |= TIO_ECHONL;
		if (tios.c_lflag & ISIG)
			tiop->c_lflag |= TIO_ISIG;
		if (tios.c_lflag & ICANON)
			tiop->c_lflag |= TIO_ICANON;
		if (tios.c_lflag & NOFLSH)
			tiop->c_lflag |= TIO_NOFLSH;
		/*
		 * Line discipline
		 */
		line = 0;
		(void) (*ioctlrout)(fp, TIOCGETD, (caddr_t)&line);
		tiop->c_line = line;
		/*
		 * Set editing chars
		 */
		tiop->c_cc[HPUXVINTR] = tios.c_cc[VINTR];
		tiop->c_cc[HPUXVQUIT] = tios.c_cc[VQUIT];
		tiop->c_cc[HPUXVERASE] = tios.c_cc[VERASE];
		tiop->c_cc[HPUXVKILL] = tios.c_cc[VKILL];
		if (tiop->c_lflag & TIO_ICANON) {
			tiop->c_cc[HPUXVEOF] = tios.c_cc[VEOF];
			tiop->c_cc[HPUXVEOL] = tios.c_cc[VEOL];
		} else {
			tiop->c_cc[HPUXVMIN] = tios.c_cc[VMIN];
			tiop->c_cc[HPUXVTIME] = tios.c_cc[VTIME];
		}
		break;

	case HPUXTCSETA:
	case HPUXTCSETAW:
	case HPUXTCSETAF:
		/*
		 * Get old characteristics and determine if we are a tty.
		 */
		if (error = (*ioctlrout)(fp, TIOCGETA, (caddr_t)&tios))
			break;
		/*
		 * Set iflag.
		 * Same through ICRNL, no HP-UX equiv for IMAXBEL
		 */
		tios.c_iflag &= ~(IXON|IXOFF|IXANY|0x1ff);
		tios.c_iflag |= tiop->c_iflag & 0x1ff;
		if (tiop->c_iflag & TIO_IXON)
			tios.c_iflag |= IXON;
		if (tiop->c_iflag & TIO_IXOFF)
			tios.c_iflag |= IXOFF;
		if (tiop->c_iflag & TIO_IXANY)
			tios.c_iflag |= IXANY;
		/*
		 * Set oflag.
		 * No HP-UX equiv for ONOEOT
		 */
		tios.c_oflag &= ~(OPOST|ONLCR|OXTABS);
		if (tiop->c_oflag & TIO_OPOST)
			tios.c_oflag |= OPOST;
		if (tiop->c_oflag & TIO_ONLCR)
			tios.c_oflag |= ONLCR;
		if (tiop->c_oflag & TIO_TAB3)
			tios.c_oflag |= OXTABS;
		/*
		 * Set cflag.
		 * No HP-UX equiv for CCTS_OFLOW/CCTS_IFLOW/MDMBUF
		 */
		tios.c_cflag &=
			~(CSIZE|CSTOPB|CREAD|PARENB|PARODD|HUPCL|CLOCAL);
		switch (tiop->c_cflag & TIO_CSIZE) {
		case TIO_CS5:
			tios.c_cflag |= CS5; break;
		case TIO_CS6:
			tios.c_cflag |= CS6; break;
		case TIO_CS7:
			tios.c_cflag |= CS7; break;
		case TIO_CS8:
			tios.c_cflag |= CS8; break;
		}
		if (tiop->c_cflag & TIO_CSTOPB)
			tios.c_cflag |= CSTOPB;
		if (tiop->c_cflag & TIO_CREAD)
			tios.c_cflag |= CREAD;
		if (tiop->c_cflag & TIO_PARENB)
			tios.c_cflag |= PARENB;
		if (tiop->c_cflag & TIO_PARODD)
			tios.c_cflag |= PARODD;
		if (tiop->c_cflag & TIO_HUPCL)
			tios.c_cflag |= HUPCL;
		if (tiop->c_cflag & TIO_CLOCAL)
			tios.c_cflag |= CLOCAL;
		/*
		 * Set lflag.
		 * No HP-UX equiv for ECHOKE/ECHOPRT/ECHOCTL
		 * IEXTEN treated as part of ICANON
		 */
		tios.c_lflag &= ~(ECHOE|ECHOK|ECHO|ISIG|ICANON|IEXTEN|NOFLSH);
		if (tiop->c_lflag & TIO_ECHOE)
			tios.c_lflag |= ECHOE;
		if (tiop->c_lflag & TIO_ECHOK)
			tios.c_lflag |= ECHOK;
		if (tiop->c_lflag & TIO_ECHO)
			tios.c_lflag |= ECHO;
		if (tiop->c_lflag & TIO_ECHONL)
			tios.c_lflag |= ECHONL;
		if (tiop->c_lflag & TIO_ISIG)
			tios.c_lflag |= ISIG;
		if (tiop->c_lflag & TIO_ICANON)
			tios.c_lflag |= (ICANON|IEXTEN);
		if (tiop->c_lflag & TIO_NOFLSH)
			tios.c_lflag |= NOFLSH;
		/*
		 * Set editing chars.
		 * No HP-UX equivs of VEOL2/VWERASE/VREPRINT/VSUSP/VDSUSP
		 * VSTOP/VLNEXT/VDISCARD/VMIN/VTIME/VSTATUS/VERASE2
		 */
		tios.c_cc[VINTR] = tiop->c_cc[HPUXVINTR];
		tios.c_cc[VQUIT] = tiop->c_cc[HPUXVQUIT];
		tios.c_cc[VERASE] = tiop->c_cc[HPUXVERASE];
		tios.c_cc[VKILL] = tiop->c_cc[HPUXVKILL];
		if (tios.c_lflag & ICANON) {
			tios.c_cc[VEOF] = tiop->c_cc[HPUXVEOF];
			tios.c_cc[VEOL] = tiop->c_cc[HPUXVEOL];
		} else {
			tios.c_cc[VMIN] = tiop->c_cc[HPUXVMIN];
			tios.c_cc[VTIME] = tiop->c_cc[HPUXVTIME];
		}
		/*
		 * Set the new stuff
		 */
		if (com == HPUXTCSETA)
			com = TIOCSETA;
		else if (com == HPUXTCSETAW)
			com = TIOCSETAW;
		else
			com = TIOCSETAF;
		error = (*ioctlrout)(fp, com, (caddr_t)&tios);
		if (error == 0) {
			/*
			 * Set line discipline
			 */
			line = tiop->c_line;
			(void) (*ioctlrout)(fp, TIOCSETD, (caddr_t)&line);
			/*
			 * Set non-blocking IO if VMIN == VTIME == 0.
			 * Should handle the other cases as well.  It also
			 * isn't correct to just turn it off as it could be
			 * on as the result of a fcntl operation.
			 * XXX - wouldn't need to do this at all if VMIN/VTIME
			 * were implemented.
			 */
			line = (tiop->c_cc[HPUXVMIN] == 0 &&
				tiop->c_cc[HPUXVTIME] == 0);
			(void) fset(fp, FNDELAY, line);
		}
		break;

	default:
		error = EINVAL;
		break;
	}
	return(error);
}

bsdtohpuxbaud(bsdspeed)
	long bsdspeed;
{
	switch (bsdspeed) {
	case B0:     return(TIO_B0);
	case B50:    return(TIO_B50);
	case B75:    return(TIO_B75);
	case B110:   return(TIO_B110);
	case B134:   return(TIO_B134);
	case B150:   return(TIO_B150);
	case B200:   return(TIO_B200);
	case B300:   return(TIO_B300);
	case B600:   return(TIO_B600);
	case B1200:  return(TIO_B1200);
	case B1800:  return(TIO_B1800);
	case B2400:  return(TIO_B2400);
	case B4800:  return(TIO_B4800);
	case B9600:  return(TIO_B9600);
	case B19200: return(TIO_B19200);
	case B38400: return(TIO_B38400);
	default:     return(TIO_B0);
	}
}

hpuxtobsdbaud(hpuxspeed)
	int hpuxspeed;
{
	static char hpuxtobsdbaudtab[32] = {
		B0,	B50,	B75,	B110,	B134,	B150,	B200,	B300,
		B600,	B0,	B1200,	B1800,	B2400,	B0,	B4800,	B0,
		B9600,	B19200,	B38400,	B0,	B0,	B0,	B0,	B0,
		B0,	B0,	B0,	B0,	B0,	B0,	EXTA,	EXTB
	};

	return(hpuxtobsdbaudtab[hpuxspeed & TIO_CBAUD]);
}

/* #ifdef COMPAT */
ohpuxgtty(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{

	return (getsettty(uap->fdes, HPUXTIOCGETP, uap->cmarg));
}

ohpuxstty(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{

	return (getsettty(uap->fdes, HPUXTIOCSETP, uap->cmarg));
}

/*
 * Simplified version of ioctl() for use by
 * gtty/stty and TIOCGETP/TIOCSETP.
 */
getsettty(fdes, com, cmarg)
	int fdes, com;
	caddr_t cmarg;
{
	register struct file *fp;
	struct hpuxsgttyb hsb;
	struct sgttyb sb;
	int error;

	if ((unsigned)fdes >= NOFILE || (fp = u.u_ofile[fdes]) == NULL)
		return (EBADF);
	if ((fp->f_flag & (FREAD|FWRITE)) == 0)
		return (EBADF);
	if (com == HPUXTIOCSETP) {
		if (error = copyin(cmarg, (caddr_t)&hsb, sizeof hsb))
			return (error);
		sb.sg_ispeed = hsb.sg_ispeed;
		sb.sg_ospeed = hsb.sg_ospeed;
		sb.sg_erase = hsb.sg_erase;
		sb.sg_kill = hsb.sg_kill;
		sb.sg_flags = hsb.sg_flags & ~(V7_HUPCL|V7_XTABS|V7_NOAL);
		if (hsb.sg_flags & V7_XTABS)
			sb.sg_flags |= XTABS;
		if (hsb.sg_flags & V7_HUPCL)
			(void)(*fp->f_ops->fo_ioctl)(fp, TIOCHPCL, (caddr_t)0);
		com = TIOCSETP;
	} else {
		bzero((caddr_t)&hsb, sizeof hsb);
		com = TIOCGETP;
	}
	error = (*fp->f_ops->fo_ioctl)(fp, com, (caddr_t)&sb);
	if (error == 0 && com == TIOCGETP) {
		hsb.sg_ispeed = sb.sg_ispeed;
		hsb.sg_ospeed = sb.sg_ospeed;
		hsb.sg_erase = sb.sg_erase;
		hsb.sg_kill = sb.sg_kill;
		hsb.sg_flags = sb.sg_flags & ~(V7_HUPCL|V7_XTABS|V7_NOAL);
		if (sb.sg_flags & XTABS)
			hsb.sg_flags |= V7_XTABS;
		error = copyout((caddr_t)&hsb, cmarg, sizeof hsb);
	}
	return (error);
}
/* #endif */
#endif
