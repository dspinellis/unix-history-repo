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
 *	@(#)hpux_tty.c	7.13 (Berkeley) %G%
 */

/*
 * stty/gtty/termio emulation stuff
 */
#ifdef HPUXCOMPAT

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/filedesc.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/file.h>
#include <sys/conf.h>
#include <sys/buf.h>
#include <sys/kernel.h>

#include <hp/hpux/hpux.h>
#include <hp/hpux/hpux_termio.h>

/*
 * Map BSD/POSIX style termios info to and from SYS5 style termio stuff.
 */
hpuxtermio(fp, com, data, p)
	struct file *fp;
	caddr_t data;
	struct proc *p;
{
	struct termios tios;
	struct hpuxtermios htios;
	int line, error, (*ioctlrout)();
	int newi = 0;

	ioctlrout = fp->f_ops->fo_ioctl;
	switch (com) {
	case HPUXTCGETATTR:
		newi = 1;
		/* fall into ... */
	case HPUXTCGETA:
		/*
		 * Get BSD terminal state
		 */
		if (error = (*ioctlrout)(fp, TIOCGETA, (caddr_t)&tios, p))
			break;
		bzero((char *)&htios, sizeof htios);
		/*
		 * Set iflag.
		 * Same through ICRNL, no BSD equivs for IUCLC, IENQAK
		 */
		htios.c_iflag = tios.c_iflag & 0x1ff;
		if (tios.c_iflag & IXON)
			htios.c_iflag |= TIO_IXON;
		if (tios.c_iflag & IXOFF)
			htios.c_iflag |= TIO_IXOFF;
		if (tios.c_iflag & IXANY)
			htios.c_iflag |= TIO_IXANY;
		/*
		 * Set oflag.
		 * No BSD equivs for OLCUC/OCRNL/ONOCR/ONLRET/OFILL/OFDEL
		 * or any of the delays.
		 */
		if (tios.c_oflag & OPOST)
			htios.c_oflag |= TIO_OPOST;
		if (tios.c_oflag & ONLCR)
			htios.c_oflag |= TIO_ONLCR;
		if (tios.c_oflag & OXTABS)
			htios.c_oflag |= TIO_TAB3;
		/*
		 * Set cflag.
		 * Baud from ospeed, rest from cflag.
		 */
		htios.c_cflag = bsdtohpuxbaud(tios.c_ospeed);
		switch (tios.c_cflag & CSIZE) {
		case CS5:
			htios.c_cflag |= TIO_CS5; break;
		case CS6:
			htios.c_cflag |= TIO_CS6; break;
		case CS7:
			htios.c_cflag |= TIO_CS7; break;
		case CS8:
			htios.c_cflag |= TIO_CS8; break;
		}
		if (tios.c_cflag & CSTOPB)
			htios.c_cflag |= TIO_CSTOPB;
		if (tios.c_cflag & CREAD)
			htios.c_cflag |= TIO_CREAD;
		if (tios.c_cflag & PARENB)
			htios.c_cflag |= TIO_PARENB;
		if (tios.c_cflag & PARODD)
			htios.c_cflag |= TIO_PARODD;
		if (tios.c_cflag & HUPCL)
			htios.c_cflag |= TIO_HUPCL;
		if (tios.c_cflag & CLOCAL)
			htios.c_cflag |= TIO_CLOCAL;
		/*
		 * Set lflag.
		 * No BSD equiv for XCASE.
		 */
		if (tios.c_lflag & ECHOE)
			htios.c_lflag |= TIO_ECHOE;
		if (tios.c_lflag & ECHOK)
			htios.c_lflag |= TIO_ECHOK;
		if (tios.c_lflag & ECHO)
			htios.c_lflag |= TIO_ECHO;
		if (tios.c_lflag & ECHONL)
			htios.c_lflag |= TIO_ECHONL;
		if (tios.c_lflag & ISIG)
			htios.c_lflag |= TIO_ISIG;
		if (tios.c_lflag & ICANON)
			htios.c_lflag |= TIO_ICANON;
		if (tios.c_lflag & NOFLSH)
			htios.c_lflag |= TIO_NOFLSH;
		/*
		 * Line discipline
		 */
		if (!newi) {
			line = 0;
			(void) (*ioctlrout)(fp, TIOCGETD, (caddr_t)&line, p);
			htios.c_reserved = line;
		}
		/*
		 * Set editing chars.
		 * No BSD equiv for VSWTCH.
		 */
		htios.c_cc[HPUXVINTR] = tios.c_cc[VINTR];
		htios.c_cc[HPUXVQUIT] = tios.c_cc[VQUIT];
		htios.c_cc[HPUXVERASE] = tios.c_cc[VERASE];
		htios.c_cc[HPUXVKILL] = tios.c_cc[VKILL];
		htios.c_cc[HPUXVEOF] = tios.c_cc[VEOF];
		htios.c_cc[HPUXVEOL] = tios.c_cc[VEOL];
		htios.c_cc[HPUXVEOL2] = tios.c_cc[VEOL2];
		htios.c_cc[HPUXVSWTCH] = 0;
		htios.c_cc[HPUXVMINS] = tios.c_cc[VMIN];
		htios.c_cc[HPUXVTIMES] = tios.c_cc[VTIME];
		htios.c_cc[HPUXVSUSP] = tios.c_cc[VSUSP];
		htios.c_cc[HPUXVSTART] = tios.c_cc[VSTART];
		htios.c_cc[HPUXVSTOP] = tios.c_cc[VSTOP];
		if (newi)
			bcopy((char *)&htios, data, sizeof htios);
		else
			termiostotermio(&htios, (struct hpuxtermio *)data);
		break;

	case HPUXTCSETATTR:
	case HPUXTCSETATTRD:
	case HPUXTCSETATTRF:
		newi = 1;
		/* fall into ... */
	case HPUXTCSETA:
	case HPUXTCSETAW:
	case HPUXTCSETAF:
		/*
		 * Get old characteristics and determine if we are a tty.
		 */
		if (error = (*ioctlrout)(fp, TIOCGETA, (caddr_t)&tios, p))
			break;
		if (newi)
			bcopy(data, (char *)&htios, sizeof htios);
		else
			termiototermios((struct termio *)data, &htios, &tios);
		/*
		 * Set iflag.
		 * Same through ICRNL, no HP-UX equiv for IMAXBEL
		 */
		tios.c_iflag &= ~(IXON|IXOFF|IXANY|0x1ff);
		tios.c_iflag |= htios.c_iflag & 0x1ff;
		if (htios.c_iflag & TIO_IXON)
			tios.c_iflag |= IXON;
		if (htios.c_iflag & TIO_IXOFF)
			tios.c_iflag |= IXOFF;
		if (htios.c_iflag & TIO_IXANY)
			tios.c_iflag |= IXANY;
		/*
		 * Set oflag.
		 * No HP-UX equiv for ONOEOT
		 */
		tios.c_oflag &= ~(OPOST|ONLCR|OXTABS);
		if (htios.c_oflag & TIO_OPOST)
			tios.c_oflag |= OPOST;
		if (htios.c_oflag & TIO_ONLCR)
			tios.c_oflag |= ONLCR;
		if (htios.c_oflag & TIO_TAB3)
			tios.c_oflag |= OXTABS;
		/*
		 * Set cflag.
		 * No HP-UX equiv for CCTS_OFLOW/CCTS_IFLOW/MDMBUF
		 */
		tios.c_cflag &=
			~(CSIZE|CSTOPB|CREAD|PARENB|PARODD|HUPCL|CLOCAL);
		switch (htios.c_cflag & TIO_CSIZE) {
		case TIO_CS5:
			tios.c_cflag |= CS5; break;
		case TIO_CS6:
			tios.c_cflag |= CS6; break;
		case TIO_CS7:
			tios.c_cflag |= CS7; break;
		case TIO_CS8:
			tios.c_cflag |= CS8; break;
		}
		if (htios.c_cflag & TIO_CSTOPB)
			tios.c_cflag |= CSTOPB;
		if (htios.c_cflag & TIO_CREAD)
			tios.c_cflag |= CREAD;
		if (htios.c_cflag & TIO_PARENB)
			tios.c_cflag |= PARENB;
		if (htios.c_cflag & TIO_PARODD)
			tios.c_cflag |= PARODD;
		if (htios.c_cflag & TIO_HUPCL)
			tios.c_cflag |= HUPCL;
		if (htios.c_cflag & TIO_CLOCAL)
			tios.c_cflag |= CLOCAL;
		/*
		 * Set lflag.
		 * No HP-UX equiv for ECHOKE/ECHOPRT/ECHOCTL
		 * IEXTEN treated as part of ICANON
		 */
		tios.c_lflag &= ~(ECHOE|ECHOK|ECHO|ISIG|ICANON|IEXTEN|NOFLSH);
		if (htios.c_lflag & TIO_ECHOE)
			tios.c_lflag |= ECHOE;
		if (htios.c_lflag & TIO_ECHOK)
			tios.c_lflag |= ECHOK;
		if (htios.c_lflag & TIO_ECHO)
			tios.c_lflag |= ECHO;
		if (htios.c_lflag & TIO_ECHONL)
			tios.c_lflag |= ECHONL;
		if (htios.c_lflag & TIO_ISIG)
			tios.c_lflag |= ISIG;
		if (htios.c_lflag & TIO_ICANON)
			tios.c_lflag |= (ICANON|IEXTEN);
		if (htios.c_lflag & TIO_NOFLSH)
			tios.c_lflag |= NOFLSH;
		/*
		 * Set editing chars.
		 * No HP-UX equivs of VWERASE/VREPRINT/VDSUSP/VLNEXT
		 * /VDISCARD/VSTATUS/VERASE2
		 */
		tios.c_cc[VINTR] = htios.c_cc[HPUXVINTR];
		tios.c_cc[VQUIT] = htios.c_cc[HPUXVQUIT];
		tios.c_cc[VERASE] = htios.c_cc[HPUXVERASE];
		tios.c_cc[VKILL] = htios.c_cc[HPUXVKILL];
		tios.c_cc[VEOF] = htios.c_cc[HPUXVEOF];
		tios.c_cc[VEOL] = htios.c_cc[HPUXVEOL];
		tios.c_cc[VEOL2] = htios.c_cc[HPUXVEOL2];
		tios.c_cc[VMIN] = htios.c_cc[HPUXVMINS];
		tios.c_cc[VTIME] = htios.c_cc[HPUXVTIMES];
		tios.c_cc[VSUSP] = htios.c_cc[HPUXVSUSP];
		tios.c_cc[VSTART] = htios.c_cc[HPUXVSTART];
		tios.c_cc[VSTOP] = htios.c_cc[HPUXVSTOP];

		/*
		 * Set the new stuff
		 */
		if (com == HPUXTCSETA || com == HPUXTCSETATTR)
			com = TIOCSETA;
		else if (com == HPUXTCSETAW || com == HPUXTCSETATTRD)
			com = TIOCSETAW;
		else
			com = TIOCSETAF;
		error = (*ioctlrout)(fp, com, (caddr_t)&tios, p);
		if (error == 0) {
			/*
			 * Set line discipline
			 */
			if (!newi) {
				line = htios.c_reserved;
				(void) (*ioctlrout)(fp, TIOCSETD,
						    (caddr_t)&line, p);
			}
			/*
			 * Set non-blocking IO if VMIN == VTIME == 0.
			 * Should handle the other cases as well.  It also
			 * isn't correct to just turn it off as it could be
			 * on as the result of a fcntl operation.
			 * XXX - wouldn't need to do this at all if VMIN/VTIME
			 * were implemented.
			 */
			line = (htios.c_cc[HPUXVMIN] == 0 &&
				htios.c_cc[HPUXVTIME] == 0);
			if (line)
				fp->f_flag |= FNONBLOCK;
			else
				fp->f_flag &= ~FNONBLOCK;
			(void) (*ioctlrout)(fp, FIONBIO, (caddr_t)&line, p);
		}
		break;

	default:
		error = EINVAL;
		break;
	}
	return(error);
}

termiototermios(tio, tios, bsdtios)
	struct hpuxtermio *tio;
	struct hpuxtermios *tios;
	struct termios *bsdtios;
{
	int i;

	bzero((char *)tios, sizeof *tios);
	tios->c_iflag = tio->c_iflag;
	tios->c_oflag = tio->c_oflag;
	tios->c_cflag = tio->c_cflag;
	tios->c_lflag = tio->c_lflag;
	tios->c_reserved = tio->c_line;
	for (i = 0; i <= HPUXVSWTCH; i++)
		tios->c_cc[i] = tio->c_cc[i];
	if (tios->c_lflag & TIO_ICANON) {
		tios->c_cc[HPUXVEOF] = tio->c_cc[HPUXVEOF];
		tios->c_cc[HPUXVEOL] = tio->c_cc[HPUXVEOL];
		tios->c_cc[HPUXVMINS] = tios->c_cc[HPUXVTIMES] = 0;
	} else {
		tios->c_cc[HPUXVEOF] = tios->c_cc[HPUXVEOL] = 0;
		tios->c_cc[HPUXVMINS] = tio->c_cc[HPUXVMIN];
		tios->c_cc[HPUXVTIMES] = tio->c_cc[HPUXVTIME];
	}
	tios->c_cc[HPUXVMINS] = bsdtios->c_cc[VMIN];
	tios->c_cc[HPUXVTIMES] = bsdtios->c_cc[VTIME];
	tios->c_cc[HPUXVSUSP] = bsdtios->c_cc[VSUSP];
	tios->c_cc[HPUXVSTART] = bsdtios->c_cc[VSTART];
	tios->c_cc[HPUXVSTOP] = bsdtios->c_cc[VSTOP];
}

termiostotermio(tios, tio)
	struct hpuxtermios *tios;
	struct hpuxtermio *tio;
{
	int i;

	tio->c_iflag = tios->c_iflag;
	tio->c_oflag = tios->c_oflag;
	tio->c_cflag = tios->c_cflag;
	tio->c_lflag = tios->c_lflag;
	tio->c_line = tios->c_reserved;
	for (i = 0; i <= HPUXVSWTCH; i++)
		tio->c_cc[i] = tios->c_cc[i];
	if (tios->c_lflag & TIO_ICANON) {
		tio->c_cc[HPUXVEOF] = tios->c_cc[HPUXVEOF];
		tio->c_cc[HPUXVEOL] = tios->c_cc[HPUXVEOL];
	} else {
		tio->c_cc[HPUXVMIN] = tios->c_cc[HPUXVMINS];
		tio->c_cc[HPUXVTIME] = tios->c_cc[HPUXVTIMES];
	}
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

#ifdef COMPAT_OHPUX
ohpuxgtty(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{

	return (getsettty(p, uap->fdes, HPUXTIOCGETP, uap->cmarg));
}

ohpuxstty(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{

	return (getsettty(p, uap->fdes, HPUXTIOCSETP, uap->cmarg));
}

/*
 * Simplified version of ioctl() for use by
 * gtty/stty and TIOCGETP/TIOCSETP.
 */
getsettty(p, fdes, com, cmarg)
	struct proc *p;
	int fdes, com;
	caddr_t cmarg;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	struct hpuxsgttyb hsb;
	struct sgttyb sb;
	int error;

	if (((unsigned)fdes) >= fdp->fd_nfiles ||
	    (fp = fdp->fd_ofiles[fdes]) == NULL)
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
			(void)(*fp->f_ops->fo_ioctl)
				(fp, TIOCHPCL, (caddr_t)0, p);
		com = TIOCSETP;
	} else {
		bzero((caddr_t)&hsb, sizeof hsb);
		com = TIOCGETP;
	}
	error = (*fp->f_ops->fo_ioctl)(fp, com, (caddr_t)&sb, p);
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
#endif
#endif
