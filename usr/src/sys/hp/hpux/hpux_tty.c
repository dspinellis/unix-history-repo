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
 * from: Utah $Hdr: hpux_tty.c 1.7 89/04/11$
 *
 *	@(#)hpux_tty.c	7.2 (Berkeley) %G%
 */

/*
 * stty/gtty/termio emulation stuff
 */
#ifdef HPUXCOMPAT

#include "param.h"
#include "systm.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "file.h"
#include "conf.h"
#include "buf.h"
#include "uio.h"
#include "kernel.h"

#include "hpux.h"
#include "hpux_termio.h"

/*
 * XXX should just include syscontext.h but RETURN definition clashes
 * with defined constant in tty.h
 */
#undef RETURN
#define RETURN(value)	{ u.u_error = (value); return (u.u_error); }

char hpuxtobsdbaud[32] = {
	B0,	B50,	B75,	B110,	B134,	B150,	B200,	B300,
	B600,	B0,	B1200,	B1800,	B2400,	B0,	B4800,	B0,
	B9600,	EXTA,	EXTB,	B0,	B0,	B0,	B0,	B0,
	B0,	B0,	B0,	B0,	B0,	B0,	B0,	B0
};

char bsdtohpuxbaud[16] = {
	TIO_B0,		TIO_B50,	TIO_B75,	TIO_B110,
	TIO_B134,	TIO_B150,	TIO_B200,	TIO_B300,
	TIO_B600,	TIO_B1200,	TIO_B1800,	TIO_B2400,
	TIO_B4800,	TIO_B9600,	TIO_B19200,	TIO_B38400
};

/*
 * Map BSD style sgtty info to and from SYS5 style termio stuff.
 * Map BSD style sgtty info to and from V7 style sgtty stuff.
 */
hpuxtermio(fp, com, data)
	struct file *fp;
	caddr_t data;
{
	struct sgttyb sg;
	struct bsdtchars {	/* avoid problem with ttychars.h */
		char bsdt_intrc;
		char bsdt_quitc;
		char bsdt_startc;
		char bsdt_stopc;
		char bsdt_eofc;
		char bsdt_brkc;
	} tc;
	struct bsdltchars {	/* avoid problem with ttychars.h */
		char bsdt_suspc;
		char bsdt_dsuspc;
		char bsdt_rprntc;
		char bsdt_flushc;
		char bsdt_werasc;
		char bsdt_lnextc;
	} ltc;
	int lmode, error, (*ioctlrout)();
	register u_short flag;
	register struct hpuxtermio *tiop;

	ioctlrout = fp->f_ops->fo_ioctl;
	tiop = (struct hpuxtermio *)data;
	switch (com) {
	case HPUXTCGETA:
		/* get everything we might need */
		bzero(data, sizeof(struct hpuxtermio));
		if (error = ioctlrout(fp, TIOCGETP, (caddr_t)&sg))
			break;
		(void) ioctlrout(fp, TIOCGETC, (caddr_t)&tc);
		(void) ioctlrout(fp, TIOCLGET, (caddr_t)&lmode);

		/* set baud rate */
		tiop->c_cflag = (u_short)bsdtohpuxbaud[sg.sg_ispeed&0xF];

		/* set editing chars except for EOF/EOL (set below) */
		tiop->c_cc[HPUXVINTR] = tc.bsdt_intrc;
		tiop->c_cc[HPUXVQUIT] = tc.bsdt_quitc;
		tiop->c_cc[HPUXVERASE] = sg.sg_erase;
		tiop->c_cc[HPUXVKILL] = sg.sg_kill;

		/* set flags */
		flag = sg.sg_flags;
		if ((flag & TBDELAY) == XTABS)
			tiop->c_oflag |= TIO_TAB3;
		else if (flag & TBDELAY)
			tiop->c_oflag |= TIO_TAB1;
		if (flag & LCASE) {
			tiop->c_iflag |= TIO_IUCLC;
			tiop->c_oflag |= TIO_OLCUC;
			tiop->c_lflag |= TIO_XCASE;
		}
		if (flag & ECHO)
			tiop->c_lflag |= TIO_ECHO;
		if (flag & CRMOD) {
			tiop->c_iflag |= TIO_ICRNL;
			tiop->c_oflag |= TIO_ONLCR;
			if (flag & CR1)
				tiop->c_oflag |= TIO_CR1;
			if (flag & CR2)
				tiop->c_oflag |= TIO_CR2|TIO_ONOCR;
		} else {
			tiop->c_oflag |= TIO_ONLRET;
			if (flag & NL1)
				tiop->c_oflag |= TIO_CR1;
			if (flag & NL2)
				tiop->c_oflag |= TIO_CR2;
		}
		if (flag & RAW) {
			tiop->c_cflag |= TIO_CS8;
			tiop->c_iflag &= ~(TIO_ICRNL|TIO_IUCLC);
			tiop->c_cc[HPUXVMIN] = 6;
			tiop->c_cc[HPUXVTIME] = 1;
		} else {
			tiop->c_iflag |= TIO_BRKINT;
			if (tc.bsdt_startc == CSTART && tc.bsdt_stopc == CSTOP)
				tiop->c_iflag |= TIO_IXON;
			if (flag & TANDEM)
				tiop->c_iflag |= TIO_IXOFF;
			else if ((lmode & LDECCTQ) == 0)
				tiop->c_iflag |= TIO_IXANY;
			if ((lmode & LLITOUT) == 0) {
				tiop->c_iflag |= TIO_IGNPAR;
				tiop->c_oflag |= TIO_OPOST;
			}
			if (lmode & LPASS8)
				tiop->c_cflag |= TIO_CS8;
			else
				tiop->c_iflag |= TIO_ISTRIP;
			tiop->c_cflag |= TIO_CS7|TIO_PARENB;
			tiop->c_lflag |= TIO_ISIG;
			if (flag & CBREAK) {
				tiop->c_cc[HPUXVMIN] = 6;
				tiop->c_cc[HPUXVTIME] = 1;
			} else {
				tiop->c_lflag |= TIO_ICANON|TIO_ECHOK;
				if (lmode & LCRTERA)
					tiop->c_lflag |= TIO_ECHOE;
				tiop->c_cc[HPUXVEOF] = tc.bsdt_eofc;
				tiop->c_cc[HPUXVEOL] = tc.bsdt_brkc;
			}
		}
		tiop->c_cflag |= TIO_PARENB;
		if (flag & ODDP) {
			if (flag & EVENP)
				tiop->c_cflag &= ~TIO_PARENB;
			tiop->c_cflag |= TIO_PARODD;
		}
		if (tiop->c_cflag & TIO_PARENB)
			tiop->c_iflag |= TIO_INPCK;
		if (flag & VTDELAY)
			tiop->c_oflag |= TIO_FFDLY;
		if (flag & BSDELAY)
			tiop->c_oflag |= TIO_BSDLY;
		break;

	case HPUXTCSETA:
	case HPUXTCSETAW:
	case HPUXTCSETAF:
		/* get old lmode and determine if we are a tty */
		if (error = ioctlrout(fp, TIOCLGET, (caddr_t)&lmode))
			break;
		(void) ioctlrout(fp, TIOCGLTC, (caddr_t)&ltc);

		/* set baud rate */
		sg.sg_ispeed = hpuxtobsdbaud[tiop->c_cflag&TIO_CBAUD];
		sg.sg_ospeed = sg.sg_ispeed;

		/* set special chars to defaults for cooked mode */
		sg.sg_erase = tiop->c_cc[HPUXVERASE];
		sg.sg_kill = tiop->c_cc[HPUXVKILL];
		tc.bsdt_intrc = tiop->c_cc[HPUXVINTR];
		tc.bsdt_quitc = tiop->c_cc[HPUXVQUIT];
		tc.bsdt_startc = CSTART;
		tc.bsdt_stopc = CSTOP;
		tc.bsdt_eofc = tiop->c_cc[HPUXVEOF];
		tc.bsdt_brkc = tiop->c_cc[HPUXVEOL];
		ltc.bsdt_suspc = CSUSP;
		ltc.bsdt_dsuspc = CDSUSP;
		ltc.bsdt_flushc = CDISCARD;
		ltc.bsdt_lnextc = CLNEXT;

		/* set flags */
		flag = 0;
		if (tiop->c_oflag & TIO_BSDLY)
			flag |= BSDELAY;
		if (tiop->c_oflag & TIO_FFDLY)
			flag |= VTDELAY;
		if (tiop->c_oflag & TIO_TAB1) {
			if (tiop->c_oflag & TIO_TAB2)
				flag |= XTABS;
			else
				flag |= TAB1;
		} else if (tiop->c_oflag & TIO_TAB2)
			flag |= TAB2;
		if (tiop->c_oflag & TIO_CR1) {
			flag |= CR1;
			if (tiop->c_oflag & TIO_ONLRET)
				flag |= NL1;
		}
		if (tiop->c_oflag & TIO_CR2) {
			flag |= CR2;
			if (tiop->c_oflag & TIO_ONLRET)
				flag |= NL2;
		}
		if ((tiop->c_oflag & (TIO_NLDLY|TIO_ONLRET)) == TIO_NLDLY)
			flag |= NL2;
		if ((tiop->c_cflag & TIO_PARENB) == 0)
			flag |= ODDP|EVENP;
		else if (tiop->c_cflag & TIO_PARODD)
			flag |= ODDP;
		else
			flag |= EVENP;
		if ((tiop->c_iflag & TIO_ICRNL) || (tiop->c_oflag & TIO_ONLCR))
			flag |= CRMOD;
		if (tiop->c_lflag & TIO_ECHO)
			flag |= ECHO;
		if (tiop->c_iflag & TIO_IUCLC)
			flag |= LCASE;
		if (tiop->c_iflag & TIO_IXOFF)
			flag |= TANDEM;
		if ((tiop->c_lflag & TIO_ICANON) == 0) {
			if (tiop->c_lflag & TIO_ISIG)
				flag |= CBREAK;
			else
				flag |= RAW;
		}
		if (flag & CBREAK) {
			ltc.bsdt_suspc = ltc.bsdt_dsuspc = -1;
			ltc.bsdt_flushc = ltc.bsdt_lnextc = -1;
			if ((tiop->c_iflag & TIO_IXON) == 0)
				tc.bsdt_startc = tc.bsdt_stopc = -1;
		}
		sg.sg_flags = flag;
		lmode &= ~(LCRTERA|LLITOUT|LDECCTQ|LPASS8);
		if (tiop->c_lflag & TIO_ECHOE)
			lmode |= LCRTERA;
		if ((tiop->c_oflag & TIO_OPOST) == 0)
			lmode |= LLITOUT;
		if ((tiop->c_iflag & TIO_IXANY) == 0)
			lmode |= LDECCTQ;
		if ((tiop->c_cflag & TIO_CS8) &&
		    (tiop->c_iflag & TIO_ISTRIP) == 0)
			lmode |= LPASS8;

		/* set the new stuff */
		if (com == HPUXTCSETA)
			com = TIOCSETN;
		else
			com = TIOCSETP;
		(void) ioctlrout(fp, com, (caddr_t)&sg);
		(void) ioctlrout(fp, TIOCSETC, (caddr_t)&tc);
		(void) ioctlrout(fp, TIOCSLTC, (caddr_t)&ltc);
		(void) ioctlrout(fp, TIOCLSET, (caddr_t)&lmode);
		if (tiop->c_cflag & TIO_HUPCL)
			(void) ioctlrout(fp, TIOCHPCL, (caddr_t)0);
		break;

	default:
		error = EINVAL;
		break;
	}
	return(error);
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

	RETURN (getsettty(uap->fdes, HPUXTIOCGETP, uap->cmarg));
}

ohpuxstty(p, uap, retval)
	struct proc *p;
	struct args {
		int	fdes;
		caddr_t	cmarg;
	} *uap;
	int *retval;
{

	RETURN (getsettty(uap->fdes, HPUXTIOCSETP, uap->cmarg));
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
