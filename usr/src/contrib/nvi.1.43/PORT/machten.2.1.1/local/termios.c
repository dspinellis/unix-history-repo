/*
 * termios.c -- POSIX termios emulation using sgtty interface.
 *
 * This is not a complete implementation; only the basic
 * functionality needed to support nvi is here (and I'm not
 * even sure that's correct either).
 */

#include <termios.h>
#include <unistd.h>


int
tcgetattr(fd, t)
	int		fd;
	struct termios	*t;
{
	if ((ioctl(fd, TIOCGETP, &t->__sg) < 0)
	    || (ioctl(fd, TIOCGETC, &t->__tc) < 0)
#ifdef TIOCGLTC
	    || (ioctl(fd, TIOCGLTC, &t->__ltc) < 0)
#endif
	   ) {
		return(-1);
	}

	t->c_cc[VERASE]		= t->__sg.sg_erase;
	t->c_cc[VKILL]		= t->__sg.sg_kill;
	t->c_cc[VINTR]		= t->__tc.t_intrc;
	t->c_cc[VQUIT]		= t->__tc.t_quitc;
	t->c_cc[VSTART]		= t->__tc.t_startc;
	t->c_cc[VSTOP]		= t->__tc.t_stopc;
	t->c_cc[VEOF]		= t->__tc.t_eofc;
	t->c_cc[VEOL]		= t->__tc.t_brkc;

#ifdef TIOCGLTC
	t->c_cc[VSUSP]		= t->__ltc.t_suspc;
	t->c_cc[VDSUSP]		= t->__ltc.t_dsuspc;
	t->c_cc[VREPRINT]	= t->__ltc.t_rprntc;
	t->c_cc[VDISCARD]	= t->__ltc.t_flushc;
	t->c_cc[VWERASE]	= t->__ltc.t_werasc;
	t->c_cc[VLNEXT]		= t->__ltc.t_lnextc;
#endif /* TIOCGLTC */

	t->c_iflag = 0;
	t->c_oflag = 0;
	t->c_lflag = 0;
	t->c_cflag = 0;

	if (t->__sg.sg_flags & RAW) {
		t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INLCR|IGNCR|ICRNL|IXON);
		t->c_oflag &= ~(OPOST);
		t->c_lflag &= ~(_TERMIOS_ECHO|ECHONL|ICANON|ISIG|IEXTEN);
		t->c_cflag |= CS8;
		t->c_cflag &= ~(CSIZE | PARENB | PARODD);
	} else if (t->__sg.sg_flags & CBREAK) {
		t->c_lflag &= ~(ICANON);
		t->c_lflag |= ISIG;
		t->c_oflag |= OPOST;
	} else {
		t->c_lflag |= (ICANON | ISIG | IEXTEN);
		t->c_oflag |= OPOST;
	}

	if (t->__sg.sg_flags & CRMOD) {
		t->c_iflag |= ICRNL;
		t->c_oflag |= (OPOST | ONLCR);
	}

	if (t->__sg.sg_flags & _SGTTY_ECHO) {
		t->c_lflag |= _TERMIOS_ECHO;
	}

	return(0);
}

int
tcsetattr(fd, act, t)
	int		fd;
	int		act;
	struct termios	*t;
{
	t->__sg.sg_erase	= t->c_cc[VERASE];
	t->__sg.sg_kill		= t->c_cc[VKILL];
	t->__tc.t_intrc		= t->c_cc[VINTR];
	t->__tc.t_quitc		= t->c_cc[VQUIT];
	t->__tc.t_startc	= t->c_cc[VSTART];
	t->__tc.t_stopc		= t->c_cc[VSTOP];
	t->__tc.t_eofc		= t->c_cc[VEOF];
	t->__tc.t_brkc		= t->c_cc[VEOL];

#ifdef TIOCGLTC
	t->__ltc.t_suspc	= t->c_cc[VSUSP];
	t->__ltc.t_dsuspc	= t->c_cc[VDSUSP];
	t->__ltc.t_rprntc	= t->c_cc[VREPRINT];
	t->__ltc.t_flushc	= t->c_cc[VDISCARD];
	t->__ltc.t_werasc	= t->c_cc[VWERASE];
	t->__ltc.t_lnextc	= t->c_cc[VLNEXT];
#endif /* TIOCGLTC */

	t->__sg.sg_flags &= ~(RAW | CBREAK);	/* reset temporarily */

	if ( !(t->c_lflag & ICANON) ) {
		t->__sg.sg_flags |= CBREAK;
	}

	if (t->c_lflag & _TERMIOS_ECHO) {
		t->__sg.sg_flags |= _SGTTY_ECHO;
	} else {
		t->__sg.sg_flags &= ~(_SGTTY_ECHO);
	}

	if (t->c_oflag & ONLCR) {
		t->__sg.sg_flags |= CRMOD;
	} else {
		t->__sg.sg_flags &= ~(CRMOD);
	}

	if ((ioctl(fd, (act==TCSADRAIN ? TIOCSETP : TIOCSETN), &t->__sg) < 0)
	    || (ioctl(fd, TIOCSETC, &t->__tc) < 0)
#ifdef TIOCGLTC
	    || (ioctl(fd, TIOCSLTC, &t->__ltc) < 0)
#endif
	   ) {
		return(-1);
	}

	return(0);
}


speed_t
cfgetospeed(t)
	struct termios	*t;
{
	return( t->__sg.sg_ospeed );
}
