/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbd.c	7.3 (Berkeley) %G%
 */

/*
 * kbd.c -- keyboard device driver
 *	remade by A.Fujita, DEC-21-1992
 */

#define NKBD	2

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <luna68k/dev/device.h>
#include <luna68k/dev/sioreg.h>
#include <luna68k/dev/siovar.h>
#include <luna68k/dev/kbio.h>

extern	struct sio_portc *sio_port_assign();
extern	struct sio_portc *sio_port_get();

struct	sio_softc kbd_softc[NKBD];
struct	sio_portc kbd_sport;
struct	sio_portc *kbd_pc;

int     kbdopen();
void    kbdstart();
int     kbdparam();
int     kbdintr();

struct	tty kbd_tty[NKBD];

int	kbddefaultrate = B9600;				/* speed of console line is fixed */
int	kbdmajor = 14;
int	kbd_state = 0;

#define	kbdunit(x)		minor(x)

/*
 *  entry routines
 */

/* ARGSUSED */
#ifdef __STDC__
kbdopen(dev_t dev, int flag, int mode, struct proc *p)
#else
kbdopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
#endif
{
	register struct tty *tp;
	register int unit, s;
	register struct sio_portc *pc;
	int error = 0;

	unit = kbdunit(dev);

	if (unit != 0)
		return (ENXIO);

	if (kbd_state == 0) {
		s = splhigh();
		pc = sio_port_get(1);
		kbd_sport = *pc;
		kbd_pc = sio_port_assign(1, kbdmajor, unit, kbdintr);
		splx(s);
	}
	kbd_softc[unit].sc_pc = kbd_pc;
	kbd_state |= 1 << unit;

	tp = &kbd_tty[unit];
	tp->t_oproc = kbdstart;
	tp->t_param = kbdparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
/*
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
 */
			tp->t_iflag = 0;
			tp->t_oflag = 0;
			tp->t_cflag = (CREAD | CS8 | HUPCL);
			tp->t_lflag = 0;

			tp->t_ispeed = tp->t_ospeed = kbddefaultrate;
		}
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);

	tp->t_state |= TS_CARR_ON;

	if (error == 0)
		error = (*linesw[tp->t_line].l_open)(dev, tp);

	return (error);
}

/*ARGSUSED*/
kbdclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct	sio_portc *pc;
	register struct tty *tp;
	register int unit, s;

	unit = kbdunit(dev);

	tp = &kbd_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);

	kbd_state &= ~(1 << unit);

	if (kbd_state == 0) {
		s = splhigh();
		pc = &kbd_sport;
		(void) sio_port_assign(1, pc->pc_major, pc->pc_unit, pc->pc_intr);
		splx(s);
	}

	return (0);
}
 
kbdread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &kbd_tty[kbdunit(dev)];
 
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

int
kbdparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	int unit = kbdunit(tp->t_dev);
	register struct sio_softc *sc = &kbd_softc[unit];
	register int cflag = t->c_cflag;
 
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	/*
	 * change line speed
	 */

	/*
	 * parity
	 */

	/*
	 * stop bit
	 */

	return (0);
}

kbdioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct siodevice *sio = kbd_pc->pc_addr;
	register struct tty *tp;
	register int unit = kbdunit(dev);
	register int error;

	tp = &kbd_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case KIOCMOUSE:
		if (*((int *) data)) {
			sio->sio_data = 0x60;	/* enable  mouse tracking */
		} else {
			sio->sio_data = 0x20;	/* disable mouse tracking */
		}
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 *
 */
void
kbdstart(tp)
	register struct tty *tp;
{
	register int unit;
	register struct siodevice *sio = kbd_pc->pc_addr;
	register int rr;
	int s, c;
 
	s = spltty();

	if (tp->t_state & (TS_TIMEOUT|TS_TTSTOP))
		goto out;

	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}

	if (tp->t_outq.c_cc != 0)
		c = getc(&tp->t_outq);

out:
	splx(s);
}

/*
 *  interrupt handling 
 */

kbdintr(unit)
	register int unit;
{
	register struct siodevice *sio = kbd_pc->pc_addr;
	register struct tty *tp;
	register u_char code;
	int s, rr;

	rr = siogetreg(sio);

	if (rr & RR_RXRDY) {
		code = sio->sio_data;
		tp = &kbd_tty[0];		/* Keyboard */
		if ((tp->t_state & TS_ISOPEN) != 0)
			(*linesw[tp->t_line].l_rint)(code, tp);
	}

	if (rr & RR_TXRDY) {
		sio->sio_cmd = WR0_RSTPEND;
		tp->t_state &= ~(TS_BUSY|TS_FLUSH);
		if (tp->t_line)
			(*linesw[tp->t_line].l_start)(tp);
		else
			kbdstart(tp);
	}
}

kbdselect(dev, rw, p)
	dev_t dev;
	int rw;
	struct proc *p;
{
	register int unit = kbdunit(dev);
	register struct tty *tp;
	int nread;
	int s = spltty();

	tp = &kbd_tty[unit];

	switch (rw) {

	case FREAD:
		nread = ttnread(tp);
		if (nread > 0 || ((tp->t_cflag&CLOCAL) == 0 && (tp->t_state&TS_CARR_ON) == 0))
			goto win;

		selrecord(p, &tp->t_rsel);
		break;

	case FWRITE:
		if (tp->t_outq.c_cc <= tp->t_lowat)
			goto win;
		selrecord(p, &tp->t_wsel);
		break;
	}
	splx(s);
	return (0);
win:
	splx(s);
	return (1);
}
