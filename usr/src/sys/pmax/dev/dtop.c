/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dtop.c	7.1 (Berkeley) %G%
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */
/*
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *
 *	Hardware-level operations for the Desktop serial line
 *	bus (i2c aka ACCESS).
 */

#include <dtop.h>
#if NDTOP > 0
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/proc.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <machine/pmioctl.h>
#include <machine/machConst.h>
#include <machine/dc7085cons.h>

#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/maxine.h>
#include <pmax/pmax/asic.h>

#include <pmax/dev/device.h>
#include <pmax/dev/dtopreg.h>
#include <pmax/dev/fbreg.h>

extern int pmax_boardtype;

extern int ttrstrt	__P((void *));
void dtop_keyboard_autorepeat	__P((void *));
int dtop_null_device_handler	__P((dtop_device_t, dtop_message_t, int, int));
int dtop_locator_handler	__P((dtop_device_t, dtop_message_t, int, int));
int dtop_keyboard_handler	__P((dtop_device_t, dtop_message_t, int, int));
int dtopparam		__P((struct tty *, struct termios *));
int dtopstop		__P((struct tty *, int));
void dtopstart		__P((struct tty *));
void dtopKBDPutc	__P((dev_t, int));

struct	tty dtop_tty[NDTOP];
void	(*dtopDivertXInput)();	/* X windows keyboard input routine */
void	(*dtopMouseEvent)();	/* X windows mouse motion event routine */
void	(*dtopMouseButtons)();	/* X windows mouse buttons event routine */

#define	DTOP_MAX_POLL	0x7fff		/* about half a sec */

typedef volatile unsigned int	*data_reg_t;	/* uC  */
#define	DTOP_GET_BYTE(data)	(((*(data)) >> 8) & 0xff)
#define	DTOP_PUT_BYTE(data,c)	{ *(data) = (c) << 8; }

typedef volatile unsigned int	*poll_reg_t;	/* SIR */
#define	DTOP_RX_AVAIL(poll)	(*(poll) & 1)
#define	DTOP_TX_AVAIL(poll)	(*(poll) & 2)

#define	GET_SHORT(b0,b1)	(((b0)<<8)|(b1))

/*
 * Driver status
 */
struct dtop_softc {
	data_reg_t	data;
	poll_reg_t	poll;
	char		polling_mode;
	char		probed_once;
	short		bad_pkts;

	struct dtop_ds {
		int		(*handler)();
		dtop_device	status;
	} device[(DTOP_ADDR_DEFAULT - DTOP_ADDR_FIRST) >> 1];

#	define	DTOP_DEVICE_NO(address)	(((address)-DTOP_ADDR_FIRST)>>1)

} dtop_softc[NDTOP];

typedef struct dtop_softc *dtop_softc_t;
struct tty dtop_tty[NDTOP];
static int dtopenabled = 0;

/*
 * Definition of the driver for the auto-configuration program.
 */
int	dtopprobe();
void	dtopintr();
struct	driver dtopdriver =  {
	"dtop", dtopprobe, 0, 0, dtopintr,
};

dtopprobe(cp)
	struct pmax_ctlr *cp;
{
	register struct tty *tp;
	register int cntr;
	int dtopunit = cp->pmax_unit, i, s;
	dtop_softc_t dtop;

	if (dtopunit >= NDTOP)
		return (0);
	if (badaddr(cp->pmax_addr, 2))
		return (0);
	dtop = &dtop_softc[dtopunit];

	dtop->poll = (poll_reg_t)MACH_PHYS_TO_UNCACHED(XINE_REG_INTR);
	dtop->data = (data_reg_t)cp->pmax_addr;

	for (i = 0; i < DTOP_MAX_DEVICES; i++)
		dtop->device[i].handler = dtop_null_device_handler;

	/* a lot more needed here, fornow: */
	dtop->device[DTOP_DEVICE_NO(0x6a)].handler = dtop_locator_handler;
	dtop->device[DTOP_DEVICE_NO(0x6c)].handler = dtop_keyboard_handler;
	dtop->device[DTOP_DEVICE_NO(0x6c)].status.keyboard.poll_frequency =
		(hz * 5) / 100; /* x0.01 secs */
	dtop->device[DTOP_DEVICE_NO(0x6c)].status.keyboard.k_ar_state =
		K_AR_IDLE;

	/*
	 * Sometimes a first interrupt gets lost, so just in case
	 * poke it now.
	 */
	dtopintr(dtopunit);
	dtopenabled = 1;
	return (1);
}

dtopopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;
	int s, error = 0;

	unit = minor(dev);
	if (unit >= NDTOP)
		return (ENXIO);
	tp = &dtop_tty[unit];
	tp->t_oproc = dtopstart;
	tp->t_param = dtopparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		}
		(void) dtopparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if ((tp->t_state & TS_XCLUDE) && curproc->p_ucred->cr_uid != 0)
		return (EBUSY);
	s = spltty();
	while (!(flag & O_NONBLOCK) && !(tp->t_cflag & CLOCAL) &&
	       !(tp->t_state & TS_CARR_ON)) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	if (error)
		return (error);
	error = (*linesw[tp->t_line].l_open)(dev, tp);
	return (error);
}

/*ARGSUSED*/
dtopclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;

	unit = minor(dev);
	tp = &dtop_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	return (ttyclose(tp));
}

dtopread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dtop_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

dtopwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dtop_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*ARGSUSED*/
dtopioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = minor(dev);
	int error;

	tp = &dtop_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		ttyoutput(0, tp);
		break;

	case TIOCCBRK:
		ttyoutput(0, tp);
		break;

	case TIOCMGET:
		*(int *)data = DML_DTR | DML_DSR | DML_CAR;
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 * Interrupt routine
 */
void
dtopintr(unit)
	int unit;
{
	dtop_message msg;
	int devno;
	dtop_softc_t dtop;

	dtop = &dtop_softc[unit];
	if (dtop_get_packet(dtop, &msg) < 0) {
		if (dtopenabled)
			printf("%s", "dtop: overrun (or stray)\n");
		return;
	}

	devno = DTOP_DEVICE_NO(msg.src_address);
	if (devno < 0 || devno > 15)
		return;

	(void) (*dtop->device[devno].handler)
			(&dtop->device[devno].status, &msg,
			 DTOP_EVENT_RECEIVE_PACKET, 0);
}

void
dtopstart(tp)
	register struct tty *tp;
{
	register int cc;
	int s;

	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	/* handle console specially */
	if (tp == dtop_tty) {
		while (tp->t_outq.c_cc > 0) {
			cc = getc(&tp->t_outq) & 0x7f;
			cnputc(cc);
		}
		/*
		 * After we flush the output queue we may need to wake
		 * up the process that made the output.
		 */
		if (tp->t_outq.c_cc <= tp->t_lowat) {
			if (tp->t_state & TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			selwakeup(&tp->t_wsel);
		}
	}
out:
	splx(s);
}

void
dtopKBDPutc(dev, c)
	dev_t dev;
	int c;
{

	/*
	 * Not yet, someday we will know how to send commands to the
	 * LK501 over the Access bus.
	 */
}

/*
 * Take a packet off dtop interface
 * A packet MUST be there, this is not checked for.
 */
#define	DTOP_ESC_CHAR		0xf8
dtop_escape(c)
{
	/* I donno much about this stuff.. */
	switch (c) {
	case 0xe8:	return (0xf8);
	case 0xe9:	return (0xf9);
	case 0xea:	return (0xfa);
	case 0xeb:	return (0xfb);
	default:	/* printf("{esc %x}", c); */
			return (c);
	}
}

dtop_get_packet(dtop, pkt)
	dtop_softc_t	dtop;
	dtop_message_t	pkt;
{
	register poll_reg_t	poll;
	register data_reg_t	data;
	register int		max, i, len;
	register unsigned char	c;

	poll = dtop->poll;
	data = dtop->data;

	/*
	 * The interface does not handle us the first byte,
	 * which is our address and cannot ever be anything
	 * else but 0x50.  This is a good thing, it makes
	 * the average packet exactly one word long, too.
	 */
	for (max = 0; (max < DTOP_MAX_POLL) && !DTOP_RX_AVAIL(poll); max++)
		DELAY(16);
	if (max == DTOP_MAX_POLL)
		goto bad;
	pkt->src_address = DTOP_GET_BYTE(data);

	for (max = 0; (max < DTOP_MAX_POLL) && !DTOP_RX_AVAIL(poll); max++)
		DELAY(16);
	if (max == DTOP_MAX_POLL)
		goto bad;
	pkt->code.bits = DTOP_GET_BYTE(data);

	/*
	 * Now get data and checksum
	 */
	len = pkt->code.val.len + 1;
	c = 0;
	for (i = 0; i < len; i++) {
again:
		for (max = 0; (max < DTOP_MAX_POLL) && !DTOP_RX_AVAIL(poll); max++)
			DELAY(16);
		if (max == DTOP_MAX_POLL)
			goto bad;
		if (c == DTOP_ESC_CHAR) {
			c = dtop_escape(DTOP_GET_BYTE(data) & 0xff);
		} else {
			c = DTOP_GET_BYTE(data);
			if (c == DTOP_ESC_CHAR)
				goto again;
		}
		pkt->body[i] = c;
	}
	return (len);
bad:
	dtop->bad_pkts++;
	return (-1);
}

/*
 * Get a keyboard char for the console
 */
dtopKBDGetc()
{
	register int c;
	dtop_softc_t dtop;

	dtop = &dtop_softc[0];
again:
	c = -1;

	/*
	 * Now check keyboard
	 */
	if (DTOP_RX_AVAIL(dtop->poll)) {

		dtop_message	msg;
		struct dtop_ds	*ds;

		if (dtop_get_packet(dtop, &msg) >= 0) {

		    ds = &dtop->device[DTOP_DEVICE_NO(msg.src_address)];
		    if (ds->handler == dtop_keyboard_handler) {

			c = dtop_keyboard_handler(
					&ds->status, &msg,
					DTOP_EVENT_RECEIVE_PACKET, -1);

			if (c > 0) return c;

			c = -1;
		    }
		}
	}

	if (c == -1) {
		DELAY(100);
		goto again;
	}

	return c;
}

int
dtopparam(tp, t)
	struct tty *tp;
	struct termios *t;
{
	if (tp->t_ispeed == 0)
		ttymodem(tp, 0);
	else
		/* called too early to invoke ttymodem, sigh */
		tp->t_state |= TS_CARR_ON;
	return (0);
}
 
/*
 * Stop output on a line.
 */
/*ARGSUSED*/
dtopstop(tp, flag)
	register struct tty *tp;
	int flag;
{
	int s;

	s = spltty();
	if (tp->t_state & TS_BUSY) {
		if (!(tp->t_state & TS_TTSTOP))
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

/*
 * Default handler function
 */
int
dtop_null_device_handler(dev, msg, event, outc)
	 dtop_device_t	dev;
	 dtop_message_t	msg;
	 int		event;
	 int		outc;
{
	/* See if the message was to the default address (powerup) */

	/* Uhmm, donno how to handle this. Drop it */
	if (event == DTOP_EVENT_RECEIVE_PACKET)
		dev->unknown_report = *msg;
	return 0;
}

/*
 * Handler for locator devices (mice)
 */
int
dtop_locator_handler(dev, msg, event, outc)
	 dtop_device_t	dev;
	 dtop_message_t	msg;
	 int		event;
	 int		outc;
{
	register unsigned short	buttons;
	register short coord;
	register int moved = 0;
	static MouseReport currentRep;
	register MouseReport *mrp = &currentRep;

	if (dtopMouseButtons) {
		/*
		 * Do the position first
		 */
		coord = GET_SHORT(msg->body[2], msg->body[3]);
		mrp->dx = coord;
		if (coord != 0)
			moved = 1;
		coord = GET_SHORT(msg->body[4], msg->body[5]);
		coord = - coord;
		mrp->dy = coord;
		if (coord != 0)
			moved = 1;
	
		/*
		 * Time for the buttons now
		 */
		buttons = GET_SHORT(msg->body[0], msg->body[1]);
		mrp->state = MOUSE_Y_SIGN | MOUSE_X_SIGN | (buttons & 0x7);
		if (moved)
			(*dtopMouseEvent)(mrp);
		(*dtopMouseButtons)(mrp);
	}
	return (0);
}

/*
 * Handler for keyboard devices
 * Special case: outc set for recv packet means
 * we are inside the kernel debugger
 */
int
dtop_keyboard_handler(dev, msg, event, outc)
	dtop_device_t dev;
	dtop_message_t msg;
	int event;
	int outc;
{
	register u_char *ls, *le, *ns, *ne;
	u_char save[11], retc;
	int msg_len, c;
	struct tty *tp = &dtop_tty[0];

	/*
	 * Fiddle about emulating an lk201 keyboard. The lk501
	 * designers carefully ensured that keyboard handlers could be
	 * stateless, then we turn around and use lots of state to
	 * emulate the stateful lk201, since the X11R5 X servers
	 * only know about the lk201... (oh well)
	 */
	if (event != DTOP_EVENT_RECEIVE_PACKET) {
		switch (event) {
		case DTOP_EVENT_POLL:
		    {
			register unsigned int	t, t0;

			/*
			 * Note we will always have at least the
			 * end-of-list marker present (a zero)
			 * Here stop and trigger of autorepeat.
			 * Do not repeat shift keys, either.
			 */
			{
				register unsigned char	uc, i = 0;

rpt_char:
				uc = dev->keyboard.last_codes[i];

				if (uc == DTOP_KBD_EMPTY) {
					dev->keyboard.k_ar_state = K_AR_OFF;
					return 0;
				}
				if ((uc >= KEY_R_SHIFT) && (uc <= KEY_R_ALT)) {
					/* sometimes swapped. Grrr. */
					if (++i < dev->keyboard.last_codes_count) 
						goto rpt_char;
					dev->keyboard.k_ar_state = K_AR_OFF;
					return 0;
				}
				c = uc;
			}

			/*
			 * Got a char. See if enough time from stroke,
			 * or from last repeat.
			 */
			t0 = (dev->keyboard.k_ar_state == K_AR_TRIGGER) ? 30 : 500;
			t = TO_MS(time);
			if ((t - dev->keyboard.last_msec) < t0)
				return 0;

			dev->keyboard.k_ar_state = K_AR_TRIGGER;

			if (dtopDivertXInput) {
				(*dtopDivertXInput)(KEY_REPEAT);
				return (0);
			}
			if ((outc = kbdMapChar(KEY_REPEAT)) >= 0)
				(*linesw[tp->t_line].l_rint)(outc, tp);
			return 0;
		    }
		default:
			printf("Unknown dtop keyb\n");
		}
		return -1;
	}

	msg_len = msg->code.val.len;

	/* Check for errors */
	c = msg->body[0];
	if ((c < DTOP_KBD_KEY_MIN) && (c != DTOP_KBD_EMPTY)) {
		printf("Keyboard error: %x %x %x..\n", msg_len, c, msg->body[1]);
		if (c != DTOP_KBD_OUT_ERR) return -1;
		/* spec sez if scan list overflow still there is data */
		msg->body[0] = 0;
	}

	dev->keyboard.last_msec = TO_MS(time);

	switch (dev->keyboard.k_ar_state) {
	case K_AR_IDLE:
		if (outc != 0xff) /* from debugger, might be too early */
			dtop_keyboard_autorepeat(dev);
		/* fall through */
	case K_AR_TRIGGER:
		dev->keyboard.k_ar_state = K_AR_ACTIVE;
		break;
	case K_AR_ACTIVE:
		break;
	case K_AR_OFF:
		printf("dtop keyb off?\n");
		dev->keyboard.k_ar_state = K_AR_IDLE;
	}

	/*
	 * To make things readable, do a first pass cancelling out
	 * all keys that are still pressed, and a second one generating
	 * events.  While generating events, do the upstrokes first
	 * from oldest to youngest, then the downstrokes from oldest
	 * to youngest.  This copes with lost packets and provides
	 * a reasonable model even if scans are too slow.
	 */

	/* make a copy of new state first */
	if (msg_len == 1)
		save[0] = msg->body[0];
	else if (msg_len > 0)
		bcopy(msg->body, save, msg_len);

	/*
	 * Cancel out any keys in both the last and current message as
	 * they are unchanged.
	 */
	if (msg_len > 0 && dev->keyboard.last_codes_count > 0) {
		ls = dev->keyboard.last_codes;
		le = &dev->keyboard.last_codes[dev->keyboard.last_codes_count];
		ne = &msg->body[msg_len];
		for (; ls < le; ls++) {
			for (ns = msg->body; ns < ne; ns++)
				if (*ls == *ns) {
					*ls = *ns = 0;
					break;
				}
		}
	}

	/*
	 * Now generate all upstrokes
	 */
	le = dev->keyboard.last_codes;
	ls = &dev->keyboard.last_codes[dev->keyboard.last_codes_count - 1];
	for ( ; ls >= le; ls--)
	    if (c = *ls) {
		/*
		 * If there are no other down/up keys currently down, we
		 * should actually generate a KEY_UP, but that would require
		 * a lot more state.
		 */
		(void) kbdMapChar(c);			

		if (outc == 0 && dtopDivertXInput)
			(*dtopDivertXInput)(c);
	    }
	/*
	 * And finally the downstrokes
	 */
	ne = (char*)msg->body;
	ns = (char*)&msg->body[msg_len - 1];
	retc = 0;
	for ( ; ns >= ne; ns--)
	    if (*ns) {
		c = kbdMapChar(*ns);
		if (outc == 0) {
		    if (dtopDivertXInput) {
			(*dtopDivertXInput)(*ns);
			c = -1; /* consumed by X */
		    } else if (c >= 0)
			(*linesw[tp->t_line].l_rint)(c, tp);
		}
		/* return the related keycode anyways */
		if ((c >= 0) && (retc == 0))
		    retc = c;
	    }
	outc = retc;
	/* install new scan state */
	if (msg_len == 1)
		dev->keyboard.last_codes[0] = save[0];
	else if (msg_len > 0)
		bcopy(save, dev->keyboard.last_codes, msg_len);
	dev->keyboard.last_codes_count = msg_len;
	return (outc);
}

/*
 * Polled operations: we must do autorepeat by hand. Sigh.
 */
void
dtop_keyboard_autorepeat(arg)
	void *arg;
{
	dtop_device_t dev = (dtop_device_t)arg;
	int s;

	s = spltty();
	if (dev->keyboard.k_ar_state != K_AR_IDLE)
		(void)dtop_keyboard_handler(dev, 0, DTOP_EVENT_POLL, 0);

	if (dev->keyboard.k_ar_state == K_AR_OFF)
		dev->keyboard.k_ar_state = K_AR_IDLE;
	else
		timeout(dtop_keyboard_autorepeat, dev, dev->keyboard.poll_frequency);

	splx(s);
}
#endif
