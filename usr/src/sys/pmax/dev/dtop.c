/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dtop.c	7.3 (Berkeley) %G%
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
/************************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

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
void dtop_keyboard_repeat	__P((void *));
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

/*
 * lk201 keyboard divisions and up/down mode key bitmap.
 */
#define NUMDIVS 14
static u_char divbeg[NUMDIVS] = {0xbf, 0x91, 0xbc, 0xbd, 0xb0, 0xad, 0xa6,
				 0xa9, 0x88, 0x56, 0x63, 0x6f, 0x7b, 0x7e};
static u_char divend[NUMDIVS] = {0xff, 0xa5, 0xbc, 0xbe, 0xb2, 0xaf, 0xa8,
				 0xac, 0x90, 0x62, 0x6e, 0x7a, 0x7d, 0x87};
/*
 * Initial defaults, groups 5 and 6 are up/down
 */
static u_long keymodes[8] = {0, 0, 0, 0, 0, 0x0003e000, 0, 0};

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
	dtop->device[DTOP_DEVICE_NO(0x6c)].status.keyboard.k_ar_state =
		K_AR_IDLE;

	dtop->probed_once = 1;
	printf("dtop%d at nexus0 csr 0x%x priority %d\n",
		cp->pmax_unit, cp->pmax_addr, cp->pmax_pri);
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
#ifdef DIAGNOSTIC
	    printf("dtop: overrun (or stray)\n");
#endif
	    /*
	     * Ugh! The most common occurrence of a data overrun is upon a
	     * key press and the result is a software generated "stuck key".
	     * All I can think to do is fake an "all keys up" whenever a
	     * data overrun occurs.
	     */
	    msg.src_address = 0x6c;
	    msg.code.val.len = 1;
	    msg.body[0] = DTOP_KBD_EMPTY;
	}

	/*
	 * If not probed yet, just throw the data away.
	 */
	if (!dtop->probed_once)
		return;

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
	register int i;
	static int param = 0, cmd, mod, typ;
	static u_char parms[2];

	/*
	 * Emulate the lk201 command codes.
	 */
	if (param == 0) {
		typ = (c & 0x1);
		cmd = ((c >> 3) & 0xf);
		mod = ((c >> 1) & 0x3);
	} else
		parms[param - 1] = (c & 0x7f);
	if (c & 0x80) {
		if (typ) {
			/*
			 * A peripheral command code. Someday this driver
			 * should know how to send commands to the lk501,
			 * but until then this is all essentially a no-op.
			 */
			;
		} else {
			/*
			 * Set modes. These have to be emulated in software.
			 */
			if (cmd > 0 && cmd < 15) {
				cmd--;
				if (mod & 0x2)
				   for (i = divbeg[cmd]; i <= divend[cmd]; i++)
					keymodes[i >> 5] |=
						(1 << (i & 0x1f));
				else
				   for (i = divbeg[cmd]; i <= divend[cmd]; i++)
					keymodes[i >> 5] &=
						~(1 << (i & 0x1f));
			}
		}
		param = 0;
	} else if (++param > 2)
		param = 2;
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
		DELAY(1);
	if (max == DTOP_MAX_POLL)
		goto bad;
	pkt->src_address = DTOP_GET_BYTE(data);

	for (max = 0; (max < DTOP_MAX_POLL) && !DTOP_RX_AVAIL(poll); max++)
		DELAY(1);
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
			DELAY(1);
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
		mrp->state = 0;
		/*
		 * Do the position first
		 */
		coord = GET_SHORT(msg->body[2], msg->body[3]);
		if (coord < 0) {
			coord = -coord;
			moved = 1;
		} else if (coord > 0) {
			mrp->state |= MOUSE_X_SIGN;
			moved = 1;
		}
		mrp->dx = (coord & 0x1f);
		coord = GET_SHORT(msg->body[4], msg->body[5]);
		if (coord < 0) {
			coord = -coord;
			moved = 1;
		} else if (coord > 0) {
			mrp->state |= MOUSE_Y_SIGN;
			moved = 1;
		}
		mrp->dy = (coord & 0x1f);
	
		/*
		 * Time for the buttons now
		 * Shuffle button bits around to serial mouse order.
		 */
		buttons = GET_SHORT(msg->body[0], msg->body[1]);
		mrp->state |= (((buttons >> 1) & 0x3) | ((buttons << 2) & 0x4));
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
	int msg_len, c, s;
	struct tty *tp = &dtop_tty[0];

	/*
	 * Fiddle about emulating an lk201 keyboard. The lk501
	 * designers carefully ensured that keyboard handlers could be
	 * stateless, then we turn around and use lots of state to
	 * emulate the stateful lk201, since the X11R5 X servers
	 * only know about the lk201... (oh well)
	 */
	/*
	 * Turn off any autorepeat timeout.
	 */
	s = splhigh();
	if (dev->keyboard.k_ar_state != K_AR_IDLE) {
		dev->keyboard.k_ar_state = K_AR_IDLE;
		untimeout(dtop_keyboard_repeat, (void *)dev);
	}
	splx(s);
	msg_len = msg->code.val.len;

	/* Check for errors */
	c = msg->body[0];
	if ((c < DTOP_KBD_KEY_MIN) && (c != DTOP_KBD_EMPTY)) {
		printf("Keyboard error: %x %x %x..\n", msg_len, c, msg->body[1]);
#ifdef notdef
		if (c != DTOP_KBD_OUT_ERR) return -1;
#endif
		/*
		 * Fake an "all ups" to avoid the stuck key syndrome.
		 */
		c = msg->body[0] = DTOP_KBD_EMPTY;
		msg_len = 1;
	}

	dev->keyboard.last_msec = TO_MS(time);
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
		(void) kbdMapChar(c);			

		if (outc == 0 && dtopDivertXInput &&
		    (keymodes[(c >> 5) & 0x7] & (1 << (c & 0x1f))))
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
		    dev->keyboard.k_ar_state = K_AR_ACTIVE;
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
	if (dev->keyboard.k_ar_state == K_AR_ACTIVE)
		timeout(dtop_keyboard_repeat, (void *)dev, hz / 2);
	return (outc);
}

/*
 * Do an autorepeat as required.
 */
void
dtop_keyboard_repeat(arg)
	void *arg;
{
	dtop_device_t dev = (dtop_device_t)arg;
	register int i, c;
	struct tty *tp = dtop_tty;
	int s = spltty(), gotone = 0;

	for (i = 0; i < dev->keyboard.last_codes_count; i++) {
		c = (int)dev->keyboard.last_codes[i];
		if (c != DTOP_KBD_EMPTY &&
		    (keymodes[(c >> 5) & 0x7] & (1 << (c & 0x1f))) == 0) {
			dev->keyboard.k_ar_state = K_AR_TRIGGER;
			if (dtopDivertXInput) {
				(*dtopDivertXInput)(KEY_REPEAT);
				gotone = 1;
				continue;
			}

			if ((c = kbdMapChar(KEY_REPEAT)) >= 0) {
				(*linesw[tp->t_line].l_rint)(c, tp);
				gotone = 1;
			}
		}
	}
	if (gotone)
		timeout(dtop_keyboard_repeat, arg, hz / 20);
	else
		dev->keyboard.k_ar_state = K_AR_IDLE;
	splx(s);
}
#endif
