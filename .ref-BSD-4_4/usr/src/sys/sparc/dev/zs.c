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
 *	@(#)zs.c	8.1 (Berkeley) 7/19/93
 *
 * from: $Header: zs.c,v 1.30 93/07/19 23:44:42 torek Exp $
 */

/*
 * Zilog Z8530 (ZSCC) driver.
 *
 * Runs two tty ports (ttya and ttyb) on zs0,
 * and runs a keyboard and mouse on zs1.
 *
 * This driver knows far too much about chip to usage mappings.
 */
#define	NZS	2		/* XXX */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/device.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <machine/autoconf.h>
#include <machine/cpu.h>

#include <sparc/sparc/vaddrs.h>
#include <sparc/sparc/auxreg.h>

#include <sparc/dev/kbd.h>
#include <sparc/dev/zsreg.h>
#include <sparc/dev/zsvar.h>

#ifdef KGDB
#include <machine/remote-sl.h>
#endif

#define	ZSMAJOR	12		/* XXX */

#define	ZS_KBD		2	/* XXX */
#define	ZS_MOUSE	3	/* XXX */

/* the magic number below was stolen from the Sprite source. */
#define PCLK	(19660800/4)	/* PCLK pin input clock rate */

/*
 * Select software interrupt bit based on TTY ipl.
 */
#if PIL_TTY == 1
# define IE_ZSSOFT IE_L1
#elif PIL_TTY == 4
# define IE_ZSSOFT IE_L4
#elif PIL_TTY == 6
# define IE_ZSSOFT IE_L6
#else
# error "no suitable software interrupt bit"
#endif

/*
 * Software state per found chip.  This would be called `zs_softc',
 * but the previous driver had a rather different zs_softc....
 */
struct zsinfo {
	struct	device zi_dev;		/* base device */
	volatile struct zsdevice *zi_zs;/* chip registers */
	struct	zs_chanstate zi_cs[2];	/* channel A and B software state */
};

struct tty zs_tty[NZS * 2];		/* XXX should be dynamic */

/* Definition of the driver for autoconfig. */
static int	zsmatch(struct device *, struct cfdata *, void *);
static void	zsattach(struct device *, struct device *, void *);
struct cfdriver zscd =
    { NULL, "zs", zsmatch, zsattach, DV_TTY, sizeof(struct zsinfo) };

/* Interrupt handlers. */
static int	zshard(void *);
static struct intrhand levelhard = { zshard };
static int	zssoft(void *);
static struct intrhand levelsoft = { zssoft };

struct zs_chanstate *zslist;

/* Routines called from other code. */
static void	zsiopen(struct tty *);
static void	zsiclose(struct tty *);
static void	zsstart(struct tty *);
static void	zsstop(struct tty *, int);
static int	zsparam(struct tty *, struct termios *);

/* Routines purely local to this driver. */
static int	zs_getspeed(volatile struct zschan *);
static void	zs_reset(volatile struct zschan *, int, int);
static void	zs_modem(struct zs_chanstate *, int);
static void	zs_loadchannelregs(volatile struct zschan *, u_char *);

/* Console stuff. */
static struct tty *zs_ctty;	/* console `struct tty *' */
static int zs_consin = -1, zs_consout = -1;
static int zscnputc(int);	/* console putc function */
static volatile struct zschan *zs_conschan;
static struct tty *zs_checkcons(struct zsinfo *, int, struct zs_chanstate *);

#ifdef KGDB
/* KGDB stuff.  Must reboot to change zs_kgdbunit. */
extern int kgdb_dev, kgdb_rate;
static int zs_kgdb_savedspeed;
static void zs_checkkgdb(int, struct zs_chanstate *, struct tty *);
#endif

extern volatile struct zsdevice *findzs(int);
static volatile struct zsdevice *zsaddr[NZS];	/* XXX, but saves work */

/*
 * Console keyboard L1-A processing is done in the hardware interrupt code,
 * so we need to duplicate some of the console keyboard decode state.  (We
 * must not use the regular state as the hardware code keeps ahead of the
 * software state: the software state tracks the most recent ring input but
 * the hardware state tracks the most recent ZSCC input.)  See also kbd.h.
 */
static struct conk_state {	/* console keyboard state */
	char	conk_id;	/* true => ID coming up (console only) */
	char	conk_l1;	/* true => L1 pressed (console only) */
} zsconk_state;

int zshardscope;
int zsshortcuts;		/* number of "shortcut" software interrupts */

/*
 * Match slave number to zs unit number, so that misconfiguration will
 * not set up the keyboard as ttya, etc.
 */
static int
zsmatch(struct device *parent, struct cfdata *cf, void *aux)
{
	struct romaux *ra = aux;

	return (getpropint(ra->ra_node, "slave", -2) == cf->cf_unit);
}

/*
 * Attach a found zs.
 *
 * USE ROM PROPERTIES port-a-ignore-cd AND port-b-ignore-cd FOR
 * SOFT CARRIER, AND keyboard PROPERTY FOR KEYBOARD/MOUSE?
 */
static void
zsattach(struct device *parent, struct device *dev, void *aux)
{
	register int zs = dev->dv_unit, unit;
	register struct zsinfo *zi;
	register struct zs_chanstate *cs;
	register volatile struct zsdevice *addr;
	register struct tty *tp, *ctp;
	register struct romaux *ra = aux;
	int pri, softcar;
	static int didintr, prevpri;

	if ((addr = zsaddr[zs]) == NULL)
		addr = zsaddr[zs] = findzs(zs);
	if ((void *)addr != ra->ra_vaddr)
		panic("zsattach");
	if (ra->ra_nintr != 1) {
		printf(": expected 1 interrupt, got %d\n", ra->ra_nintr);
		return;
	}
	pri = ra->ra_intr[0].int_pri;
	printf(" pri %d, softpri %d\n", pri, PIL_TTY);
	if (!didintr) {
		didintr = 1;
		prevpri = pri;
		intr_establish(pri, &levelhard);
		intr_establish(PIL_TTY, &levelsoft);
	} else if (pri != prevpri)
		panic("broken zs interrupt scheme");
	zi = (struct zsinfo *)dev;
	zi->zi_zs = addr;
	unit = zs * 2;
	cs = zi->zi_cs;
	tp = &zs_tty[unit];

	if (unit == 0) {
		/* Get software carrier flags from options node in OPENPROM. */
		extern int optionsnode;

		softcar = 0;
		if (*getpropstring(optionsnode, "ttya-ignore-cd") == 't')
			softcar |= 1;
		if (*getpropstring(optionsnode, "ttyb-ignore-cd") == 't')
			softcar |= 2;
	} else
		softcar = dev->dv_cfdata->cf_flags;

	/* link into interrupt list with order (A,B) (B=A+1) */
	cs[0].cs_next = &cs[1];
	cs[1].cs_next = zslist;
	zslist = cs;

	cs->cs_unit = unit;
	cs->cs_speed = zs_getspeed(&addr->zs_chan[CHAN_A]);
	cs->cs_softcar = softcar & 1;
	cs->cs_zc = &addr->zs_chan[CHAN_A];
	tp->t_dev = makedev(ZSMAJOR, unit);
	tp->t_oproc = zsstart;
	tp->t_param = zsparam;
	tp->t_stop = zsstop;
	if ((ctp = zs_checkcons(zi, unit, cs)) != NULL)
		tp = ctp;
	cs->cs_ttyp = tp;
#ifdef KGDB
	if (ctp == NULL)
		zs_checkkgdb(unit, cs, tp);
#endif
	if (unit == ZS_KBD) {
		/*
		 * Keyboard: tell /dev/kbd driver how to talk to us.
		 */
		tp->t_ispeed = tp->t_ospeed = cs->cs_speed;
		tp->t_cflag = CS8;
		kbd_serial(tp, zsiopen, zsiclose);
		cs->cs_conk = 1;		/* do L1-A processing */
	}
	unit++;
	cs++;
	tp = &zs_tty[unit];
	cs->cs_unit = unit;
	cs->cs_speed = zs_getspeed(&addr->zs_chan[CHAN_B]);
	cs->cs_softcar = softcar & 2;
	cs->cs_zc = &addr->zs_chan[CHAN_B];
	tp->t_dev = makedev(ZSMAJOR, unit);
	tp->t_oproc = zsstart;
	tp->t_param = zsparam;
	tp->t_stop = zsstop;
	if ((ctp = zs_checkcons(zi, unit, cs)) != NULL)
		tp = ctp;
	cs->cs_ttyp = tp;
#ifdef KGDB
	if (ctp == NULL)
		zs_checkkgdb(unit, cs, tp);
#endif
	if (unit == ZS_MOUSE) {
		/*
		 * Mouse: tell /dev/mouse driver how to talk to us.
		 */
		tp->t_ispeed = tp->t_ospeed = cs->cs_speed;
		tp->t_cflag = CS8;
		ms_serial(tp, zsiopen, zsiclose);
	}
}

/*
 * Put a channel in a known state.  Interrupts may be left disabled
 * or enabled, as desired.
 */
static void
zs_reset(zc, inten, speed)
	volatile struct zschan *zc;
	int inten, speed;
{
	int tconst;
	static u_char reg[16] = {
		0,
		0,
		0,
		ZSWR3_RX_8 | ZSWR3_RX_ENABLE,
		ZSWR4_CLK_X16 | ZSWR4_ONESB | ZSWR4_EVENP,
		ZSWR5_TX_8 | ZSWR5_TX_ENABLE,
		0,
		0,
		0,
		0,
		ZSWR10_NRZ,
		ZSWR11_TXCLK_BAUD | ZSWR11_RXCLK_BAUD,
		0,
		0,
		ZSWR14_BAUD_FROM_PCLK | ZSWR14_BAUD_ENA,
		ZSWR15_BREAK_IE | ZSWR15_DCD_IE,
	};

	reg[9] = inten ? ZSWR9_MASTER_IE | ZSWR9_NO_VECTOR : ZSWR9_NO_VECTOR;
	tconst = BPS_TO_TCONST(PCLK / 16, speed);
	reg[12] = tconst;
	reg[13] = tconst >> 8;
	zs_loadchannelregs(zc, reg);
}

/*
 * Declare the given tty (which is in fact &cons) as a console input
 * or output.  This happens before the zs chip is attached; the hookup
 * is finished later, in zs_setcons() below.
 *
 * This is used only for ports a and b.  The console keyboard is decoded
 * independently (we always send unit-2 input to /dev/kbd, which will
 * direct it to /dev/console if appropriate).
 */
void
zsconsole(tp, unit, out)
	register struct tty *tp;
	register int unit;
	int out;
{
	extern int (*v_putc)();
	int zs;
	volatile struct zsdevice *addr;

	if (unit >= ZS_KBD)
		panic("zsconsole");
	if (out) {
		zs_consout = unit;
		zs = unit >> 1;
		if ((addr = zsaddr[zs]) == NULL)
			addr = zsaddr[zs] = findzs(zs);
		zs_conschan = (unit & 1) == 0 ? &addr->zs_chan[CHAN_A] :
		    &addr->zs_chan[CHAN_B];
		v_putc = zscnputc;
	} else
		zs_consin = unit;
	zs_ctty = tp;
}

/*
 * Polled console output putchar.
 */
static int
zscnputc(c)
	int c;
{
	register volatile struct zschan *zc = zs_conschan;
	register int s;

	if (c == '\n')
		zscnputc('\r');
	/*
	 * Must block output interrupts (i.e., raise to >= splzs) without
	 * lowering current ipl.  Need a better way.
	 */
	s = splhigh();
#ifdef sun4c		/* XXX */
	if (s <= (12 << 8))
		(void) splzs();
#endif
	while ((zc->zc_csr & ZSRR0_TX_READY) == 0)
		continue;
	zc->zc_data = c;
	splx(s);
}

/*
 * Set up the given unit as console input, output, both, or neither, as
 * needed.  Return console tty if it is to receive console input.
 */
static struct tty *
zs_checkcons(struct zsinfo *zi, int unit, struct zs_chanstate *cs)
{
	register struct tty *tp;
	char *i, *o;

	if ((tp = zs_ctty) == NULL)
		return (0);
	i = zs_consin == unit ? "input" : NULL;
	o = zs_consout == unit ? "output" : NULL;
	if (i == NULL && o == NULL)
		return (0);

	/* rewire the minor device (gack) */
	tp->t_dev = makedev(major(tp->t_dev), unit);

	/*
	 * Rewire input and/or output.  Note that baud rate reflects
	 * input settings, not output settings, but we can do no better
	 * if the console is split across two ports.
	 *
	 * XXX	split consoles don't work anyway -- this needs to be
	 *	thrown away and redone
	 */
	if (i) {
		tp->t_param = zsparam;
		tp->t_ispeed = tp->t_ospeed = cs->cs_speed;
		tp->t_cflag = CS8;
		ttsetwater(tp);
	}
	if (o) {
		tp->t_oproc = zsstart;
		tp->t_stop = zsstop;
	}
	printf("%s%c: console %s\n",
	    zi->zi_dev.dv_xname, (unit & 1) + 'a', i ? (o ? "i/o" : i) : o);
	cs->cs_consio = 1;
	cs->cs_brkabort = 1;
	return (tp);
}

#ifdef KGDB
/*
 * The kgdb zs port, if any, was altered at boot time (see zs_kgdb_init).
 * Pick up the current speed and character size and restore the original
 * speed.
 */
static void
zs_checkkgdb(int unit, struct zs_chanstate *cs, struct tty *tp)
{

	if (kgdb_dev == makedev(ZSMAJOR, unit)) {
		tp->t_ispeed = tp->t_ospeed = kgdb_rate;
		tp->t_cflag = CS8;
		cs->cs_kgdb = 1;
		cs->cs_speed = zs_kgdb_savedspeed;
		(void) zsparam(tp, &tp->t_termios);
	}
}
#endif

/*
 * Compute the current baud rate given a ZSCC channel.
 */
static int
zs_getspeed(zc)
	register volatile struct zschan *zc;
{
	register int tconst;

	tconst = ZS_READ(zc, 12);
	tconst |= ZS_READ(zc, 13) << 8;
	return (TCONST_TO_BPS(PCLK / 16, tconst));
}


/*
 * Do an internal open.
 */
static void
zsiopen(struct tty *tp)
{

	(void) zsparam(tp, &tp->t_termios);
	ttsetwater(tp);
	tp->t_state = TS_ISOPEN | TS_CARR_ON;
}

/*
 * Do an internal close.  Eventually we should shut off the chip when both
 * ports on it are closed.
 */
static void
zsiclose(struct tty *tp)
{

	ttylclose(tp, 0);	/* ??? */
	ttyclose(tp);		/* ??? */
	tp->t_state = 0;
}


/*
 * Open a zs serial port.  This interface may not be used to open
 * the keyboard and mouse ports. (XXX)
 */
int
zsopen(dev_t dev, int flags, int mode, struct proc *p)
{
	register struct tty *tp;
	register struct zs_chanstate *cs;
	struct zsinfo *zi;
	int unit = minor(dev), zs = unit >> 1, error, s;

	if (zs >= zscd.cd_ndevs || (zi = zscd.cd_devs[zs]) == NULL ||
	    unit == ZS_KBD || unit == ZS_MOUSE)
		return (ENXIO);
	cs = &zi->zi_cs[unit & 1];
	if (cs->cs_consio)
		return (ENXIO);		/* ??? */
	tp = cs->cs_ttyp;
	s = spltty();
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = cs->cs_speed;
		}
		(void) zsparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state & TS_XCLUDE && p->p_ucred->cr_uid != 0) {
		splx(s);
		return (EBUSY);
	}
	error = 0;
	for (;;) {
		/* loop, turning on the device, until carrier present */
		zs_modem(cs, 1);
		if (cs->cs_softcar)
			tp->t_state |= TS_CARR_ON;
		if (flags & O_NONBLOCK || tp->t_cflag & CLOCAL ||
		    tp->t_state & TS_CARR_ON)
			break;
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	if (error == 0)
		error = linesw[tp->t_line].l_open(dev, tp);
	if (error)
		zs_modem(cs, 0);
	return (error);
}

/*
 * Close a zs serial port.
 */
int
zsclose(dev_t dev, int flags, int mode, struct proc *p)
{
	register struct zs_chanstate *cs;
	register struct tty *tp;
	struct zsinfo *zi;
	int unit = minor(dev), s;

	zi = zscd.cd_devs[unit >> 1];
	cs = &zi->zi_cs[unit & 1];
	tp = cs->cs_ttyp;
	linesw[tp->t_line].l_close(tp, flags);
	if (tp->t_cflag & HUPCL || tp->t_state & TS_WOPEN ||
	    (tp->t_state & TS_ISOPEN) == 0) {
		zs_modem(cs, 0);
		/* hold low for 1 second */
		(void) tsleep((caddr_t)cs, TTIPRI, ttclos, hz);
	}
	ttyclose(tp);
#ifdef KGDB
	/* Reset the speed if we're doing kgdb on this port */
	if (cs->cs_kgdb) {
		tp->t_ispeed = tp->t_ospeed = kgdb_rate;
		(void) zsparam(tp, &tp->t_termios);
	}
#endif
	return (0);
}

/*
 * Read/write zs serial port.
 */
int
zsread(dev_t dev, struct uio *uio, int flags)
{
	register struct tty *tp = &zs_tty[minor(dev)];

	return (linesw[tp->t_line].l_read(tp, uio, flags));
}

int
zswrite(dev_t dev, struct uio *uio, int flags)
{
	register struct tty *tp = &zs_tty[minor(dev)];

	return (linesw[tp->t_line].l_write(tp, uio, flags));
}

/*
 * ZS hardware interrupt.  Scan all ZS channels.  NB: we know here that
 * channels are kept in (A,B) pairs.
 *
 * Do just a little, then get out; set a software interrupt if more
 * work is needed.
 *
 * We deliberately ignore the vectoring Zilog gives us, and match up
 * only the number of `reset interrupt under service' operations, not
 * the order.
 */
/* ARGSUSED */
int
zshard(void *intrarg)
{
	register struct zs_chanstate *a;
#define	b (a + 1)
	register volatile struct zschan *zc;
	register int rr3, intflags = 0, v, i;
	static int zsrint(struct zs_chanstate *, volatile struct zschan *);
	static int zsxint(struct zs_chanstate *, volatile struct zschan *);
	static int zssint(struct zs_chanstate *, volatile struct zschan *);

	for (a = zslist; a != NULL; a = b->cs_next) {
		rr3 = ZS_READ(a->cs_zc, 3);
		if (rr3 & (ZSRR3_IP_A_RX|ZSRR3_IP_A_TX|ZSRR3_IP_A_STAT)) {
			intflags |= 2;
			zc = a->cs_zc;
			i = a->cs_rbput;
			if (rr3 & ZSRR3_IP_A_RX && (v = zsrint(a, zc)) != 0) {
				a->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			if (rr3 & ZSRR3_IP_A_TX && (v = zsxint(a, zc)) != 0) {
				a->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			if (rr3 & ZSRR3_IP_A_STAT && (v = zssint(a, zc)) != 0) {
				a->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			a->cs_rbput = i;
		}
		if (rr3 & (ZSRR3_IP_B_RX|ZSRR3_IP_B_TX|ZSRR3_IP_B_STAT)) {
			intflags |= 2;
			zc = b->cs_zc;
			i = b->cs_rbput;
			if (rr3 & ZSRR3_IP_B_RX && (v = zsrint(b, zc)) != 0) {
				b->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			if (rr3 & ZSRR3_IP_B_TX && (v = zsxint(b, zc)) != 0) {
				b->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			if (rr3 & ZSRR3_IP_B_STAT && (v = zssint(b, zc)) != 0) {
				b->cs_rbuf[i++ & ZLRB_RING_MASK] = v;
				intflags |= 1;
			}
			b->cs_rbput = i;
		}
	}
#undef b
	if (intflags & 1) {
#if sun4c /* XXX -- but this will go away when zshard moves to locore.s */
		struct clockframe *p = intrarg;

		if ((p->psr & PSR_PIL) < (PIL_TTY << 8)) {
			zsshortcuts++;
			(void) spltty();
			if (zshardscope) {
				LED_ON;
				LED_OFF;
			}
			return (zssoft(intrarg));
		}
#endif
		ienab_bis(IE_ZSSOFT);
	}
	return (intflags & 2);
}

static int
zsrint(register struct zs_chanstate *cs, register volatile struct zschan *zc)
{
	register int c = zc->zc_data;

	if (cs->cs_conk) {
		register struct conk_state *conk = &zsconk_state;

		/*
		 * Check here for console abort function, so that we
		 * can abort even when interrupts are locking up the
		 * machine.
		 */
		if (c == KBD_RESET) {
			conk->conk_id = 1;	/* ignore next byte */
			conk->conk_l1 = 0;
		} else if (conk->conk_id)
			conk->conk_id = 0;	/* stop ignoring bytes */
		else if (c == KBD_L1)
			conk->conk_l1 = 1;	/* L1 went down */
		else if (c == (KBD_L1|KBD_UP))
			conk->conk_l1 = 0;	/* L1 went up */
		else if (c == KBD_A && conk->conk_l1) {
			zsabort();
			conk->conk_l1 = 0;	/* we never see the up */
			goto clearit;		/* eat the A after L1-A */
		}
	}
#ifdef KGDB
	if (c == FRAME_START && cs->cs_kgdb && 
	    (cs->cs_ttyp->t_state & TS_ISOPEN) == 0) {
		zskgdb(cs->cs_unit);
		goto clearit;
	}
#endif
	/* compose receive character and status */
	c <<= 8;
	c |= ZS_READ(zc, 1);

	/* clear receive error & interrupt condition */
	zc->zc_csr = ZSWR0_RESET_ERRORS;
	zc->zc_csr = ZSWR0_CLR_INTR;

	return (ZRING_MAKE(ZRING_RINT, c));

clearit:
	zc->zc_csr = ZSWR0_RESET_ERRORS;
	zc->zc_csr = ZSWR0_CLR_INTR;
	return (0);
}

static int
zsxint(register struct zs_chanstate *cs, register volatile struct zschan *zc)
{
	register int i = cs->cs_tbc;

	if (i == 0) {
		zc->zc_csr = ZSWR0_RESET_TXINT;
		zc->zc_csr = ZSWR0_CLR_INTR;
		return (ZRING_MAKE(ZRING_XINT, 0));
	}
	cs->cs_tbc = i - 1;
	zc->zc_data = *cs->cs_tba++;
	zc->zc_csr = ZSWR0_CLR_INTR;
	return (0);
}

static int
zssint(register struct zs_chanstate *cs, register volatile struct zschan *zc)
{
	register int rr0;

	rr0 = zc->zc_csr;
	zc->zc_csr = ZSWR0_RESET_STATUS;
	zc->zc_csr = ZSWR0_CLR_INTR;
	/*
	 * The chip's hardware flow control is, as noted in zsreg.h,
	 * busted---if the DCD line goes low the chip shuts off the
	 * receiver (!).  If we want hardware CTS flow control but do
	 * not have it, and carrier is now on, turn HFC on; if we have
	 * HFC now but carrier has gone low, turn it off.
	 */
	if (rr0 & ZSRR0_DCD) {
		if (cs->cs_ttyp->t_cflag & CCTS_OFLOW &&
		    (cs->cs_creg[3] & ZSWR3_HFC) == 0) {
			cs->cs_creg[3] |= ZSWR3_HFC;
			ZS_WRITE(zc, 3, cs->cs_creg[3]);
		}
	} else {
		if (cs->cs_creg[3] & ZSWR3_HFC) {
			cs->cs_creg[3] &= ~ZSWR3_HFC;
			ZS_WRITE(zc, 3, cs->cs_creg[3]);
		}
	}
	if ((rr0 & ZSRR0_BREAK) && cs->cs_brkabort) {
		zsabort();
		return (0);
	}
	return (ZRING_MAKE(ZRING_SINT, rr0));
}

zsabort()
{

	printf("stopping on keyboard abort\n");
	callrom();
}

#ifdef KGDB
/*
 * KGDB framing character received: enter kernel debugger.  This probably
 * should time out after a few seconds to avoid hanging on spurious input.
 */
zskgdb(int unit)
{

	printf("zs%d%c: kgdb interrupt\n", unit >> 1, (unit & 1) + 'a');
	kgdb_connect(1);
}
#endif

/*
 * Print out a ring or fifo overrun error message.
 */
static void
zsoverrun(int unit, long *ptime, char *what)
{

	if (*ptime != time.tv_sec) {
		*ptime = time.tv_sec;
		log(LOG_WARNING, "zs%d%c: %s overrun\n", unit >> 1,
		    (unit & 1) + 'a', what);
	}
}

/*
 * ZS software interrupt.  Scan all channels for deferred interrupts.
 */
int
zssoft(void *arg)
{
	register struct zs_chanstate *cs;
	register volatile struct zschan *zc;
	register struct linesw *line;
	register struct tty *tp;
	register int get, n, c, cc, unit, s;

	for (cs = zslist; cs != NULL; cs = cs->cs_next) {
		get = cs->cs_rbget;
again:
		n = cs->cs_rbput;	/* atomic */
		if (get == n)		/* nothing more on this line */
			continue;
		unit = cs->cs_unit;	/* set up to handle interrupts */
		zc = cs->cs_zc;
		tp = cs->cs_ttyp;
		line = &linesw[tp->t_line];
		/*
		 * Compute the number of interrupts in the receive ring.
		 * If the count is overlarge, we lost some events, and
		 * must advance to the first valid one.  It may get
		 * overwritten if more data are arriving, but this is
		 * too expensive to check and gains nothing (we already
		 * lost out; all we can do at this point is trade one
		 * kind of loss for another).
		 */
		n -= get;
		if (n > ZLRB_RING_SIZE) {
			zsoverrun(unit, &cs->cs_rotime, "ring");
			get += n - ZLRB_RING_SIZE;
			n = ZLRB_RING_SIZE;
		}
		while (--n >= 0) {
			/* race to keep ahead of incoming interrupts */
			c = cs->cs_rbuf[get++ & ZLRB_RING_MASK];
			switch (ZRING_TYPE(c)) {

			case ZRING_RINT:
				c = ZRING_VALUE(c);
				if (c & ZSRR1_DO)
					zsoverrun(unit, &cs->cs_fotime, "fifo");
				cc = c >> 8;
				if (c & ZSRR1_FE)
					cc |= TTY_FE;
				if (c & ZSRR1_PE)
					cc |= TTY_PE;
				/*
				 * this should be done through
				 * bstreams	XXX gag choke
				 */
				if (unit == ZS_KBD)
					kbd_rint(cc);
				else if (unit == ZS_MOUSE)
					ms_rint(cc);
				else
					line->l_rint(cc, tp);
				break;

			case ZRING_XINT:
				/*
				 * Transmit done: change registers and resume,
				 * or clear BUSY.
				 */
				if (cs->cs_heldchange) {
					s = splzs();
					c = zc->zc_csr;
					if ((c & ZSRR0_DCD) == 0)
						cs->cs_preg[3] &= ~ZSWR3_HFC;
					bcopy((caddr_t)cs->cs_preg,
					    (caddr_t)cs->cs_creg, 16);
					zs_loadchannelregs(zc, cs->cs_creg);
					splx(s);
					cs->cs_heldchange = 0;
					if (cs->cs_heldtbc &&
					    (tp->t_state & TS_TTSTOP) == 0) {
						cs->cs_tbc = cs->cs_heldtbc - 1;
						zc->zc_data = *cs->cs_tba++;
						goto again;
					}
				}
				tp->t_state &= ~TS_BUSY;
				if (tp->t_state & TS_FLUSH)
					tp->t_state &= ~TS_FLUSH;
				else
					ndflush(&tp->t_outq,
					    cs->cs_tba - tp->t_outq.c_cf);
				line->l_start(tp);
				break;

			case ZRING_SINT:
				/*
				 * Status line change.  HFC bit is run in
				 * hardware interrupt, to avoid locking
				 * at splzs here.
				 */
				c = ZRING_VALUE(c);
				if ((c ^ cs->cs_rr0) & ZSRR0_DCD) {
					cc = (c & ZSRR0_DCD) != 0;
					if (line->l_modem(tp, cc) == 0)
						zs_modem(cs, cc);
				}
				cs->cs_rr0 = c;
				break;

			default:
				log(LOG_ERR, "zs%d%c: bad ZRING_TYPE (%x)\n",
				    unit >> 1, (unit & 1) + 'a', c);
				break;
			}
		}
		cs->cs_rbget = get;
		goto again;
	}
	return (1);
}

int
zsioctl(dev_t dev, int cmd, caddr_t data, int flag, struct proc *p)
{
	int unit = minor(dev);
	struct zsinfo *zi = zscd.cd_devs[unit >> 1];
	register struct tty *tp = zi->zi_cs[unit & 1].cs_ttyp;
	register int error;

	error = linesw[tp->t_line].l_ioctl(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		/* FINISH ME ... need implicit TIOCCBRK in zsclose as well */

	case TIOCCBRK:

	case TIOCSDTR:

	case TIOCCDTR:

	case TIOCMSET:

	case TIOCMBIS:

	case TIOCMBIC:

	case TIOCMGET:

	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 * Start or restart transmission.
 */
static void
zsstart(register struct tty *tp)
{
	register struct zs_chanstate *cs;
	register int s, nch;
	int unit = minor(tp->t_dev);
	struct zsinfo *zi = zscd.cd_devs[unit >> 1];

	cs = &zi->zi_cs[unit & 1];
	s = spltty();

	/*
	 * If currently active or delaying, no need to do anything.
	 */
	if (tp->t_state & (TS_TIMEOUT | TS_BUSY | TS_TTSTOP))
		goto out;

	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, awaken.
	 */
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}

	nch = ndqb(&tp->t_outq, 0);	/* XXX */
	if (nch) {
		register char *p = tp->t_outq.c_cf;

		/* mark busy, enable tx done interrupts, & send first byte */
		tp->t_state |= TS_BUSY;
		(void) splzs();
		cs->cs_preg[1] |= ZSWR1_TIE;
		cs->cs_creg[1] |= ZSWR1_TIE;
		ZS_WRITE(cs->cs_zc, 1, cs->cs_creg[1]);
		cs->cs_zc->zc_data = *p;
		cs->cs_tba = p + 1;
		cs->cs_tbc = nch - 1;
	} else {
		/*
		 * Nothing to send, turn off transmit done interrupts.
		 * This is useful if something is doing polled output.
		 */
		(void) splzs();
		cs->cs_preg[1] &= ~ZSWR1_TIE;
		cs->cs_creg[1] &= ~ZSWR1_TIE;
		ZS_WRITE(cs->cs_zc, 1, cs->cs_creg[1]);
	}
out:
	splx(s);
}

/*
 * Stop output, e.g., for ^S or output flush.
 */
static void
zsstop(register struct tty *tp, int flag)
{
	register struct zs_chanstate *cs;
	register int s, unit = minor(tp->t_dev);
	struct zsinfo *zi = zscd.cd_devs[unit >> 1];

	cs = &zi->zi_cs[unit & 1];
	s = splzs();
	if (tp->t_state & TS_BUSY) {
		/*
		 * Device is transmitting; must stop it.
		 */
		cs->cs_tbc = 0;
		if ((tp->t_state & TS_TTSTOP) == 0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

/*
 * Set ZS tty parameters from termios.
 *
 * This routine makes use of the fact that only registers
 * 1, 3, 4, 5, 9, 10, 11, 12, 13, 14, and 15 are written.
 */
static int
zsparam(register struct tty *tp, register struct termios *t)
{
	int unit = minor(tp->t_dev);
	struct zsinfo *zi = zscd.cd_devs[unit >> 1];
	register struct zs_chanstate *cs = &zi->zi_cs[unit & 1];
	register int tmp, tmp5, cflag, s;

	/*
	 * Because PCLK is only run at 4.9 MHz, the fastest we
	 * can go is 51200 baud (this corresponds to TC=1).
	 * This is somewhat unfortunate as there is no real
	 * reason we should not be able to handle higher rates.
	 */
	tmp = t->c_ospeed;
	if (tmp < 0 || (t->c_ispeed && t->c_ispeed != tmp))
		return (EINVAL);
	if (tmp == 0) {
		/* stty 0 => drop DTR and RTS */
		zs_modem(cs, 0);
		return (0);
	}
	tmp = BPS_TO_TCONST(PCLK / 16, tmp);
	if (tmp < 2)
		return (EINVAL);

	cflag = t->c_cflag;
	tp->t_ispeed = tp->t_ospeed = TCONST_TO_BPS(PCLK / 16, tmp);
	tp->t_cflag = cflag;

	/*
	 * Block interrupts so that state will not
	 * be altered until we are done setting it up.
	 */
	s = splzs();
	cs->cs_preg[12] = tmp;
	cs->cs_preg[13] = tmp >> 8;
	cs->cs_preg[1] = ZSWR1_RIE | ZSWR1_TIE | ZSWR1_SIE;
	switch (cflag & CSIZE) {
	case CS5:
		tmp = ZSWR3_RX_5;
		tmp5 = ZSWR5_TX_5;
		break;
	case CS6:
		tmp = ZSWR3_RX_6;
		tmp5 = ZSWR5_TX_6;
		break;
	case CS7:
		tmp = ZSWR3_RX_7;
		tmp5 = ZSWR5_TX_7;
		break;
	case CS8:
	default:
		tmp = ZSWR3_RX_8;
		tmp5 = ZSWR5_TX_8;
		break;
	}

	/*
	 * Output hardware flow control on the chip is horrendous: if
	 * carrier detect drops, the receiver is disabled.  Hence we
	 * can only do this when the carrier is on.
	 */
	if (cflag & CCTS_OFLOW && cs->cs_zc->zc_csr & ZSRR0_DCD)
		tmp |= ZSWR3_HFC | ZSWR3_RX_ENABLE;
	else
		tmp |= ZSWR3_RX_ENABLE;
	cs->cs_preg[3] = tmp;
	cs->cs_preg[5] = tmp5 | ZSWR5_TX_ENABLE | ZSWR5_DTR | ZSWR5_RTS;

	tmp = ZSWR4_CLK_X16 | (cflag & CSTOPB ? ZSWR4_TWOSB : ZSWR4_ONESB);
	if ((cflag & PARODD) == 0)
		tmp |= ZSWR4_EVENP;
	if (cflag & PARENB)
		tmp |= ZSWR4_PARENB;
	cs->cs_preg[4] = tmp;
	cs->cs_preg[9] = ZSWR9_MASTER_IE | ZSWR9_NO_VECTOR;
	cs->cs_preg[10] = ZSWR10_NRZ;
	cs->cs_preg[11] = ZSWR11_TXCLK_BAUD | ZSWR11_RXCLK_BAUD;
	cs->cs_preg[14] = ZSWR14_BAUD_FROM_PCLK | ZSWR14_BAUD_ENA;
	cs->cs_preg[15] = ZSWR15_BREAK_IE | ZSWR15_DCD_IE;

	/*
	 * If nothing is being transmitted, set up new current values,
	 * else mark them as pending.
	 */
	if (cs->cs_heldchange == 0) {
		if (cs->cs_ttyp->t_state & TS_BUSY) {
			cs->cs_heldtbc = cs->cs_tbc;
			cs->cs_tbc = 0;
			cs->cs_heldchange = 1;
		} else {
			bcopy((caddr_t)cs->cs_preg, (caddr_t)cs->cs_creg, 16);
			zs_loadchannelregs(cs->cs_zc, cs->cs_creg);
		}
	}
	splx(s);
	return (0);
}

/*
 * Raise or lower modem control (DTR/RTS) signals.  If a character is
 * in transmission, the change is deferred.
 */
static void
zs_modem(struct zs_chanstate *cs, int onoff)
{
	int s, bis, and;

	if (onoff) {
		bis = ZSWR5_DTR | ZSWR5_RTS;
		and = ~0;
	} else {
		bis = 0;
		and = ~(ZSWR5_DTR | ZSWR5_RTS);
	}
	s = splzs();
	cs->cs_preg[5] = (cs->cs_preg[5] | bis) & and;
	if (cs->cs_heldchange == 0) {
		if (cs->cs_ttyp->t_state & TS_BUSY) {
			cs->cs_heldtbc = cs->cs_tbc;
			cs->cs_tbc = 0;
			cs->cs_heldchange = 1;
		} else {
			cs->cs_creg[5] = (cs->cs_creg[5] | bis) & and;
			ZS_WRITE(cs->cs_zc, 5, cs->cs_creg[5]);
		}
	}
	splx(s);
}

/*
 * Write the given register set to the given zs channel in the proper order.
 * The channel must not be transmitting at the time.  The receiver will
 * be disabled for the time it takes to write all the registers.
 */
static void
zs_loadchannelregs(volatile struct zschan *zc, u_char *reg)
{
	int i;

	zc->zc_csr = ZSM_RESET_ERR;	/* reset error condition */
	i = zc->zc_data;		/* drain fifo */
	i = zc->zc_data;
	i = zc->zc_data;
	ZS_WRITE(zc, 4, reg[4]);
	ZS_WRITE(zc, 10, reg[10]);
	ZS_WRITE(zc, 3, reg[3] & ~ZSWR3_RX_ENABLE);
	ZS_WRITE(zc, 5, reg[5] & ~ZSWR5_TX_ENABLE);
	ZS_WRITE(zc, 1, reg[1]);
	ZS_WRITE(zc, 9, reg[9]);
	ZS_WRITE(zc, 11, reg[11]);
	ZS_WRITE(zc, 12, reg[12]);
	ZS_WRITE(zc, 13, reg[13]);
	ZS_WRITE(zc, 14, reg[14]);
	ZS_WRITE(zc, 15, reg[15]);
	ZS_WRITE(zc, 3, reg[3]);
	ZS_WRITE(zc, 5, reg[5]);
}

#ifdef KGDB
/*
 * Get a character from the given kgdb channel.  Called at splhigh().
 */
static int
zs_kgdb_getc(void *arg)
{
	register volatile struct zschan *zc = (volatile struct zschan *)arg;

	while ((zc->zc_csr & ZSRR0_RX_READY) == 0)
		continue;
	return (zc->zc_data);
}

/*
 * Put a character to the given kgdb channel.  Called at splhigh().
 */
static void
zs_kgdb_putc(void *arg, int c)
{
	register volatile struct zschan *zc = (volatile struct zschan *)arg;

	while ((zc->zc_csr & ZSRR0_TX_READY) == 0)
		continue;
	zc->zc_data = c;
}

/*
 * Set up for kgdb; called at boot time before configuration.
 * KGDB interrupts will be enabled later when zs0 is configured.
 */
void
zs_kgdb_init()
{
	volatile struct zsdevice *addr;
	volatile struct zschan *zc;
	int unit, zs;

	if (major(kgdb_dev) != ZSMAJOR)
		return;
	unit = minor(kgdb_dev);
	/*
	 * Unit must be 0 or 1 (zs0).
	 */
	if ((unsigned)unit >= ZS_KBD) {
		printf("zs_kgdb_init: bad minor dev %d\n", unit);
		return;
	}
	zs = unit >> 1;
	if ((addr = zsaddr[zs]) == NULL)
		addr = zsaddr[zs] = findzs(zs);
	unit &= 1;
	zc = unit == 0 ? &addr->zs_chan[CHAN_A] : &addr->zs_chan[CHAN_B];
	zs_kgdb_savedspeed = zs_getspeed(zc);
	printf("zs_kgdb_init: attaching zs%d%c at %d baud\n",
	    zs, unit + 'a', kgdb_rate);
	zs_reset(zc, 1, kgdb_rate);
	kgdb_attach(zs_kgdb_getc, zs_kgdb_putc, (void *)zc);
}
#endif /* KGDB */
