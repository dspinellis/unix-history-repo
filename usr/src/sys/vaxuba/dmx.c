/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	%W% (Berkeley) %G%
 */

/*
 * Common code for DMF32 and DMZ32 drivers
 */
#include "dmf.h"
#include "dmz.h"
#if NDMF + NDMZ > 0

#include "../machine/pte.h"

#include "bk.h"
#include "uba.h"
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "ioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "bkmac.h"
#include "clist.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "dmx.h"
#include "ubareg.h"
#include "ubavar.h"
#include "dmxreg.h"
#include "dmreg.h"

#ifndef	PORTSELECTOR
#define	ISPEED	B9600
#define	IFLAGS	(EVENP|ODDP|ECHO)
#else
#define	ISPEED	B4800
#define	IFLAGS	(EVENP|ODDP)
#endif

#ifndef DMX_TIMEOUT
#define DMX_TIMEOUT	10
#endif
int	dmx_timeout = DMX_TIMEOUT;		/* silo timeout, in ms */
int	dmx_mindma = 4;			/* don't dma below this point */

char	dmx_speeds[] =
	{ 0, 0, 1, 2, 3, 4, 0, 5, 6, 7, 010, 012, 014, 016, 017, 0 };

/*
 * The clist space is mapped by the drivers onto each UNIBUS.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an I/O space address for the DMA routine.
 */
int	cbase[NUBA];			/* base address in unibus map */
#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))

int	ttrstrt();

/*
 * DMF/DMZ open common code
 */
dmxopen(tp, sc)
	register struct tty *tp;
	register struct dmx_softc *sc;
{
	int s, unit;

	s = spltty();
	if ((sc->dmx_flags & DMX_ACTIVE) == 0) {
		sc->dmx_octet->csr |= DMF_IE;
		sc->dmx_flags |= DMX_ACTIVE;
		sc->dmx_octet->rsp = dmx_timeout;
	}
	splx(s);
	if (tp->t_state & TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	/*
	 * If this is first open, initialize tty state to default.
	 */
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#else
			tp->t_state |= TS_HUPCLS;
#endif PORTSELECTOR
			tp->t_ispeed = ISPEED;
			tp->t_ospeed = ISPEED;
			tp->t_flags = IFLAGS;
#ifndef PORTSELECTOR
		}
#endif PORTSELECTOR
	}
	dmxparam(tp);

	unit = minor(tp->t_dev) & 07;
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	s = spltty();
	for (;;) {
		if ((dmxmctl(tp, DMF_ON, DMSET) & DMF_CAR) ||
		    (sc->dmx_softCAR & (1 << unit)))
			tp->t_state |= TS_CARR_ON;
		if (tp->t_state & TS_CARR_ON)
			break;
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	splx(s);
	return ((*linesw[tp->t_line].l_open)(tp->t_dev, tp));
}

dmxclose(tp)
	register struct tty *tp;
{

	(*linesw[tp->t_line].l_close)(tp);
	(void) dmxmctl(tp, DMF_BRK, DMBIC);
	if (tp->t_state & TS_HUPCLS || (tp->t_state & TS_ISOPEN) == 0)
		(void) dmxmctl(tp, DMF_OFF, DMSET);
	ttyclose(tp);
}

dmxrint(sc)
	register struct dmx_softc *sc;
{
	register c;
	register struct tty *tp;
	register struct dmx_octet *addr;
	int unit;
	int overrun = 0;

	addr = (struct dmx_octet *)sc->dmx_octet;
	/*
	 * Loop fetching characters from the silo for this
	 * octet until there are no more in the silo.
	 */
	while ((c = addr->rbuf) < 0) {

		unit = (c >> 8) & 07;
		tp = sc->dmx_tty + unit;
		if (c & DMF_DSC) {
			addr->csr = DMF_IE | DMFIR_RMSTSC | unit;
			if (addr->rmstsc & DMF_CAR)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
			else if ((sc->dmx_softCAR & (1 << unit)) == 0 &&
			    (*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
				addr->csr = DMF_IE | DMFIR_LCR | unit;
				addr->lctms = DMF_ENA;
			}
			continue;
		}
		if ((tp->t_state&TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if ((tp->t_state & TS_WOPEN) == 0)
#endif
				continue;
		}
		if (c & (DMF_PE|DMF_DO|DMF_FE)) {
			if (c & DMF_PE)
				if ((tp->t_flags & (EVENP|ODDP)) == EVENP
			 	|| (tp->t_flags & (EVENP|ODDP)) == ODDP)
					continue;
			if ((c & DMF_DO) && overrun == 0) {
				log(LOG_WARNING,
				    "dm%c%d: silo overflow, line %d\n",
				    sc->dmx_type, sc->dmx_unit,
				    sc->dmx_unit0 + unit);
				overrun = 1;
			}
			if (c & DMF_FE)
				/*
				 * At framing error (break) generate
				 * a null (in raw mode, for getty), or an
				 * interrupt (in cooked/cbreak mode).
				 */
				if (tp->t_flags & RAW)
					c = 0;
				else
					c = tp->t_intrc;
		}
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
}

dmxioctl(tp, cmd, data, flag)
	register struct tty *tp;
	caddr_t data;
{
	int error;

	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0) {
		if (cmd == TIOCSETP || cmd == TIOCSETN || cmd == TIOCLBIS ||
		    cmd == TIOCLBIC || cmd == TIOCLSET)
			dmxparam(tp);
		return (error);
	}
	switch (cmd) {

	case TIOCSBRK:
		(void) dmxmctl(tp, DMF_BRK, DMBIS);
		break;

	case TIOCCBRK:
		(void) dmxmctl(tp, DMF_BRK, DMBIC);
		break;

	case TIOCSDTR:
		(void) dmxmctl(tp, DMF_DTR|DMF_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dmxmctl(tp, DMF_DTR|DMF_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dmxmctl(tp, dmtodmx(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) dmxmctl(tp, dmtodmx(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) dmxmctl(tp, dmtodmx(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dmxmctl(tp, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 * modem control
 * "bits" are dmf/dmz lcr format;
 * return of DMGET is DM11 format.
 */
dmxmctl(tp, bits, how)
	struct tty *tp;
	int bits, how;
{
	register struct dmx_octet *addr;
	register int unit, mbits, lcr;
	int s;

	unit = minor(tp->t_dev) & 07;
	addr = (struct dmx_octet *)(tp->t_addr);

	s = spltty();
	addr->csr = DMF_IE | DMFIR_RMSTSC | unit;
	mbits = addr->rmstsc & 0xff00;
	addr->csr = DMF_IE | DMFIR_LCR | unit;
	lcr = addr->lctms;

	switch (how) {
	case DMSET:
		lcr = bits;
		break;

	case DMBIS:
		lcr |= bits;
		break;

	case DMBIC:
		lcr &= ~bits;
		break;

	case DMGET:
		splx(s);
		return (dmxtodm(mbits, lcr));
	}
	addr->lctms = lcr;
	(void) splx(s);
	return (mbits);
}

/*
 * Routine to convert modem status from dm to dmf/dmz lctmr format.
 */
dmtodmx(bits)
	register int bits;
{
	register int lcr = DMF_ENA;

	if (bits & DML_DTR)
		lcr |= DMF_DTR;
	if (bits & DML_RTS)
		lcr |= DMF_RTS;
	if (bits & DML_ST)
		lcr |= DMF_SRTS;
	if (bits & DML_USR)
		lcr |= DMF_USRW;
	return (lcr);
}

/*
 * Routine to convert modem status from dmf/dmz receive modem status
 * and line control register to dm format.
 * If dmf/dmz user modem read bit set, set DML_USR.
 */
dmxtodm(mstat, lcr)
	register int mstat, lcr;
{

	mstat = ((mstat & (DMF_DSR|DMF_RNG|DMF_CAR|DMF_CTS|DMF_SR)) >> 7) | 
		((mstat & DMF_USRR) >> 1) | DML_LE;
	if (lcr & DMF_DTR)
		mstat |= DML_DTR;
	if (lcr & DMF_SRTS)
		mstat |= DML_ST;
	if (lcr & DMF_RTS)
		mstat |= DML_RTS;
	return (mstat);
}
 

/*
 * Set parameters from open or ioctl into the hardware registers.
 */
dmxparam(tp)
	register struct tty *tp;
{
	register struct dmx_octet *addr;
	register int lpar, lcr;
	int s, unit;

	addr = (struct dmx_octet *)tp->t_addr;
	unit = minor(tp->t_dev) & 07;
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spltty();
	addr->csr = unit | DMFIR_LCR | DMF_IE;
	if (tp->t_ispeed == 0) {
		tp->t_state |= TS_HUPCLS;
		(void) dmxmctl(tp, DMF_OFF, DMSET);
		splx(s);
		return;
	}
	lpar = (dmx_speeds[tp->t_ospeed]<<12) | (dmx_speeds[tp->t_ispeed]<<8);
	lcr = DMF_ENA;
	if ((tp->t_ispeed) == B134)
		lpar |= BITS6|PENABLE;
	else if (tp->t_flags & (RAW|LITOUT|PASS8))
		lpar |= BITS8;
	else {
		lpar |= BITS7|PENABLE;
		/* CHECK FOR XON/XOFF AND SET lcr |= DMF_AUTOX; */
	}
	if (tp->t_flags&EVENP)
		lpar |= EPAR;
	if ((tp->t_ospeed) == B110)
		lpar |= TWOSB;
	lpar |= (unit&07);
	addr->lpr = lpar;
	addr->lctms = (addr->lctms &~ 0xff) | lcr;
	splx(s);
}

/*
 * Process a transmit interrupt on an octet.
 */
dmxxint(sc)
	register struct dmx_softc *sc;
{
	register struct tty *tp;
	register struct dmx_octet *addr;
	register int t;

	addr = (struct dmx_octet *)sc->dmx_octet;
	while ((t = addr->csr) & DMF_TI) {
		if (t & DMF_NXM)
			/* SHOULD RESTART OR SOMETHING... */
			printf("dm%c%d: NXM line %d\n", sc->dmx_type,
			    sc->dmx_unit, sc->dmx_unit0 + (t >> 8 & 7));
		t = t >> 8 & 7;
		tp = sc->dmx_tty + t;
		tp->t_state &= ~TS_BUSY;
		if (tp->t_state & TS_FLUSH)
			tp->t_state &= ~TS_FLUSH;
#define new
#ifndef new
		else if (sc->dmx_dmacount[t]) {
			short cntr;

			/*
			 * Do arithmetic in a short to make up
			 * for lost 16&17 bits.
			 */
			addr->csr = DMFIR_TBA | DMF_IE | t;
			cntr = addr->tba -
			    UBACVT(tp->t_outq.c_cf, sc->dmx_ubanum);
			ndflush(&tp->t_outq, (int)cntr);
		}
#else
		else if (sc->dmx_dmacount[t])
			ndflush(&tp->t_outq, sc->dmx_dmacount[t]);
		sc->dmx_dmacount[t] = 0;
#endif
		(*linesw[tp->t_line].l_start)(tp);
	}
}

dmxstart(tp, sc)
	register struct tty *tp;
	struct dmx_softc *sc;
{
	register struct dmx_octet *addr;
	register int unit, nch;
	int s;

	unit = minor(tp->t_dev) & 07;
	addr = (struct dmx_octet *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spltty();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If there are still characters to dma or in the silo,
	 * just reenable the transmitter.
	 */
	addr->csr = DMF_IE | DMFIR_TBUF | unit;
#ifdef new
	if (addr->tsc || sc->dmx_dmacount[unit]) {
#else
	if (addr->tsc) {
#endif
		addr->csr = DMF_IE | DMFIR_LCR | unit;
		addr->lctms = addr->lctms | DMF_TE;
		tp->t_state |= TS_BUSY;
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & (RAW|LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		if ((nch = ndqb(&tp->t_outq, 0200)) == 0) {
			/*
		 	* If first thing on queue is a delay process it.
		 	*/
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch >= dmx_mindma) {
		register car;

		sc->dmx_dmacount[unit] = nch;
		addr->csr = DMF_IE | DMFIR_LCR | unit;
		addr->lctms = addr->lctms | DMF_TE;
		car = UBACVT(tp->t_outq.c_cf, sc->dmx_ubanum);
		addr->csr = DMF_IE | DMFIR_TBA | unit;
		addr->tba = car;
		addr->tcc = ((car >> 2) & 0xc000) | nch;
		tp->t_state |= TS_BUSY;
	} else if (nch) {
		register char *cp = tp->t_outq.c_cf;
		register int i;

#ifndef new
		sc->dmx_dmacount[unit] = 0;
#endif
		nch = MIN(nch, DMF_SILOCNT);
		addr->csr = DMF_IE | DMFIR_LCR | unit;
		addr->lctms = addr->lctms | DMF_TE;
		addr->csr = DMF_IE | DMFIR_TBUF | unit;
		for (i = 0; i < nch; i++)
			addr->tbuf = *cp++;
		ndflush(&tp->t_outq, nch);
		tp->t_state |= TS_BUSY;
	}
out:
	splx(s);
}

dmxstop(tp, sc, flag)
	register struct tty *tp;
	struct dmx_softc *sc;
{
	register struct dmx_octet *addr;
	register unit = minor(tp->t_dev) & 7;
	int s;

	addr = (struct dmx_octet *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spltty();
	if (flag) {
		addr->csr = DMF_IE | DMFIR_TBUF | unit;
		if (addr->tsc) {
			/*
			 * Flush regardless of whether we're transmitting
			 * (TS_BUSY), if the silo contains untransmitted
			 * characters.
			 */
			addr->csr = DMFIR_LCR | unit | DMF_IE;
			addr->lctms = addr->lctms | DMF_TE | DMF_FLUSH;
			/* this will interrupt so let dmxxint handle the rest */
			tp->t_state |= TS_FLUSH|TS_BUSY;
		}
/*#ifdef new*/
		sc->dmx_dmacount[unit] = 0;
/*#endif*/
	} else {
		/*
		 * Stop transmission by disabling
		 * the transmitter.  We'll pick up where we
		 * left off by reenabling in dmxstart.
		 */
		addr->csr = DMFIR_LCR | unit | DMF_IE;
		addr->lctms = addr->lctms &~ DMF_TE;
		/* no interrupt here */
		tp->t_state &= ~TS_BUSY;
	}
	splx(s);
}
#endif NDMF + NDMZ
