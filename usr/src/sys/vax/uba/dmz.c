/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmz.c	6.4 (Berkeley) %G%
 */

/*
 * DMZ-32 driver
 * HISTORY
 * 23-Apr-85  Joe Camaratta (jcc) at Siemens RTL
 *	Driver for DEC's DMZ32 24-line asynchronous multiplexor.
 *	Based on Chris Maloney's driver for DEC's DMF32
 *	NOTE: The modem control routines have NOT been tested yet!!!
 *
 * 9-Aug-85	Mike Meyer (mwm) at ucb
 *	Mangled into shape for 4.3.
 */

#include "dmz.h"
#if NDMZ > 0


#include "../machine/pte.h"


#include "bk.h"
#include "uba.h"
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
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

#include "ubareg.h"
#include "ubavar.h"
#include "dmzreg.h"

int dmzprobe(), dmzattach(), dmzrint(), dmzxint();
struct uba_device *dmzinfo[NDMZ];
u_short dmzstd[] = {0, 0};
struct uba_driver dmzdriver = {
	dmzprobe, 0, dmzattach, 0, dmzstd, "dmz", dmzinfo
};

#define	NDMZLINES	(NDMZ*24)

int ttrstrt();
struct tty dmz_tty[NDMZLINES];

int dmzsoftCAR[NDMZ];

struct {
	char dmz_state;		/* dmz state */
	int dmz_count;		/* dmz dma count */
} dmz_softc[NDMZ*24];

#define	ST_TXOFF	(0x01)	/* transmission turned off (^S) */
#define	ST_DMA		(0x02)	/* dma inprogress */
#define	ST_INBUSY	(0x04)	/* stop transmission in busy */

char dmz_speeds[] = {
	0, 0, 1, 2, 3, 4, 0, 5, 6, 7, 010, 012, 014, 016, 017, 0 
};

#ifndef	PORTSELECTOR
#define	ISPEED	B9600
#define	IFLAGS	(EVENP|ODDP|ECHO)
#else
#define	ISPEED	B4800
#define	IFLAGS	(EVENP|ODDP)
#endif

#ifndef lint
int ndmz = NDMZLINES;		/* Used by pstat/iostat */
#endif

short dmzact[NDMZ];		/* Mask of active octets on the dmz */
int dmzstart();

/*
 * SILO_TIMEOUT represents the number of milliseconds characters can sit
 * in the input silo without causing an interrupt.  If data overruns or
 * slow XON/XOFF occur, set it lower but AT LEAST equal to 1.
 */
#define	SILO_TIMEOUT	(3)

/*
 * DO_DMA_COUNT represents the threshold of the number of output
 * characters beyond which the driver uses DMA mode.
 */
#define	DO_DMA_COUNT	(10)

#define	TRUE		(1)
#define	FALSE		(0)

int cbase[NUBA];		/* base address in unibus map */
int dmz_ubinfo[NUBA];		/* info about allocated unibus map */

#define	UBACVT(x, uban)	    (cbase[uban] + ((x) - (char *)cfree))

/* These flags are for debugging purposes only */
int dmz_dma_on = 1;
int dmz_debug_level;

dmzprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct dmzdevice *dmz_addr;
	register unsigned int a;

	dmz_addr = (struct dmzdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec; dmzxinta(0); dmzxintb(0); dmzxintc(0);
	dmzrinta(0); dmzrintb(0); dmzrintc(0);
#endif

	br = 0x15;

	a = dmz_addr->dmz_config;
	if (((a>>12) & ~DMZ_INTERFACE) != 0) {
		printf("	Unknown interface type\n");
		return (0);
	}
	if (((a>>8) & DMZ_NOC_MASK) != 3) {
		printf("	Not all octets are available\n");
		return (0);
	}

	cvec = (uba_hd[numuba].uh_lastiv -= 4 * 6);
	dmz_addr->dmz_config = cvec >> 2;

	return (sizeof(struct dmzdevice));
}

dmzattach(ui)
	struct uba_device *ui;
{
	dmzsoftCAR[ui->ui_unit] = ui->ui_flags;
	return;
}

/* ARGSUSED */
dmzopen(device, flag)
	dev_t device;
	int flag;
{
	register struct tty *tp;
	register int unit, controller;
	register struct dmzdevice *dmz_addr;
	register struct uba_device *ui;
	int priority;
	int octet;

	unit = minor(device);
	controller = DMZ(unit);
	octet = OCTET(unit);

	if (unit >= NDMZLINES ||
	    (ui = dmzinfo[controller]) == 0 ||
	    ui->ui_alive == 0)
		return (ENXIO);

	tp = &dmz_tty[unit];

	if ((tp->t_state & TS_XCLUDE) && u.u_uid != 0)
		return (EBUSY);

	dmz_addr = (struct dmzdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)dmz_addr;
	tp->t_oproc = dmzstart;
	tp->t_state |= TS_WOPEN;

	/*
	 * Set up Unibus map registers.  Block uba resets, which can
	 * clear the state.
	 */
	priority = spl5();
	if (cbase[ui->ui_ubanum] == 0) {
		dmz_ubinfo[ui->ui_ubanum] = 
			uballoc(ui->ui_ubanum, (caddr_t)cfree,
				nclist * sizeof(struct cblock), 0);
		if (dmz_ubinfo[ui->ui_ubanum] == 0) {
			splx(priority);
			printf("dmz: insufficient unibus map regs\n");
			return (ENOMEM);
		}
		cbase[ui->ui_ubanum] = dmz_ubinfo[ui->ui_ubanum] & 0x3ffff;
	}

	if ((dmzact[controller] & (1 << octet)) == 0) {
		dmz_addr->octet[octet].octet_csr |= DMZ_IE;
		dmzact[controller] |= 1 << octet;
		dmz_addr->octet[octet].octet_receive.octet_sato = SILO_TIMEOUT;
	}

	splx(priority);

	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ispeed = tp->t_ospeed = ISPEED;
		tp->t_flags = IFLAGS;
		dmzparam(unit);
		dmz_softc[unit].dmz_state = 0;
	}

	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	if ((dmzmctl(device, DMZ_ON, DMSET) & (DMZ_CAR << 8)) ||
	    (dmzsoftCAR[controller] & (1 << (unit % 24))))
		tp->t_state |= TS_CARR_ON;
	priority = spl5();	
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t) &tp->t_rawq, TTIPRI);
	}
	splx(priority);

	return ((*linesw[tp->t_line].l_open)(device, tp));
}

dmzparam(unit)
	register int unit;
{
	register struct tty *tp;
	register struct dmzdevice *dmz_addr;
	register int line_parameters, line_control;
	register int octet;
	int priority;

	octet = OCTET(unit);

	tp = &dmz_tty[unit];
	dmz_addr = (struct dmzdevice *)tp->t_addr;

	priority = spl5();
	if ((tp->t_ispeed) == 0) {
		tp->t_state |= TS_HUPCLS;
		(void) dmzmctl(unit, DMZ_OFF, DMSET);
		splx(priority);
		return;
	}

	line_parameters = (dmz_speeds[tp->t_ospeed] << 12) | (dmz_speeds[tp->t_ispeed] << 8);
	line_control = DMZ_LCE;

	if ((tp->t_ispeed) == B134)
		line_parameters |= DMZ_6BT | DMZ_PEN;
	else if (tp->t_flags & (RAW | LITOUT))
		line_parameters |= DMZ_8BT;
	else
		line_parameters |= DMZ_7BT | DMZ_PEN;

	if (tp->t_flags & EVENP)
		line_parameters |= DMZ_EPR;
	if ((tp->t_ospeed) == B110)
		line_parameters |= DMZ_SCD;

	line_parameters |= (unit & 07);

	dmz_addr->octet[octet].octet_lprm = line_parameters;
	dmz_addr->octet[octet].octet_csr = DMZ_IE | IR_LCTMR | (unit & 07);
	dmz_addr->octet[octet].octet_lctmr = 
	    (dmz_addr->octet[octet].octet_lctmr | (line_control & 0xff));

	splx(priority);
	return;
}

/* ARGSUSED */
dmzclose(device, flag)
	dev_t device;
	int flag;
{
	register struct  tty *tp;
	register int unit;

	unit = minor(device);
	tp = &dmz_tty[unit];

	/*
	 * Break, hang-up and close the modem.
	 */
	(void) dmzmctl(unit, DMZ_BRK, DMBIC);
	if (tp->t_state & TS_HUPCLS || (tp->t_state & TS_ISOPEN) == 0)
		(void) dmzmctl(unit, DMZ_OFF, DMSET);
	ttyclose(tp);
	return;
}

dmzreset(uban)
	int uban;
{
	register int controller, unit;
	register struct tty *tp;
	register struct uba_device *ui;
	register struct dmzdevice *dmz_addr;
	int i;
	int octet;

	for (controller = 0; controller < NDMZ; controller++) {
		ui = dmzinfo[controller];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf("dmz%d ", controller);
		dmz_addr = (struct dmzdevice *) ui->ui_addr;

		if (cbase[uban] == 0) {
			dmz_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
				nclist * sizeof(struct cblock), 0);
			cbase[uban] = dmz_ubinfo[uban] & 0x3ffff;
		}

		for (octet = 0; octet < 3; octet++)
			if ((dmzact[controller] & (1 << octet)) != 0) {
				dmz_addr->octet[octet].octet_csr |= DMZ_IE;
				dmz_addr->octet[octet].octet_receive.octet_sato = SILO_TIMEOUT;
			}

		unit = controller * 24;

		/*
		 * If a unit is open or waiting for open to complete,
		 * reset it.
		 */
		for (i = 0; i < 24; i++) {
			dmz_softc[unit].dmz_state = 0;
			tp = &dmz_tty[unit];
			if (tp->t_state & (TS_ISOPEN | TS_WOPEN)) {
				dmzparam(unit);
				(void) dmzmctl(unit, DMZ_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dmzstart(tp);
			}
			unit++;
		}
	}
	return;
}

dmzread(device, uio)
	dev_t device;
	struct uio *uio;
{
	register struct tty *tp;
	int xstatus;

	tp = &dmz_tty[minor(device)];
	xstatus = (*linesw[tp->t_line].l_read)(tp, uio);
	return (xstatus);
}

dmzwrite(device, uio)
	dev_t device;
	struct uio *uio;
{
	register struct tty *tp;
	int xstatus;

	tp = &dmz_tty[minor(device)];
	xstatus = (*linesw[tp->t_line].l_write)(tp, uio);
	return (xstatus);
}

dmzrinta(controller)
	int controller;
{
	dmzrint(controller, 0);
}

dmzrintb(controller)
	int controller;
{
	dmzrint(controller, 1);
}

dmzrintc(controller)
	int controller;
{
	dmzrint(controller, 2);
}

dmzrint(controller, octet)
	int controller;
	register int octet;
{
	register struct tty *tp;
	register int character;
	register struct dmzdevice *dmz_addr;
	register struct tty *tp0;
	register int unit;
	register struct uba_device *ui;
	int overrun;

	overrun = 0;
	ui = dmzinfo[controller];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	dmz_addr = (struct dmzdevice *) ui->ui_addr;
	tp0 = &dmz_tty[controller * 24];

	while ((character = dmz_addr->octet[octet].octet_receive.octet_rb) < 0) {
		unit = (character >> 8) & 07;	/* unit is bits 8-10 of rb */
		tp = tp0 + (octet * 8 + unit);

		if (character & DMZ_DSC &&
		    (dmzsoftCAR[controller] & (1 << (octet * 8 + unit))) == 0) {
			dmz_addr->octet[octet].octet_csr = DMZ_IE | IR_RMS | unit;
			if (dmz_addr->octet[octet].octet_rms & DMZ_CAR)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
			else if ((*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
				dmz_addr->octet[octet].octet_csr =
				   DMZ_IE | IR_LCTMR | unit;
				dmz_addr->octet[octet].octet_lctmr =
				   dmz_addr->octet[octet].octet_lctmr &
				   ((DMZ_OFF<<8) | 0xff);
			}
			continue;
		}

		if ((tp->t_state&TS_ISOPEN)==0) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if ((tp->t_state&TS_WOPEN) == 0)
#endif
				continue;
		}

		if (character & DMZ_PE) {
			if ((tp->t_flags & (EVENP | ODDP)) == EVENP ||
			    (tp->t_flags & (EVENP | ODDP)) == ODDP)
				continue;
		}

		if ((character & DMZ_DO) && overrun == 0) {
			log(LOG_WARNING, "dmz%d: silo overflow\n", controller);
			overrun = 1;
		}

		if (character & DMZ_FE) {
			if (tp->t_flags & RAW)
				character = 0;
			else
				character = tp->t_intrc;
		}

		(*linesw[tp->t_line].l_rint)(character, tp);
	}

	return;
}

dmzxinta(controller)
	int controller;
{
	dmzxint(controller, 0);
}

dmzxintb(controller)
	int controller;
{
	dmzxint(controller, 1);
}

dmzxintc(controller)
	int controller;
{
	dmzxint(controller, 2);
}

dmzxint(controller, octet)
	int controller;
	register int octet;
{
	register struct tty *tp;
	register struct dmzdevice *dmz_addr;
	register struct uba_device *ui;
	register int unit, t;
	int priority;

	ui = dmzinfo[controller];
	dmz_addr = (struct dmzdevice *)ui->ui_addr;

	priority = spl5();

	while ((t = dmz_addr->octet[octet].octet_csr) & DMZ_TRDY) {
		unit = controller * 24 + (octet * 8 + ((t>>8) & 07));
		tp = &dmz_tty[unit];
		tp->t_state &= ~TS_BUSY;

		if (t & DMZ_NXM)
			printf("dmz%d: NXM line %d\n", controller, 
				octet * 8 + (unit & 07));

		if (tp->t_state & TS_FLUSH) {
			tp->t_state &= ~TS_FLUSH;
			dmz_addr->octet[octet].octet_csr = 
				DMZ_IE | IR_LCTMR | (unit & 07);
			dmz_addr->octet[octet].octet_lctmr = 
				(dmz_addr->octet[octet].octet_lctmr | DMZ_TE);
		} else
			if (dmz_softc[unit].dmz_state & ST_DMA)
				ndflush(&tp->t_outq, dmz_softc[unit].dmz_count);
		dmz_softc[unit].dmz_state = 0;

		if (tp->t_line)
			(*linesw[tp->t_line].l_start)(tp);
		else
			dmzstart(tp);
	}

	splx(priority);
	return;
}

dmzstart(tp)
	register struct tty *tp;
{
	register struct dmzdevice *dmz_addr;
	register int unit, nch, room;
	int controller, octet;
	int priority, car, use_dma;
	register int i;
	register char *cp;

	unit = minor(tp->t_dev);
	controller = DMZ(unit);
	octet = OCTET(unit);
	dmz_addr = (struct dmzdevice *)tp->t_addr;

	priority = spl5();

	if (tp->t_state & (TS_TIMEOUT | TS_BUSY | TS_TTSTOP))
		goto out;

	/*
	 * If the transmitter has been disabled, reenable it.
	 * If the transmitter was disabled before the xint (the
	 * ST_INBUSY was still on), then reset the BUSY state and
	 * we will wait for the interrupt.  If !TS_BUSY, we already
	 * saw the interrupt so we can start another transmission.
	 */
	if (dmz_softc[unit].dmz_state & ST_TXOFF) {
		dmz_addr->octet[octet].octet_csr = 
			DMZ_IE | IR_LCTMR | (unit & 07);
		dmz_addr->octet[octet].octet_lctmr = 
			(dmz_addr->octet[octet].octet_lctmr | DMZ_TE);
		dmz_softc[unit].dmz_state &= ~ST_TXOFF;
		if (dmz_softc[unit].dmz_state & ST_INBUSY) {
			dmz_softc[unit].dmz_state &= ~ST_INBUSY;
			tp->t_state |= TS_BUSY;
			goto out;
		}
	}

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

	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & (RAW | LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch & 0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}

	/*
	 * Should we use DMA or SILO mode?
	 * If nch is greater than DO_DMA_COUNT then DMA.
	 */
	if (nch) {
		dmz_addr->octet[octet].octet_csr = 
			DMZ_IE | IR_LCTMR | (unit & 07);
		dmz_addr->octet[octet].octet_lctmr = 
			(dmz_addr->octet[octet].octet_lctmr | DMZ_TE);
		tp->t_state |= TS_BUSY;

		use_dma = FALSE;
		room = DMZ_SIZ;

		if (nch > DO_DMA_COUNT)
			use_dma = TRUE;
		
		if (use_dma && dmz_dma_on) {
			car = UBACVT(tp->t_outq.c_cf, 
				dmzinfo[controller]->ui_ubanum);
			dmz_softc[unit].dmz_count = nch;
			dmz_softc[unit].dmz_state |= ST_DMA;
			dmz_addr->octet[octet].octet_csr = 
				DMZ_IE | IR_TBA | (unit & 07);
			dmz_addr->octet[octet].octet_tba = car;
			dmz_addr->octet[octet].octet_tcc = 
				((car >> 2) & 0xc000) | nch;
		} else {
			dmz_softc[unit].dmz_state &= ~ST_DMA;
			cp = tp->t_outq.c_cf;
			nch = MIN(nch, room);
			dmz_addr->octet[octet].octet_csr = 
				DMZ_IE | IR_TBUF | (unit & 07);
			for (i = 0; i < nch; i++)
				dmz_addr->octet[octet].octet_tbf = *cp++ ;
			ndflush(&tp->t_outq, nch);
		}
	}

out:
	splx(priority);
	return;
}

/* ARGSUSED */
dmzstop(tp, flag)
	register struct tty *tp;
{
	register struct dmzdevice *dmz_addr;
	register int unit, priority, octet;

	priority = spl5();
	dmz_addr = (struct dmzdevice *) tp->t_addr;
	unit = minor(tp->t_dev);
	octet = OCTET(unit);

	dmz_addr->octet[octet].octet_csr = IR_LCTMR | (unit & 07) | DMZ_IE;
	dmz_addr->octet[octet].octet_lctmr = 
		(dmz_addr->octet[octet].octet_lctmr & ~DMZ_TE);
	dmz_softc[unit].dmz_state |= ST_TXOFF;
	if ((tp->t_state & TS_TTSTOP) == 0) {
		tp->t_state |= (TS_FLUSH | TS_BUSY);
		dmz_addr->octet[octet].octet_lctmr = 
			(dmz_addr->octet[octet].octet_lctmr | DMZ_FLS);
	} else if (tp->t_state & TS_BUSY) {
		dmz_softc[unit].dmz_state |= ST_INBUSY;
		tp->t_state &= ~TS_BUSY;
	}

	splx(priority);
	return;
}

/* ARGSUSED */
dmzioctl(device, command, data, flag)
	dev_t device;
	caddr_t data;
{
	register struct tty *tp;
	register int unit;
	int error;

	unit = minor(device);
	tp = &dmz_tty[unit];

	error = (*linesw[tp->t_line].l_ioctl)(tp, command, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, command, data, flag);
	if (error >= 0) {
		if (command == TIOCSETP || command == TIOCSETN)
			dmzparam(unit);
		return (error);
	}

	switch (command) {
		case TIOCSBRK:
			(void) dmzmctl(device, DMZ_BRK, DMBIS);
			break;
		case TIOCCBRK:
			(void) dmzmctl(device, DMZ_BRK, DMBIC);
			break;
		case TIOCSDTR:
			(void) dmzmctl(device, DMZ_DTR | DMZ_RTS, DMBIS);
			break;
		case TIOCCDTR:
			(void) dmzmctl(device, DMZ_DTR | DMZ_RTS, DMBIC);
			break;
		case TIOCMSET:
			(void) dmzmctl(device, dmtodmz(*(int *)data), DMSET);
			break;
		case TIOCMBIS:
			(void) dmzmctl(device, dmtodmz(*(int *)data), DMBIS);
			break;
		case TIOCMBIC:
			(void) dmzmctl(device, dmtodmz(*(int *)data), DMBIC);
			break;
		case TIOCMGET:
			*(int *)data = dmztodm(dmzmctl(device, 0, DMGET));
			break;
		default:
			return (ENOTTY);
	}
	return (0);
}

dmzmctl(device, bits, how)
	dev_t device;
	int bits, how;
{
	register struct dmzdevice *dmz_addr;
	register int unit, modem_status, line_control;
	register int temp;
	int priority;
	int octet;

	unit = minor(device);
	octet = OCTET(unit);
	dmz_addr = (struct dmzdevice *) dmz_tty[unit].t_addr;

	priority = spl5();
	dmz_addr->octet[octet].octet_csr = DMZ_IE | IR_RMS | (unit & 07);
	modem_status = dmz_addr->octet[octet].octet_rms << 8;

	dmz_addr->octet[octet].octet_csr = DMZ_IE | IR_LCTMR | (unit & 07);
	temp = dmz_addr->octet[octet].octet_lctmr;
	modem_status |= ((temp>>8) & (0x1f));
	line_control = (temp & (0x3f));

	if (line_control & DMZ_RBK)
		modem_status |= DMZ_BRK;

	switch (how) {
		case DMSET:
			modem_status = (modem_status & 0xff00) | bits;
			break;
		case DMBIS:
			modem_status |= bits;
			break;
		case DMBIC:
			modem_status &= ~bits;
			break;
		case DMGET:
			(void) splx(priority);
			return (modem_status);
	}

	if (modem_status & DMZ_BRK)
		line_control |= DMZ_RBK;
	else
		line_control &= ~DMZ_RBK;
	modem_status &= ~DMZ_BRK;

	dmz_addr->octet[octet].octet_csr =
		DMZ_IE | IR_LCTMR | (unit & 07);
	dmz_addr->octet[octet].octet_lctmr =
		((modem_status & 0x1f) << 8) | (line_control & 0x3f);

	(void) splx(priority);
	return (modem_status);
}

/*
 * Routine to convert modem status from dm to dmz format.
 * Pull bits 1 & 3 through unchanged. If dm secondary transmit bit is set,
 * and/or dm request to send bit is set, and/or dm user modem signal bit
 * is set, set the corresponding dmz bits.
 */
dmtodmz(bits)
	register int bits;
{
	register int b;

	b = bits & 012;
	if (bits & DM_ST)
		b |= DMZ_RAT;
	if (bits & DM_RTS)
		b |= DMZ_RTS;
	if (bits & DM_USR)
		b |= DMZ_USW;
	return (b);
}

/*
 * Routine to convert modem status from dmz to dm format.  Pull bits 1 & 3
 * through unchanged. Pull bits 11 - 15 through as bits 4 - 8 and set bit
 * 0 to dm line enable.  If dmz user modem signal bit set, and/or  dmz
 * request to send bit set, then set the corresponding dm bit also.
 */
dmztodm(bits)
	register int bits;
{
	register int b;

	b = (bits & 012) | ((bits >> 7) & 0760) | DM_LE;
	if (bits & DMZ_USR)
		b |= DM_USR;
	if (bits & DMZ_RTS)
		b |= DM_RTS;
	return (b);
}
#endif
