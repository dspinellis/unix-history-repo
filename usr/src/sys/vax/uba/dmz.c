/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmz.c	7.4 (Berkeley) %G%
 */

/*
 * DMZ-32 driver
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
#include "dmzreg.h"
#include "dmreg.h"

extern	int dmx_timeout;		/* silo timeout, in ms */
extern	char dmx_speeds[];
int	dmzstart();

/*
 * The clist space is mapped by one terminal driver onto each UNIBUS.
 * The identity of the board which allocated resources is recorded,
 * so the process may be repeated after UNIBUS resets.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dmz_uballoc[NUBA];	/* which dmz (if any) allocated unibus map */
int	cbase[NUBA];		/* base address of clists in unibus map */

/*
 * Autoconfiguration and variables for DMZ32
 */
int dmzprobe(), dmzattach();
struct uba_device *dmzinfo[NDMZ];
u_short dmzstd[] = { 0 };
struct uba_driver dmzdriver = {
	dmzprobe, 0, dmzattach, 0, dmzstd, "dmz", dmzinfo
};

struct	tty dmz_tty[NDMZ*24];
struct	dmx_softc dmz_softc[3 * NDMZ];
#ifndef lint
int	ndmz = NDMZ*24;			/* used by iostat */
#endif

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
	register struct uba_device *ui;
{
	register struct dmx_softc *sc;
	register int i;

	sc = &dmz_softc[3 * ui->ui_unit];
	for (i = 0; i < 3; i++, sc++) {
		sc->dmx_type = 'z';
		sc->dmx_unit = ui->ui_unit;
		sc->dmx_unit0 = 8 * i;
		sc->dmx_ubanum = ui->ui_ubanum;
		sc->dmx_softCAR = (ui->ui_flags >> (8 * i)) & 0xff;
		sc->dmx_tty = &dmz_tty[((ui->ui_unit * 3) + i) * 8];
		sc->dmx_octet = (struct dmx_octet *)
		    &((struct dmzdevice *)ui->ui_addr)->dmz_octet[i];
	}

	cbase[ui->ui_ubanum] = -1;
	dmz_uballoc[ui->ui_ubanum] = -1;
}

/*
 * Open a DMF32 line, mapping the clist onto the uba if this
 * is the first dmf on this uba.  Turn on this dmf if this is
 * the first use of it.
 */
/*ARGSUSED*/
dmzopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	struct dmx_softc *sc;
	int unit, dmz;
	struct uba_device *ui;
	int s;

	unit = minor(dev);
	dmz = DMZ(unit);
	if (unit >= NDMZ*24 || (ui = dmzinfo[dmz])== 0 || ui->ui_alive == 0)
		return (ENXIO);

	tp = &dmz_tty[unit];
	sc = &dmz_softc[unit / 8];
	tp->t_addr = (caddr_t)sc->dmx_octet;
	tp->t_oproc = dmzstart;
	tp->t_dev = dev;			/* needed before dmxopen */

	/*
	 * While setting up state for this uba,
	 * block uba resets which can clear the state.
	 */
	s = spl6();
	if (cbase[ui->ui_ubanum] == -1) {
		dmz_uballoc[ui->ui_ubanum] = dmz;
		cbase[ui->ui_ubanum] = UBAI_ADDR(uballoc(ui->ui_ubanum,
		    (caddr_t)cfree, nclist*sizeof(struct cblock), 0));
	}
	splx(s);

	return (dmxopen(tp, sc));
}

/*
 * Close a DMZ32 line.
 */
/*ARGSUSED*/
dmzclose(dev, flag)
	dev_t dev;
	int flag;
{

	dmxclose(&dmz_tty[minor(dev)]);
}

dmzread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dmz_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

dmzwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dmz_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * DMZ32 receiver interrupts.
 */
dmzrinta(dmz)
	int dmz;
{
	struct uba_device *ui;

	ui = dmzinfo[dmz];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	dmxrint(&dmz_softc[3 * dmz + 0]);
}

dmzrintb(dmz)
	int dmz;
{
	struct uba_device *ui;

	ui = dmzinfo[dmz];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	dmxrint(&dmz_softc[3 * dmz + 1]);
}

dmzrintc(dmz)
	int dmz;
{
	struct uba_device *ui;

	ui = dmzinfo[dmz];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	dmxrint(&dmz_softc[3 * dmz + 2]);
}

/*
 * Ioctl for DMZ32.
 */
dmzioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	int unit = minor(dev);
 
	return (dmxioctl(&dmz_tty[unit], cmd, data, flag));
}

/*
 * DMZ32 transmitter interrupts.
 */
dmzxinta(dmz)
	int dmz;
{

	dmxxint(&dmz_softc[3 * dmz + 0]);
}

dmzxintb(dmz)
	int dmz;
{

	dmxxint(&dmz_softc[3 * dmz + 1]);
}

dmzxintc(dmz)
	int dmz;
{

	dmxxint(&dmz_softc[3 * dmz + 2]);
}

/*
 * Start (restart) transmission on the given line.
 */
dmzstart(tp)
	struct tty *tp;
{

	dmxstart(tp, &dmz_softc[minor(tp->t_dev) >> 3]);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
dmzstop(tp, flag)
	struct tty *tp;
{

	dmxstop(tp, &dmz_softc[minor(tp->t_dev) >> 3], flag);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset the csr, lpr, and lcr registers on open lines, and
 * restart transmitters.
 */
dmzreset(uban)
	int uban;
{
	register int dmz;
	register struct tty *tp;
	register struct uba_device *ui;
	register struct dmzdevice *addr;
	int i;

	for (dmz = 0; dmz < NDMZ; dmz++) {
		ui = dmzinfo[dmz];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf("dmz%d ", dmz);
		addr = (struct dmzdevice *)ui->ui_addr;

		if (dmz_uballoc[uban] == dmz) {
			int info;

			info = uballoc(uban, (caddr_t)cfree,
			    nclist * sizeof(struct cblock), UBA_CANTWAIT);
			if (info)
				cbase[uban] = UBAI_ADDR(info);
			else {
				printf(" [can't get uba map]");
				cbase[uban] = -1;
			}
		}

		for (i = 0; i < 3; i++)
			if (dmz_softc[3 * dmz + i].dmx_flags & DMX_ACTIVE) {
				addr->dmz_octet[i].csr = DMF_IE;
				addr->dmz_octet[i].rsp = dmx_timeout;
			}

		/*
		 * If a unit is open or waiting for open to complete,
		 * reset it.
		 */
		tp = &dmz_tty[dmz * 24];
		for (i = 0; i < 24; i++, tp++) {
			if (tp->t_state & (TS_ISOPEN | TS_WOPEN)) {
				dmxparam(tp);
				(void) dmxmctl(tp, DMF_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dmzstart(tp);
			}
		}
	}
}
#endif
