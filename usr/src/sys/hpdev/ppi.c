/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ppi.c	7.1 (Berkeley) 5/8/90
 */

/*
 * Printer/Plotter HPIB interface
 */

#include "ppi.h"
#if NPPI > 0

#include "param.h"
#include "errno.h"
#include "uio.h"
#include "malloc.h"

#include "device.h"

int	ppiattach(), ppistart();
struct	driver ppidriver = {
	ppiattach, "ppi", ppistart,
};

struct	ppi_softc {
	int	sc_flags;
	struct	devqueue sc_dq;
	struct	hp_device *sc_hd;
} ppi_softc[NPPI];

/* sc_flags values */
#define	PPIF_ALIVE	0x1	
#define	PPIF_OPEN	0x2	

#define UNIT(x)		minor(x)

ppiattach(hd)
	register struct hp_device *hd;
{
	register struct ppi_softc *sc = &ppi_softc[hd->hp_unit];

	/*
	 * XXX: the printer/plotter doesn't seem to really return
	 * an ID but this will at least prevent us from mistaking
	 * a cs80 disk or tape for a ppi device.
	 */
	if (hpibid(hd->hp_ctlr, hd->hp_slave) & 0x200)
		return(0);
	sc->sc_flags = PPIF_ALIVE;
	sc->sc_dq.dq_ctlr = hd->hp_ctlr;
	sc->sc_dq.dq_unit = hd->hp_unit;
	sc->sc_dq.dq_slave = hd->hp_slave;
	sc->sc_dq.dq_driver = &ppidriver;
	sc->sc_hd = hd;
	return(1);
}

ppiopen(dev, flags)
	dev_t dev;
{
	register int unit = UNIT(dev);
	register struct ppi_softc *sc = &ppi_softc[unit];

	if (unit >= NPPI || (sc->sc_flags & PPIF_ALIVE) == 0)
		return(ENXIO);
	if (sc->sc_flags & PPIF_OPEN)
		return(EBUSY);
	sc->sc_flags |= PPIF_OPEN;
	return(0);
}

ppiclose(dev, flags)
	dev_t dev;
{
	register int unit = UNIT(dev);
	register struct ppi_softc *sc = &ppi_softc[unit];

	sc->sc_flags &= ~PPIF_OPEN;
	return(0);
}

ppistart(unit)
	register int unit;
{
	wakeup(&ppi_softc[unit]);
}

ppiread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (ppirw(dev, uio, UIO_READ));
}

ppiwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (ppirw(dev, uio, UIO_WRITE));
}

ppirw(dev, uio, rw)
	dev_t dev;
	register struct uio *uio;
	enum uio_rw rw;
{
	register struct ppi_softc *sc = &ppi_softc[UNIT(dev)];
	register int s, len, cnt;
	register char *cp;
	int error = 0;

	len = MIN(CLBYTES, uio->uio_resid);
	cp = (char *)malloc(len, M_TEMP, M_WAITOK);
	while (uio->uio_resid > 0) {
		len = MIN(CLBYTES, uio->uio_resid);
		if (rw == UIO_WRITE) {
			error = uiomove(cp, len, uio);
			if (error)
				break;
		}
		s = splbio();
		if (hpibreq(&sc->sc_dq) == 0)
			sleep(sc, PRIBIO + 1);
		splx(s);
		if (rw == UIO_WRITE)
			cnt = hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				-1, cp, len);
		else
			cnt = hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				-1, cp, len);
		s = splbio();
		hpibfree(&sc->sc_dq);
		splx(s);
		if (rw == UIO_READ) {
			error = uiomove(cp, cnt, uio);
			if (error)
				break;
		}
		if (cnt != len) {
			if (rw == UIO_WRITE)
				uio->uio_resid += len - cnt;
			break;
		}
	}
	free(cp, M_TEMP);
	return (error);
}
#endif
