/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)ppi.c	8.1 (Berkeley) 6/16/93
 */

/*
 * Printer/Plotter HPIB interface
 */

#include "ppi.h"
#if NPPI > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/errno.h>
#include <sys/uio.h>
#include <sys/malloc.h>

#include <hp/dev/device.h>
#include <hp300/dev/ppiioctl.h>

int	ppiattach(), ppistart();
void	ppitimo();
struct	driver ppidriver = {
	ppiattach, "ppi", ppistart,
};

struct	ppi_softc {
	int	sc_flags;
	struct	devqueue sc_dq;
	struct	hp_device *sc_hd;
	struct	ppiparam sc_param;
#define sc_burst sc_param.burst
#define sc_timo  sc_param.timo
#define sc_delay sc_param.delay
	int	sc_sec;
} ppi_softc[NPPI];

/* sc_flags values */
#define	PPIF_ALIVE	0x01	
#define	PPIF_OPEN	0x02	
#define PPIF_UIO	0x04
#define PPIF_TIMO	0x08
#define PPIF_DELAY	0x10

#define UNIT(x)		minor(x)

#ifdef DEBUG
int	ppidebug = 0x80;
#define PDB_FOLLOW	0x01
#define PDB_IO		0x02
#define PDB_NOCHECK	0x80
#endif

ppiattach(hd)
	register struct hp_device *hd;
{
	register struct ppi_softc *sc = &ppi_softc[hd->hp_unit];

#ifdef DEBUG
	if ((ppidebug & PDB_NOCHECK) == 0)
#endif
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
#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppiopen(%x, %x): flags %x\n",
		       dev, flags, sc->sc_flags);
#endif
	if (sc->sc_flags & PPIF_OPEN)
		return(EBUSY);
	sc->sc_flags |= PPIF_OPEN;
	sc->sc_burst = PPI_BURST;
	sc->sc_timo = ppimstohz(PPI_TIMO);
	sc->sc_delay = ppimstohz(PPI_DELAY);
	sc->sc_sec = -1;
	return(0);
}

ppiclose(dev, flags)
	dev_t dev;
{
	register int unit = UNIT(dev);
	register struct ppi_softc *sc = &ppi_softc[unit];

#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppiclose(%x, %x): flags %x\n",
		       dev, flags, sc->sc_flags);
#endif
	sc->sc_flags &= ~PPIF_OPEN;
	return(0);
}

ppistart(unit)
	int unit;
{
#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppistart(%x)\n", unit);
#endif
	ppi_softc[unit].sc_flags &= ~PPIF_DELAY;
	wakeup(&ppi_softc[unit]);
	return (0);
}

void
ppitimo(unit)
	int unit;
{
#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppitimo(%x)\n", unit);
#endif
	ppi_softc[unit].sc_flags &= ~(PPIF_UIO|PPIF_TIMO);
	wakeup(&ppi_softc[unit]);
}

ppiread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppiread(%x, %x)\n", dev, uio);
#endif
	return (ppirw(dev, uio));
}

ppiwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

#ifdef DEBUG
	if (ppidebug & PDB_FOLLOW)
		printf("ppiwrite(%x, %x)\n", dev, uio);
#endif
	return (ppirw(dev, uio));
}

ppirw(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	int unit = UNIT(dev);
	register struct ppi_softc *sc = &ppi_softc[unit];
	register int s, len, cnt;
	register char *cp;
	int error = 0, gotdata = 0;
	int buflen;
	char *buf;

	if (uio->uio_resid == 0)
		return(0);

#ifdef DEBUG
	if (ppidebug & (PDB_FOLLOW|PDB_IO))
		printf("ppirw(%x, %x, %c): burst %d, timo %d, resid %x\n",
		       dev, uio, uio->uio_rw == UIO_READ ? 'R' : 'W',
		       sc->sc_burst, sc->sc_timo, uio->uio_resid);
#endif
	buflen = min(sc->sc_burst, uio->uio_resid);
	buf = (char *)malloc(buflen, M_DEVBUF, M_WAITOK);
	sc->sc_flags |= PPIF_UIO;
	if (sc->sc_timo > 0) {
		sc->sc_flags |= PPIF_TIMO;
		timeout(ppitimo, (void *)unit, sc->sc_timo);
	}
	while (uio->uio_resid > 0) {
		len = min(buflen, uio->uio_resid);
		cp = buf;
		if (uio->uio_rw == UIO_WRITE) {
			error = uiomove(cp, len, uio);
			if (error)
				break;
		}
again:
		s = splbio();
		if ((sc->sc_flags & PPIF_UIO) && hpibreq(&sc->sc_dq) == 0)
			sleep(sc, PRIBIO+1);
		/*
		 * Check if we timed out during sleep or uiomove
		 */
		(void) splsoftclock();
		if ((sc->sc_flags & PPIF_UIO) == 0) {
#ifdef DEBUG
			if (ppidebug & PDB_IO)
				printf("ppirw: uiomove/sleep timo, flags %x\n",
				       sc->sc_flags);
#endif
			if (sc->sc_flags & PPIF_TIMO) {
				untimeout(ppitimo, (void *)unit);
				sc->sc_flags &= ~PPIF_TIMO;
			}
			splx(s);
			break;
		}
		splx(s);
		/*
		 * Perform the operation
		 */
		if (uio->uio_rw == UIO_WRITE)
			cnt = hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				       sc->sc_sec, cp, len);
		else
			cnt = hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				       sc->sc_sec, cp, len);
		s = splbio();
		hpibfree(&sc->sc_dq);
#ifdef DEBUG
		if (ppidebug & PDB_IO)
			printf("ppirw: %s(%d, %d, %x, %x, %d) -> %d\n",
			       uio->uio_rw == UIO_READ ? "recv" : "send",
			       sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
			       sc->sc_sec, cp, len, cnt);
#endif
		splx(s);
		if (uio->uio_rw == UIO_READ) {
			if (cnt) {
				error = uiomove(cp, cnt, uio);
				if (error)
					break;
				gotdata++;
			}
			/*
			 * Didn't get anything this time, but did in the past.
			 * Consider us done.
			 */
			else if (gotdata)
				break;
		}
		s = splsoftclock();
		/*
		 * Operation timeout (or non-blocking), quit now.
		 */
		if ((sc->sc_flags & PPIF_UIO) == 0) {
#ifdef DEBUG
			if (ppidebug & PDB_IO)
				printf("ppirw: timeout/done\n");
#endif
			splx(s);
			break;
		}
		/*
		 * Implement inter-read delay
		 */
		if (sc->sc_delay > 0) {
			sc->sc_flags |= PPIF_DELAY;
			timeout((void (*)__P((void *)))ppistart, (void *)unit,
			    sc->sc_delay);
			error = tsleep(sc, PCATCH|PZERO+1, "hpib", 0);
			if (error) {
				splx(s);
				break;
			}
		}
		splx(s);
		/*
		 * Must not call uiomove again til we've used all data
		 * that we already grabbed.
		 */
		if (uio->uio_rw == UIO_WRITE && cnt != len) {
			cp += cnt;
			len -= cnt;
			cnt = 0;
			goto again;
		}
	}
	s = splsoftclock();
	if (sc->sc_flags & PPIF_TIMO) {
		untimeout(ppitimo, (void *)unit);
		sc->sc_flags &= ~PPIF_TIMO;
	}
	if (sc->sc_flags & PPIF_DELAY) {
		untimeout((void (*)__P((void *)))ppistart, (void *)unit);
		sc->sc_flags &= ~PPIF_DELAY;
	}
	splx(s);
	/*
	 * Adjust for those chars that we uiomove'ed but never wrote
	 */
	if (uio->uio_rw == UIO_WRITE && cnt != len) {
		uio->uio_resid += (len - cnt);
#ifdef DEBUG
		if (ppidebug & PDB_IO)
			printf("ppirw: short write, adjust by %d\n",
			       len-cnt);
#endif
	}
	free(buf, M_DEVBUF);
#ifdef DEBUG
	if (ppidebug & (PDB_FOLLOW|PDB_IO))
		printf("ppirw: return %d, resid %d\n", error, uio->uio_resid);
#endif
	return (error);
}

ppiioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	struct ppi_softc *sc = &ppi_softc[UNIT(dev)];
	struct ppiparam *pp, *upp;
	int error = 0;

	switch (cmd) {
	case PPIIOCGPARAM:
		pp = &sc->sc_param;
		upp = (struct ppiparam *)data;
		upp->burst = pp->burst;
		upp->timo = ppihztoms(pp->timo);
		upp->delay = ppihztoms(pp->delay);
		break;
	case PPIIOCSPARAM:
		pp = &sc->sc_param;
		upp = (struct ppiparam *)data;
		if (upp->burst < PPI_BURST_MIN || upp->burst > PPI_BURST_MAX ||
		    upp->delay < PPI_DELAY_MIN || upp->delay > PPI_DELAY_MAX)
			return(EINVAL);
		pp->burst = upp->burst;
		pp->timo = ppimstohz(upp->timo);
		pp->delay = ppimstohz(upp->delay);
		break;
	case PPIIOCSSEC:
		sc->sc_sec = *(int *)data;
		break;
	default:
		return(EINVAL);
	}
	return (error);
}

ppihztoms(h)
	int h;
{
	extern int hz;
	register int m = h;

	if (m > 0)
		m = m * 1000 / hz;
	return(m);
}

ppimstohz(m)
	int m;
{
	extern int hz;
	register int h = m;

	if (h > 0) {
		h = h * hz / 1000;
		if (h == 0)
			h = 1000 / hz;
	}
	return(h);
}
#endif
