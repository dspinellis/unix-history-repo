/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hpib.c	8.2 (Berkeley) %G%
 */

/*
 * HPIB driver
 */
#include "hpib.h"
#if NHPIB > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>

#include <hp/dev/device.h>
#include <hp300/dev/hpibvar.h>
#include <hp300/dev/dmavar.h>

#include <machine/cpu.h>
#include <hp300/hp300/isr.h>

int	hpibinit(), hpibstart(), hpibgo(), hpibintr(), hpibdone();
struct	driver hpibdriver = {
	hpibinit, "hpib", hpibstart, hpibgo, hpibintr, hpibdone,
};

struct	hpib_softc hpib_softc[NHPIB];
struct	isr hpib_isr[NHPIB];
int	nhpibppoll(), fhpibppoll();

int	hpibtimeout = 100000;	/* # of status tests before we give up */
int	hpibidtimeout = 10000;	/* # of status tests for hpibid() calls */
int	hpibdmathresh = 3;	/* byte count beyond which to attempt dma */

hpibinit(hc)
	register struct hp_ctlr *hc;
{
	register struct hpib_softc *hs = &hpib_softc[hc->hp_unit];
	
	if (!nhpibtype(hc) && !fhpibtype(hc))
		return(0);
	hs->sc_hc = hc;
	hs->sc_dq.dq_unit = hc->hp_unit;
	hs->sc_dq.dq_driver = &hpibdriver;
	hs->sc_sq.dq_forw = hs->sc_sq.dq_back = &hs->sc_sq;
	hpib_isr[hc->hp_unit].isr_intr = hpibintr;
	hpib_isr[hc->hp_unit].isr_ipl = hc->hp_ipl;
	hpib_isr[hc->hp_unit].isr_arg = hc->hp_unit;
	isrlink(&hpib_isr[hc->hp_unit]);
	hpibreset(hc->hp_unit);
	return(1);
}

hpibreset(unit)
	register int unit;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		fhpibreset(unit);
	else
		nhpibreset(unit);
}

hpibreq(dq)
	register struct devqueue *dq;
{
	register struct devqueue *hq;

	hq = &hpib_softc[dq->dq_ctlr].sc_sq;
	insque(dq, hq->dq_back);
	if (dq->dq_back == hq)
		return(1);
	return(0);
}

hpibfree(dq)
	register struct devqueue *dq;
{
	register struct devqueue *hq;

	hq = &hpib_softc[dq->dq_ctlr].sc_sq;
	remque(dq);
	if ((dq = hq->dq_forw) != hq)
		(dq->dq_driver->d_start)(dq->dq_unit);
}

hpibid(unit, slave)
	int unit, slave;
{
	short id;
	int ohpibtimeout;

	/*
	 * XXX shorten timeout value so autoconfig doesn't
	 * take forever on slow CPUs.
	 */
	ohpibtimeout = hpibtimeout;
	hpibtimeout = hpibidtimeout * cpuspeed;
	if (hpibrecv(unit, 31, slave, &id, 2) != 2)
		id = 0;
	hpibtimeout = ohpibtimeout;
	return(id);
}

hpibsend(unit, slave, sec, addr, cnt)
	register int unit;
	int slave, sec, addr, cnt;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		return(fhpibsend(unit, slave, sec, addr, cnt));
	else
		return(nhpibsend(unit, slave, sec, addr, cnt));
}

hpibrecv(unit, slave, sec, addr, cnt)
	register int unit;
	int slave, sec, addr, cnt;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		return(fhpibrecv(unit, slave, sec, addr, cnt));
	else
		return(nhpibrecv(unit, slave, sec, addr, cnt));
}

hpibpptest(unit, slave)
	register int unit;
	int slave;
{
	int (*ppoll)();

	ppoll = (hpib_softc[unit].sc_type == HPIBC) ? fhpibppoll : nhpibppoll;
	return((*ppoll)(unit) & (0x80 >> slave));
}

hpibawait(unit)
	int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];

	hs->sc_flags |= HPIBF_PPOLL;
	if (hs->sc_type == HPIBC)
		fhpibppwatch((void *)unit);
	else
		nhpibppwatch((void *)unit);
}

hpibswait(unit, slave)
	register int unit;
	int slave;
{
	register int timo = hpibtimeout;
	register int mask, (*ppoll)();

	ppoll = (hpib_softc[unit].sc_type == HPIBC) ? fhpibppoll : nhpibppoll;
	mask = 0x80 >> slave;
	while (((ppoll)(unit) & mask) == 0)
		if (--timo == 0) {
			printf("hpib%d: swait timeout\n", unit);
			return(-1);
		}
	return(0);
}

hpibustart(unit)
	int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];

	if (hs->sc_type == HPIBA)
		hs->sc_dq.dq_ctlr = DMA0;
	else
		hs->sc_dq.dq_ctlr = DMA0 | DMA1;
	if (dmareq(&hs->sc_dq))
		return(1);
	return(0);
}

hpibstart(unit)
	int unit;
{
	register struct devqueue *dq;
	
	dq = hpib_softc[unit].sc_sq.dq_forw;
	(dq->dq_driver->d_go)(dq->dq_unit);
}

hpibgo(unit, slave, sec, addr, count, rw)
	register int unit;
	int slave, sec, addr, count, rw;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		fhpibgo(unit, slave, sec, addr, count, rw);
	else
		nhpibgo(unit, slave, sec, addr, count, rw);
}

hpibdone(unit)
	register int unit;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		fhpibdone(unit);
	else
		nhpibdone(unit);
}

hpibintr(unit)
	register int unit;
{
	int found;

	if (hpib_softc[unit].sc_type == HPIBC)
		found = fhpibintr(unit);
	else
		found = nhpibintr(unit);
	return(found);
}
#endif
