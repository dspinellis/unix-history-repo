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
 *	@(#)fhpib.c	7.1 (Berkeley) 5/8/90
 */

/*
 * 98625A/B HPIB driver
 */
#include "hpib.h"
#if NHPIB > 0

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "device.h"
#include "fhpibreg.h"
#include "hpibvar.h"
#include "dmavar.h"

/*
 * Inline version of fhpibwait to be used in places where
 * we don't worry about getting hung.
 */
#define	FHPIBWAIT(hd, m)	while (((hd)->hpib_intr & (m)) == 0)

#ifdef DEBUG
int	fhpibdebugunit = -1;
int	fhpibdebug = 0;
#define FDB_FAIL	0x01
#define FDB_DMA		0x02
#define FDB_WAIT	0x04
#define FDB_PPOLL	0x08

int	dopriodma = 0;	/* use high priority DMA */
int	doworddma = 1;	/* non-zero if we should attempt word dma */
int	dolworddma = 1;	/* use longword dma (scsi) */
int	doppollint = 1;	/* use ppoll interrupts instead of watchdog */

long	fhpibbadint[2] = { 0 };
long	fhpibtransfer[NHPIB] = { 0 };
long	fhpibnondma[NHPIB] = { 0 };
long	fhpibworddma[NHPIB] = { 0 };
long	fhpibremain[NHPIB] = { 0 };
long	fhpibloops[NHPIB] = { 0 };

#endif

int	fhpibcmd[NHPIB];
int	dmathresh = 3;	/* char count beyond which we will attempt dma */

extern	int hpibtimeout;

fhpibtype(hc)
	register struct hp_ctlr *hc;
{
	register struct hpib_softc *hs = &hpib_softc[hc->hp_unit];
	register struct fhpibdevice *hd = (struct fhpibdevice *)hc->hp_addr;

	if (hd->hpib_cid != HPIBC)
		return(0);
	hs->sc_type = HPIBC;
	hs->sc_ba = HPIBC_BA;
	hc->hp_ipl = HPIB_IPL(hd->hpib_ids);
	return(1);
}

fhpibreset(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;

	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_cid = 0xFF;
	DELAY(100);
	hd->hpib_cmd = CT_8BIT;
	hd->hpib_ar = AR_ARONC;
	fhpibifc(hd);
	hd->hpib_ie = IDS_IE;
	hd->hpib_data = C_DCL;
	DELAY(100000);
	/*
	 * See if we can do word dma.
	 * If so, we should be able to write and read back the appropos bit.
	 */
	hd->hpib_ie |= IDS_WDMA;
	if (hd->hpib_ie & IDS_WDMA) {
		hd->hpib_ie &= ~IDS_WDMA;
		hs->sc_flags |= HPIBF_DMA16;
#ifdef DEBUG
		if (fhpibdebug & FDB_DMA)
			printf("fhpibtype: unit %d has word dma\n", unit);

#endif
	}
}

fhpibifc(hd)
	register struct fhpibdevice *hd;
{
	hd->hpib_cmd |= CT_IFC;
	hd->hpib_cmd |= CT_INITFIFO;
	DELAY(100);
	hd->hpib_cmd &= ~CT_IFC;
	hd->hpib_cmd |= CT_REN;
	hd->hpib_stat = ST_ATN;
}

fhpibsend(unit, slave, sec, addr, cnt)
	register char *addr;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register int timo;
	int origcnt = cnt;
	int err = 0;

	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_stat = 0;
	hd->hpib_imask = IM_IDLE | IM_ROOM;
	if (fhpibwait(hd, IM_IDLE) < 0)
		err++;
	hd->hpib_stat = ST_ATN;
	hd->hpib_data = C_UNL;
	hd->hpib_data = C_TAG + hs->sc_ba;
	hd->hpib_data = C_LAG + slave;
	if (sec != -1)
		hd->hpib_data = C_SCG + sec;
	if (fhpibwait(hd, IM_IDLE) < 0)
		err++;
	hd->hpib_stat = ST_WRITE;
	if (cnt && !err) {
		while (--cnt) {
			hd->hpib_data = *addr++;
			timo = hpibtimeout;
			while ((hd->hpib_intr & IM_ROOM) == 0)
				if (--timo == 0) {
					err++;
					goto out;
				}
		}
		hd->hpib_stat = ST_EOI;
		hd->hpib_data = *addr;
		FHPIBWAIT(hd, IM_ROOM);
		hd->hpib_stat = ST_ATN;
		/* XXX: HP-UX claims bug with CS80 transparent messages */
		if (sec == 0x12)
			DELAY(150);
		hd->hpib_data = C_UNL;
		if (fhpibwait(hd, IM_IDLE) < 0)
			err++;
	}
out:
	if (err) {
		cnt++;
		fhpibifc(hd);
#ifdef DEBUG
		if (fhpibdebug & FDB_FAIL) {
			printf("hpib%d: fhpibsend failed: slave %d, sec %x, ",
			       unit, slave, sec);
			printf("sent %d of %d bytes\n", origcnt-cnt, origcnt);
		}
#endif
	}
	hd->hpib_imask = 0;
	return(origcnt - cnt);
}

fhpibrecv(unit, slave, sec, addr, cnt)
	register char *addr;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register int timo;
	int origcnt = cnt;
	int err = 0;

	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_stat = 0;
	hd->hpib_imask = IM_IDLE | IM_ROOM | IM_BYTE;
	if (fhpibwait(hd, IM_IDLE) < 0)
		err++;
	hd->hpib_stat = ST_ATN;
	hd->hpib_data = C_UNL;
	hd->hpib_data = C_LAG + hs->sc_ba;
	hd->hpib_data = C_TAG + slave;
	if (sec != -1)
		hd->hpib_data = C_SCG + sec;
	if (fhpibwait(hd, IM_IDLE) < 0)
		err++;
	hd->hpib_stat = ST_READ0;
	hd->hpib_data = 0;
	if (cnt && !err) {
		do {
			timo = hpibtimeout;
			while ((hd->hpib_intr & IM_BYTE) == 0)
				if (--timo == 0) {
					err++;
					goto out;
				}
			*addr++ = hd->hpib_data;
		} while (--cnt);
out:
		FHPIBWAIT(hd, IM_ROOM);
		hd->hpib_stat = ST_ATN;
		hd->hpib_data = (slave == 31) ? C_UNA : C_UNT;
		if (fhpibwait(hd, IM_IDLE) < 0)
			err++;
	}
	if (err) {
		if (!cnt)
			cnt++;
		fhpibifc(hd);
#ifdef DEBUG
		if (fhpibdebug & FDB_FAIL) {
			printf("hpib%d: fhpibrecv failed: slave %d, sec %x, ",
			       unit, slave, sec);
			printf("got %d of %d bytes\n", origcnt-cnt, origcnt);
		}
#endif
	}
	hd->hpib_imask = 0;
	return(origcnt - cnt);
}

fhpibgo(unit, slave, sec, addr, count, rw)
	register int unit;
	char *addr;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register int i;
	int flags = 0;

#ifdef lint
	i = unit; if (i) return;
#endif
	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	hs->sc_flags |= HPIBF_IO;
	if (rw == B_READ)
		hs->sc_flags |= HPIBF_READ;
#ifdef DEBUG
	else if (hs->sc_flags & HPIBF_READ) {
		printf("fhpibgo: HPIBF_READ still set\n");
		hs->sc_flags &= ~HPIBF_READ;
	}
#endif
	hs->sc_count = count;
	hs->sc_addr = addr;
#ifdef DEBUG
	fhpibtransfer[unit]++;
#endif
	if ((hs->sc_flags & HPIBF_DMA16) &&
	    ((int)addr & 1) == 0 && count && (count & 1) == 0
#ifdef DEBUG
	    && doworddma
#endif
	    ) {
#ifdef DEBUG
		fhpibworddma[unit]++;
#endif
		flags |= DMAGO_WORD;
		hd->hpib_latch = 0;
	}
#ifdef DEBUG
	if (dopriodma)
		flags |= DMAGO_PRI;
#endif
	if (hs->sc_flags & HPIBF_READ) {
		fhpibcmd[unit] = CT_REN | CT_8BIT;
		hs->sc_curcnt = count;
		dmago(hs->sc_dq.dq_ctlr, addr, count, flags|DMAGO_READ);
		if (fhpibrecv(unit, slave, sec, 0, 0) < 0) {
#ifdef DEBUG
			printf("fhpibgo: recv failed, retrying...\n");
#endif
			(void) fhpibrecv(unit, slave, sec, 0, 0);
		}
		i = hd->hpib_cmd;
		hd->hpib_cmd = fhpibcmd[unit];
		hd->hpib_ie = IDS_DMA(hs->sc_dq.dq_ctlr) |
			((flags & DMAGO_WORD) ? IDS_WDMA : 0);
		return;
	}
	fhpibcmd[unit] = CT_REN | CT_8BIT | CT_FIFOSEL;
	if (count < dmathresh) {
#ifdef DEBUG
		fhpibnondma[unit]++;
		fhpibworddma[unit]--;
#endif
		hs->sc_curcnt = count;
		(void) fhpibsend(unit, slave, sec, addr, count);
		fhpibdone(unit);
		return;
	}
	count -= (flags & DMAGO_WORD) ? 2 : 1;
	hs->sc_curcnt = count;
	dmago(hs->sc_dq.dq_ctlr, addr, count, flags);
	if (fhpibsend(unit, slave, sec, 0, 0) < 0) {
#ifdef DEBUG
		printf("fhpibgo: send failed, retrying...\n");
#endif
		(void) fhpibsend(unit, slave, sec, 0, 0);
	}
	i = hd->hpib_cmd;
	hd->hpib_cmd = fhpibcmd[unit];
	hd->hpib_ie = IDS_DMA(hs->sc_dq.dq_ctlr) | IDS_WRITE |
		((flags & DMAGO_WORD) ? IDS_WDMA : 0);
}

fhpibdone(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register char *addr;
	register int cnt;

	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	cnt = hs->sc_curcnt;
	hs->sc_addr += cnt;
	hs->sc_count -= cnt;
#ifdef DEBUG
	if ((fhpibdebug & FDB_DMA) && fhpibdebugunit == unit)
		printf("fhpibdone: addr %x cnt %d\n",
		       hs->sc_addr, hs->sc_count);
#endif
	if (hs->sc_flags & HPIBF_READ)
		hd->hpib_imask = IM_IDLE | IM_BYTE;
	else {
		cnt = hs->sc_count;
		if (cnt) {
			addr = hs->sc_addr;
			hd->hpib_imask = IM_IDLE | IM_ROOM;
#ifdef DEBUG
			fhpibremain[unit]++;
			while ((hd->hpib_intr & IM_IDLE) == 0)
				fhpibloops[unit]++;
#else
			FHPIBWAIT(hd, IM_IDLE);
#endif
			hd->hpib_stat = ST_WRITE;
			while (--cnt) {
				hd->hpib_data = *addr++;
				FHPIBWAIT(hd, IM_ROOM);
			}
			hd->hpib_stat = ST_EOI;
			hd->hpib_data = *addr;
		}
		hd->hpib_imask = IM_IDLE;
	}
	hs->sc_flags |= HPIBF_DONE;
	hd->hpib_stat = ST_IENAB;
	hd->hpib_ie = IDS_IE;
}

fhpibintr(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register struct devqueue *dq;
	register int stat0;

	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	stat0 = hd->hpib_ids;
	if ((stat0 & (IDS_IE|IDS_IR)) != (IDS_IE|IDS_IR)) {
#ifdef DEBUG
		if ((fhpibdebug & FDB_FAIL) && (stat0 & IDS_IR) &&
		    (hs->sc_flags & (HPIBF_IO|HPIBF_DONE)) != HPIBF_IO)
			printf("hpib%d: fhpibintr: bad status %x\n",
			       unit, stat0);
		fhpibbadint[0]++;
#endif
		return(0);
	}
	if ((hs->sc_flags & (HPIBF_IO|HPIBF_DONE)) == HPIBF_IO) {
#ifdef DEBUG
		fhpibbadint[1]++;
#endif
		return(0);
	}
#ifdef DEBUG
	if ((fhpibdebug & FDB_DMA) && fhpibdebugunit == unit)
		printf("fhpibintr: flags %x\n", hs->sc_flags);
#endif
	dq = hs->sc_sq.dq_forw;
	if (hs->sc_flags & HPIBF_IO) {
		stat0 = hd->hpib_cmd;
		hd->hpib_cmd = fhpibcmd[unit] & ~CT_8BIT;
		hd->hpib_stat = 0;
		hd->hpib_cmd = CT_REN | CT_8BIT;
		stat0 = hd->hpib_intr;
		hd->hpib_imask = 0;
		hs->sc_flags &= ~(HPIBF_DONE|HPIBF_IO|HPIBF_READ);
		dmafree(&hs->sc_dq);
		(dq->dq_driver->d_intr)(dq->dq_unit);
	} else if (hs->sc_flags & HPIBF_PPOLL) {
		stat0 = hd->hpib_intr;
#ifdef DEBUG
		if ((fhpibdebug & FDB_FAIL) &&
		    doppollint && (stat0 & IM_PPRESP) == 0)
			printf("hpib%d: fhpibintr: bad intr reg %x\n",
			       unit, stat0);
#endif
		hd->hpib_stat = 0;
		hd->hpib_imask = 0;
#ifdef DEBUG
		stat0 = fhpibppoll(unit);
		if ((fhpibdebug & FDB_PPOLL) && unit == fhpibdebugunit)
			printf("fhpibintr: got PPOLL status %x\n", stat0);
		if ((stat0 & (0x80 >> dq->dq_slave)) == 0) {
			printf("fhpibintr: PPOLL: unit %d slave %d stat %x\n",
			       unit, dq->dq_slave, stat0);
			return(1);
		}
#endif
		hs->sc_flags &= ~HPIBF_PPOLL;
		(dq->dq_driver->d_intr)(dq->dq_unit);
	}
	return(1);
}

fhpibppoll(unit)
{
	register struct fhpibdevice *hd;
	register int ppoll;

	hd = (struct fhpibdevice *)hpib_softc[unit].sc_hc->hp_addr;
	hd->hpib_stat = 0;
	hd->hpib_psense = 0;
	hd->hpib_pmask = 0xFF;
	hd->hpib_imask = IM_PPRESP | IM_PABORT;
	DELAY(25);
	hd->hpib_intr = IM_PABORT;
	ppoll = hd->hpib_data;
	if (hd->hpib_intr & IM_PABORT)
		ppoll = 0;
	hd->hpib_imask = 0;
	hd->hpib_pmask = 0;
	hd->hpib_stat = ST_IENAB;
	return(ppoll);
}

fhpibwait(hd, x)
	register struct fhpibdevice *hd;
{
	register int timo = hpibtimeout;

	while ((hd->hpib_intr & x) == 0 && --timo)
		;
	if (timo == 0) {
#ifdef DEBUG
		if (fhpibdebug & FDB_FAIL)
			printf("fhpibwait(%x, %x) timeout\n", hd, x);
#endif
		return(-1);
	}
	return(0);
}

/*
 * XXX: this will have to change if we every allow more than one
 * pending operation per HP-IB.
 */
fhpibppwatch(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct fhpibdevice *hd;
	register int slave;

	if ((hs->sc_flags & HPIBF_PPOLL) == 0)
		return;
	hd = (struct fhpibdevice *)hs->sc_hc->hp_addr;
	slave = (0x80 >> hs->sc_sq.dq_forw->dq_slave);
#ifdef DEBUG
	if (!doppollint) {
		if (fhpibppoll(unit) & slave) {
			hd->hpib_stat = ST_IENAB;
			hd->hpib_imask = IM_IDLE | IM_ROOM;
		} else
			timeout(fhpibppwatch, unit, 1);
		return;
	}
	if ((fhpibdebug & FDB_PPOLL) && unit == fhpibdebugunit)
		printf("fhpibppwatch: sense request on %d\n", unit);
#endif
	hd->hpib_psense = ~slave;
	hd->hpib_pmask = slave;
	hd->hpib_stat = ST_IENAB;
	hd->hpib_imask = IM_PPRESP | IM_PABORT;
	hd->hpib_ie = IDS_IE;
}
#endif
