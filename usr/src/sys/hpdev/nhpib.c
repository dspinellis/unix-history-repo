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
 *	@(#)nhpib.c	7.1 (Berkeley) 5/8/90
 */

/*
 * Internal/98624 HPIB driver
 */
#include "hpib.h"
#if NHPIB > 0

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "device.h"
#include "nhpibreg.h"
#include "hpibvar.h"
#include "dmavar.h"

nhpibtype(hc)
	register struct hp_ctlr *hc;
{
	register struct hpib_softc *hs = &hpib_softc[hc->hp_unit];
	register struct nhpibdevice *hd = (struct nhpibdevice *)hc->hp_addr;

	if ((int)hc->hp_addr == internalhpib) {
		hs->sc_type = HPIBA;
		hs->sc_ba = HPIBA_BA;
		hc->hp_ipl = HPIBA_IPL;
	}
	else if (hd->hpib_cid == HPIBB) {
		hs->sc_type = HPIBB;
		hs->sc_ba = hd->hpib_csa & CSA_BA;
		hc->hp_ipl = HPIB_IPL(hd->hpib_ids);
	}
	else
		return(0);
	return(1);
}

nhpibreset(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_acr = AUX_SSWRST;
	hd->hpib_ar = hs->sc_ba;
	hd->hpib_lim = LIS_ERR;
	hd->hpib_mim = 0;
	hd->hpib_acr = AUX_CDAI;
	hd->hpib_acr = AUX_CSHDW;
	hd->hpib_acr = AUX_SSTD1;
	hd->hpib_acr = AUX_SVSTD1;
	hd->hpib_acr = AUX_CPP;
	hd->hpib_acr = AUX_CHDFA;
	hd->hpib_acr = AUX_CHDFE;
	hd->hpib_acr = AUX_RHDF;
	hd->hpib_acr = AUX_CSWRST;
	nhpibifc(hd);
	hd->hpib_ie = IDS_IE;
	hd->hpib_data = C_DCL;
	DELAY(100000);
}

nhpibifc(hd)
	register struct nhpibdevice *hd;
{
	hd->hpib_acr = AUX_TCA;
	hd->hpib_acr = AUX_CSRE;
	hd->hpib_acr = AUX_SSIC;
	DELAY(100);
	hd->hpib_acr = AUX_CSIC;
	hd->hpib_acr = AUX_SSRE;
}

nhpibsend(unit, slave, sec, addr, cnt)
	register char *addr;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int origcnt = cnt;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_acr = AUX_TCA;
	hd->hpib_data = C_UNL;
	nhpibowait(hd);
	hd->hpib_data = C_TAG + hs->sc_ba;
	hd->hpib_acr = AUX_STON;
	nhpibowait(hd);
	hd->hpib_data = C_LAG + slave;
	nhpibowait(hd);
	if (sec != -1) {
		hd->hpib_data = C_SCG + sec;
		nhpibowait(hd);
	}
	hd->hpib_acr = AUX_GTS;
	if (cnt) {
		while (--cnt) {
			hd->hpib_data = *addr++;
			if (nhpibowait(hd) < 0) {
				nhpibifc(hd);
				cnt++;
				goto out;
			}
		}
		hd->hpib_acr = AUX_EOI;
		hd->hpib_data = *addr;
		if (nhpibowait(hd) < 0) {
			nhpibifc(hd);
			cnt++;
		}
		else
			hd->hpib_acr = AUX_TCA;
	}
out:
	return(origcnt - cnt);
}

nhpibrecv(unit, slave, sec, addr, cnt)
	register char *addr;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int origcnt = cnt;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_acr = AUX_TCA;
	hd->hpib_data = C_UNL;
	nhpibowait(hd);
	hd->hpib_data = C_LAG + hs->sc_ba;
	hd->hpib_acr = AUX_SLON;
	nhpibowait(hd);
	hd->hpib_data = C_TAG + slave;
	nhpibowait(hd);
	if (sec != -1) {
		hd->hpib_data = C_SCG + sec;
		nhpibowait(hd);
	}
	hd->hpib_acr = AUX_RHDF;
	hd->hpib_acr = AUX_GTS;
	if (cnt) {
		while (--cnt >= 0) {
			if (nhpibiwait(hd) < 0) {
				nhpibifc(hd);
				break;
			}
			*addr++ = hd->hpib_data;
		}
		cnt++;
		hd->hpib_acr = AUX_TCA;
	}
	return(origcnt - cnt);
}

nhpibgo(unit, slave, sec, addr, count, rw)
	register int unit, slave;
	char *addr;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	hs->sc_flags |= HPIBF_IO;
	if (rw == B_READ)
		hs->sc_flags |= HPIBF_READ;
#ifdef DEBUG
	else if (hs->sc_flags & HPIBF_READ) {
		printf("nhpibgo: HPIBF_READ still set\n");
		hs->sc_flags &= ~HPIBF_READ;
	}
#endif
	hs->sc_count = count;
	hs->sc_addr = addr;
	if (hs->sc_flags & HPIBF_READ) {
		hs->sc_curcnt = count;
		dmago(hs->sc_dq.dq_ctlr, addr, count, DMAGO_BYTE|DMAGO_READ);
		nhpibrecv(unit, slave, sec, 0, 0);
		hd->hpib_mim = MIS_END;
	}
	else {
		if (count == 1) {
			hs->sc_curcnt = 1;
			dmago(hs->sc_dq.dq_ctlr, addr, 1, DMAGO_BYTE);
			nhpibsend(unit, slave, sec, 0, 0);
			hd->hpib_acr = AUX_EOI;
		}
		else {
			hs->sc_curcnt = count - 1;
			dmago(hs->sc_dq.dq_ctlr, addr, count - 1, DMAGO_BYTE);
			nhpibsend(unit, slave, sec, 0, 0);
		}
	}
	hd->hpib_ie = IDS_IE | IDS_DMA(hs->sc_dq.dq_ctlr);
}

nhpibdone(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int cnt;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	cnt = hs->sc_curcnt;
	hs->sc_addr += cnt;
	hs->sc_count -= cnt;
	if (hs->sc_flags & HPIBF_READ) {
		hs->sc_flags |= HPIBF_DONE;
		hd->hpib_ie = IDS_IE;
	} else {
		if (hs->sc_count == 1) {
			hs->sc_curcnt = 1;
			hd->hpib_acr = AUX_EOI;
			dmago(hs->sc_dq.dq_ctlr, hs->sc_addr, 1, DMAGO_BYTE);
			return;
		}
		hs->sc_flags |= HPIBF_DONE;
		hd->hpib_ie = IDS_IE;
		hd->hpib_mim = MIS_BO;
	}
}

nhpibintr(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register struct devqueue *dq = hs->sc_sq.dq_forw;
	register int stat0;
	int stat1;

#ifdef lint
	if (stat1 = unit) return(1);
#endif
	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	if ((hd->hpib_ids & IDS_IR) == 0)
		return(0);
	stat0 = hd->hpib_mis;
	stat1 = hd->hpib_lis;
	if (hs->sc_flags & HPIBF_IO) {
		hd->hpib_mim = 0;
		if ((hs->sc_flags & HPIBF_DONE) == 0)
			dmastop(hs->sc_dq.dq_ctlr);
		hd->hpib_acr = AUX_TCA;
		hs->sc_flags &= ~(HPIBF_DONE|HPIBF_IO|HPIBF_READ);
		dmafree(&hs->sc_dq);
		(dq->dq_driver->d_intr)(dq->dq_unit);
		return(1);
	}
	if (hs->sc_flags & HPIBF_PPOLL) {
		hd->hpib_mim = 0;
		stat0 = nhpibppoll(unit);
		if (stat0 & (0x80 >> dq->dq_slave)) {
			hs->sc_flags &= ~HPIBF_PPOLL;
			(dq->dq_driver->d_intr)(dq->dq_unit);
		}
		return(1);
	}
	return(1);
}

nhpibppoll(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int ppoll;

	hd = (struct nhpibdevice *)hs->sc_hc->hp_addr;
	hd->hpib_acr = AUX_SPP;
	DELAY(25);
	ppoll = hd->hpib_cpt;
	hd->hpib_acr = AUX_CPP;
	return(ppoll);
}

nhpibowait(hd)
	register struct nhpibdevice *hd;
{
	extern int hpibtimeout;
	register int timo = hpibtimeout;

	while ((hd->hpib_mis & MIS_BO) == 0 && --timo)
		;
	if (timo == 0)
		return(-1);
	return(0);
}

nhpibiwait(hd)
	register struct nhpibdevice *hd;
{
	extern int hpibtimeout;
	register int timo = hpibtimeout;

	while ((hd->hpib_mis & MIS_BI) == 0 && --timo)
		;
	if (timo == 0)
		return(-1);
	return(0);
}

nhpibppwatch(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct devqueue *dq = hs->sc_sq.dq_forw;
	register int slave = 0x80 >> dq->dq_slave;

	if ((hs->sc_flags & HPIBF_PPOLL) == 0)
		return;
	if (nhpibppoll(unit) & slave)
       		((struct nhpibdevice *)hs->sc_hc->hp_addr)->hpib_mim = MIS_BO;
	else
		timeout(nhpibppwatch, unit, 1);
}
#endif
