/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nhpib.c	7.2 (Berkeley) %G%
 */

/*
 * Internal/98624 HPIB driver
 */

#include "sys/param.h"
#include "../dev/nhpibreg.h"
#include "hpibvar.h"

nhpibinit(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd = (struct nhpibdevice *)hs->sc_addr;
	extern int internalhpib;

	if ((int)hd == internalhpib) {
		hs->sc_type = HPIBA;
		hs->sc_ba = HPIBA_BA;
	}
	else if (hd->hpib_cid == HPIBB) {
		hs->sc_type = HPIBB;
		hs->sc_ba = hd->hpib_csa & CSA_BA;
	}
	else
		return(0);
	nhpibreset(unit);
	return(1);
}

nhpibreset(unit)
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;

	hd = (struct nhpibdevice *)hs->sc_addr;
	hd->hpib_acr = AUX_SSWRST;
	hd->hpib_ar = hs->sc_ba;
	hd->hpib_lim = 0;
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
	hd->hpib_acr = AUX_TCA;
	hd->hpib_acr = AUX_CSRE;
	hd->hpib_acr = AUX_SSIC;
	DELAY(100);
	hd->hpib_acr = AUX_CSIC;
	hd->hpib_acr = AUX_SSRE;
	hd->hpib_data = C_DCL;
	DELAY(100000);
}

nhpibsend(unit, slave, sec, buf, cnt)
	register char *buf;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int origcnt = cnt;

	hd = (struct nhpibdevice *)hs->sc_addr;
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
			hd->hpib_data = *buf++;
			if (nhpibowait(hd) < 0)
				break;
		}
		hd->hpib_acr = AUX_EOI;
		hd->hpib_data = *buf;
		if (nhpibowait(hd) < 0)
			cnt++;
		hd->hpib_acr = AUX_TCA;
	}
	return(origcnt - cnt);
}

nhpibrecv(unit, slave, sec, buf, cnt)
	register char *buf;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int origcnt = cnt;

	hd = (struct nhpibdevice *)hs->sc_addr;
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
			if (nhpibiwait(hd) < 0)
				break;
			*buf++ = hd->hpib_data;
		}
		cnt++;
		hd->hpib_acr = AUX_TCA;
	}
	return(origcnt - cnt);
}

nhpibppoll(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[unit];
	register struct nhpibdevice *hd;
	register int ppoll;

	hd = (struct nhpibdevice *)hs->sc_addr;
	hd->hpib_acr = AUX_SPP;
	DELAY(25);
	ppoll = hd->hpib_cpt;
	hd->hpib_acr = AUX_CPP;
	return(ppoll);
}

nhpibowait(hd)
	register struct nhpibdevice *hd;
{
	register int timo = 100000;

	while ((hd->hpib_mis & MIS_BO) == 0 && --timo)
		;
	if (timo == 0)
		return(-1);
	return(0);
}

nhpibiwait(hd)
	register struct nhpibdevice *hd;
{
	register int timo = 100000;

	while ((hd->hpib_mis & MIS_BI) == 0 && --timo)
		;
	if (timo == 0)
		return(-1);
	return(0);
}
