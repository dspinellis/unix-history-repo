/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ps.c	7.2 (Berkeley) %G%
 */

/*
 * Evans and Sutherland Picture System 2 driver -- Bill Reeves.
 */

/*
 *	Still to be done:
 *		WAIT_HIT
 */

#include "ps.h"
#if NPS > 0

#define EXTERNAL_SYNC

#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "map.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "uio.h"

#include "ubareg.h"
#include "ubavar.h"
#include "psreg.h"

int	psprobe(), psattach(), psextsync();
int	psclockintr(), pssystemintr(), psdeviceintr(), psdmaintr();
struct	uba_device *psdinfo[NPS];
u_short	psstd[] = { 0 };
struct	uba_driver psdriver = 
    { psprobe, 0, psattach, 0, psstd, "ps", psdinfo };

#define	PSUNIT(dev)	(minor(dev))

#define MAXAUTOREFRESH	4
#define MAXAUTOMAP	4
#define MAXDBSIZE	(0177777/2)

#define PSPRI		(PZERO+1)

#define PSWAIT(psaddr) { \
	register short i = 20000, j; \
	while (i-- != 0 && ((j = psaddr->ps_iostat) & DIOREADY) == 0) \
		;\
}

struct psrefresh {
	enum {
		SINGLE_STEP_RF,
		AUTO_RF,
		TIME_RF
	} state;
	enum {
		RUNNING_RF,
		SYNCING_RF,
		WAITING_MAP,
		STOPPED_RF
	} mode;
	u_short	sraddrs[MAXAUTOREFRESH];
	short	nsraddrs;
	short	srcntr;
	char	waiting;
	char	stop;
	int	icnt;
	int	timecnt;
};

struct psdbuffer {
	enum {
		ON_DB,
		OFF_DB
	} state;
	u_short	dbaddrs[2];
	u_short	dbsize;
	short	rbuffer;
};

struct psmap {
	enum {
		SINGLE_STEP_MAP,
		AUTO_MAP
	} state;
	enum {
		RUNNING_MAP,
		WAITING_RF,
		WAITING_START,
		STOPPED_MAP
	} mode;
	u_short	maddrs[MAXAUTOMAP];
	short	nmaddrs;
	short	mcntr;
	short	outputstart;
	char	waiting;
	char	stop;
	int	icnt;
};

/*
 * PS2 software state.
 */
struct ps {
	char	ps_open;		/* device is open */
	uid_t 	ps_uid;			/* uid of device owner */
	struct	psrefresh ps_refresh;	/* refresh state */
	struct	psdbuffer ps_dbuffer;	/* double buffering state */
	struct	psmap ps_map;		/* segment map state */
	int	ps_clockticks;		/* clock ints between refresh */
	int	ps_clockmiss;		/* clock ints w/o refresh */
	int	ps_clockcnt;		/* count of clock interrupts */
	int	ps_hitcnt;		/* count of hit interrupts */
	int	ps_strayintr;		/* count of stray interrupts */
	int	ps_icnt;		/* count of system interrupts */
/* BEGIN GROT */
	int	ps_lastrequest;
	int	ps_lastrequest2;
	int	ps_lastfunnyrequest;
	int	ps_funnycnt;
/* END GROT */
} ps[NPS];

psprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct psdevice *psaddr = (struct psdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	psclockintr((dev_t)0); pssystemintr((dev_t)0);
	psdeviceintr((dev_t)0); psdmaintr((dev_t)0);
	psextsync(0, 0);
#endif
	psaddr->ps_iostat = PSRESET;
	DELAY(200);
	psaddr->ps_addr = RTCIE;
	PSWAIT(psaddr); psaddr->ps_data = 01;
	psaddr->ps_iostat = PSIE;
	psaddr->ps_addr = RTCSR;
	PSWAIT(psaddr); psaddr->ps_data = SYNC|RUN;
	DELAY(200000);
	psaddr->ps_addr = RTCREQ;
	PSWAIT(psaddr); psaddr->ps_data = 01;
	psaddr->ps_iostat = 0;
	psaddr->ps_iostat = PSRESET;
	return (sizeof (struct psdevice));
}

/*ARGSUSED*/
psattach(ui)
	struct uba_device *ui;
{

}

psopen(dev)
	dev_t dev;
{
	register struct ps *psp;
	register struct uba_device *ui;
	register int unit = PSUNIT(dev);

	if (unit >= NPS || (psp = &ps[minor(dev)])->ps_open ||
	    (ui = psdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	psp->ps_open = 1;
	psp->ps_uid = u.u_uid;
	psp->ps_strayintr = 0;
	psp->ps_refresh.state = SINGLE_STEP_RF;
	psp->ps_refresh.mode = STOPPED_RF;
	psp->ps_refresh.waiting = 0;
	psp->ps_refresh.stop = 0;
	psp->ps_dbuffer.state = OFF_DB;
	psp->ps_map.state = SINGLE_STEP_MAP;
	psp->ps_map.mode = STOPPED_MAP;
	psp->ps_map.waiting = 0;
	psp->ps_map.stop = 0;
	psp->ps_clockticks = 0;
	psp->ps_clockmiss = 0;
	psp->ps_refresh.icnt = psp->ps_map.icnt = psp->ps_clockcnt = 0;
	psp->ps_hitcnt = 0;
	psp->ps_icnt = 0;
	maptouser(ui->ui_addr);
	return (0);
}

psclose(dev)
	dev_t dev;
{
	register struct psdevice *psaddr = 
	    (struct psdevice *)psdinfo[PSUNIT(dev)]->ui_addr;

	ps[PSUNIT(dev)].ps_open = 0;
	psaddr->ps_iostat = 0;			 /* clear IENABLE */
	PSWAIT(psaddr); psaddr->ps_addr = RFSR;	 /* set in auto refresh mode */
	PSWAIT(psaddr); psaddr->ps_data = AUTOREF;
	unmaptouser((caddr_t)psaddr);
}

/*ARGSUSED*/
psread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
}

/*ARGSUSED*/
pswrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
}

/*ARGSUSED*/
psioctl(dev, cmd, data, flag)
	register caddr_t data;
{
	register struct uba_device *ui = psdinfo[PSUNIT(dev)];
	register struct ps *psp = &ps[PSUNIT(dev)];
	int *waddr = *(int **)data;
	int n, arg, i;

	switch (cmd) {

	case PSIOGETADDR:
		*(caddr_t *)data = ui->ui_addr;
		break;

	case PSIOAUTOREFRESH:
		n = fuword((caddr_t)waddr++);
		if (n == -1)
			return (EFAULT);
		if (n < 0 || n > MAXAUTOREFRESH)
			return (EINVAL);
		for (i = 0; i < n; i++) {
			if ((arg = fuword((caddr_t)waddr++)) == -1)
				return (EFAULT);
			psp->ps_refresh.sraddrs[i] = arg;
		}
		psp->ps_refresh.state = AUTO_RF;
		psp->ps_refresh.nsraddrs = n;
		psp->ps_refresh.srcntr = 0;
		psp->ps_refresh.mode = WAITING_MAP;
		break;

	case PSIOAUTOMAP:
		n = fuword((caddr_t)waddr++);
		if (n == -1)
			return (EFAULT);
		if (n < 0 || n > MAXAUTOMAP)
			return (EINVAL);
		for (i = 0; i < n; i++) {
			if ((arg = fuword((caddr_t)waddr++)) == -1)
				return (EFAULT);
			psp->ps_map.maddrs[i] = arg;
		}
		if ((arg = fuword((caddr_t)waddr++)) == -1)
			return (EFAULT);
		psp->ps_map.outputstart = arg;
		psp->ps_map.state = AUTO_MAP;
		psp->ps_map.nmaddrs = n;
		psp->ps_map.mcntr = 0;
		psp->ps_map.mode = WAITING_START;
		break;

	case PSIOSINGLEREFRESH:
		psp->ps_refresh.state = SINGLE_STEP_RF;
		break;

	case PSIOSINGLEMAP:
		psp->ps_map.state = SINGLE_STEP_MAP;
		break;

	case PSIODOUBLEBUFFER:
		if ((arg = fuword((caddr_t)waddr++)) == -1)
			return (EFAULT);
		psp->ps_dbuffer.dbaddrs[0] = arg;
		if ((arg = fuword((caddr_t)waddr++)) == -1)
			return (EFAULT);
		if (arg <= 0 || arg > MAXDBSIZE)
			return (EINVAL);
		psp->ps_dbuffer.dbsize = arg;
		psp->ps_dbuffer.dbaddrs[1] = psp->ps_dbuffer.dbaddrs[0]+arg;
		psp->ps_dbuffer.state = ON_DB;
		psp->ps_dbuffer.rbuffer = 0;
		break;

	case PSIOSINGLEBUFFER:
		psp->ps_dbuffer.state = OFF_DB;
		break;

	case PSIOTIMEREFRESH:
		if (psp->ps_refresh.state != SINGLE_STEP_RF)
			return (EINVAL);
		if ((arg = fuword((caddr_t)waddr++)) == -1)
			return (EFAULT);
		psp->ps_refresh.state = TIME_RF;
		psp->ps_refresh.timecnt = arg;
		break;

	case PSIOWAITREFRESH:
		if (psp->ps_refresh.mode != RUNNING_RF)	/* not running */
			return (0);			/* dont wait */
		/* fall into ... */

	case PSIOSTOPREFRESH:
		if (cmd == PSIOSTOPREFRESH) {
			if (psp->ps_refresh.mode == STOPPED_RF &&
			    psp->ps_refresh.state != TIME_RF)
				return (0);
			psp->ps_refresh.stop = 1;
		}
		(void) spl5();
		psp->ps_refresh.waiting = 1;
		while (psp->ps_refresh.waiting)
			sleep(&psp->ps_refresh.waiting, PSPRI);
		(void) spl0();
		if (cmd == PSIOSTOPREFRESH)
			psp->ps_refresh.mode = STOPPED_RF;
		if (psp->ps_refresh.state == TIME_RF)
			psp->ps_refresh.state = SINGLE_STEP_RF;
		break;

	case PSIOWAITMAP:
		if (psp->ps_map.mode != RUNNING_MAP)	/* not running */
			return (0);			/* dont wait */
		/* fall into ... */

	case PSIOSTOPMAP:
		if (cmd == PSIOSTOPMAP)
			psp->ps_map.stop = 1;
		(void) spl5();
		psp->ps_map.waiting = 1;
		while (psp->ps_map.waiting)
			sleep(&psp->ps_map.waiting, PSPRI);
		(void) spl0();
		break;

	default:
		return (ENOTTY);
		break;
	}
	return (0);
}

#define SAVEPSADDR(psaddr, savepsaddr) { \
	register short i, xx1; \
	xx1 = splclock(); \
	i = psaddr->ps_addr; \
	while ((psaddr->ps_iostat & DIOREADY) == 0) \
		; \
	savepsaddr = psaddr->ps_data; \
	splx(xx1); \
}
#define RESTORPSADDR(psaddr, savepsaddr) { \
	register short xx2; \
	xx2 = splclock(); \
	while ((psaddr->ps_iostat & DIOREADY) == 0) \
		;\
	psaddr->ps_addr = savepsaddr; \
	splx(xx2); \
}

psclockintr(dev)
	dev_t dev;
{
	register struct psdevice *psaddr =
	    (struct psdevice *)psdinfo[PSUNIT(dev)]->ui_addr;
	register struct ps *psp = &ps[PSUNIT(dev)];
	int savepsaddr;

	if (!psp->ps_open)
		return;
	psp->ps_clockcnt++;
	SAVEPSADDR(psaddr, savepsaddr);
#ifndef EXTERNAL_SYNC
	if (psp->ps_refresh.state == AUTO_RF) {
		if (psp->ps_refresh.mode == SYNCING_RF &&
		    psp->ps_refresh.state != TIME_RF) {
			(void) psrfnext(psp, psaddr);
		} else {
			psp->ps_clockticks++;
			psp->ps_clockmiss++;
		}
	}
#endif
	PSWAIT(psaddr); psaddr->ps_addr = RTCREQ;
	PSWAIT(psaddr); psaddr->ps_data = 01;	/* clear the request bits */
	RESTORPSADDR(psaddr, savepsaddr);
}

/*ARGSUSED*/
pssystemintr(dev)
	dev_t dev;
{
	register struct psdevice *psaddr =
	    (struct psdevice *)psdinfo[PSUNIT(dev)]->ui_addr;
	register struct ps *psp = &ps[PSUNIT(dev)];
	short request, tmp;
	register int savepsaddr, x;

	if (!psp->ps_open)
		return;
	psp->ps_icnt++;
	SAVEPSADDR(psaddr, savepsaddr);
	PSWAIT(psaddr); psaddr->ps_addr = SYSREQ;
	PSWAIT(psaddr); request = psaddr->ps_data;
	request = request&0377;
	psp->ps_lastrequest2 = psp->ps_lastrequest;
	psp->ps_lastrequest = request;
	if (request &~ (HALT_REQ|RFSTOP_REQ|HIT_REQ)) {
		psp->ps_lastfunnyrequest = request;
		psp->ps_funnycnt++;
	}
	PSWAIT(psaddr); psaddr->ps_addr = SYSREQ;
	tmp = request&(~(HALT_REQ|MOSTOP_REQ));	/* acknowledge */
	PSWAIT(psaddr); psaddr->ps_data = tmp;

	if (request & (MOSTOP_REQ|HALT_REQ)) {	/* Map stopped */
		psp->ps_map.icnt++;
		psmapstop(psaddr, psp, request);/* kill it dead */
		if (psp->ps_map.waiting) {
			psp->ps_map.waiting = 0;
			wakeup(&psp->ps_map.waiting);
			if (psp->ps_map.stop) {
				psp->ps_map.stop = 0;
				goto tryrf;
			}
		}
		if (psp->ps_map.state == AUTO_MAP && !psmapnext(psp, psaddr)) {
			psp->ps_map.mcntr = 0;
			/* prepare for next round */
			pssetmapbounds(psp, psaddr);
			if (psp->ps_refresh.state == AUTO_RF) {
				if (psp->ps_refresh.mode == WAITING_MAP){
					if (psp->ps_dbuffer.state == ON_DB)
						/* fill other db */
						psdbswitch(psp, psaddr);
					else
						psp->ps_map.mode = WAITING_RF;
#ifdef EXTERNAL_SYNC
					x = splclock();
#endif
					(void) psrfnext(psp, psaddr);
#ifdef EXTERNAL_SYNC
					splx(x);
#endif
				} else
					psp->ps_map.mode = WAITING_RF;
			} else {	/* no auto refresh */
				if (psp->ps_dbuffer.state == ON_DB)
					/* fill other db */
					psdbswitch(psp, psaddr);
				else
					(void) psmapnext(psp, psaddr);
			}
		}
	}
tryrf:
	if (request & RFSTOP_REQ) {		/* Refresh stopped */
		psp->ps_refresh.icnt++;
		if (psp->ps_refresh.state == TIME_RF)
			if (--psp->ps_refresh.timecnt > 0)
				goto tryhit;
		psrfstop(psaddr, psp);
		if (psp->ps_refresh.waiting) {
			psp->ps_refresh.waiting = 0;
			wakeup(&psp->ps_refresh.waiting);
			if (psp->ps_refresh.stop) {
				psp->ps_refresh.stop = 0;
				goto tryhit;
			}
		}
		if (psp->ps_refresh.state == AUTO_RF)
			if (!psrfnext(psp, psaddr)) {	/* at end of refresh cycle */
				if (psp->ps_map.state == AUTO_MAP && 
				    psp->ps_map.mode == WAITING_RF) {
					if (psp->ps_dbuffer.state == ON_DB)
						psdbswitch(psp, psaddr);
					else
						(void) psmapnext(psp, psaddr);
				}
				psp->ps_refresh.srcntr = 0;
#ifdef EXTERNAL_SYNC
				x = splclock();
#endif
				psp->ps_refresh.mode = SYNCING_RF;
				if (psp->ps_clockticks)
					(void) psrfnext(psp, psaddr);
				psp->ps_clockticks = 0;
#ifdef EXTERNAL_SYNC
				splx(x);
#endif
			}
	}
tryhit:
	if (request & HIT_REQ)			/* Hit request */
		psp->ps_hitcnt++;
	if (request == 0)
		psp->ps_strayintr++;
	RESTORPSADDR(psaddr, savepsaddr);
}

psrfnext(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	u_short start, last;

	if (psp->ps_refresh.srcntr < psp->ps_refresh.nsraddrs) {
		psrfstart(psp->ps_refresh.sraddrs[psp->ps_refresh.srcntr++],
		    0, psp, psaddr);
		return (1);
	}
	if (psp->ps_refresh.srcntr == psp->ps_refresh.nsraddrs &&
	    psp->ps_dbuffer.state == ON_DB) {
		start = psp->ps_dbuffer.dbaddrs[psp->ps_dbuffer.rbuffer];
		last = start+psp->ps_dbuffer.dbsize;
		psrfstart(start, last, psp, psaddr);
		psp->ps_refresh.srcntr++;	/* flag for after dbuffer */
		return (1);
	}
	return (0);
}

psrfstart(dfaddr, last, psp, psaddr)
	u_short dfaddr, last;
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	short dummy;

	PSWAIT(psaddr); psaddr->ps_addr = RFASA;
	PSWAIT(psaddr); psaddr->ps_data = dfaddr;
	PSWAIT(psaddr);
	if (last != 0)
		psaddr->ps_data = last;
	else
		dummy = psaddr->ps_data;/* just access to get to status reg */
	PSWAIT(psaddr); psaddr->ps_data = RFSTART;	/* may want | here */
	psp->ps_refresh.mode = RUNNING_RF;
}

/*ARGSUSED*/
psrfstop(psaddr, psp)
	register struct psdevice *psaddr;
	register struct ps *psp;
{

	PSWAIT(psaddr); psaddr->ps_addr = RFSR;
	PSWAIT(psaddr); psaddr->ps_data = 0;
}

psdbswitch(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	psp->ps_dbuffer.rbuffer = !psp->ps_dbuffer.rbuffer;
	pssetmapbounds(psp, psaddr);
	(void) psmapnext(psp, psaddr);
}

psmapnext(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	if (psp->ps_map.mcntr < psp->ps_map.nmaddrs) {
		psmapstart(psp->ps_map.maddrs[psp->ps_map.mcntr++],
		    psp, psaddr);
		return (1);
	}
	return (0);
}

pssetmapbounds(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	u_short start, last;

	PSWAIT(psaddr); psaddr->ps_addr = MAOL;
	PSWAIT(psaddr);
	if (psp->ps_dbuffer.state == ON_DB) {
		start = psp->ps_dbuffer.dbaddrs[!psp->ps_dbuffer.rbuffer];
		last = start+psp->ps_dbuffer.dbsize-2;   /* 2 for halt cmd */
		psaddr->ps_data = last;
		PSWAIT(psaddr); psaddr->ps_data = start;
	} else {
		start = psaddr->ps_data;	/* dummy: don't update limit */
		PSWAIT(psaddr); psaddr->ps_data = psp->ps_map.outputstart;
	}
}

psmapstart(dfaddr, psp, psaddr)
	u_short dfaddr;
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	PSWAIT(psaddr); psaddr->ps_addr = MAIA;
	PSWAIT(psaddr); psaddr->ps_data = dfaddr;
	PSWAIT(psaddr); psaddr->ps_data = MAO|MAI;	/* may want more here */
	psp->ps_map.mode = RUNNING_MAP;
}

int pskillcnt = 1;

psmapstop(psaddr, psp, request)
	register struct psdevice *psaddr;
	register struct ps *psp;
	short request;
{
	register int i;

	request &= HALT_REQ|MOSTOP_REQ; 	/* overkill?? */
	for (i = 0; i < pskillcnt; i++) {
		PSWAIT(psaddr); psaddr->ps_addr = MASR;
		PSWAIT(psaddr); psaddr->ps_data = IOUT;	/* zero MAI & MAO */
		PSWAIT(psaddr); psaddr->ps_addr = MAIA;
		PSWAIT(psaddr); psaddr->ps_data = 0;	/* 0 input addr reg */
		PSWAIT(psaddr); psaddr->ps_addr = MAOA;
		PSWAIT(psaddr); psaddr->ps_data = 0;	/* 0 output addr reg */
		PSWAIT(psaddr); psaddr->ps_addr = SYSREQ;
		PSWAIT(psaddr); psaddr->ps_data = request;
	}
	psp->ps_map.mode = STOPPED_MAP;
}

/*ARGSUSED*/
psdeviceintr(dev)
	dev_t dev;
{

	printf("ps device intr\n");
}

/*ARGSUSED*/
psdmaintr(dev)
	dev_t dev;
{

	printf("ps dma intr\n");
}

/*ARGSUSED*/
psreset(uban)
	int uban;
{

}

/*ARGSUSED*/
psextsync(PC, PS)
{
	register int n;
	register struct psdevice *psaddr;
	register struct ps *psp;
	register int savepsaddr;

#ifdef EXTERNAL_SYNC
	for (psp = ps, n = 0; n < NPS; psp++, n++) {
		if (!psp->ps_open)
			continue;
		if (psp->ps_refresh.mode == SYNCING_RF &&
		    psp->ps_refresh.state != TIME_RF) {
			psaddr = (struct psdevice *)psdinfo[n]->ui_addr;
			SAVEPSADDR(psaddr, savepsaddr);
			(void) psrfnext(psp, psaddr);
			RESTORPSADDR(psaddr, savepsaddr);
		} else {
			psp->ps_clockticks++;
			psp->ps_clockmiss++;
		}
	}
#endif
}
#endif
