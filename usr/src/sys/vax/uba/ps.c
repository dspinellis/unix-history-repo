/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ps.c	6.3 (Berkeley) %G%
 */

/*
 * Evans and Sutherland Picture System 2 driver
 */

/*
 *	Still to be done:
 *		WAIT_HIT
 */

#include "ps.h"
#if NPS > 0

#define EXTERNAL_SYNC

#include "../machine/pte.h"

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

int	psprobe(), psattach(), psintr();
struct	uba_device *psdinfo[NPS];
u_short	psstd[] = { 0 };
struct	uba_driver psdriver = 
    { psprobe, 0, psattach, 0, psstd, "ps", psdinfo };

#define	PSUNIT(dev)	(minor(dev))

#define MAXAUTOREFRESH			(4)
#define MAXAUTOMAP			(4)
#define MAXDBSIZE			(0177777/2)

#define PSPRI				(PZERO+1)

#define PSWAIT() {register short int i, j; i=20000; while((i-- != 0)\
	&& (((j=psaddr->ps_iostat)&DIOREADY)==0));}

struct ps {
	char		ps_open;
	short int 	ps_uid;
	struct {
		enum { SINGLE_STEP_RF, AUTO_RF, TIME_RF } state;
		enum { RUNNING_RF, SYNCING_RF, WAITING_MAP, STOPPED_RF } mode;
		unsigned short int sraddrs[MAXAUTOREFRESH];
		short int nsraddrs;
		short int srcntr;
		char waiting;
		char stop;
		int icnt;
		int timecnt;
	} ps_refresh;
	struct {
		enum { ON_DB, OFF_DB } state;
		unsigned short int dbaddrs[2];
		unsigned short int dbsize;
		short int rbuffer;
	} ps_dbuffer;
	struct {
		enum { SINGLE_STEP_MAP, AUTO_MAP } state;
		enum { RUNNING_MAP, WAITING_RF, WAITING_START } mode;
		unsigned short int maddrs[MAXAUTOMAP];
		short int nmaddrs;
		short int mcntr;
		short int outputstart;
		char waiting;
		char stop;
		int icnt;
	} ps_map;
	struct {
		short int ticked;
		short int missed;
		int icnt;
	} ps_clock;
	struct {
		int icnt;
	} ps_hit;
	int ps_strayintr;
	int last_request;
	int strayrequest;
	int ps_icnt;
} ps[NPS];

psprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct psdevice *psaddr = (struct psdevice *) reg;

	psaddr->ps_iostat = PSRESET;
	DELAY(200);
	psaddr->ps_addr = RTCIE;
	PSWAIT();
	psaddr->ps_data = 01;
	psaddr->ps_iostat = PSIE;
	psaddr->ps_addr = RTCSR;
	PSWAIT();
	psaddr->ps_data = (SYNC|RUN);
	DELAY(200000);
	psaddr->ps_addr = RTCREQ;
	PSWAIT();
	psaddr->ps_data = 01;
	psaddr->ps_iostat = 0;
	psaddr->ps_iostat = PSRESET;
	return (sizeof (struct psdevice));
}

/*ARGSUSED*/
psattach(ui)
	register struct uba_device *ui;
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
	psp->ps_refresh.waiting = 0;
	psp->ps_refresh.stop = 0;
	psp->ps_dbuffer.state = OFF_DB;
	psp->ps_map.state = SINGLE_STEP_MAP;
	psp->ps_map.waiting = 0;
	psp->ps_map.stop = 0;
	psp->ps_clock.ticked = 0;
	psp->ps_refresh.icnt = psp->ps_map.icnt = psp->ps_clock.icnt = 0;
	psp->ps_icnt = 0;
	maptouser(ui->ui_addr);
	return (0);
}

psclose(dev)
	dev_t dev;
{
	register struct psdevice *psaddr = 
			(struct psdevice *) psdinfo[PSUNIT(dev)]->ui_addr;

	ps[PSUNIT(dev)].ps_open = 0;
	psaddr->ps_iostat = 0;		/* clear IENABLE */
	PSWAIT();
	psaddr->ps_addr = RFSR;		/* set in auto refresh mode */
	PSWAIT();
	psaddr->ps_data = AUTOREF;
	unmaptouser(psaddr);
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
		n = fuword(waddr++);
		if (n == -1)
			return (EFAULT);
		if (n < 0 || n > MAXAUTOREFRESH)
			return (EINVAL);
		for (i = 0; i < n; i++) {
			if ((arg = fuword(waddr++)) == -1)
				return (EFAULT);
			psp->ps_refresh.sraddrs[i] = arg;
		}
		psp->ps_refresh.state = AUTO_RF;
		psp->ps_refresh.nsraddrs = n;
		psp->ps_refresh.srcntr = 0;
		psp->ps_refresh.mode = WAITING_MAP;
		break;

	case PSIOAUTOMAP:
		n = fuword(waddr++);
		if (n == -1)
			return (EFAULT);
		if (n < 0 || n > MAXAUTOMAP)
			return (EINVAL);
		for (i = 0; i < n; i++) {
			if ((arg = fuword(waddr++)) == -1)
				return (EFAULT);
			psp->ps_map.maddrs[i] = arg;
		}
		if ((arg = fuword(waddr++)) == -1)
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
		if ((arg = fuword(waddr++)) == -1)
			return (EFAULT);
		psp->ps_dbuffer.dbaddrs[0] = arg;
		if ((arg = fuword(waddr++)) == -1)
			return (EFAULT);
		if (arg <= 0 || arg > MAXDBSIZE)
			return (EINVAL);
		psp->ps_dbuffer.dbsize = arg;
		psp->ps_dbuffer.dbaddrs[1] =
		    psp->ps_dbuffer.dbaddrs[0]+arg;
		psp->ps_dbuffer.state = ON_DB;
		psp->ps_dbuffer.rbuffer = 0;
		break;

	case PSIOSINGLEBUFFER:
		psp->ps_dbuffer.state = OFF_DB;
		break;

	case PSIOTIMEREFRESH:
		if (psp->ps_refresh.state != SINGLE_STEP_RF)
			return(EINVAL);
		if ((arg = fuword(waddr++)) == -1)
			return(EFAULT);
		psp->ps_refresh.state = TIME_RF;
		psp->ps_refresh.timecnt = arg;
		break;

	case PSIOWAITREFRESH:
		if (psp->ps_refresh.mode != RUNNING_RF)	/* not running */
			return (0);				/* dont wait */
		/* fall into ... */

	case PSIOSTOPREFRESH:
		if (cmd == PSIOSTOPREFRESH) {
			if (psp->ps_refresh.mode == STOPPED_RF
					&& psp->ps_refresh.state != TIME_RF)
				return (0);
			psp->ps_refresh.stop = 1;
		}
		spl5();
		psp->ps_refresh.waiting = 1;
		while (psp->ps_refresh.waiting)
			sleep(&psp->ps_refresh.waiting, PSPRI);
		spl0();
		if (cmd == PSIOSTOPREFRESH)
			psp->ps_refresh.mode = STOPPED_RF;
		if (psp->ps_refresh.state == TIME_RF)
			psp->ps_refresh.state = SINGLE_STEP_RF;
		break;

	case PSIOWAITMAP:
		if (psp->ps_map.mode != RUNNING_MAP)	/* not running */
			return (0);				/* dont wait */
		/* fall into ... */

	case PSIOSTOPMAP:
		if (cmd == PSIOSTOPMAP)
			psp->ps_map.stop = 1;
		spl5();
		psp->ps_map.waiting = 1;
		while (psp->ps_map.waiting)
			sleep(&psp->ps_map.waiting, PSPRI);
		spl0();
		break;

	default:
		return (ENOTTY);
		break;
	}
	return (0);
}

#define SAVEPSADDR() {register short int i, x;x=spl6();i=psaddr->ps_addr;\
		while(((i=psaddr->ps_iostat)&DIOREADY)==0);\
		savepsaddr=psaddr->ps_data;splx(x);}
#define RESTORPSADDR() {register int x,i;x=spl6();\
		while(((i=psaddr->ps_iostat)&DIOREADY)==0);\
		psaddr->ps_addr=savepsaddr;splx(x);}

psclockintr(dev)
	dev_t dev;
{
	register struct psdevice *psaddr =
			(struct psdevice *) psdinfo[PSUNIT(dev)]->ui_addr;
	register struct ps *psp = &ps[PSUNIT(dev)];
	int savepsaddr;

	if (!psp->ps_open)
		return;
	psp->ps_clock.icnt++;
	SAVEPSADDR();
#ifndef EXTERNAL_SYNC
	if (psp->ps_refresh.state == AUTO_RF) {
		if (psp->ps_refresh.mode == SYNCING_RF
					&& psp->ps_refresh.state != TIME_RF) {
			psrfnext(psp, psaddr);
		} else {
			psp->ps_clock.ticked++;
			psp->ps_clock.missed++;
		}
	}
#endif
	PSWAIT();
	psaddr->ps_addr = RTCREQ;
	PSWAIT();
	psaddr->ps_data = 01;		/* clear the request bits */
	RESTORPSADDR();
}

/*ARGSUSED*/
pssystemintr(dev)
	dev_t dev;
{
	register struct psdevice *psaddr =
			(struct psdevice *) psdinfo[PSUNIT(dev)]->ui_addr;
	register struct ps *psp = &ps[PSUNIT(dev)];
	short int request;
	register int savepsaddr, x;

	if (!psp->ps_open)
		return;
	psp->ps_icnt++;
	SAVEPSADDR();
	PSWAIT();
	psaddr->ps_addr = SYSREQ;
	PSWAIT();
	request = psaddr->ps_data&0377;
	psp->last_request = request;
	PSWAIT();
	psaddr->ps_addr = SYSREQ;
	PSWAIT();
	psaddr->ps_data = request&(~(HALT_REQ|MOSTOP_REQ));   /* acknowledge */

	if (request & (MOSTOP_REQ|HALT_REQ)) {	/* Map stopped */
		psp->ps_map.icnt++;
		psmapstop(psaddr);		/* kill it dead */
		if (psp->ps_map.waiting) {
			psp->ps_map.waiting = 0;
			wakeup(&psp->ps_map.waiting);
			if (psp->ps_map.stop) {
				psp->ps_map.stop = 0;
				goto tryrf;
			}
		}
		if (psp->ps_map.state == AUTO_MAP)
			if (!psmapnext(psp, psaddr)) {
				psp->ps_map.mcntr = 0;
				/* prepare for next round */
				pssetmapbounds(psp, psaddr);
				if (psp->ps_refresh.mode == WAITING_MAP) {
					if (psp->ps_dbuffer.state == ON_DB)
						/* fill other db */
						psdbswitch(psp, psaddr);
					else
						psp->ps_map.mode = WAITING_RF;
					psrfnext(psp, psaddr);	/* start rf */
				} else
					psp->ps_map.mode = WAITING_RF;
			}
	}
tryrf:
	if (request & RFSTOP_REQ) {		/* Refresh stopped */
		psp->ps_refresh.icnt++;
		if (psp->ps_refresh.state == TIME_RF)
			if(--psp->ps_refresh.timecnt > 0)
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
						psp->ps_map.mode==WAITING_RF) {
					if (psp->ps_dbuffer.state == ON_DB)
						psdbswitch(psp, psaddr);
					else
						psmapnext(psp, psaddr);
				}
				psp->ps_refresh.srcntr = 0;
#ifdef EXTERNAL_SYNC
				x = spl6();
#endif
				if (!psp->ps_clock.ticked ||
						!psrfnext(psp, psaddr)) {
					psp->ps_refresh.mode = SYNCING_RF;
				}
				psp->ps_clock.ticked = 0;
				psp->ps_refresh.mode = SYNCING_RF;
#ifdef EXTERNAL_SYNC
				splx(x);
#endif
			}
	}
tryhit:
	if (request & HIT_REQ) {		/* Hit request */
		psp->ps_hit.icnt++;
	}
	if (request == 0)
		psp->ps_strayintr++;
	RESTORPSADDR();
}

psrfnext(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	if (psp->ps_refresh.srcntr < psp->ps_refresh.nsraddrs)
		psrfstart(psp->ps_refresh.sraddrs[psp->ps_refresh.srcntr++],
						psp, psaddr);
	else if (psp->ps_refresh.srcntr == psp->ps_refresh.nsraddrs
				&& psp->ps_dbuffer.state == ON_DB) {
		psrfstart(psp->ps_dbuffer.dbaddrs[psp->ps_dbuffer.rbuffer],
						psp, psaddr);
		psp->ps_refresh.srcntr++;	/* flag for after dbuffer */
	} else
		return(0);
	return(1);
}

psrfstart(dfaddr, psp, psaddr)
	short int dfaddr;
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	int dummy;

	PSWAIT();
	psaddr->ps_addr = RFASA;
	PSWAIT();
	psaddr->ps_data = dfaddr;
	PSWAIT();
	dummy = psaddr->ps_data;	/* just access to get to status reg */
	PSWAIT();
	psaddr->ps_data = RFSTART;	/* may want to | this value in */
	psp->ps_refresh.mode = RUNNING_RF;
}

psrfstop(psaddr, psp)
	register struct psdevice *psaddr;
	register struct ps *psp;
{

	PSWAIT();
	psaddr->ps_addr = RFSR;
	PSWAIT();
	psaddr->ps_data = 0;
}

psdbswitch(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	psp->ps_dbuffer.rbuffer = !psp->ps_dbuffer.rbuffer;
	pssetmapbounds(psp, psaddr);
	psmapnext(psp, psaddr);
}

psmapnext(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{

	if (psp->ps_map.mcntr < psp->ps_map.nmaddrs)
		psmapstart(psp->ps_map.maddrs[psp->ps_map.mcntr++], psp, psaddr);
	else
		return(0);
	return(1);
}

pssetmapbounds(psp, psaddr)
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	unsigned short int start;

	PSWAIT();
	psaddr->ps_addr = MAOL;
	PSWAIT();
	if (psp->ps_dbuffer.state == ON_DB) {
		psaddr->ps_data = (start = psp->ps_dbuffer.dbaddrs[!psp->ps_dbuffer.rbuffer])
				+psp->ps_dbuffer.dbsize-2;   /* 2 for a refresh halt command */
		PSWAIT();
		psaddr->ps_data = start;
	} else {
		start = psaddr->ps_data;	/* dummy: don't update limit */
		PSWAIT();
		psaddr->ps_data = psp->ps_map.outputstart;
	}
}

psmapstart(dfaddr, psp, psaddr)
	int dfaddr;
	register struct ps *psp;
	register struct psdevice *psaddr;
{
	int data;

	PSWAIT();
	psaddr->ps_addr = MAIA;
	PSWAIT();
	psaddr->ps_data = dfaddr;
	PSWAIT();
	psaddr->ps_data = MAO|MAI;	/* may want more here */
	psp->ps_map.mode = RUNNING_MAP;
}

psmapstop(psaddr)
	register struct psdevice *psaddr;
{

	PSWAIT();
	psaddr->ps_addr = MASR;
	PSWAIT();
	psaddr->ps_data = 0;	/* zero MAI bit */
	PSWAIT();
	psaddr->ps_addr = MAIA;
	PSWAIT();
	psaddr->ps_data = 0;	/* zero input address register */
	PSWAIT();
	psaddr->ps_addr = SYSREQ;
	PSWAIT();
	psaddr->ps_data = HALT_REQ|MOSTOP_REQ;	/* overkill?? */
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

psreset(uban)
	int uban;
{
}

psextsync(PC, PS) {
	register int n;
	register struct psdevice *psaddr;
	register struct ps *psp;
	register int savepsaddr;

#ifdef EXTERNAL_SYNC
	for (psp = ps, n = 0; n < NPS; psp++, n++) {
		if (!psp->ps_open)
			continue;
		if(psp->ps_refresh.mode == SYNCING_RF
					&& psp->ps_refresh.state != TIME_RF) {
			psaddr = (struct psdevice *) psdinfo[n]->ui_addr;
			SAVEPSADDR();
			psrfnext(psp, psaddr);
			RESTORPSADDR();
		} else {
			psp->ps_clock.ticked++;
			psp->ps_clock.missed++;
		}
	}
#endif
}
#endif
