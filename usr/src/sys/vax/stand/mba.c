/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mba.c	6.2 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/vm.h"

#include "../vax/mtpr.h"
#include "../vaxmba/mbareg.h"
#include "../vaxmba/hpreg.h"

#include "saio.h"
#include "savax.h"

mbastart(io, func)
	register struct iob *io;
	int func;
{
	struct mba_regs *mba = mbamba(io->i_unit);
	struct mba_drv *drv = mbadrv(io->i_unit);
	register struct pte *pte = mba->mba_map;
	int npf;
	unsigned v;
	int o;
	int vaddr;

	v = btop(io->i_ma);
	o = (int)io->i_ma & PGOFSET;
	npf = btoc(io->i_cc + o);
	vaddr = o;
	while (--npf >= 0)
		*(int *)pte++ = v++ | PG_V;
	mba->mba_sr = -1;
	mba->mba_bcr = -io->i_cc;
	mba->mba_var = vaddr;
	if (io->i_flgs&F_SSI)
		drv->mbd_of |= HPOF_SSEI;
	switch (io->i_flgs & F_TYPEMASK) {

	case F_RDDATA:			/* standard read */
		drv->mbd_cs1 = MB_RCOM|MB_GO;
		mbawait(io);
		return(0);

	case F_WRDATA:			/* standard write */
		drv->mbd_cs1 = MB_WCOM|MB_GO;
		mbawait(io);
		return(0);

	/* the following commands apply to disks only */

	case F_HDR|F_RDDATA:	
		drv->mbd_cs1 = HP_RHDR|HP_GO;
		break;

	case F_HDR|F_WRDATA:
		drv->mbd_cs1 = HP_WHDR|HP_GO;
		break;

	case F_CHECK|F_WRDATA:
	case F_CHECK|F_RDDATA:
		drv->mbd_cs1 = HP_WCDATA|HP_GO;
		break;

	case F_HCHECK|F_WRDATA:
	case F_HCHECK|F_RDDATA:
		drv->mbd_cs1 = HP_WCHDR|HP_GO;
		break;

	default:
		goto error;
	}
	mbawait(io);
	if ((drv->mbd_dt & MBDT_TAP) == 0)
		return (0);
error:
	io->i_error = ECMD;
	io->i_flgs &= ~F_TYPEMASK;
	return (1);
}

mbawait(io)
	register struct iob *io;
{
	struct mba_regs *mba = mbamba(io->i_unit);
	struct mba_drv *drv = mbadrv(io->i_unit);

	while (mba->mba_sr & MBSR_DTBUSY)
		DELAY(100);
}

mbainit(mbanum)
	int mbanum;
{
	register struct mba_regs *mba = mbaddr[mbanum];

	/* SHOULD BADADDR IT */
	if (mbaact & (1<<mbanum))
		return;
	mba->mba_cr = MBCR_INIT;
	mbaact |= 1<<mbanum;
}
