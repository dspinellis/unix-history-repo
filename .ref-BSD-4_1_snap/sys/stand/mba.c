/*	mba.c	4.3	81/03/15	*/

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/mbareg.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
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
	if (func == WRITE)
		drv->mbd_cs1 = MB_WCOM | MB_GO;
	else
		drv->mbd_cs1 = MB_RCOM | MB_GO;
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
