/*	mba.c	4.1	11/9/80	*/

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "saio.h"

/*
 * startup routine for MBA controllers.
 */
#define	MBAWCOM	0x30
#define	MBARCOM	0x38
#define	GO	01

struct mba_info mbainfo[] = {
	PHYSMBA0,
	PHYSMBA1,
};

mbastart(io, adcr, func)
register struct iob *io;
int *adcr;
{
	register int i;
	int npf;
	unsigned v;
	register struct pte *pte;
	int o;
	int vaddr;
	extern int mbanum[], *mbaloc[];
	register struct mba_regs *mbap;

	mbap = (struct mba_regs *)mbaloc[mbanum[io->i_ino.i_dev]];
	pte = &mbap->mba_map[128];
	v = btop(io->i_ma);
	o = (int)io->i_ma & PGOFSET;
	npf = btoc(io->i_cc + o);
	vaddr = (128 << 9) | o;
	v &= 0x1fffff;		/* drop to physical addr */
	while (--npf >= 0)
		*(int *)pte++ = v++ | PG_V;
	mbap->mba_sr = -1;	/* clear status (error) bits */
	mbap->mba_bcr = -io->i_cc;
	mbap->mba_var = vaddr;
	if (func == READ)
		*adcr = MBARCOM | GO;
	else if (func == WRITE) {
		*adcr = MBAWCOM | GO;
	}
}

mbainit(mbanum)
	int mbanum;
{
	register struct mba_regs *mbap = mbainfo[mbanum].mi_phys;

	mbap->mba_cr = MBAINIT;
	mbaact |= 1<<mbanum;
}
