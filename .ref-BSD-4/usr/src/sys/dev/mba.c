/*	mba.c	4.2	11/9/80	*/

#include "../h/param.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/vm.h"

/*
 * startup routine for MBA controllers.
 */
#define	MBAWCOM	0x30
#define	MBARCOM	0x38
#define	GO	01

mbastart(bp, adcr)
	register struct buf *bp;
	int *adcr;
{
	register int i;
	int npf;
	unsigned v;
	register struct pte *pte, *io;
	int o;
	int vaddr;
	register struct mba_regs *mbap;
	struct proc *rp;

	mbap = mbainfo[mbanum[major(bp->b_dev)]].mi_loc;
	io = mbap->mba_map;
	v = btop(bp->b_un.b_addr);
	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = btoc(bp->b_bcount + o);
	rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
	vaddr = o;
	if (bp->b_flags & B_UAREA) {
		for (i = 0; i < UPAGES; i++) {
			if (rp->p_addr[i].pg_pfnum == 0)
				panic("mba: zero upage");
			*(int *)io++ = rp->p_addr[i].pg_pfnum | PG_V;
		}
	} else if ((bp->b_flags & B_PHYS) == 0) {
		pte = &Sysmap[btop(((int)bp->b_un.b_addr)&0x7fffffff)];
		while (--npf >= 0)
			*(int *)io++ = pte++->pg_pfnum | PG_V;
	} else {
		if (bp->b_flags & B_PAGET)
			pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
		else
			pte = vtopte(rp, v);
		while (--npf >= 0) {
			if (pte->pg_pfnum == 0)
				panic("mba, zero entry");
			*(int *)io++ = pte++->pg_pfnum | PG_V;
		}
	}
	*(int *)io++ = 0;
	mbap->mba_sr = -1;	/* clear status (error) bits */
	mbap->mba_bcr = -bp->b_bcount;
	mbap->mba_var = vaddr;
	if (bp->b_flags & B_READ)
		*adcr = MBARCOM | GO;
	else
		*adcr = MBAWCOM | GO;
}

mbainit(mbano)
	int mbano;
{
	register struct pte *b;
	register int i;
	register struct mba_info *mi;
	register struct mba_regs *mbap;
	unsigned v;

	mi = &mbainfo[mbano];
	v = btop((int)mi->mi_phys);
	b = mi->mi_map;
	for (i = 0; i < 8192; i += NBPG) {
		*(int *)b++ = PG_V | PG_KW | v;
		mtpr(TBIS, ptob(v));
		v++;
	}
	mbap = mi->mi_loc;
	mbap->mba_cr = MBAINIT;
	mbap->mba_cr = MBAIE;
	mbaact |= (1<<mbano);
}
