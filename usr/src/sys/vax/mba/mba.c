/*	mba.c	3.1	%H%	*/

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

int mbaboff;

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
	extern int mbanum[], *mbaloc[];
	extern char buffers[][];

	mbap = (struct mba_regs *)mbaloc[mbanum[major(bp->b_dev)]];
	if ((bp->b_flags & B_PHYS) == 0)
		vaddr = (bp->b_un.b_addr - (char *)buffers) + mbaboff;
	else {
		io = (struct pte *)mbap;
		io += (MBA_MAP + 128*4)/4;
		v = btop(bp->b_un.b_addr);
		o = (int)bp->b_un.b_addr & PGOFSET;
		npf = btoc(bp->b_bcount + o);
		rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
		vaddr = (128 << 9) | o;
		if (bp->b_flags & B_UAREA) {
			for (i = 0; i < UPAGES; i++) {
				if (rp->p_addr[i].pg_pfnum == 0)
					panic("mba: zero upage");
				*(int *)io++ = rp->p_addr[i].pg_pfnum | PG_V;
			}
		} else if ((bp->b_flags & B_PHYS) == 0) {
			v &= 0x1fffff;		/* drop to physical addr */
			while (--npf >= 0)
				*(int *)io++ = v++ | PG_V;
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
	}
	mbap->mba_sr = -1;	/* clear status (error) bits */
	mbap->mba_bcr = -bp->b_bcount;
	mbap->mba_var = vaddr;
	if (bp->b_flags & B_READ)
		*adcr = MBARCOM | GO;
	else
		*adcr = MBAWCOM | GO;
}

mbainit()
{
	register int *io0, *io1, *b, t, j;
	extern int *mbaloc[];
	extern char buffers[][];

	io0 = mbaloc[0] + (MBA_MAP/4);
	io1 = mbaloc[1] + (MBA_MAP/4);
	b = (int *)Sysmap + ((((int) buffers)>>9)&PG_PFNUM);
	j = NBUF * CLSIZE + ((int)buffers & 0x1ff ? 1 : 0);
	do {
		t = PG_V | (*b++ & PG_PFNUM);
		*io0++ = t;
		*io1++ = t;
	} while (--j>0);
	*io0 = 0;		/* invalidate next entry */
	*io1 = 0;
	mbaboff = (int)buffers & 0x1ff;
}
