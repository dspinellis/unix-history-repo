#
/*
 */

#include "../h/param.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/page.h"
#include "../h/uba.h"
#include "../h/map.h"
#include "../h/mba.h"
#include "../h/mtpr.h"

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
	int vaddr, com;
	register int *io, num;
	register struct mba_regs *mbap;
	register int *pt, i;
	int pf;
	extern int mbanum[], *mbaloc[], mbautl[], Mbamap[];
	extern char buffers[][];

	mbap = (struct mba_regs *)mbaloc[mbanum[major(bp->b_dev)]];
	if ( (bp->b_flags & B_PHYS) == 0 ) {
		vaddr = (bp->b_un.b_addr - (char *)buffers) + mbaboff;
	} else {
		ptaccess(bp->b_proc, Mbamap, mbautl);	/* get to u-area & page tables */
		io = (int *)mbap;
		io += (MBA_MAP + 128*4)/4;
		vaddr = (128 << 9) | ((int)bp->b_un.b_addr & 0x1ff);
		pf = (int)bp->b_un.b_addr >> 9;
		num = (((int)bp->b_un.b_addr + bp->b_bcount - 1) >> 9) - pf +1;
		if ((bp -> b_flags & B_UAREA) != 0)  {	/* u-area should be part of the I/O */
			for(i=0; i<UPAGES; i++) {
			    if ((*io++ = PG_V | bp->b_proc->p_addr[i])==PG_V)
				panic("mba, zero u-page");
			}
			num -= UPAGES;
		}
		if (pf & 0x200000) {	/* I/O to stack */
			i = ((struct user *)mbautl)->u_pcb.pcb_szpt;
			pf = i*128 + pf - 0x400000;
		}
		pt = mbautl + UPAGES*128 + pf;
		while ( num-->0) {
			if ((*io++ = *pt++ | PG_V)==PG_V)
				panic("mba, zero entry");
		}
	}
	mbap->mba_sr = -1;	/* clear status (error) bits */
	mbap->mba_bcr = -bp->b_bcount;
	mbap->mba_var = vaddr;
	if (bp->b_flags & B_READ)
		com = MBARCOM | GO;
	else
		com = MBAWCOM | GO;

	*adcr = com;		/* set cmd in device control and status register */
}

mbainit()
{
	register int *io0, *io1, *b, t, j;
	extern int Sysmap[], *mbaloc[];
	extern char buffers[][];

	io0 = mbaloc[0] + (MBA_MAP/4);
	io1 = mbaloc[1] + (MBA_MAP/4);
	b = Sysmap + ((((int) buffers)>>9)&PG_PFNUM);
	j = NBUF + ((int)buffers & 0x1ff ? 1 : 0);
	do {
		t = PG_V | (*b++ & PG_PFNUM);
		*io0++ = t;
		*io1++ = t;
	} while (--j>0);
	*io0 = 0;		/* invalidate next entry */
	*io1 = 0;
	mbaboff = (int)buffers & 0x1ff;
}
