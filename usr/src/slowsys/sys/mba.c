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
	register int vaddr, com, *io, num;
	register struct mba_regs *mbap;
	int *pt, pf;
	extern int mbanum[], *mbaloc[];
	extern char buffers[][];

	mbap = (struct mba_regs *)(mbaloc[mbanum[major(bp->b_dev)]]);
	if ( (bp->b_flags & B_PHYS) == 0 ) {
		vaddr = (bp->b_un.b_addr - (char *)buffers) + mbaboff;
	} else {
		io = (int *)mbap;
		io += (MBA_MAP + 128*4)/4;
		num = btoc(bp->b_bcount) + 1;
		pf = (int)bp->b_un.b_addr >> 9;
		while ( num-->0)
			*io++ = 0x80000000 | pf++;
		vaddr = (128 << 9) | ((int)bp->b_un.b_addr & 0x1ff);
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
	b = Sysmap + ((((int) buffers)>>9)&0x1fffff);
	j = NBUF + ((int)buffers & 0x1ff ? 1 : 0);
	do {
		t = 0x80000000 | (*b++ & 0x1fffff);
		*io0++ = t;
		*io1++ = t;
	} while (--j>0);
	*io0 = 0;		/* invalidate next entry */
	*io1 = 0;
	mbaboff = (int)buffers & 0x1ff;
}
