/*	uba.c	3.3	%G%	*/

#include "../h/param.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/vm.h"
#include "../h/conf.h"

/*
 * Allocate as many contiguous UBA mapping registers
 * as are necessary to do transfer of bcnt bytes
 * to/from location baddr.  Wait for enough map registers.
 *
 * Bdpflg is non-zero if a "buffered data path" (BDP) is
 * to be used, else 0 -> use direct data path (DDP).  Return
 *
 *	Bits 0-8	Byte offset
 *	Bits 9-17	Start map reg. no.
 *	Bits 18-27	No. mapping reg's
 *	Bits 28-31	BDP no.
 */
ubasetup(bp, bdpflg)
struct buf *bp;
{
	register int temp, i;
	int npf, reg, bdp;
	unsigned v;
	register struct pte *pte, *io;
	struct proc *rp;
	int a, o, ubinfo;

	v = btop(bp->b_un.b_addr);
	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = btoc(bp->b_bcount + o) + 1;
	a = spl6();
	while ((reg = malloc(ubamap, npf)) == 0) {
		umrwant++;
		sleep((caddr_t)ubamap, PSWP);
	}
	reg--;
	bdp = 0;
	if (bdpflg)
		while ((bdp = malloc(bdpmap, 1)) == 0) {
			bdpwant++;
			sleep((caddr_t)bdpmap, PSWP);
		}
	splx(a);
	ubinfo = (bdp << 28) | (npf << 18) | (reg << 9) | o;
	io = &(((struct uba_regs *)UBA0)->uba_map[reg]);
	temp = (bdp << 21) | MRV;
	rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
	if (bdp && (o & 01))
		temp |= BO;
	if (bp->b_flags & B_UAREA) {
		for (i = UPAGES - bp->b_bcount / NBPG; i < UPAGES; i++) {
			if (rp->p_addr[i].pg_pfnum == 0)
				panic("uba: zero upage");
			*(int *)io++ = rp->p_addr[i].pg_pfnum | temp;
		}
	} else if ((bp->b_flags & B_PHYS) == 0) {
		v &= 0x1fffff;			/* drop to physical addr */
		while (--npf != 0)
			*(int *)io++ = v++ | temp;
	} else {
		if (bp->b_flags & B_PAGET)
			pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
		else
			pte = vtopte(rp, v);
		while (--npf != 0) {
			if (pte->pg_pfnum == 0)
				panic("uba zero uentry");
			*(int *)io++ = pte++->pg_pfnum | temp;
		}
	}
	*(int *)io++ = 0;
	return (ubinfo);
}

struct	buf ubabuf;
/*
 * Non buffer unibus interface... set up a buffer and call ubasetup.
 */
uballoc(addr, bcnt, bdpflg)
	caddr_t addr;
	unsigned short bcnt;
{
	register int a, ubinfo;

	a = spl6();
	while (ubabuf.b_flags & B_BUSY) {
		ubabuf.b_flags |= B_WANTED;
		sleep((caddr_t)&ubabuf, PRIUBA);
	}
	ubabuf.b_un.b_addr = addr;
	ubabuf.b_flags = B_BUSY;
	ubabuf.b_bcount = bcnt;
	splx(a);
	ubinfo = ubasetup(&ubabuf, bdpflg);
	ubabuf.b_flags &= ~B_BUSY;
	if (ubabuf.b_flags & B_WANTED)
		wakeup((caddr_t)&ubabuf);
	return (ubinfo);
}
 
ubafree(mr)
	int mr;
{
	register int bdp, reg, npf, a;
 
	a = spl6();
	bdp = (mr >> 28) & 0x0f;
	if (bdp) {
		((struct uba_regs *)UBA0)->uba_dpr[bdp] |= BNE;	/* purge */
		mfree(bdpmap, 1, bdp);
		if (bdpwant) {
			bdpwant = 0;
			wakeup((caddr_t)bdpmap);
		}
	}
	npf = (mr >> 18) & 0x3ff;
	reg = ((mr >> 9) & 0x1ff) + 1;
	mfree(ubamap, npf, reg);
	if (umrwant) {
		umrwant = 0;
		wakeup((caddr_t)ubamap);
	}
	splx(a);
}

ubainit()
{

	mfree(ubamap, 496, 1);
	mfree(bdpmap, 15, 1);
}

#define	DELAY(N)	{ register int d; d = N; while (--d > 0); }

ubareset()
{
	struct uba_regs *up = (struct uba_regs *)UBA0;
	register struct cdevsw *cdp;
	int i;

	(void) spl6();
	printf("UBA RESET:");
	up->uba_cr = ADINIT;
	up->uba_cr = IFS|BRIE|USEFIE|SUEFIE;
	while ((up->uba_cnfgr & UBIC) == 0)
		;
	for (cdp = cdevsw; cdp->d_open; cdp++)
		(*cdp->d_reset)();
	printf("\n");
	(void) spl0();
}
