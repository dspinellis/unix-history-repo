/*	uba.c	4.7	%G%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/cpu.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/uba.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/vm.h"
#include "../h/conf.h"
#include "../h/mtpr.h"
#include "../h/nexus.h"

/*
 * Allocate and setup UBA map registers, and bdp's
 * Flags says whether bdp is needed, whether the caller can't
 * wait (e.g. if the caller is at interrupt level).
 *
 * Return value (cruft):
 *	Bits 0-8	Byte offset
 *	Bits 9-17	Start map reg. no.
 *	Bits 18-27	No. mapping reg's
 *	Bits 28-31	BDP no.
 */
ubasetup(uban, bp, flags)
	struct buf *bp;
{
	register struct uba_hd *uh = &uba_hd[uban];
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
	while ((reg = malloc(uh->uh_map, npf)) == 0) {
		if (flags & UBA_CANTWAIT)
			return (0);
		uh->uh_mrwant++;
		sleep((caddr_t)uh->uh_map, PSWP);
	}
	reg--;
	bdp = 0;
	if (flags & UBA_NEEDBDP) {
		while ((bdp = ffs(uh->uh_bdpfree)) == 0) {
			if (flags & UBA_CANTWAIT) {
				mfree(uh->uh_map, npf, reg);
				return (0);
			}
			uh->uh_bdpwant++;
			sleep((caddr_t)uh->uh_map, PSWP);
		}
		uh->uh_bdpfree &= ~ (1<<bdp);
	}
	splx(a);
	ubinfo = (bdp << 28) | (npf << 18) | (reg << 9) | o;
	io = &uh->uh_uba->uba_map[reg];
	temp = (bdp << 21) | UBA_MRV;
	rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
	if (bdp && (o & 01))
		temp |= UBA_BO;
	if (bp->b_flags & B_UAREA) {
		for (i = UPAGES - bp->b_bcount / NBPG; i < UPAGES; i++) {
			if (rp->p_addr[i].pg_pfnum == 0)
				panic("uba: zero upage");
			*(int *)io++ = rp->p_addr[i].pg_pfnum | temp;
		}
	} else if ((bp->b_flags & B_PHYS) == 0) {
		pte = &Sysmap[btop(((int)bp->b_un.b_addr)&0x7fffffff)];
		while (--npf != 0)
			*(int *)io++ = pte++->pg_pfnum | temp;
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

/*
 * Non buffer unibus interface... set up a buffer and call ubasetup.
 */
uballoc(uban, addr, bcnt, flags)
	caddr_t addr;
	unsigned short bcnt;
{
	struct buf ubabuf;

	ubabuf.b_un.b_addr = addr;
	ubabuf.b_flags = B_BUSY;
	ubabuf.b_bcount = bcnt;
	/* that's all the fields ubasetup() needs */
	return (ubasetup(uban, &ubabuf, flags));
}
 
/*
 * Old ubafree(info) is now ubarelse(&info) to avoid races.
 */
ubarelse(uban, amr)
	int *amr;
{
	register struct uba_hd *uh = &uba_hd[uban];
	register int bdp, reg, npf, a;
	int mr;
 
	a = spl6();
	mr = *amr;
	if (mr == 0) {
		splx(a);
		return;
	}
	*amr = 0;
	bdp = (mr >> 28) & 0x0f;
	if (bdp) {
		uh->uh_uba->uba_dpr[bdp] |= UBA_BNE;
		uh->uh_bdpfree |= 1 << bdp;
		if (uh->uh_bdpwant) {
			uh->uh_bdpwant = 0;
			wakeup((caddr_t)uh->uh_map);
		}
	}
	npf = (mr >> 18) & 0x3ff;
	reg = ((mr >> 9) & 0x1ff) + 1;
	mfree(uh->uh_map, npf, reg);
	if (uh->uh_mrwant) {
		uh->uh_mrwant = 0;
		wakeup((caddr_t)uh->uh_map);
	}
	splx(a);
}

#define	DELAY(N)	{ register int d; d = N; while (--d > 0); }

ubareset(uban)
{
	struct uba_regs *up;
	register struct cdevsw *cdp;
	int s;

	s = spl6();
	switch (cpu) {
#if VAX==780
	case VAX_780:
		printf("UBA RESET %d:", uban);
		ubainit(uba_hd[uban].uh_uba);
		break;
#endif
#if VAX==750
	case VAX_750:
		printf("UNIBUS INIT:");
		mtpr(IUR, 1);
		DELAY(100000);
		break;
#endif
	}
	for (cdp = cdevsw; cdp->d_open; cdp++)
		(*cdp->d_reset)(uban);
	printf("\n");
	splx(s);
}

/* pointer rather than number so we can be called with virt and phys addrs */
ubainit(up)
	register struct uba_regs *up;
{

	up->uba_cr = UBA_ADINIT;
	up->uba_cr = UBA_IFS|UBA_BRIE|UBA_USEFIE|UBA_SUEFIE;
	while ((up->uba_cnfgr & UBA_UBIC) == 0)
		;
}

#if VAX780
unhang()
{
	register int uban;

	for (uban = 0; uban < numuba; uban++) {
		register struct uba_hd *uh = &uba_hd[uban];
		register struct uba_regs *up = uh->uh_uba;

		if (up->uba_sr == 0)
			return;
		uh->uh_hangcnt++;
		if (uh->uh_hangcnt > 5*HZ) {
			uh->uh_hangcnt = 0;
			printf("HANG ");
			ubareset(uban);
		}
	}
}

/* timeout routine to decrement ``i forgot to interrupt counts */
/* this prevents the counts from growing slowly, which isn't interesting */
ubawatch()
{
	register struct uba_hd *uh;
	register int uban;

	for (uban = 0; uban < numuba; uban++) {
		uh = &uba_hd[uban];
		if (uh->uh_hangcnt)
			uh->uh_hangcnt--;
	}
}

/* called from locore.s; parameters here (i.e. uvec) are value-result! */
ubaerror(uban, uh, xx, uvec, uba)
	register int uban;
	register struct uba_hd *uh;
	int uvec;
	register struct uba_regs *uba;
{
	register sr, s;

	if (uvec == 0) {
		uh->uh_zvcnt++;
		if (uh->uh_zvcnt > 250000) {
			printf("ZERO VECTOR ");
			ubareset(uban);
		}
		uvec = 0;
		return;
	}
	if (uba->uba_cnfgr & NEX_CFGFLT) {
		printf("UBA%d SBI FAULT sr %x cnfgr %x\n",
		    uban, uba->uba_sr, uba->uba_cnfgr);
		ubareset(uban);
		uvec = 0;
		return;
	}
	sr = uba->uba_sr;
	s = spl7();
	printf("UBA%d ERROR SR %x FMER %x FUBAR %o\n",
	    uba->uba_sr, uba->uba_fmer, uba->uba_fubar);
	splx(s);
	uba->uba_sr = sr;
	uvec &= UBA_DIV;
	return;
}
#endif
