/*	hp.c	4.8	81/02/10	*/

#include "hp.h"
#if NHP > 0
/*
 * RP/RM disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "../h/cmap.h"

#include "../h/hpreg.h"

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct	size {
	daddr_t	nblocks;
	int	cyloff;
} hp_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 37 */
	33440,	38,		/* B=cyl 38 thru 117 */
	340670,	0,		/* C=cyl 0 thru 814 */
	0,	0,
	0,	0,
	0,	0,
	291346,	118,		/* G=cyl 118 thru 814 */
	0,	0,
}, rm_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 99 */
	33440,	100,		/* B=cyl 100 thru 309 */
	131680,	0,		/* C=cyl 0 thru 822 */
	2720,	291,
	0,	0,
	0,	0,
	82080,	310,		/* G=cyl 310 thru 822 */
	0,	0,
}, rm5_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	500992,	0,		/* C=cyl 0 thru 823 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
	86944,	681,		/* F=cyl 681 thru 823 */
	159296,	562,		/* G=cyl 562 thru 823 */
	291346,	82,		/* H=cyl 82 thru 561 */
}, rm80_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 36 */
	33440,	37,		/* B=cyl 37 thru 114 */
	242606,	0,		/* C=cyl 0 thru 558 */
	0,	0,
	0,	0,
	0,	0,
	82080,	115,		/* G=cyl 115 thru 304 */
	110236,	305,		/* H=cyl 305 thru 558 */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

#define	_hpSDIST	2
#define	_hpRDIST	3

int	hpSDIST = _hpSDIST;
int	hpRDIST = _hpRDIST;

short	hptypes[] =
	{ MBDT_RM03, MBDT_RM05, MBDT_RP06, MBDT_RM80, 0 };
struct	mba_info *hpinfo[NHP];
int	hpdkinit(),hpustart(),hpstart(),hpdtint();
struct	mba_driver hpdriver =
	{ hpdkinit, hpustart, hpstart, hpdtint, 0, hptypes, hpinfo };

struct hpst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	struct	size *sizes;
} hpst[] = {
	32,	5,	32*5,	823,	rm_sizes,	/* RM03 */
	32,	19,	32*19,	823,	rm5_sizes,	/* RM05 */
	22,	19,	22*19,	815,	hp_sizes,	/* RP06 */
	31,	14, 	31*14,	559,	rm80_sizes	/* RM80 */
};

int	hp_offset[16] = {
	P400, M400, P400, M400,
	P800, M800, P800, M800,
	P1200, M1200, P1200, M1200,
	0, 0, 0, 0,
};

struct	buf	rhpbuf;

#define	b_cylin b_resid
 
#ifdef INTRLVE
daddr_t dkblock();
#endif
 
hpstrategy(bp)
	register struct buf *bp;
{
	register struct mba_info *mi;
	register struct hpst *st;
	register int unit;
	long sz, bn;
	int xunit = minor(bp->b_dev) & 07;

	sz = bp->b_bcount;
	sz = (sz+511) >> 9;
	unit = dkunit(bp);
	if (unit >= NHP)
		goto bad;
	mi = hpinfo[unit];
	if (mi == 0 || mi->mi_alive == 0)
		goto bad;
	st = &hpst[mi->mi_type];
	if (bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > st->sizes[xunit].nblocks)
		goto bad;
	bp->b_cylin = bn/st->nspc + st->sizes[xunit].cyloff;
	(void) spl5();
	disksort(&mi->mi_tab, bp);
	if (mi->mi_tab.b_active == 0)
		mbustart(mi);
	(void) spl0();
	return;

bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

hpustart(mi)
	register struct mba_info *mi;
{
	register struct device *hpaddr = (struct device *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st;
	daddr_t bn;
	int sn, dist, flags;

	if ((hpaddr->hpcs1&DVA) == 0)
		return (MBU_BUSY);
	if ((hpaddr->hpds & VV) == 0) {
		hpaddr->hpcs1 = DCLR|GO;
		hpaddr->hpcs1 = PRESET|GO;
		hpaddr->hpof = FMT22;
	}
	if (mi->mi_tab.b_active)
		return (MBU_DODATA);
	if ((hpaddr->hpds & (DPR|MOL)) != (DPR|MOL))
		return (MBU_DODATA);
	if (flags&MH_NOSEEK)
		return (MBU_DODATA);
	hpaddr->hpdc = bp->b_cylin;
	st = &hpst[mi->mi_type];
	bn = dkblock(bp);
	sn = bn%st->nspc;
	sn = (sn+st->nsect-hpSDIST)%st->nsect;
	flags = mi->mi_hd->mh_flags;
	if (bp->b_cylin == (hpaddr->hpdc & 0xffff)) {
		if (flags&MH_NOSEARCH)
			return (MBU_DODATA);
		dist = ((hpaddr->hpla & 0xffff)>>6) - st->nsect + 1;
		if (dist < 0)
			dist += st->nsect;
		if (dist > st->nsect - hpRDIST)
			return (MBU_DODATA);
	}
	if (flags&MH_NOSEARCH)
		hpaddr->hpcs1 = SEEK|GO;
	else {
		hpaddr->hpda = sn;
		hpaddr->hpcs1 = SEARCH|GO;
	}
	return (MBU_STARTED);
}

hpstart(mi)
	register struct mba_info *mi;
{
	register struct device *hpaddr = (struct device *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st = &hpst[mi->mi_type];
	daddr_t bn;
	int sn, tn;

	bn = dkblock(bp);
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	if (mi->mi_tab.b_errcnt >= 16 && (bp->b_flags&B_READ) != 0) {
		hpaddr->hpof = hp_offset[mi->mi_tab.b_errcnt & 017] | FMT22;
		hpaddr->hpcs1 = OFFSET|GO;
		while (hpaddr->hpds & PIP)
			;
		mbclrattn(mi);
	}
	hpaddr->hpdc = bp->b_cylin;
	hpaddr->hpda = (tn << 8) + sn;
}

hpdtint(mi, mbastat)
	register struct mba_info *mi;
	int mbastat;
{
	register struct device *hpaddr = (struct device *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;

	while ((hpaddr->hpds & DRY) == 0)	/* shouldn't happen */
		printf("hp dry not set\n");
	if (hpaddr->hpds & ERR || mbastat & MBAEBITS)
		if (++mi->mi_tab.b_errcnt < 28 && (hpaddr->hper1&WLE) == 0) {
			if ((hpaddr->hper1&0xffff) != DCK) {
				hpaddr->hpcs1 = DCLR|GO;
				if ((mi->mi_tab.b_errcnt&07) == 4) {
					hpaddr->hpcs1 = RECAL|GO;
					while (hpaddr->hpds & PIP)
						;
					mbclrattn(mi);
				}
				return (MBD_RETRY);
			} else if (hpecc(mi))
				return (MBD_RESTARTED);
		} else {
			deverror(bp, mbastat, hpaddr->hper1);
			bp->b_flags |= B_ERROR;
		}
	bp->b_resid = -(mi->mi_mba->mba_bcr) & 0xffff;
	if (mi->mi_tab.b_errcnt) {
		hpaddr->hpcs1 = RTC|GO;
		while (hpaddr->hpds & PIP)
			;
		mbclrattn(mi);
	}
	hpaddr->hpcs1 = RELEASE|GO;
	return (MBD_DONE);
}

hpread(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_READ, minphys);
}

hpwrite(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_WRITE, minphys);
}

hpecc(mi)
	register struct mba_info *mi;
{
	register struct mba_regs *mbp = mi->mi_mba;
	register struct device *rp = (struct device *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o;
	int bn, cn, tn, sn;
	struct pte mpte;
	int bcr;

	/*
	 * Npf is the number of sectors transferred before the sector
	 * containing the ECC error, and reg is the MBA register
	 * mapping (the first part of)the transfer.
	 * O is offset within a memory page of the first byte transferred.
	 */
	bcr = mbp->mba_bcr & 0xffff;
	if (bcr)
		bcr |= 0xffff0000;		/* sxt */
	npf = btop(bcr + bp->b_bcount) - 1;
	reg = npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("%D ", bp->b_blkno + npf);
	prdev("ECC", bp->b_dev);
	mask = rp->hpec2&0xffff;
	if (mask == 0) {
		rp->hpof = FMT22;
		return (0);
	}

	/*
	 * Compute the byte and bit position of the error.
	 * The variable i is the byte offset in the transfer,
	 * the variable byte is the offset from a page boundary
	 * in main memory.
	 */
	i = (rp->hpec1&0xffff) - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	/*
	 * Correct while possible bits remain of mask.  Since mask
	 * contains 11 bits, we continue while the bit offset is > -11.
	 * Also watch out for end of this block and the end of the whole
	 * transfer.
	 */
	while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
		mpte = mbp->mba_map[reg+btop(byte)];
		addr = ptob(mpte.pg_pfnum) + (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	mi->mi_hd->mh_active++;		/* Either complete or continuing */
	if (bcr == 0)
		return (0);
	/*
	 * Have to continue the transfer... clear the drive,
	 * and compute the position where the transfer is to continue.
	 * We have completed npf+1 sectores of the transfer already;
	 * restart at offset o of next sector (i.e. in MBA register reg+1).
	 */
	rp->hpcs1 = DCLR|GO;
	bn = dkblock(bp);
	st = &hpst[mi->mi_type];
	cn = bp->b_cylin;
	sn = bn%(st->nspc) + npf + 1;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
	rp->hpdc = cn;
	rp->hpda = (tn<<8) + sn;
	mbp->mba_sr = -1;
	mbp->mba_var = (int)ptob(reg+1) + o;
	rp->hpcs1 = RCOM|GO;
	return (1);
}

#define	DBSIZE	20

hpdump(dev)
	dev_t dev;
{
	register struct mba_info *mi;
	register struct mba_regs *mba;
	struct device *hpaddr;
	char *start;
	int num, unit;
	register struct hpst *st;

	num = maxfree;
	start = 0;
	unit = minor(dev) >> 3;
	if (unit >= NHP) {
		printf("bad unit\n");
		return (-1);
	}
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	mi = phys(hpinfo[unit],struct mba_info *);
	if (mi->mi_alive == 0) {
		printf("dna\n");
		return (-1);
	}
	mba = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
	mba->mba_cr = MBAINIT;
	hpaddr = (struct device *)&mba->mba_drv[mi->mi_drive];
	if ((hpaddr->hpds & VV) == 0) {
		hpaddr->hpcs1 = DCLR|GO;
		hpaddr->hpcs1 = PRESET|GO;
		hpaddr->hpof = FMT22;
	}
	st = &hpst[mi->mi_type];
	if (dumplo < 0 || dumplo + num >= st->sizes[minor(dev)&07].nblocks) {
		printf("oor\n");
		return (-1);
	}
	while (num > 0) {
		register struct pte *hpte = mba->mba_map;
		register int i;
		int blk, cn, sn, tn;
		daddr_t bn;

		blk = num > DBSIZE ? DBSIZE : num;
		bn = dumplo + btop(start);
		cn = bn/st->nspc + st->sizes[minor(dev)&07].cyloff;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		hpaddr->hpdc = cn;
		hpaddr->hpda = (tn << 8) + sn;
		for (i = 0; i < blk; i++)
			*(int *)hpte++ = (btop(start)+i) | PG_V;
		mba->mba_sr = -1;
		mba->mba_bcr = -(blk*NBPG);
		mba->mba_var = 0;
		hpaddr->hpcs1 = WCOM | GO;
		while ((hpaddr->hpds & DRY) == 0)
			;
		if (hpaddr->hpds&ERR) {
			printf("dskerr: (%d,%d,%d) ds=%x er=%x\n",
			    cn, tn, sn, hpaddr->hpds, hpaddr->hper1);
			return (-1);
		}
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}

hpdkinit()
{
	/* I don't really care what this does .. kre */
}
#endif
