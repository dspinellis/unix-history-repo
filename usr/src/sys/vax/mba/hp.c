/*	hp.c	4.24	81/03/07	*/

#include "hp.h"
#if NHP > 0
/*
 * HP disk driver for RP0x+RM0x
 *
 * TODO:
 *	Check RM80 skip sector handling, esp when ECC's occur later
 *	Add reading of bad sector information and disk layout from sector 1
 *	Add bad sector forwarding code
 *	Check interaction with tape driver on same mba
 *	Check multiple drive handling
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
#include "../h/mbareg.h"
#include "../h/mbavar.h"
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
struct	mba_device *hpinfo[NHP];
int	hpattach(),hpustart(),hpstart(),hpdtint();
struct	mba_driver hpdriver =
	{ hpattach, 0, hpustart, hpstart, hpdtint, 0,
	  hptypes, "hp", 0, hpinfo };

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

u_char	hp_offset[16] = {
    HP_P400, HP_M400, HP_P400, HP_M400, HP_P800, HP_M800, HP_P800, HP_M800,
    HP_P1200, HP_M1200, HP_P1200, HP_M1200, 0, 0, 0, 0,
};

struct	buf	rhpbuf[NHP];
char	hprecal[NHP];

#define	b_cylin b_resid
 
#ifdef INTRLVE
daddr_t dkblock();
#endif
 
int	hpseek;

/*ARGSUSED*/
hpattach(mi, slave)
	struct mba_device *mi;
{
	register struct hpst *st = &hpst[mi->mi_type];

	if (mi->mi_dk >= 0)
		dk_mspw[mi->mi_dk] = 1.0 / 60 / (st->nsect * 256);
}

hpstrategy(bp)
	register struct buf *bp;
{
	register struct mba_device *mi;
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
	register struct mba_device *mi;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st;
	daddr_t bn;
	int sn, dist, flags;

	if ((hpaddr->hpcs1&HP_DVA) == 0)
		return (MBU_BUSY);
	if ((hpaddr->hpds & HP_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		hpaddr->hpof = HP_FMT22;
	}
	if (mi->mi_tab.b_active || mi->mi_hd->mh_ndrive == 1)
		return (MBU_DODATA);
	if ((hpaddr->hpds & (HP_DPR|HP_MOL)) != (HP_DPR|HP_MOL))
		return (MBU_DODATA);
	st = &hpst[mi->mi_type];
	bn = dkblock(bp);
	sn = bn%st->nspc;
	sn = (sn+st->nsect-hpSDIST)%st->nsect;
	if (bp->b_cylin == (hpaddr->hpdc & 0xffff)) {
		if (hpseek)
			return (MBU_DODATA);
		dist = ((hpaddr->hpla & 0xffff)>>6) - st->nsect + 1;
		if (dist < 0)
			dist += st->nsect;
		if (dist > st->nsect - hpRDIST)
			return (MBU_DODATA);
	} else
		hpaddr->hpdc = bp->b_cylin;
	if (hpseek)
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
	else {
		hpaddr->hpda = sn;
		hpaddr->hpcs1 = HP_SEARCH|HP_GO;
	}
	return (MBU_STARTED);
}

hpstart(mi)
	register struct mba_device *mi;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st = &hpst[mi->mi_type];
	daddr_t bn;
	int sn, tn;

	bn = dkblock(bp);
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	if (mi->mi_tab.b_errcnt >= 16 && (bp->b_flags&B_READ) != 0) {
		hpaddr->hpof = hp_offset[mi->mi_tab.b_errcnt & 017] | HP_FMT22;
		hpaddr->hpcs1 = HP_OFFSET|HP_GO;
		while (hpaddr->hpds & HP_PIP)
			;
		mbclrattn(mi);
	}
	hpaddr->hpdc = bp->b_cylin;
	hpaddr->hpda = (tn << 8) + sn;
}

hpdtint(mi, mbasr)
	register struct mba_device *mi;
	int mbasr;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	int retry = 0;

	if (hpaddr->hpds&HP_ERR || mbasr&MBAEBITS) {
		if (hpaddr->hper1&HP_WLE) {
			printf("hp%d: write locked\n", dkunit(bp));
			bp->b_flags |= B_ERROR;
		} else if (++mi->mi_tab.b_errcnt > 27 ||
		    mbasr & MBASR_HARD ||
		    hpaddr->hper1 & HPER1_HARD ||
		    hpaddr->hper2 & HPER2_HARD) {
			harderr(bp, "hp");
			printf("mbasr=%b er1=%b er2=%b\n",
			    mbasr, mbasr_bits,
			    hpaddr->hper1, HPER1_BITS,
			    hpaddr->hper2, HPER2_BITS);
			bp->b_flags |= B_ERROR;
#ifdef notdef
		} else if (hpaddr->hper2&HP_SSE) {
			hpecc(mi, 1);
			return (MBD_RESTARTED);
#endif
		} else if ((hpaddr->hper1&(HP_DCK|HP_ECH)) == HP_DCK) {
			if (hpecc(mi, 0))
				return (MBD_RESTARTED);
			/* else done */
		} else
			retry = 1;
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		if ((mi->mi_tab.b_errcnt&07) == 4) {
			hpaddr->hpcs1 = HP_RECAL|HP_GO;
			hprecal[mi->mi_unit] = 1;
			return (MBD_RESTARTED);
		}
		if (retry)
			return (MBD_RETRY);
	}
	if (hprecal[mi->mi_unit]) {
		hprecal[mi->mi_unit] = 0;
		return (MBD_RETRY);
	}
	bp->b_resid = -(mi->mi_mba->mba_bcr) & 0xffff;
	if (mi->mi_tab.b_errcnt > 16) {
		hpaddr->hpcs1 = HP_RTC|HP_GO;
		while (hpaddr->hpds & HP_PIP)
			;
		mbclrattn(mi);
	}
	hpaddr->hpcs1 = HP_RELEASE|HP_GO;
	return (MBD_DONE);
}

hpread(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NHP)
		u.u_error = ENXIO;
	else
		physio(hpstrategy, &rhpbuf[unit], dev, B_READ, minphys);
}

hpwrite(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NHP)
		u.u_error = ENXIO;
	else
		physio(hpstrategy, &rhpbuf[unit], dev, B_WRITE, minphys);
}

hpecc(mi, rm80sse)
	register struct mba_device *mi;
	int rm80sse;
{
	register struct mba_regs *mbp = mi->mi_mba;
	register struct hpdevice *rp = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o;
	int bn, cn, tn, sn;
	struct pte mpte;
	int bcr;

	bcr = mbp->mba_bcr & 0xffff;
	if (bcr)
		bcr |= 0xffff0000;		/* sxt */
	npf = btop(bcr + bp->b_bcount) - 1;
	reg = npf;
#ifdef notdef
	if (rm80sse) {
		rp->hpof |= HP_SSEI;
		reg--;		/* compensate in advance for reg-- below */
		goto sse;
	}
#endif
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("hp%d%c: soft ecc sn%d\n", dkunit(bp),
	    'a'+(minor(bp->b_dev)&07), bp->b_blkno + npf);
	mask = rp->hpec2&0xffff;
	i = (rp->hpec1&0xffff) - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
		mpte = mbp->mba_map[reg+btop(byte)];
		addr = ptob(mpte.pg_pfnum) + (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	if (bcr == 0)
		return (0);
#ifdef notdef
sse:
	if (rpof&HP_SSEI)
		rp->hpda = rp->hpda + 1;
	rp->hper1 = 0;
	rp->hpcs1 = HP_RCOM|HP_GO;
#else
sse:
	rp->hpcs1 = HP_DCLR|HP_GO;
	bn = dkblock(bp);
	st = &hpst[mi->mi_type];
	cn = bp->b_cylin;
	sn = bn%(st->nspc) + npf + 1;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
#ifdef notdef
	if (rp->hpof&SSEI)
		sn++;
#endif
	rp->hpdc = cn;
	rp->hpda = (tn<<8) + sn;
	mbp->mba_sr = -1;
	mbp->mba_var = (int)ptob(reg+1) + o;
	rp->hpcs1 = HP_RCOM|HP_GO;
#endif
	return (1);
}

#define	DBSIZE	20

hpdump(dev)
	dev_t dev;
{
	register struct mba_device *mi;
	register struct mba_regs *mba;
	struct hpdevice *hpaddr;
	char *start;
	int num, unit;
	register struct hpst *st;

	num = maxfree;
	start = 0;
	unit = minor(dev) >> 3;
	if (unit >= NHP)
		return (ENXIO);
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	mi = phys(hpinfo[unit],struct mba_device *);
	if (mi == 0 || mi->mi_alive == 0)
		return (ENXIO);
	mba = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
	mba->mba_cr = MBAINIT;
	hpaddr = (struct hpdevice *)&mba->mba_drv[mi->mi_drive];
	if ((hpaddr->hpds & HP_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		hpaddr->hpof = HP_FMT22;
	}
	st = &hpst[mi->mi_type];
	if (dumplo < 0 || dumplo + num >= st->sizes[minor(dev)&07].nblocks)
		return (EINVAL);
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
		hpaddr->hpcs1 = HP_WCOM | HP_GO;
		while ((hpaddr->hpds & HP_DRY) == 0)
			;
		if (hpaddr->hpds&HP_ERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
