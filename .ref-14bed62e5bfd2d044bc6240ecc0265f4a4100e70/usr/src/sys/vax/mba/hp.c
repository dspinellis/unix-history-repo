/*	hp.c	4.43	82/01/17	*/

#ifdef HPDEBUG
int	hpdebug;
#endif
#ifdef HPBDEBUG
int	hpbdebug;
#endif

#include "hp.h"
#if NHP > 0
/*
 * HP disk driver for RP0x+RMxx
 *
 * TODO:
 *	check RM80 skip sector handling when ECC's occur later
 *	check offset recovery handling
 *	see if DCLR and/or RELEASE set attention status
 *	print bits of mr && mr2 symbolically
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
#include "../h/dkbad.h"

#include "../h/hpreg.h"

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct	size {
	daddr_t	nblocks;
	int	cyloff;
} hp6_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 37 */
	33440,	38,		/* B=cyl 38 thru 117 */
	340670,	0,		/* C=cyl 0 thru 814 */
	0,	0,
	0,	0,
	0,	0,
#ifndef NOBADSECT
	291280,	118,		/* G=cyl 118 thru 814 */
#else
	291346,	118,
#endif
	0,	0,
}, rm3_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 99 */
	33440,	100,		/* B=cyl 100 thru 309 */
	131680,	0,		/* C=cyl 0 thru 822 */
	0,	0,
	0,	0,
	0,	0,
#ifndef NOBADSECT
	81984,	310,		/* G=cyl 310 thru 822 */
#else
	82080,	310,
#endif
	0,	0,
}, rm5_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	500384,	0,		/* C=cyl 0 thru 822 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
#ifndef NOBADSECT
	86240,	681,		/* F=cyl 681 thru 822 */
	158592,	562,		/* G=cyl 562 thru 822 */
#else
	86636,	681,
	158688,	562,
#endif
	291346,	82,		/* H=cyl 82 thru 561 */
}, rm80_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 36 */
	33440,	37,		/* B=cyl 37 thru 114 */
	242606,	0,		/* C=cyl 0 thru 558 */
	0,	0,
	0,	0,
	0,	0,
	82080,	115,		/* G=cyl 115 thru 304 */
	110143,	305,		/* H=cyl 305 thru 558 */
}, hp7_sizes[8] = {
	15844,	0,		/* A=cyl 0 thru 9 */
	64000,	10,		/* B=cyl 10 thru 49 */
	1008000,0,		/* C=cyl 0 thru 629 */
	15884,	330,		/* D=cyl 330 thru 339 */
	256000,	340,		/* E=cyl 340 thru 499 */
	207850,	500,		/* F=cyl 500 thru 629 */
	479850,	330,		/* G=cyl 330 thru 629 */
	448000,	50,		/* H=cyl 50 thru 329 */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

#define	_hpSDIST	2
#define	_hpRDIST	3

int	hpSDIST = _hpSDIST;
int	hpRDIST = _hpRDIST;

short	hptypes[] =
	{ MBDT_RM03, MBDT_RM05, MBDT_RP06, MBDT_RM80, MBDT_RP05, MBDT_RP07, 0 };
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
	32,	5,	32*5,	823,	rm3_sizes,	/* RM03 */
	32,	19,	32*19,	823,	rm5_sizes,	/* RM05 */
	22,	19,	22*19,	815,	hp6_sizes,	/* RP06 */
	31,	14, 	31*14,	559,	rm80_sizes,	/* RM80 */
	22,	19,	22*19,	411,	hp6_sizes,	/* RP05 */
	50,	32,	50*32,	630,	hp7_sizes,	/* RP07 */
};

u_char	hp_offset[16] = {
    HPOF_P400, HPOF_M400, HPOF_P400, HPOF_M400,
    HPOF_P800, HPOF_M800, HPOF_P800, HPOF_M800,
    HPOF_P1200, HPOF_M1200, HPOF_P1200, HPOF_M1200,
    0, 0, 0, 0,
};

struct	buf	rhpbuf[NHP];
#ifndef NOBADSECT
struct	buf	bhpbuf[NHP];
struct	dkbad	hpbad[NHP];
#endif
char	hpinit[NHP];
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
	int s;

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
	s = spl5();
	disksort(&mi->mi_tab, bp);
	if (mi->mi_tab.b_active == 0)
		mbustart(mi);
	splx(s);
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
	register struct hpst *st = &hpst[mi->mi_type];
	daddr_t bn;
	int sn, dist;

	hpaddr->hpcs1 = 0;
	if ((hpaddr->hpcs1&HP_DVA) == 0)
		return (MBU_BUSY);
	if ((hpaddr->hpds & HPDS_VV) == 0 || hpinit[mi->mi_unit] == 0) {
#ifndef NOBADSECT
		struct buf *bbp = &bhpbuf[mi->mi_unit];
#endif

		hpinit[mi->mi_unit] = 1;
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		if (mi->mi_mba->mba_drv[0].mbd_as & (1<<mi->mi_drive))
			printf("DCLR attn\n");
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		hpaddr->hpof = HPOF_FMT22;
		mbclrattn(mi);
#ifndef NOBADSECT
		bbp->b_flags = B_READ|B_BUSY;
		bbp->b_dev = bp->b_dev;
		bbp->b_bcount = 512;
		bbp->b_un.b_addr = (caddr_t)&hpbad[mi->mi_unit];
		bbp->b_blkno = st->ncyl*st->nspc - st->nsect;
		bbp->b_cylin = st->ncyl - 1;
		mi->mi_tab.b_actf = bbp;
		bbp->av_forw = bp;
		bp = bbp;
#endif
	}
	if (mi->mi_tab.b_active || mi->mi_hd->mh_ndrive == 1)
		return (MBU_DODATA);
	if ((hpaddr->hpds & HPDS_DREADY) != HPDS_DREADY)
		return (MBU_DODATA);
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
	hpaddr->hpdc = bp->b_cylin;
	hpaddr->hpda = (tn << 8) + sn;
	return(0);
}

hpdtint(mi, mbsr)
	register struct mba_device *mi;
	int mbsr;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	int retry = 0;

#ifndef NOBADSECT
	if (bp->b_flags&B_BAD) {
		if (hpecc(mi, CONT))
			return(MBD_RESTARTED);
	}
#endif
	if (hpaddr->hpds&HPDS_ERR || mbsr&MBSR_EBITS) {
#ifdef HPDEBUG
		if (hpdebug) {
			int dc = hpaddr->hpdc, da = hpaddr->hpda;

			printf("hperr: bp %x cyl %d blk %d as %o ",
				bp, bp->b_cylin, bp->b_blkno,
				hpaddr->hpas&0xff);
			printf("dc %x da %x\n",dc&0xffff, da&0xffff);
			printf("errcnt %d ", mi->mi_tab.b_errcnt);
			printf("mbsr=%b ", mbsr, mbsr_bits);
			printf("er1=%b er2=%b\n",
			    hpaddr->hper1, HPER1_BITS,
			    hpaddr->hper2, HPER2_BITS);
			DELAY(1000000);
		}
#endif
		if (hpaddr->hper1&HPER1_WLE) {
			printf("hp%d: write locked\n", dkunit(bp));
			bp->b_flags |= B_ERROR;
		} else if (++mi->mi_tab.b_errcnt > 27 ||
		    mbsr & MBSR_HARD ||
		    hpaddr->hper1 & HPER1_HARD ||
		    hpaddr->hper2 & HPER2_HARD) {
hard:
			harderr(bp, "hp");
			if (mbsr & (MBSR_EBITS &~ (MBSR_DTABT|MBSR_MBEXC)))
				printf("mbsr=%b ", mbsr, mbsr_bits);
			printf("er1=%b er2=%b",
			    hpaddr->hper1, HPER1_BITS,
			    hpaddr->hper2, HPER2_BITS);
			if (hpaddr->hpmr)
				printf(" mr=%o", hpaddr->hpmr&0xffff);
			if (hpaddr->hpmr2)
				printf(" mr2=%o", hpaddr->hpmr2&0xffff);
			printf("\n");
			bp->b_flags |= B_ERROR;
			hprecal[mi->mi_unit] = 0;
		} else if (hpaddr->hper2 & HPER2_BSE) {
#ifndef NOBADSECT
			if (hpecc(mi, BSE))
				return(MBD_RESTARTED);
			else
#endif
				goto hard;
		} else if (hptypes[mi->mi_type] == MBDT_RM80 &&
		    hpaddr->hper2&HPER2_SSE) {
			(void) hpecc(mi, SSE);
			return (MBD_RESTARTED);
		} else if ((hpaddr->hper1&(HPER1_DCK|HPER1_ECH))==HPER1_DCK) {
			if (hpecc(mi, ECC))
				return (MBD_RESTARTED);
			/* else done */
		} else
			retry = 1;
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		if ((mi->mi_tab.b_errcnt&07) == 4) {
			hpaddr->hpcs1 = HP_RECAL|HP_GO;
			hprecal[mi->mi_unit] = 1;
			return(MBD_RESTARTED);
		}
		if (retry)
			return (MBD_RETRY);
	}
#ifdef HPDEBUG
	else
		if (hpdebug && hprecal[mi->mi_unit]) {
			printf("recal %d ", hprecal[mi->mi_unit]);
			printf("errcnt %d\n", mi->mi_tab.b_errcnt);
			printf("mbsr=%b ", mbsr, mbsr_bits);
			printf("er1=%b er2=%b\n",
			    hpaddr->hper1, HPER1_BITS,
			    hpaddr->hper2, HPER2_BITS);
		}
#endif
	switch (hprecal[mi->mi_unit]) {

	case 1:
		hpaddr->hpdc = bp->b_cylin;
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
		hprecal[mi->mi_unit]++;
		return (MBD_RESTARTED);
	case 2:
		if (mi->mi_tab.b_errcnt < 16 ||
		    (bp->b_flags & B_READ) == 0)
			goto donerecal;
		hpaddr->hpof = hp_offset[mi->mi_tab.b_errcnt & 017]|HPOF_FMT22;
		hpaddr->hpcs1 = HP_OFFSET|HP_GO;
		hprecal[mi->mi_unit]++;
		return (MBD_RESTARTED);
	donerecal:
	case 3:
		hprecal[mi->mi_unit] = 0;
		return (MBD_RETRY);
	}
	bp->b_resid = -(mi->mi_mba->mba_bcr) & 0xffff;
	if (mi->mi_tab.b_errcnt >= 16) {
		/*
		 * This is fast and occurs rarely; we don't
		 * bother with interrupts.
		 */
		hpaddr->hpcs1 = HP_RTC|HP_GO;
		while (hpaddr->hpds & HPDS_PIP)
			;
		mbclrattn(mi);
	}
	hpaddr->hpof = HPOF_FMT22;
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

hpecc(mi, flag)
	register struct mba_device *mi;
	int flag;
{
	register struct mba_regs *mbp = mi->mi_mba;
	register struct hpdevice *rp = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct hpst *st = &hpst[mi->mi_type];
	int npf, o;
	int bn, cn, tn, sn;
	int bcr;

	bcr = mbp->mba_bcr & 0xffff;
	if (bcr)
		bcr |= 0xffff0000;		/* sxt */
#ifndef NOBADSECT
	if (flag == CONT)
		npf = bp->b_error;
	else
#endif
		npf = btop(bcr + bp->b_bcount);
	o = (int)bp->b_un.b_addr & PGOFSET;
	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(st->nspc) + npf;
	tn = sn/st->nsect;
	sn %= st->nsect;
	cn += tn/st->ntrak;
	tn %= st->ntrak;
	switch (flag) {
	case ECC:
		{
		register int i;
		caddr_t addr;
		struct pte mpte;
		int bit, byte, mask;

		npf--;		/* because block in error is previous block */
		printf("hp%d%c: soft ecc sn%d\n", dkunit(bp),
		    'a'+(minor(bp->b_dev)&07), bp->b_blkno + npf);
		mask = rp->hpec2&0xffff;
		i = (rp->hpec1&0xffff) - 1;		/* -1 makes 0 origin */
		bit = i&07;
		i = (i&~07)>>3;
		byte = i + o;
		while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
			mpte = mbp->mba_map[npf+btop(byte)];
			addr = ptob(mpte.pg_pfnum) + (byte & PGOFSET);
			putmemc(addr, getmemc(addr)^(mask<<bit));
			byte++;
			i++;
			bit -= 8;
		}
		if (bcr == 0)
			return (0);
		npf++;
		break;
		}

	case SSE:
		rp->hpof |= HPOF_SSEI;
		mbp->mba_bcr = -(bp->b_bcount - (int)ptob(npf));
		break;

#ifndef NOBADSECT
	case BSE:
#ifdef HPBDEBUG
		if (hpbdebug)
		printf("hpecc, BSE: bn %d cn %d tn %d sn %d\n", bn, cn, tn, sn);
#endif
		if ((bn = isbad(&hpbad[mi->mi_unit], cn, tn, sn)) < 0)
			return(0);
		bp->b_flags |= B_BAD;
		bp->b_error = npf + 1;
		bn = st->ncyl*st->nspc - st->nsect - 1 - bn;
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn %= st->nsect;
		mbp->mba_bcr = -512;
#ifdef HPBDEBUG
		if (hpbdebug)
		printf("revector to cn %d tn %d sn %d\n", cn, tn, sn);
#endif
		break;

	case CONT:
#ifdef HPBDEBUG
		if (hpbdebug)
		printf("hpecc, CONT: bn %d cn %d tn %d sn %d\n", bn,cn,tn,sn);
#endif
		npf = bp->b_error;
		bp->b_flags &= ~B_BAD;
		mbp->mba_bcr = -(bp->b_bcount - (int)ptob(npf));
		if ((mbp->mba_bcr & 0xffff) == 0)
			return(0);
		break;
#endif
	}
	rp->hpcs1 = HP_DCLR|HP_GO;
	if (rp->hpof&HPOF_SSEI)
		sn++;
	rp->hpdc = cn;
	rp->hpda = (tn<<8) + sn;
	mbp->mba_sr = -1;
	mbp->mba_var = (int)ptob(npf) + o;
	rp->hpcs1 = bp->b_flags&B_READ ? HP_RCOM|HP_GO : HP_WCOM|HP_GO;
	mi->mi_tab.b_errcnt = 0;	/* error has been corrected */
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
	mba->mba_cr = MBCR_INIT;
	hpaddr = (struct hpdevice *)&mba->mba_drv[mi->mi_drive];
	if ((hpaddr->hpds & HPDS_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		hpaddr->hpof = HPOF_FMT22;
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
		while ((hpaddr->hpds & HPDS_DRY) == 0)
			;
		if (hpaddr->hpds&HPDS_ERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}
#endif
