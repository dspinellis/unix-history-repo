/*	hp.c	4.1	11/9/80	*/

#include "../conf/hp.h"
#if NHP > 0
/*
 * RP06/RM03/RM05 disk driver
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

struct	device
{
	int	hpcs1;		/* control and Status register 1 */
	int	hpds;		/* Drive Status */
	int	hper1;		/* Error register 1 */
	int	hpmr;		/* Maintenance */ 
	int	hpas;		/* Attention Summary */
	int	hpda;		/* Desired address register */
	int	hpdt;		/* Drive type */
	int	hpla;		/* Look ahead */
	int	hpsn;		/* serial number */
	int	hpof;		/* Offset register */
	int	hpdc;		/* Desired Cylinder address register */
	int	hpcc;		/* Current Cylinder */
	int	hper2;		/* Error register 2 */
	int	hper3;		/* Error register 3 */
	int	hpec1;		/* Burst error bit position */
	int	hpec2;		/* Burst error bit pattern */
};

#define	RP	022
#define	RM	024
#define	RM5	027
#define	NSECT	22
#define	NTRAC	19
#define	NRMSECT	32
#define	NRMTRAC	5

#define	_hpSDIST	2
#define	_hpRDIST	3

int	hpSDIST = _hpSDIST;
int	hpRDIST = _hpRDIST;
int	hpseek;

struct	size
{
	daddr_t	nblocks;
	int	cyloff;
} hp_sizes[8] =
{
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
	0,	0,
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
};

#define	P400	020
#define	M400	0220
#define	P800	040
#define	M800	0240
#define	P1200	060
#define	M1200	0260
int	hp_offset[16] =
{
	P400, M400, P400, M400,
	P800, M800, P800, M800,
	P1200, M1200, P1200, M1200,
	0, 0, 0, 0,
};

struct	buf	hptab;
struct	buf	rhpbuf;
struct	buf	hputab[NHP];
char	hp_type[NHP];	/* drive type */

#define	GO	01
#define	PRESET	020
#define	RTC	016
#define	OFFSET	014
#define	SEEK	04
#define	SEARCH	030
#define	RECAL	06
#define	DCLR	010
#define	WCOM	060
#define	RCOM	070

#define	IE	0100
#define	PIP	020000
#define	DRY	0200
#define	ERR	040000
#define	TRE	040000
#define	DCK	0100000
#define	WLE	04000
#define	ECH	0100
#define	VV	0100
#define	DPR	0400
#define	MOL	010000
#define	FMT22	010000

#define	b_cylin b_resid
 
#ifdef INTRLVE
daddr_t dkblock();
#endif
 
hpstrategy(bp)
register struct buf *bp;
{
	register struct buf *dp;
	register unit, xunit, nspc;
	long sz, bn;
	struct size *sizes;

	if ((mbaact&(1<<HPMBANUM)) == 0)
		mbainit(HPMBANUM);
	xunit = minor(bp->b_dev) & 077;
	sz = bp->b_bcount;
	sz = (sz+511) >> 9;
	unit = dkunit(bp);
	if (hp_type[unit] == 0) {
		struct device *hpaddr;
		double mspw;

		/* determine device type */
		hpaddr = mbadev(HPMBA, unit);

		/* record transfer rate (these are guesstimates secs/word) */
		switch (hp_type[unit] = hpaddr->hpdt) {
		case RM:	mspw = .0000019728; break;
		case RM5:	mspw = .0000020345; break;
		case RP:	mspw = .0000029592; break;
		}
		if (DK_N + unit <= DK_NMAX)
			dk_mspw[DK_N+unit] = mspw;
	}
	switch (hp_type[unit]) {

	case RM:
		sizes = rm_sizes;
		nspc = NRMSECT*NRMTRAC;
		break;
	case RM5:
		sizes = rm5_sizes;
		nspc = NRMSECT*NTRAC;
		break;
	case RP:
		sizes = hp_sizes;
		nspc = NSECT*NTRAC;
		break;
	default:
		printf("hp: unknown device type 0%o\n", hp_type[unit]);
		u.u_error = ENXIO;
		unit = NHP+1;	/* force error */
	}
	if (unit >= NHP ||
	    bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > sizes[xunit&07].nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->b_cylin = bn/nspc + sizes[xunit&07].cyloff;
	dp = &hputab[unit];
	(void) spl5();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		hpustart(unit);
		if(hptab.b_active == 0)
			hpstart();
	}
	(void) spl0();
}

hpustart(unit)
register unit;
{
	register struct buf *bp, *dp;
	register struct device *hpaddr;
	daddr_t bn;
	int sn, cn, csn;

	((struct mba_regs *)HPMBA)->mba_cr |= MBAIE;
	hpaddr = mbadev(HPMBA, 0);
	hpaddr->hpas = 1<<unit;

	if(unit >= NHP)
		return;
	if (unit+DK_N <= DK_NMAX)
		dk_busy &= ~(1<<(unit+DK_N));
	dp = &hputab[unit];
	if((bp=dp->b_actf) == NULL)
		return;
	hpaddr = mbadev(HPMBA, unit);
	if((hpaddr->hpds & VV) == 0) {
		hpaddr->hpcs1 = PRESET|GO;
		hpaddr->hpof = FMT22;
	}
	if(dp->b_active)
		goto done;
	dp->b_active++;
	if ((hpaddr->hpds & (DPR|MOL)) != (DPR|MOL))
		goto done;

	bn = dkblock(bp);
	cn = bp->b_cylin;
	switch (hp_type[unit]) {

	case RM:
		sn = bn%(NRMSECT*NRMTRAC);
		sn = (sn+NRMSECT-hpSDIST)%NRMSECT;
		break;
	case RM5:
		sn = bn%(NRMSECT*NTRAC);
		sn = (sn+NRMSECT-hpSDIST)%NRMSECT;
		break;
	case RP:
		sn = bn%(NSECT*NTRAC);
		sn = (sn+NSECT-hpSDIST)%NSECT;
		break;
	default:
		panic("hpustart");
	}

	if(cn - (hpaddr->hpdc & 0xffff))
		goto search;
	else if (hpseek)
		goto done;
	csn = ((hpaddr->hpla & 0xffff)>>6) - sn + 1;
	if(csn < 0)
		csn += NSECT;
	if(csn > NSECT-hpRDIST)
		goto done;

search:
	hpaddr->hpdc = cn;
	if (hpseek)
		hpaddr->hpcs1 = SEEK|GO;
	else {
		hpaddr->hpda = sn;
		hpaddr->hpcs1 = SEARCH|GO;
	}
	unit += DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_seek[unit]++;
	}
	return;

done:
	dp->b_forw = NULL;
	if(hptab.b_actf == NULL)
		hptab.b_actf = dp;
	else
		hptab.b_actl->b_forw = dp;
	hptab.b_actl = dp;
}

hpstart()
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *hpaddr;
	daddr_t bn;
	int dn, sn, tn, cn, nspc, ns;

loop:
	if ((dp = hptab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		hptab.b_actf = dp->b_forw;
		goto loop;
	}
	hptab.b_active++;
	unit = minor(bp->b_dev) & 077;
	dn = dkunit(bp);
	bn = dkblock(bp);
	switch (hp_type[dn]) {
	case RM:
		nspc = NRMSECT*NRMTRAC;
		ns = NRMSECT;
		cn = rm_sizes[unit&07].cyloff;
		break;
	case RM5:
		nspc = NRMSECT*NTRAC;
		ns = NRMSECT;
		cn = rm5_sizes[unit&07].cyloff;
		break;
	case RP:
		nspc = NSECT*NTRAC;
		ns = NSECT;
		cn = hp_sizes[unit&07].cyloff;
		break;
	default:
		panic("hpstart");
	}
	cn += bn/nspc;
	sn = bn%nspc;
	tn = sn/ns;
	sn = sn%ns;

	hpaddr = mbadev(HPMBA, dn);
	if ((hpaddr->hpds & (DPR|MOL)) != (DPR|MOL)) {
		hptab.b_active = 0;
		hptab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	if(hptab.b_errcnt >= 16 && (bp->b_flags&B_WRITE) == 0) {
		hpaddr->hpof = hp_offset[hptab.b_errcnt & 017] | FMT22;
		HPMBA->mba_cr &= ~MBAIE;
		hpaddr->hpcs1 = OFFSET|GO;
		while(hpaddr->hpds & PIP)
			;
		HPMBA->mba_cr |= MBAIE;
	}
	hpaddr->hpdc = cn;
	hpaddr->hpda = (tn << 8) + sn;
	mbastart(bp, (int *)hpaddr);

	unit = dn+DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_xfer[unit]++;
		dk_wds[unit] += bp->b_bcount>>6;
	}
}

hpintr(mbastat, as)
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *hpaddr;

	if(hptab.b_active) {
		dp = hptab.b_actf;
		bp = dp->b_actf;
		unit = dkunit(bp);
		if (DK_N+unit <= DK_NMAX)
			dk_busy &= ~(1<<(DK_N+unit));
		hpaddr = mbadev(HPMBA, unit);
		if (hpaddr->hpds & ERR || mbastat & MBAEBITS) {
			while((hpaddr->hpds & DRY) == 0)
				;
			if(++hptab.b_errcnt > 28 || hpaddr->hper1&WLE)
				bp->b_flags |= B_ERROR;
			else
				hptab.b_active = 0;
			if(hptab.b_errcnt > 27)
				deverror(bp, mbastat, hpaddr->hper1);
			if ((hpaddr->hper1&0xffff) == DCK) {
				if (hpecc(hpaddr, bp))
					return;
			}
			hpaddr->hpcs1 = DCLR|GO;
			if((hptab.b_errcnt&07) == 4) {
				HPMBA->mba_cr &= ~MBAIE;
				hpaddr->hpcs1 = RECAL|GO;
				while(hpaddr->hpds & PIP)
					;
				HPMBA->mba_cr |= MBAIE;
			}
		}
		if(hptab.b_active) {
			if(hptab.b_errcnt) {
				HPMBA->mba_cr &= ~MBAIE;
				hpaddr->hpcs1 = RTC|GO;
				while(hpaddr->hpds & PIP)
					;
				HPMBA->mba_cr |= MBAIE;
			}
			hptab.b_active = 0;
			hptab.b_errcnt = 0;
			hptab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = -HPMBA->mba_bcr & 0xffff;
			iodone(bp);
			if(dp->b_actf)
				hpustart(unit);
		}
		as &= ~(1<<unit);
	} else {
		if(as == 0)
			HPMBA->mba_cr |= MBAIE;
	}
	for(unit=0; unit<NHP; unit++)
		if(as & (1<<unit))
			hpustart(unit);
	hpstart();
}

hpread(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_READ, minphys);
}

hpwrite(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_WRITE, minphys);
}

hpecc(rp, bp)
register struct device *rp;
register struct buf *bp;
{
	struct mba_regs *mbp = HPMBA;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o;
	int dn, bn, cn, tn, sn, ns, nt;
	extern char buffers[NBUF][BSIZE];
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
	hptab.b_active++;		/* Either complete or continuing */
	if (bcr == 0)
		return (0);
	/*
	 * Have to continue the transfer... clear the drive,
	 * and compute the position where the transfer is to continue.
	 * We have completed npf+1 sectores of the transfer already;
	 * restart at offset o of next sector (i.e. in MBA register reg+1).
	 */
	rp->hpcs1 = DCLR|GO;
	dn = dkunit(bp);
	bn = dkblock(bp);
	switch (hp_type[dn]) {

	case RM:
		ns = NRMSECT; nt = NRMTRAC; break;
	case RM5:
		ns = NRMSECT; nt = NTRAC; break;
	case RP:
		ns = NSECT; nt = NTRAC; break;
	default:
		panic("hpecc");
	}
	cn = bp->b_cylin;
	sn = bn%(ns*nt) + npf + 1;
	tn = sn/ns;
	sn %= ns;
	cn += tn/nt;
	tn %= nt;
	rp->hpdc = cn;
	rp->hpda = (tn<<8) + sn;
	mbp->mba_sr = -1;
	mbp->mba_var = (int)ptob(reg+1) + o;
	rp->hpcs1 = RCOM|GO;
	return (1);
}
#endif
