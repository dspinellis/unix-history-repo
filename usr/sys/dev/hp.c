/*
 * RP04/RP06 disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"

#define	DK_N	0

struct	device
{
	union {
		int	w;
		char	c[2];
	} hpcs1;		/* Control and Status register 1 */
	int	hpwc;		/* Word count register */
	caddr_t	hpba;		/* UNIBUS address register */
	int	hpda;		/* Desired address register */
	union {
		int	w;
		char	c[2];
	} hpcs2;		/* Control and Status register 2*/
	int	hpds;		/* Drive Status */
	int	hper1;		/* Error register 1 */
	int	hpas;		/* Attention Summary */
	int	hpla;		/* Look ahead */
	int	hpdb;		/* Data buffer */
	int	hpmr;		/* Maintenance register */
	int	hpdt;		/* Drive type */
	int	hpsn;		/* Serial number */
	int	hpof;		/* Offset register */
	int	hpdc;		/* Desired Cylinder address register*/
	int	hpcc;		/* Current Cylinder */
	int	hper2;		/* Error register 2 */
	int	hper3;		/* Error register 3 */
	int	hpec1;		/* Burst error bit position */
	int	hpec2;		/* Burst error bit pattern */
	int	hpbae;		/* 11/70 bus extension */
	int	hpcs3;
};

#define	HPADDR	((struct device *)0176700)
#define	NHP	2
#define	NSECT	22
#define	NTRAC	19
#define	SDIST	2
#define	RDIST	6

struct	size
{
	daddr_t	nblocks;
	int	cyloff;
} hp_sizes[8] =
{
	9614,	0,		/* cyl 0 thru 22 */
	8778,	23,		/* cyl 23 thru 43 */
	0,	0,
	0,	0,
	161348,	44,		/* cyl 44 thru 429 */
	160930, 430,		/* cyl 430 thru 814 */
	153406,	44,		/* cyl 44 thru 410 (rp04, rp05) */
	322278,	44,		/* cyl 44 thru 814 (rp06) */
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

#define	GO	01
#define	PRESET	020
#define	RTC	016
#define	OFFSET	014
#define	SEARCH	030
#define	RECAL	06
#define DCLR	010
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
#define VV	0100
#define	DPR	0400
#define	MOL	010000
#define FMT22	010000

#define	b_cylin	b_resid

daddr_t dkblock();

hpstrategy(bp)
register struct buf *bp;
{
	register struct buf *dp;
	register unit;
	long sz, bn;

	unit = minor(bp->b_dev) & 077;
	sz = bp->b_bcount;
	sz = (sz+511) >> 9;
	if (unit >= (NHP<<3) ||
	    bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > hp_sizes[unit&07].nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->b_cylin = bn/(NSECT*NTRAC) + hp_sizes[unit&07].cyloff;
	unit = dkunit(bp);
	dp = &hputab[unit];
	spl5();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		hpustart(unit);
		if(hptab.b_active == 0)
			hpstart();
	}
	spl0();
}

hpustart(unit)
register unit;
{
	register struct buf *bp, *dp;
	daddr_t bn;
	int sn, cn, csn;

	HPADDR->hpcs2.w = unit;
	HPADDR->hpcs1.c[0] = IE;
	HPADDR->hpas = 1<<unit;

	if(unit >= NHP)
		return;
	dk_busy &= ~(1<<(unit+DK_N));
	dp = &hputab[unit];
	if((bp=dp->b_actf) == NULL)
		return;
	if((HPADDR->hpds & VV) == 0) {
		HPADDR->hpcs1.c[0] = IE|PRESET|GO;
		HPADDR->hpof = FMT22;
	}
	if(dp->b_active)
		goto done;
	dp->b_active++;
	if ((HPADDR->hpds & (DPR|MOL)) != (DPR|MOL))
		goto done;

	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(NSECT*NTRAC);
	sn = (sn+NSECT-SDIST)%NSECT;

	if(HPADDR->hpcc != cn)
		goto search;
	csn = (HPADDR->hpla>>6) - sn + SDIST - 1;
	if(csn < 0)
		csn += NSECT;
	if(csn > NSECT-RDIST)
		goto done;

search:
	HPADDR->hpdc = cn;
	HPADDR->hpda = sn;
	HPADDR->hpcs1.c[0] = IE|SEARCH|GO;
	unit += DK_N;
	dk_busy |= 1<<unit;
	dk_numb[unit] += 1;
	return;

done:
	dp->b_forw = NULL;
	if(hptab.b_actf == NULL)
		hptab.b_actf = dp; else
		hptab.b_actl->b_forw = dp;
	hptab.b_actl = dp;
}

hpstart()
{
	register struct buf *bp, *dp;
	register unit;
	daddr_t bn;
	int dn, sn, tn, cn;

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
	cn = bn/(NSECT*NTRAC) + hp_sizes[unit&07].cyloff;
	sn = bn%(NSECT*NTRAC);
	tn = sn/NSECT;
	sn = sn%NSECT;

	HPADDR->hpcs2.w = dn;
	if ((HPADDR->hpds & (DPR|MOL)) != (DPR|MOL)) {
		hptab.b_active = 0;
		hptab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	if(hptab.b_errcnt >= 16) {
		HPADDR->hpof = hp_offset[hptab.b_errcnt & 017] | FMT22;
		HPADDR->hpcs1.w = OFFSET|GO;
		while(HPADDR->hpds & PIP)
			;
	}
	HPADDR->hpdc = cn;
	HPADDR->hpda = (tn << 8) + sn;
	HPADDR->hpba = bp->b_un.b_addr;
	if(cputype == 70)
		HPADDR->hpbae = bp->b_xmem;
	HPADDR->hpwc = -(bp->b_bcount>>1);
	unit = ((bp->b_xmem&3) << 8) | IE | GO;
	if(bp->b_flags & B_READ)
		unit |= RCOM; else
		unit |= WCOM;
	HPADDR->hpcs1.w = unit;

	dk_busy |= 1<<(DK_N+NHP);
	dk_numb[DK_N+NHP] += 1;
	unit = bp->b_bcount>>6;
	dk_wds[DK_N+NHP] += unit;
}

hpintr()
{
	register struct buf *bp, *dp;
	register unit;
	int as, i, j;

	as = HPADDR->hpas & 0377;
	if(hptab.b_active) {
		dk_busy &= ~(1<<(DK_N+NHP));
		dp = hptab.b_actf;
		bp = dp->b_actf;
		unit = dkunit(bp);
		HPADDR->hpcs2.c[0] = unit;
		if (HPADDR->hpcs1.w & TRE) {		/* error bit */
			while((HPADDR->hpds & DRY) == 0)
				;
			if(++hptab.b_errcnt > 28 || HPADDR->hper1&WLE)
				bp->b_flags |= B_ERROR; else
				hptab.b_active = 0;
			if(hptab.b_errcnt > 27)
				deverror(bp, HPADDR->hpcs2.w, HPADDR->hper1);
			if((bp->b_flags&B_PHYS) == 0 &&
			   (HPADDR->hper1 & (DCK|ECH)) == DCK) {
				i = HPADDR->hpec1 - 1;
				j = i&017;
				i >>= 4;
				if(i >= 0 && i <256) {
					bp->b_un.b_words[i] ^= HPADDR->hpec2 << j;
					bp->b_un.b_words[i+1] ^= HPADDR->hpec2 >> (16-j);
				}
				hptab.b_active++;
				printf("%D ", bp->b_blkno);
				prdev("ECC", bp->b_dev);
			}
			HPADDR->hpcs1.w = TRE|IE|DCLR|GO;
			if((hptab.b_errcnt&07) == 4) {
				HPADDR->hpcs1.w = RECAL|IE|GO;
				while(HPADDR->hpds & PIP)
					;
			}
		}
		if(hptab.b_active) {
			if(hptab.b_errcnt) {
				HPADDR->hpcs1.w = RTC|GO;
				while(HPADDR->hpds & PIP)
					;
			}
			hptab.b_active = 0;
			hptab.b_errcnt = 0;
			hptab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = -(HPADDR->hpwc<<1);
			iodone(bp);
			HPADDR->hpcs1.w = IE;
			if(dp->b_actf)
				hpustart(unit);
		}
		as &= ~(1<<unit);
	} else {
		if(as == 0)
			HPADDR->hpcs1.w = IE;
		HPADDR->hpcs1.c[1] = TRE>>8;
	}
	for(unit=0; unit<NHP; unit++)
		if(as & (1<<unit))
			hpustart(unit);
	hpstart();
}

hpread(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_READ);
}

hpwrite(dev)
{

	physio(hpstrategy, &rhpbuf, dev, B_WRITE);
}
