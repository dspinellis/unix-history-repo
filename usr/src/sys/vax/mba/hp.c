/*	hp.c	3.2	%H%	*/

/*
 * RP04/RP06/RM03 disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/pte.h"

#define	DK_N	0

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

#define	HPADDR	((struct device *)(MBA0 + MBA_ERB))
#define	NHP	1
#define	RP	022
#define	RM	024
#define	NSECT	22
#define	NTRAC	19
#define	NRMSECT	32
#define	NRMTRAC	5
#define	SDIST	2
#define	RDIST	6

struct	size
{
	daddr_t	nblocks;
	int	cyloff;
} hp_sizes[8] =
{
	15884,	0,		/* cyl 0 thru 37 */
	33440,	38,		/* cyl 38 thru 117 */
	8360,	98,		/* cyl 98 thru 117 */
#ifdef ERNIE
	15884,	118,		/* cyl 118 thru 155 */
	66880,	156,		/* cyl 156 thru 315 */
	0,	0,
	291346,	118,		/* cyl 118 thru 814, (like distrib) */
	208582,	316,		/* cyl 316 thru 814 */
#else
	0,	0,
	0,	0,
	0,	0,
	291346,	118,		/* cyl 118 thru 814 */
	0,	0,
#endif
}, rm_sizes[8] = {
	15884,	0,		/* cyl 0 thru 99 */
	33440,	100,		/* cyl 100 thru 309 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	82080,	310,		/* cyl 310 thru 822 */
	0,	0,
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

	xunit = minor(bp->b_dev) & 077;
	sz = bp->b_bcount;
	sz = (sz+511) >> 9;
	unit = dkunit(bp);
	if (hp_type[unit] == 0) {
		struct device *hpaddr;

		/* determine device type */
		hpaddr = (struct device *)((int*)HPADDR + 32*unit);
		hp_type[unit] = hpaddr->hpdt;
	}
	if (hp_type[unit] == RM) {
		sizes = rm_sizes;
		nspc = NRMSECT*NRMTRAC;
	} else {
		sizes = hp_sizes;
		nspc = NSECT*NTRAC;
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
	VOID spl5();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		hpustart(unit);
		if(hptab.b_active == 0)
			hpstart();
	}
	VOID spl0();
}

hpustart(unit)
register unit;
{
	register struct buf *bp, *dp;
	register struct device *hpaddr;
	daddr_t bn;
	int sn, cn, csn;

	((struct mba_regs *)MBA0)->mba_cr |= MBAIE;
	HPADDR->hpas = 1<<unit;

	if(unit >= NHP)
		return;
/*
	dk_busy &= ~(1<<(unit+DK_N));
*/
	dp = &hputab[unit];
	if((bp=dp->b_actf) == NULL)
		return;
	hpaddr = (struct device *)((int *)HPADDR + 32*unit);
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
	if(hp_type[unit] == RM) {
		sn = bn%(NRMSECT*NRMTRAC);
		sn = (sn+NRMSECT-SDIST)%NRMSECT;
	} else {
		sn = bn%(NSECT*NTRAC);
		sn = (sn+NSECT-SDIST)%NSECT;
	}

	if(cn - (hpaddr->hpdc & 0xffff))
		goto search;
	csn = ((hpaddr->hpla & 0xffff)>>6) - sn + SDIST - 1;
	if(csn < 0)
		csn += NSECT;
	if(csn > NSECT-RDIST)
		goto done;

search:
	hpaddr->hpdc = cn;
	hpaddr->hpda = sn;
	hpaddr->hpcs1 = SEARCH|GO;
/*
	unit += DK_N;
	dk_busy |= 1<<unit;
	dk_numb[unit] += 1;
*/
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
	if (hp_type[dn] == RM) {
		nspc = NRMSECT*NRMTRAC;
		ns = NRMSECT;
		cn = rm_sizes[unit&07].cyloff;
	} else {
		nspc = NSECT*NTRAC;
		ns = NSECT;
		cn = hp_sizes[unit&07].cyloff;
	}
	cn += bn/nspc;
	sn = bn%nspc;
	tn = sn/ns;
	sn = sn%ns;

	hpaddr =  (struct device *)((int *)HPADDR + 32*dn);
	if ((hpaddr->hpds & (DPR|MOL)) != (DPR|MOL)) {
		hptab.b_active = 0;
		hptab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	if(hptab.b_errcnt >= 16) {
		hpaddr->hpof = hp_offset[hptab.b_errcnt & 017] | FMT22;
		((struct mba_regs *)MBA0)->mba_cr &= ~MBAIE;
		hpaddr->hpcs1 = OFFSET|GO;
		while(hpaddr->hpds & PIP)
			;
		((struct mba_regs *)MBA0)->mba_cr |= MBAIE;
	}
	hpaddr->hpdc = cn;
	hpaddr->hpda = (tn << 8) + sn;
	mbastart(bp, (int *)hpaddr);

	dk_busy |= 1<<(DK_N /*+NHP*/);
	dk_numb[DK_N /*+NHP*/] += 1;
	unit = bp->b_bcount>>6;
	dk_wds[DK_N /*+NHP*/] += unit;
}

hpintr(mbastat, as)
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *hpaddr;

	if(hptab.b_active) {
		dk_busy &= ~(1<<(DK_N /*+NHP*/));
		dp = hptab.b_actf;
		bp = dp->b_actf;
		unit = dkunit(bp);
		hpaddr = (struct device *)((int *)HPADDR + 32*unit);
		if (hpaddr->hpds & ERR || mbastat & MBAEBITS) {		/* error bit */
			while((hpaddr->hpds & DRY) == 0)
				;
			if(++hptab.b_errcnt > 28 || hpaddr->hper1&WLE)
				bp->b_flags |= B_ERROR; else
				hptab.b_active = 0;
			if(hptab.b_errcnt > 27)
				deverror(bp, mbastat, hpaddr->hper1);
			if ((hpaddr->hper1&0xffff) == DCK) {
				if (hpecc(hpaddr, bp))
					return;
			}
			hpaddr->hpcs1 = DCLR|GO;
			if((hptab.b_errcnt&07) == 4) {
				((struct mba_regs *)MBA0)->mba_cr &= ~MBAIE;
				hpaddr->hpcs1 = RECAL|GO;
				while(hpaddr->hpds & PIP)
					;
				((struct mba_regs *)MBA0)->mba_cr |= MBAIE;
			}
		}
		if(hptab.b_active) {
			if(hptab.b_errcnt) {
				((struct mba_regs *)MBA0)->mba_cr &= ~MBAIE;
				hpaddr->hpcs1 = RTC|GO;
				while(hpaddr->hpds & PIP)
					;
				((struct mba_regs *)MBA0)->mba_cr |= MBAIE;
			}
			hptab.b_active = 0;
			hptab.b_errcnt = 0;
			hptab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = -(((struct mba_regs *)MBA0)->mba_bcr) & 0xffff;
			iodone(bp);
			if(dp->b_actf)
				hpustart(unit);
		}
		as &= ~(1<<unit);
	} else {
		if(as == 0)
			((struct mba_regs *)MBA0)->mba_cr |= MBAIE;
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
	register i;
	register b, n, map, mix;
	register char *cp;
	register mask;
	short piget();
	extern char buffers[NBUF][BSIZE];

	b = (((((struct mba_regs *)MBA0)->mba_bcr&0xffff) +
		(bp->b_bcount) - 1)>>9)&0177;
	printf("%D ", bp->b_blkno+b);
	prdev("ECC", bp->b_dev);
	mask = rp->hpec2&0xffff;
	if (mask == 0) {
		rp->hpof = FMT22;
		return(0);
	}
	i = (rp->hpec1&0xffff) - 1;
	n = i&017;
	i = (i&~017)>>3;
	if (bp->b_flags&B_PHYS)
		map = 128 + b;
	else
		map = ((bp->b_un.b_addr - (char *)buffers)>>9) + b;
	mix = i + ((int)bp->b_un.b_addr&0x1ff);
	i += b<<9;
	if ( i < bp->b_bcount) {
		cp = (char *)((((int *)MBA0_MAP)[map+(mix>>9)]&0x1fffff)<<9)+(mix&0x1ff);
		piput((int)cp,piget((int)cp)^(mask<<n));
	}
	mix += 2;
	i += 2;
	if (i < bp->b_bcount) {
		cp = (char *)((((int *)MBA0_MAP)[map+(mix>>9)]&0x1fffff)<<9)+(mix&0x1ff);
		piput((int)cp,piget((int)cp)^(mask>>(16-n)));
	}
	hptab.b_active++;
	if (((struct mba_regs *)MBA0)->mba_bcr) {
		i = bp->b_blkno%(NSECT*NTRAC);
		i = ((i/NSECT)<<8)+(i%NSECT);
		i = NSECT*(i>>8) + (i&0377) + b + 1;
		if (i >= NSECT*NTRAC) {
			i -= NSECT*NTRAC;
			rp->hpdc = bp->b_cylin + 1;
		} else
			rp->hpdc = bp->b_cylin;
		rp->hpda = ((i/NSECT)<<8) + (i%NSECT);
		rp->hpcs1 = DCLR|GO;
		((struct mba_regs *)MBA0)->mba_sr = -1;
		((struct mba_regs *)MBA0)->mba_var =
			((map+1)<<9)|((int)bp->b_un.b_addr&0x1ff);
		rp->hpcs1 = RCOM|GO;
		return(1);
	} else
		return(0);
}

short
piget(pad)
{
	register b, savemap;
	register short s;

	savemap = (int)mmap;
	b = (pad>>9)&0x7fffff;
	*(int *)mmap = b|(PG_V|PG_KR);
	mtpr(TBIS, vmmap);
	s = *(short *)&vmmap[pad&0x1ff];
	*(int *)mmap = savemap;
	mtpr(TBIS, vmmap);
	return(s);
}

piput(pad, val)
{
	register b, savemap;
	register short *p;

	savemap = (int)mmap;
	b = (pad>>9)&0x7fffff;
	*(int *)mmap = b|(PG_V|PG_KW);
	mtpr(TBIS, vmmap);
	p = (short *)&vmmap[pad&0x1ff];
	*p = val;
	*(int *)mmap = savemap;
	mtpr(TBIS, vmmap);
}
