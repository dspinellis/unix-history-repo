#
/*
 */

/*
 * RP04 disk driver
 *
 * This driver has been tested on a working RP04 for a few hours.
 * It does not attempt ECC error correction and is probably
 * deficient in general in the case of errors and when packs
 * are dismounted.
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"

struct {
	int	hpcs1;	/* Control and Status register 1 */
	int	hpwc;	/* Word count register */
	int	hpba;	/* UNIBUS address register */
	int	hpda;	/* Desired address register */
	int	hpcs2;	/* Control and Status register 2*/
	int	hpds;	/* Drive Status */
	int	hper1;	/* Error register 1 */
	int	hpas;	/* Attention Summary */
	int	hpla;	/* Look ahead */
	int	hpdb;	/* Data buffer */
	int	hpmr;	/* Maintenance register */
	int	hpdt;	/* Drive type */
	int	hpsn;	/* Serial number */
	int	hpof;	/* Offset register */
	int	hpca;	/* Desired Cylinder address register*/
	int	hpcc;	/* Current Cylinder */
	int	hper2;	/* Error register 2 */
	int	hper3;	/* Error register 3 */
	int	hppos;	/* Burst error bit position */
	int	hppat;	/* Burst error bit pattern */
	int	hpbae;	/* 11/70 bus extension */
};

#define	HPADDR	0176700
#define	NHP	8

struct {
	char	*nblocks;
	int	cyloff;
} hp_sizes[] {
	9614,	0,		/* cyl 0 thru 23 */
				/* cyl 24 thru 43 available */
	-1,	44,		/* cyl 44 thru 200 */
	-1,	201,		/* cyl 201 thru 357 */
	20900,	358,		/* cyl 358 thru 407 */
				/* cyl 408 thru 410 blank */
	40600,	0,
	40600,	100,
	40600,	200,
	40600,	300,
};


struct	devtab	hptab;
struct	buf	hpbuf;

char	hp_openf;

			/* Drive Commands */
#define	GO	01
#define	PRESET	020
#define	RECAL	06
#define RCLR	010
#define OFFSET	014

#define	READY	0200	/* hpds - drive ready */
#define	PIP	020000	/* hpds - Positioning Operation in Progress */
#define	ERR	040000	/* hpcs1 - composite error */

#define	DU	040000	/* hper1 - Drive Unsafe	*/
#define	DTE	010000  /* hper1 - Drive Timing Error	*/
#define	OPI	020000  /* hper1 - Operation Incomplete	*/
		/* Error Correction Code errors */
#define DCK	0100000	/* hper1 - Data Check error */
#define ECH	0100    /* hper1 - ECC hard error */

#define CLR	040	/* hpcs2 - Controller Clear */

#define FMT22	010000	/* hpof - 16 bit /word format */
/*
 * Use av_back to save track+sector,
 * b_resid for cylinder.
 */

#define	trksec	av_back
#define	cylin	b_resid

hpopen()
{

	if(!hp_openf){
		hp_openf++;
		HPADDR->hpcs2 = CLR;
		HPADDR->hpcs1 = RCLR|GO;
		HPADDR->hpcs1 = PRESET|GO;
		HPADDR->hpof = FMT22;
	}
}

hpstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register char *p1, *p2;

	bp = abp;
	p1 = &hp_sizes[bp->b_dev.d_minor&07];
	if (bp->b_dev.d_minor >= (NHP<<3) ||
	    bp->b_blkno >= p1->nblocks) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	bp->cylin = p1->cyloff;
	p1 = bp->b_blkno;
	p2 = lrem(p1, 22);
	p1 = ldiv(p1, 22);
	bp->trksec = (p1%19)<<8 | p2;
	bp->cylin =+ p1/19;
	spl5();
	if ((p1 = hptab.d_actf)==0)
		hptab.d_actf = bp;
	else {
		for (; p2 = p1->av_forw; p1 = p2) {
			if (p1->cylin <= bp->cylin
			 && bp->cylin <  p2->cylin
			 || p1->cylin >= bp->cylin
			 && bp->cylin >  p2->cylin) 
				break;
		}
		bp->av_forw = p2;
		p1->av_forw = bp;
	}
	if (hptab.d_active==0)
		hpstart();
	spl0();
}

hpstart()
{
	register struct buf *bp;

	if ((bp = hptab.d_actf) == 0)
		return;
	hptab.d_active++;
	HPADDR->hpcs2 = bp->b_dev.d_minor >> 3;
	HPADDR->hpca = bp->cylin;
	rhstart(bp, &HPADDR->hpda, bp->trksec, &HPADDR->hpbae);
}

hpintr()
{
	register struct buf *bp;
	register int ctr;

	if (hptab.d_active == 0)
		return;
	bp = hptab.d_actf;
	hptab.d_active = 0;
	if (HPADDR->hpcs1 & ERR) {		/* error bit */
		deverror(bp, HPADDR->hpcs2, 0);
		if(HPADDR->hper1 & (DU|DTE|OPI)) {
			HPADDR->hpcs2 = CLR;
			HPADDR->hpcs1 = RECAL|GO;
			ctr = 0;
			while ((HPADDR->hpds&PIP) && --ctr);
		}
		HPADDR->hpcs1 = RCLR|GO;
		if (++hptab.d_errcnt <= 10) {
			hpstart();
			return;
		}
		bp->b_flags =| B_ERROR;
	}
	hptab.d_errcnt = 0;
	hptab.d_actf = bp->av_forw;
	bp->b_resid = HPADDR->hpwc;
	iodone(bp);
	hpstart();
}

hpread(dev)
{

	if(hpphys(dev))
	physio(hpstrategy, &hpbuf, dev, B_READ);
}

hpwrite(dev)
{

	if(hpphys(dev))
	physio(hpstrategy, &hpbuf, dev, B_WRITE);
}

hpphys(dev)
{
	register c;

	c = lshift(u.u_offset, -9);
	c =+ ldiv(u.u_count+511, 512);
	if(c > hp_sizes[dev.d_minor & 07].nblocks) {
		u.u_error = ENXIO;
		return(0);
	}
	return(1);
}

