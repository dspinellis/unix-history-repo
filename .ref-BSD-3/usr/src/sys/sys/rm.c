/*	rm.c	2.1	1/5/80	*/

#ifdef ERNIE
#include "../h/mba.h"
#include "../h/param.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"

int	urmdebug	= 0;
#define	dprintf(i)	if(urmdebug&i) printf

#define DK_N	1
/*
 * RM03 unibus driver
 *
 * This driver has been tested on a working RP04 for a few hours.
 * It does not attempt ECC error correction and is probably
 * deficient in general in the case of errors and when packs
 * are dismounted.
 *
 * This driver coexists with VAX/UNIX, but doesn't use many
 * of the system routines (like sort and leave)... especially if you
 * have more than one drive you should think about modifying hp.c
 * to get a split seeking UNIBUS rm03/rp04/rp06 driver.
 */


struct uhpregs {
	short	uhpcs1;	/* Control and Status register 1 */
	short	uhpwc;	/* Word count register */
	short	uhpba;	/* UNIBUS address register */
	short	uhpda;	/* Desired address register */
	short	uhpcs2;	/* Control and Status register 2*/
	short	uhpds;	/* Drive Status */
	short	uhper1;	/* Error register 1 */
	short	uhpas;	/* Attention Summary */
	short	uhpla;	/* Look ahead */
	short	uhpdb;	/* Data buffer */
	short	uhpmr;	/* Maintenance register */
	short	uhpdt;	/* Drive type */
	short	uhpsn;	/* Serial number */
	short	uhpof;	/* Offset register */
	short	uhpca;	/* Desired Cylinder address register*/
	short	uhpcc;	/* Current Cylinder */
	short	uhper2;	/* Error register 2 */
	short	uhper3;	/* Error register 3 */
	short	uhppos;	/* Burst error bit position */
	short	uhppat;	/* Burst error bit pattern */
	short	uhpbae;	/* 11/70 bus extension */
};

#define	UHPADDR ((struct uhpregs *)(UBA0_DEV + 0176300))
#define	NUHP	3

struct devsize {
	unsigned int	nblocks;
	int	cyloff;
} uhp_sizes[] = {
	15884,	0,	/* cyl 0 thru 99 */
	33440,	100,	/* cyl 100 thru 308, 24960 if rm0c in use */
	8480,	256,	/* cyl 256 thru 308 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	82240,	309,	/* cyl 309 thru 822 */
};

struct	buf	uhptab;
struct	buf	uhpbuf;

int	uhp_ubinfo;	/* map info for xfer */

char	uhp_openf;

#ifdef	WCHECK
char	uhp_wcsw;	/* do write checking if set */
#endif
/* Drive Commands */
#define	GO	01
#define	PRESET	020
#define	RECAL	06
#define	RCLR	010
#define	OFFSET	014
#define	READ	070
#define	WRITE	060

#define	IENABLE	0100
#define	READY	0200		/* uhpds - drive ready */
#define	PIP	020000		/* uhpds - Positioning Operation in Progress */
#define	ERR	040000		/* uhpcs1 - composite error */

#define	DTE	010000  	/* uhper1 - Drive Timing Error	*/
#define	OPI	020000  	/* uhper1 - Operation Incomplete */
#define	DU	040000		/* uhper1 - Drive Unsafe */

/* Error Correction Code errors */
#define	DCK	0100000		/* uhper1 - Data Check error */
#define	ECH	0100    	/* uhper1 - ECC hard error */

#define	CLR	040		/* uhpcs2 - Controller Clear */

#define	FMT22	010000		/* uhpof - 16 bit /word format */

/*
 * Use b_error to save track+sector,
 * b_resid for cylinder.
 */

#define	trk	av_back
#define	sec	b_error
#define	cylin	b_resid

#define	track(x)	(struct buf *)(x)
/*
 * These kludge the rm i/o so that no transfers go switch
 * tracks, which doesnt work the way t
 */
char	*savuaddr;
int	savucnt;

uhpopen()
{

	if (!uhp_openf){
		uhp_openf++;
		UHPADDR->uhpcs2 = CLR;
		UHPADDR->uhpcs1 = RCLR|GO;
		UHPADDR->uhpcs1 = PRESET|GO;
		UHPADDR->uhpof = FMT22;
	}
}

uhpstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register long p1, p2;
	register struct devsize *sp;
	register struct buf *q1, *q2;

	if (!uhp_openf)
		uhpopen();
	bp = abp;
	sp = &uhp_sizes[minor(bp->b_dev) & 07];
	if (minor(bp->b_dev) >= (NUHP<<3) ||
	    bp->b_blkno >= sp->nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	bp->cylin = sp->cyloff;
	p1 = bp->b_blkno;
	p2 = p1%32;
	p1 = p1/32;
	bp->trk = track(p1%5);
	bp->sec = p2;
	bp->cylin += p1/5;
	VOID spl5();
	if ((q1 = uhptab.b_actf)==0)
		uhptab.b_actf = bp;
	else {
		for (; q2 = q1->av_forw; q1 = q2) {
			if (q1->cylin <= bp->cylin
			 && bp->cylin <  q2->cylin
			 || q1->cylin >= bp->cylin
			 && bp->cylin >  q2->cylin) 
				break;
		}
		bp->av_forw = q2;
		q1->av_forw = bp;
	}
	if (uhptab.b_active==0) {
		dprintf(4)("starting the uhp...\n");
		uhpstart();
		dprintf(4)("uhp start done\n");
	}
	VOID spl0();
}

uhpstart()
{
	register struct buf *bp;
	register short *rp;
	register int c;

	if ((bp = uhptab.b_actf) == 0)
		return;
	uhptab.b_active++;
/* begin kludge */
	c = bp->b_bcount;
	if (bp->sec + (bp->b_bcount >> 9) >= 32) {
		if (savucnt == 0) {
			savuaddr = bp->b_un.b_addr;
			savucnt = bp->b_bcount;
		}
		c = (32 - bp->sec) << 9;
	}
/* end kludge code */
	uhp_ubinfo = ubasetup(bp, 1);
	UHPADDR->uhpcs2 = minor(bp->b_dev) >> 3;
	UHPADDR->uhpca = bp->cylin;
	rp = (short *) &UHPADDR->uhpda;
	*rp = ((int)bp->trk << 8) | bp->sec;
	*--rp = uhp_ubinfo;
	*--rp = -c / sizeof (short);
	dprintf(2)("ubinfo=%x cs2=%d ca=%d trk=%x sec=%x count=%d\n",
	    uhp_ubinfo, minor(bp->b_dev) >> 3, bp->cylin, (int) bp->trk,
	    bp->sec, bp->b_bcount / sizeof (short));
	if (bp->b_flags & B_READ) {
		dprintf(4)("read... ");
		*--rp = IENABLE|GO|READ;
	} else {
		dprintf(4)("write... ");
		*--rp = IENABLE|GO|WRITE;
	}
#ifdef DK_N
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	dk_wds[DK_N] += bp->b_bcount>>6;
#endif
}

uhpintr()
{
	register struct buf *bp;
	register int ctr;

	dprintf(4)("intr!...\n");
	if (uhptab.b_active == 0)
		return;
#ifdef DK_N
	dk_busy &= ~(1<<DK_N);
#endif
	bp = uhptab.b_actf;
	uhptab.b_active = 0;
	if (UHPADDR->uhpcs1 & ERR) {		/* error bit */
		deverror(bp, UHPADDR->uhpcs2, UHPADDR->uhper1);
#ifdef CHECKW
		if (bp->b_flags & B_WCHK) {
			printf("UHP wchk\n");
			bp->b_flags &= ~B_WCHK;		/* re-write it */
		}
#endif
		if (UHPADDR->uhper1 & (DU|DTE|OPI)) {
			UHPADDR->uhpcs2 = CLR;
			UHPADDR->uhpcs1 = RECAL|GO;
			ctr = 0;
			while ((UHPADDR->uhpds&PIP) && --ctr);
		}
		UHPADDR->uhpcs1 = RCLR|GO;
		if (++uhptab.b_errcnt <= 4) {
			ubafree(uhp_ubinfo);
			uhpstart();
			return;
		}
		bp->b_flags |= B_ERROR;
	}
#ifdef CHECKW
	else if ((bp->b_flags&(B_READ|B_WCHK))==0 && uhp_wcsw) {
		bp->b_flags =| B_WCHK;
		ubafree(uhp_ubinfo);
		uhpstart();
		return;
	}
#endif
/* begin kludge */
	if (savucnt && (bp->b_flags&B_ERROR) == 0) {
		register int c;

		c = (32 - bp->sec) << 9;
		if (bp->b_bcount > c) {
			bp->b_bcount -= c;
			bp->b_un.b_addr += c;
			bp->sec = 0;
			bp->trk = track((int)bp->trk + 1);
			if ((int)bp->trk == 5) {
				bp->trk = track(0);
				bp->cylin++;
			}
			ubafree(uhp_ubinfo);
			uhpstart();
			return;
		}
	}
/* end kludge */
	dprintf(4)("okie dokie it workie.\n");
	uhptab.b_errcnt = 0;
	uhptab.b_actf = bp->av_forw;
	bp->b_resid = -UHPADDR->uhpwc * sizeof (short);
	ubafree(uhp_ubinfo);
	uhpfix(bp);
	iodone(bp);
	uhpstart();
}

uhpread(dev)
{

	if (uhpphys(dev))
		physio(uhpstrategy, &uhpbuf, dev, B_READ, minphys);
}

uhpwrite(dev)
{

	if (uhpphys(dev))
		physio(uhpstrategy, &uhpbuf, dev, B_WRITE, minphys);
}

uhpphys(dev)
{
	register int c;

	c = (u.u_offset >> 9) + (u.u_count + 511) / 512;
	if (c > uhp_sizes[minor(dev) & 07].nblocks) {
		u.u_error = ENXIO;
		return (0);
	}
	return (1);
}

/* begin klude */
uhpfix(bp)
	register struct buf *bp;
{

	if (savucnt) {
		bp->b_un.b_addr = savuaddr;
		bp->b_bcount = savucnt;
		savuaddr = 0;
		savucnt = 0;
	}
}
/* end kludge */
#endif
