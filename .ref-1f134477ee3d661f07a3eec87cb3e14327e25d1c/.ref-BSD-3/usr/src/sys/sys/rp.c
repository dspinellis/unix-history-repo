/*	rp.c	2.3	2/18/80	*/

#ifdef ERNIE
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
#include "../h/uba.h"

#define	EMULEX
#ifdef	EMULEX
#define DELAY(N)	{ register int d; d = N; while (--d > 0); }
#else
#define	DELAY(N)
#endif
int	rpdelay = 0;
int	idelay = 500;

int	urpdebug;

#define DK_N	2
/*
 * RP unibus driver
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

struct urpregs {
	short	urpcs1;	/* Control and Status register 1 */
	short	urpwc;	/* Word count register */
	short	urpba;	/* UNIBUS address register */
	short	urpda;	/* Desired address register */
	short	urpcs2;	/* Control and Status register 2*/
	short	urpds;	/* Drive Status */
	short	urper1;	/* Error register 1 */
	short	urpas;	/* Attention Summary */
	short	urpla;	/* Look ahead */
	short	urpdb;	/* Data buffer */
	short	urpmr;	/* Maintenance register */
	short	urpdt;	/* Drive type */
	short	urpsn;	/* Serial number */
	short	urpof;	/* Offset register */
	short	urpca;	/* Desired Cylinder address register*/
	short	urpcc;	/* Current Cylinder */
	short	urper2;	/* Error register 2 */
	short	urper3;	/* Error register 3 */
	short	urppos;	/* Burst error bit position */
	short	urppat;	/* Burst error bit pattern */
	short	urpbae;	/* 11/70 bus extension */
};

#define	URPADDR ((struct urpregs *)(UBA0_DEV + 0176700))
#define	NURP	1
int	urpdrive;

#define	NSECT	32
#define	NSURF	19

struct devsize {
	unsigned int	nblocks;
	int	cyloff;
} urp_sizes[] = {
	15884,	0,	/* cyl 0 thru 26 */
	33440,	27,	/* cyl 27 thru 81, 24928 if rp2c in use */
	8480,	68,	/* cyl 68 thru 81 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	445664,	82,	/* cyl 82 thru 814 */
};

struct	buf	urptab;
struct	buf	urpbuf;

int	urp_ubinfo;	/* map info for xfer */

char	urp_openf;

#ifdef	WCHECK
char	urp_wcsw;	/* do write checking if set */
#endif
/* Drive Commands */
#define	GO	01
#define	PRESET	020
#define	RECAL	06
#define	DCLR	010
#define	OFFSET	014
#define	WCOM	060
#define	RCOM	070

#define	IE	0100
#define	READY	0200		/* urpds - drive ready */
#define	PIP	020000		/* urpds - Positioning Operation in Progress */
#define	ERR	040000		/* urpcs1 - composite error */

#define	DTE	010000  	/* urper1 - Drive Timing Error	*/
#define	OPI	020000  	/* urper1 - Operation Incomplete */
#define	DU	040000		/* urper1 - Drive Unsafe */

/* Error Correction Code errors */
#define	DCK	0100000		/* urper1 - Data Check error */
#define	ECH	0100    	/* urper1 - ECC hard error */

#define	CLR	040		/* urpcs2 - Controller Clear */

#define	FMT22	010000		/* urpof - 16 bit /word format */

/*
 * Use b_error to save track+sector,
 * b_resid for cylinder.
 */

#define	trk	av_back
#define	sec	b_error
#define	b_cylin	b_resid

#define	track(x)	(struct buf *)(x)

urpopen()
{

	if (!urp_openf) {
		urp_openf++;
		URPADDR->urpcs2 = CLR;
		DELAY(idelay);
		URPADDR->urpcs1 = DCLR|GO;
		DELAY(idelay);
		URPADDR->urpcs1 = PRESET|GO;
		DELAY(idelay);
		URPADDR->urpof = FMT22;
		DELAY(idelay);
	}
}

urpstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register long p1, p2;
	register struct devsize *sp;
	register struct buf *q1, *q2;

	if (!urp_openf)
		urpopen();
	bp = abp;
	sp = &urp_sizes[minor(bp->b_dev) & 07];
	if (minor(bp->b_dev) >= (NURP<<3) ||
	    bp->b_blkno >= sp->nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	bp->b_cylin = sp->cyloff;
	p1 = bp->b_blkno;
	p2 = p1%NSECT;
	p1 = p1/NSECT;
	bp->trk = track(p1%NSURF);
	bp->sec = p2;
	bp->b_cylin += p1/NSURF;
	VOID spl5();
	if ((q1 = urptab.b_actf)==0)
		urptab.b_actf = bp;
	else {
		for (; q2 = q1->av_forw; q1 = q2) {
			if (q1->b_cylin <= bp->b_cylin
			 && bp->b_cylin <  q2->b_cylin
			 || q1->b_cylin >= bp->b_cylin
			 && bp->b_cylin >  q2->b_cylin) 
				break;
		}
		bp->av_forw = q2;
		q1->av_forw = bp;
	}
	if (urptab.b_active==0) {
		urpstart();
	}
	VOID spl0();
}

urpstart()
{
	register struct buf *bp;
	register short *rp;
	register int c;

	if ((bp = urptab.b_actf) == 0)
		return;
	URPADDR->urpcs2 = minor(bp->b_dev) >> 3;
	/* the code to urpca address provides delay for slow controller */
	urptab.b_active++;
	c = bp->b_bcount;
	urp_ubinfo = ubasetup(bp, 1);
	rp = (short *) &URPADDR->urpda;
	URPADDR->urpca = bp->b_cylin;
	*rp = ((int)bp->trk << 8) | bp->sec;
	*--rp = urp_ubinfo;
	*--rp = -c / sizeof (short);
	if (bp->b_flags & B_READ) {
		*--rp = IE|GO|RCOM;
	} else {
		*--rp = IE|GO|WCOM;
	}
#ifdef DK_N
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	dk_wds[DK_N] += bp->b_bcount>>6;
#endif
}

urpintr()
{
	register struct buf *bp;
	register int ctr;

	if (urptab.b_active == 0)
		return;
#ifdef DK_N
	dk_busy &= ~(1<<DK_N);
#endif
	bp = urptab.b_actf;
	urptab.b_active = 0;
	if (URPADDR->urpcs1 & ERR) {		/* error bit */
#ifdef CHECKW
		if (bp->b_flags & B_WCHK) {
			printf("URP wchk\n");
			bp->b_flags &= ~B_WCHK;		/* re-write it */
		}
#endif
/*
		printf("note: ");
		deverror(bp, URPADDR->urpcs2, URPADDR->urper1);
*/
		if ((URPADDR->urper1&0xffff) == DCK) {
			if (urpecc(URPADDR, bp))
				return;
			URPADDR->urpcs1 = DCLR|GO;
			DELAY(idelay);
			goto ok;
		}
		deverror(bp, URPADDR->urpcs2, URPADDR->urper1);
		if (URPADDR->urper1 & (DU|DTE|OPI)) {
			URPADDR->urpcs2 = CLR;
			DELAY(idelay);
			URPADDR->urpcs1 = RECAL|GO;
			DELAY(idelay);
			ctr = 50000;
			while ((URPADDR->urpds&PIP) && --ctr)
				DELAY(idelay);
		}
		URPADDR->urpcs1 = DCLR|GO;
		DELAY(idelay);
		if (++urptab.b_errcnt <= 4) {
			ubafree(urp_ubinfo), urp_ubinfo = 0;
			urpstart();
			return;
		}
		bp->b_flags |= B_ERROR;
		bp->b_error = 0;
	}
#ifdef CHECKW
	else if ((bp->b_flags&(B_READ|B_WCHK))==0 && urp_wcsw) {
		bp->b_flags =| B_WCHK;
		ubafree(urp_ubinfo), urp_ubinfo = 0;
		urpstart();
		return;
	}
#endif
ok:
	urptab.b_errcnt = 0;
	urptab.b_actf = bp->av_forw;
	bp->b_resid =0;
	ubafree(urp_ubinfo), urp_ubinfo = 0;
	iodone(bp);
	urpstart();
}

urpread(dev)
{

	if (urpphys(dev))
		physio(urpstrategy, &urpbuf, dev, B_READ, minphys);
}

urpwrite(dev)
{

	if (urpphys(dev))
		physio(urpstrategy, &urpbuf, dev, B_WRITE, minphys);
}

urpphys(dev)
{
	register int c;

	c = (u.u_offset >> 9) + (u.u_count + 511) / 512;
	if (c > urp_sizes[minor(dev) & 07].nblocks) {
		u.u_error = ENXIO;
		return (0);
	}
	return (1);
}

urpecc(rp, bp)
register struct urpregs *rp;
register struct buf *bp;
{
	register i;
	register b, n, map, mix;
	register char *cp;
	register mask;
	short urpiget();
	extern char buffers[][];
	struct uba_regs *ubareg;

	ubareg = (struct uba_regs *) UBA0;
	b = (((URPADDR->urpwc * sizeof(short)) + (bp->b_bcount) - 1) >> 9) & 0177;
	printf("%D ", bp->b_blkno+b);
	prdev("ECC", bp->b_dev);
	printf("pat 0x%x pos %d -wc %d\n", rp->urppat & 0xffff, rp->urppos & 0xffff, -rp->urpwc);
	mask = rp->urppat&0xffff;
	if (mask == 0) {
		rp->urpof = FMT22;
		DELAY(idelay);
		return(0);
	}
	i = (rp->urppos) - 1;
	n = i&017;
	i = (i&~017)>>3;
	map = ((urp_ubinfo&0xffff)>>9) + b;
	mix = i + ((int)bp->b_un.b_addr&0x1ff);
	i += b<<9;
	if ( i < bp->b_bcount) {
		cp = (char *)((ubareg->uba_map[map+(mix>>9)].pg_pfnum<<9)+(0x1ff&(int)mix));
		urpiput((int)cp,urpiget((int)cp)^(mask<<n));
	}
	mix += 2;
	i += 2;
	if (i < bp->b_bcount) {
		cp = (char *)((ubareg->uba_map[map+(mix>>9)].pg_pfnum<<9)+(0x1ff&(int)mix));
		urpiput((int)cp,urpiget((int)cp)^(mask>>(16-n)));
	}
	if (i = rp->urpwc) {
		rp->urpcs1 = DCLR|GO;
		DELAY(idelay);
		rp->urpwc = i;
		i = bp->b_blkno%(NSECT*NSURF);
		i = ((i/NSECT)<<8)+(i%NSECT);
		i = NSECT*(i>>8) + (i&0377) + b + 1;
		if (i >= NSECT*NSURF) {
			i -= NSECT*NSURF;
			rp->urpca = bp->b_cylin + 1;
		} else
			rp->urpca = bp->b_cylin;
		rp->urpda = ((i/NSECT)<<8) + (i%NSECT);
		rp->urpba = ((map+1)<<9)|((int)bp->b_un.b_addr&0x1ff);
		rp->urpcs1 = IE|RCOM|GO;
		urptab.b_active++;
		return(1);
	} else
		return(0);
}

short
urpiget(pad)
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

urpiput(pad, val)
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
#endif
