#
/*
 * RP disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/conf.h"
#include "../h/user.h"

struct device {
	int	rpds;
	int	rper;
	union {
		int	w;
		char	c;
	} rpcs;
	int	rpwc;
	char	*rpba;
	int	rpca;
	int	rpda;
};

#define RPADDR ((struct device *) 0176710)
#define	NRP	8

struct {
	daddr_t	nblocks;
	int	cyloff;
} rp_sizes[] = {
	81000,	0,		/* cyl 0 thru 405 */
	5000,	0,		/* cyl 0 thru 24 */
	2000,	25,		/* cyl 25 thru 34 */
	74000,	35,		/* cyl 35 thru 405 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
};

struct	buf	rptab;
struct	buf	rrpbuf;

#define	GO	01
#define	RESET	0
#define	HSEEK	014

#define	IENABLE	0100
#define	READY	0200
#define	RCOM	4
#define	WCOM	2

#define	SUFU	01000
#define	SUSU	02000
#define	SUSI	04000
#define	HNF	010000

/*
 * Use av_back to save track+sector,
 * b_resid for cylinder.
 */

#define	trksec	av_back
#define	cylin	b_resid

/*
 * Monitoring device number
 */
#define	DK_N	2

rpstrategy(bp)
register struct buf *bp;
{
	register struct buf *dp;
	register int unit;
	long sz;

	unit = minor(bp->b_dev);
	sz = bp->b_bcount;
	sz = (sz+511)>>9;
	if (unit >= (NRP<<3) ||
	   bp->b_blkno+sz >= rp_sizes[unit&07].nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = NULL;
	unit >>= 3;
	spl5();
	dp = & rptab;
	if (dp->b_actf == NULL)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	if (dp->b_active == NULL)
		rpstart();
	spl0();
}

rpstart()
{
	register struct buf *bp;
	register int unit;
	int com,cn,tn,sn,dn;
	daddr_t bn;


	if ((bp = rptab.b_actf) == NULL)
		return;
	rptab.b_active++;
	unit = minor(bp->b_dev);
	dn = unit>>3;
	bn = bp->b_blkno;
	cn = bn/(20*10) + rp_sizes[unit&07].cyloff;
	sn = bn%(20*10);
	tn = sn/10;
	sn = sn%10;
	RPADDR->rpcs.w = (dn<<8);
	RPADDR->rpda = (tn<<8) | sn;
	RPADDR->rpca = cn;
	RPADDR->rpba = bp->b_un.b_addr;
	RPADDR->rpwc = -(bp->b_bcount>>1);
	com = ((bp->b_xmem&3)<<4) | IENABLE | GO;
	if (bp->b_flags & B_READ)
		com |= RCOM; else
		com |= WCOM;
	
	RPADDR->rpcs.w |= com;
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	unit = bp->b_bcount>>6;
	dk_wds[DK_N] += unit;
}

rpintr()
{
	register struct buf *bp;
	register int ctr;

	if (rptab.b_active == NULL)
		return;
	dk_busy &= ~(1<<DK_N);
	bp = rptab.b_actf;
	rptab.b_active = NULL;
	if (RPADDR->rpcs.w < 0) {		/* error bit */
		deverror(bp, RPADDR->rper, RPADDR->rpds);
		if(RPADDR->rpds & (SUFU|SUSI|HNF)) {
			RPADDR->rpcs.c = HSEEK|GO;
			ctr = 0;
			while ((RPADDR->rpds&SUSU) && --ctr)
				;
		}
		RPADDR->rpcs.w = RESET|GO;
		ctr = 0;
		while ((RPADDR->rpcs.w&READY) == 0 && --ctr)
			;
		if (++rptab.b_errcnt <= 10) {
			rpstart();
			return;
		}
		bp->b_flags |= B_ERROR;
	}
	rptab.b_errcnt = 0;
	rptab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	iodone(bp);
	rpstart();
}

rpread(dev)
{

	physio(rpstrategy, &rrpbuf, dev, B_READ);
}

rpwrite(dev)
{

	physio(rpstrategy, &rrpbuf, dev, B_WRITE);
}
