#
/*
 */

/*
 * RP disk driver
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"

struct {
	int	rpds;
	int	rper;
	int	rpcs;
	int	rpwc;
	int	rpba;
	int	rpca;
	int	rpda;
};

#define	RPADDR	0176710
#define	NRP	8

struct {
	char	*nblocks;
	int	cyloff;
} rp_sizes[] {
	40600,	0,		/* cyl 0 thru 202 */
	40600,	203,		/* cyl 203 thru 405 */
	9200,	0,		/* cyl 0 thru 45 */
	9200,	360,		/* cyl 360 thru 405 */
	-1,	0,		/* cyl 0 thru 327 */
	-1,	78,		/* cyl 78 thru 405 */
	15600,	0,		/* cyl 0 thru 77 */
	15600,	328,		/* cyl 328 thru 405 */
};

struct	devtab	rptab;
struct	buf	rrpbuf;

#define	GO	01
#define	RESET	0
#define	HSEEK	014

#define	IENABLE	0100
#define	READY	0200

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

rpstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register char *p1, *p2;

	bp = abp;
	if(bp->b_flags&B_PHYS)
		mapalloc(bp);
	p1 = &rp_sizes[bp->b_dev.d_minor&07];
	if (bp->b_dev.d_minor >= (NRP<<3) ||
	    bp->b_blkno >= p1->nblocks) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	bp->cylin = p1->cyloff;
	p1 = bp->b_blkno;
	p2 = lrem(p1, 10);
	p1 = ldiv(p1, 10);
	bp->trksec = (p1%20)<<8 | p2;
	bp->cylin =+ p1/20;
	spl5();
	if ((p1 = rptab.d_actf)==0)
		rptab.d_actf = bp;
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
	if (rptab.d_active==0)
		rpstart();
	spl0();
}

rpstart()
{
	register struct buf *bp;

	if ((bp = rptab.d_actf) == 0)
		return;
	rptab.d_active++;
	RPADDR->rpda = bp->trksec;
	devstart(bp, &RPADDR->rpca, bp->cylin, bp->b_dev.d_minor>>3);
}

rpintr()
{
	register struct buf *bp;
	register int ctr;

	if (rptab.d_active == 0)
		return;
	bp = rptab.d_actf;
	rptab.d_active = 0;
	if (RPADDR->rpcs < 0) {		/* error bit */
		deverror(bp, RPADDR->rper, RPADDR->rpds);
		if(RPADDR->rpds & (SUFU|SUSI|HNF)) {
			RPADDR->rpcs.lobyte = HSEEK|GO;
			ctr = 0;
			while ((RPADDR->rpds&SUSU) && --ctr);
		}
		RPADDR->rpcs = RESET|GO;
		ctr = 0;
		while ((RPADDR->rpcs&READY) == 0 && --ctr);
		if (++rptab.d_errcnt <= 10) {
			rpstart();
			return;
		}
		bp->b_flags =| B_ERROR;
	}
	rptab.d_errcnt = 0;
	rptab.d_actf = bp->av_forw;
	bp->b_resid = RPADDR->rpwc;
	iodone(bp);
	rpstart();
}

rpread(dev)
{

	if(rpphys(dev))
	physio(rpstrategy, &rrpbuf, dev, B_READ);
}

rpwrite(dev)
{

	if(rpphys(dev))
	physio(rpstrategy, &rrpbuf, dev, B_WRITE);
}

rpphys(dev)
{
	register c;

	c = lshift(u.u_offset, -9);
	c =+ ldiv(u.u_count+511, 512);
	if(c > rp_sizes[dev.d_minor & 07].nblocks) {
		u.u_error = ENXIO;
		return(0);
	}
	return(1);
}
