#
/*
 */

/*
 * RF disk driver
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"

struct {
	int	rfcs;
	int	rfwc;
	int	rfba;
	int	rfda;
	int	rfdae;
};

struct	devtab	rftab;
struct	buf	rrfbuf;

#define	NRFBLK	1024
#define	RFADDR	0177460

#define	GO	01
#define	RCOM	02
#define	WCOM	04
#define	CTLCLR	0400
#define	IENABLE	0100

rfstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;

	bp = abp;
	if(bp->b_flags&B_PHYS)
		mapalloc(bp);
	if (bp->b_blkno >= NRFBLK*(bp->b_dev.d_minor+1)) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl5();
	if (rftab.d_actf==0)
		rftab.d_actf = bp;
	else
		rftab.d_actl->av_forw = bp;
	rftab.d_actl = bp;
	if (rftab.d_active==0)
		rfstart();
	spl0();
}

rfstart()
{
	register struct buf *bp;

	if ((bp = rftab.d_actf) == 0)
		return;
	rftab.d_active++;
	RFADDR->rfdae = bp->b_blkno.hibyte;
	devstart(bp, &RFADDR->rfda, bp->b_blkno<<8, 0);
}

rfintr()
{
	register struct buf *bp;

	if (rftab.d_active == 0)
		return;
	bp = rftab.d_actf;
	rftab.d_active = 0;
	if (RFADDR->rfcs < 0) {		/* error bit */
		deverror(bp, RFADDR->rfcs, RFADDR->rfdae);
		RFADDR->rfcs = CTLCLR;
		if (++rftab.d_errcnt <= 10) {
			rfstart();
			return;
		}
		bp->b_flags =| B_ERROR;
	}
	rftab.d_errcnt = 0;
	rftab.d_actf = bp->av_forw;
	iodone(bp);
	rfstart();
}

rfread(dev)
{

	physio(rfstrategy, &rrfbuf, dev, B_READ);
}

rfwrite(dev)
{

	physio(rfstrategy, &rrfbuf, dev, B_WRITE);
}
