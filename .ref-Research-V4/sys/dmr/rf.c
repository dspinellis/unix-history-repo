#
/*
 * RF disk driver
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"

struct { char lbyte, hbyte; };

struct {
	int	rfcs;
	int	rfwc;
	int	rfba;
	int	rfda;
	int	rfdae;
};

#define	NRFBLK	2048
#define	RFADDR	0177460
#define	JRF	0

#define	GO	01
#define	RCOM	02
#define	WCOM	04
#define	IENABLE	0100

rfstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;

	bp = abp;
	if (bp->b_blkno >= NRFBLK) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl5();
	if (devtab[JRF].d_actf==0)
		devtab[JRF].d_actf = bp;
	else
		devtab[JRF].d_actl->av_forw = bp;
	devtab[JRF].d_actl = bp;
	if (devtab[JRF].d_active==0)
		rfstart();
	spl0();
}

rfstart()
{
	register struct buf *bp;

	if ((bp = devtab[JRF].d_actf) == 0)
		return;
	devtab[JRF].d_active++;
	RFADDR->rfdae = bp->b_blkno.hbyte;
	devstart(bp, &RFADDR->rfda, bp->b_blkno<<8);
}

rfintr()
{
	register struct buf *bp;

	if (devtab[JRF].d_active == 0)
		return;
	bp = devtab[JRF].d_actf;
	devtab[JRF].d_active = 0;
	if (RFADDR->rfcs < 0) {		/* error bit */
		if (++devtab[JRF].d_errcnt <= 10) {
			rfstart();
			return;
		}
		bp->b_flags =| B_ERROR;
	}
	devtab[JRF].d_errcnt = 0;
	devtab[JRF].d_actf = bp->av_forw;
	iodone(bp);
	rfstart();
}
