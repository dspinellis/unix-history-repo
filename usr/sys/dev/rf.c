#
/*
 * RF disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"

struct device {
	int	rfcs;
	int	rfwc;
	char	*rfba;
	int	rfda;
	int	rfdae;
};

struct	buf	rftab;
struct	buf	rrfbuf;

#define	NRFBLK	1024
#define RFADDR ((struct device *)0177460)

#define	GO	01
#define	RCOM	04
#define	WCOM	02
#define	CTLCLR	0400
#define	IENABLE	0100

/*
 * Monitoring device number
 */
#define	DK_N	0

rfstrategy(bp)
register struct buf *bp;
{
	if(bp->b_flags&B_PHYS)
		mapalloc(bp);
	if (bp->b_blkno >= NRFBLK*(minor(bp->b_dev)+1)) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl5();
	if (rftab.b_actf == NULL)
		rftab.b_actf = bp;
	else
		rftab.b_actl->av_forw = bp;
	rftab.b_actl = bp;
	if (rftab.b_active == NULL)
		rfstart();
	spl0();
}

rfstart()
{
	register struct buf *bp;
	register int com;

	if ((bp = rftab.b_actf) == NULL)
		return;
	rftab.b_active++;
	RFADDR->rfda = (int)(bp->b_blkno<<8)&0177777;
	RFADDR->rfdae = (int)(bp->b_blkno>>8)&037;
	RFADDR->rfba = bp->b_un.b_addr;
	RFADDR->rfwc = -(bp->b_bcount>>1);
	com = (bp->b_xmem&3) << 4;
	com |= (bp->b_flags & B_READ) ? RCOM+GO+IENABLE : WCOM+GO+IENABLE;
	RFADDR->rfcs = com;
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	com = (-bp->b_bcount>>5) & 03777;
	dk_wds[DK_N] += com;
}

rfintr()
{
	register struct buf *bp;

	if (rftab.b_active == NULL)
		return;
	dk_busy &= ~(1<<DK_N);
	bp = rftab.b_actf;
	rftab.b_active = NULL;
	if (RFADDR->rfcs < 0) {		/* error bit */
		deverror(bp, RFADDR->rfcs, RFADDR->rfdae);
		RFADDR->rfcs = CTLCLR;
		if (++rftab.b_errcnt <= 10) {
			rfstart();
			return;
		}
		bp->b_flags |= B_ERROR;
	}
	rftab.b_errcnt = 0;
	rftab.b_actf = bp->av_forw;
	bp->b_resid = 0;
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
