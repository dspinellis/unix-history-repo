#
/*
 */

/*
 * RS03/04 disk driver
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"


struct {
	int	hscs1;	/* Control and Status register 1 */
	int	hswc;	/* Word count register */
	int	hsba;	/* UNIBUS address register */
	int	hsda;	/* Desired address register */
	int	hscs2;	/* Control and Status register 2 */
	int	hsds;	/* Drive Status */
	int	hser;	/* Error register */
	int	hsas;	/* not used */
	int	hsla;	/* not used */
	int	hsdb;	/* not used */
	int	hsmr;	/* not used */
	int	hsdt;	/* not used */
	int	hsbae;	/* 11/70 bus extension */
};

struct	devtab	hstab;
struct	buf	rhsbuf;

#define	HSADDR	0172040

#define ERR	040000	/* hscs1 - composite error */

#define GO	01
#define RCLR	010
#define	DRY	0200	/* hsds - Drive Ready */

hsstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register mblks;

	bp = abp;
	mblks = 1024; /* RJS03 */
	if(bp->b_dev.d_minor >= 8)
		mblks = 2048; /* RJS04 */
	if(bp->b_blkno >= mblks) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl5();
	if (hstab.d_actf==0)
		hstab.d_actf = bp; else
		hstab.d_actl->av_forw = bp;
	hstab.d_actl = bp;
	if (hstab.d_active==0)
		hsstart();
	spl0();
}

hsstart()
{
	register struct buf *bp;
	register addr;

	if ((bp = hstab.d_actf) == 0)
		return;
	hstab.d_active++;
	addr = bp->b_blkno;
	if(bp->b_dev.d_minor < 8)
		addr =<< 1; /* RJS03 */
	HSADDR->hscs2 = bp->b_dev.d_minor & 07;
	rhstart(bp, &HSADDR->hsda, addr<<1, &HSADDR->hsbae);
}

hsintr()
{
	register struct buf *bp;

	if (hstab.d_active == 0)
		return;
	bp = hstab.d_actf;
	hstab.d_active = 0;
	if(HSADDR->hscs1 & ERR){	/* error bit */
		deverror(bp, HSADDR->hscs2, 0);
		HSADDR->hscs1 = RCLR|GO;
		if (++hstab.d_errcnt <= 10) {
			hsstart();
			return;
		}
		bp->b_flags =| B_ERROR;
	}
	hstab.d_errcnt = 0;
	hstab.d_actf = bp->av_forw;
	iodone(bp);
	hsstart();
}

hsread(dev)
{

	physio(hsstrategy, &rhsbuf, dev, B_READ);
}

hswrite(dev)
{

	physio(hsstrategy, &rhsbuf, dev, B_WRITE);
}
