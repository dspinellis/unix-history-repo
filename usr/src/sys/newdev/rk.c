/*	rk.c	2.2	1/23/80	*/

/*
 * RK disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/uba.h"

#define	RKADDR	((struct rk_regs *)( UBA0_DEV + 0177400))
#define	NRK	4
#define	NRKBLK	4872

#define	RESET	0
#define	WCOM	2
#define	RCOM	4
#define	GO	01
#define	DRESET	014
#define	IENABLE	0100
#define	DRY	0200
#define	ARDY	0100
#define	WLO	020000
#define	CTLRDY	0200

/*
 * Monitoring device bit
 */
#define	DK_N	1

struct	rk_regs
{
	short	rkds;
	short	rker;
	short	rkcs;
	short	rkwc;
	unsigned short 	rkba;
	short	rkda;
};

struct	buf	rktab;
struct	buf	rrkbuf;

int rk_ubinfo;

rkstrategy(bp)
register struct buf *bp;
{

	if (bp->b_blkno >= NRKBLK) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = (struct buf *)NULL;
	VOID spl5();
	if(rktab.b_actf == NULL)
		rktab.b_actf = bp;
	else
		rktab.b_actl->av_forw = bp;
	rktab.b_actl = bp;
	if(rktab.b_active == NULL)
		rkstart();
	VOID spl0();
}

rkstart()
{
	register struct buf *bp;
	register com;
	daddr_t bn;
	int dn, cn, sn;

	if ((bp = rktab.b_actf) == NULL)
		return;
	rktab.b_active++;
	rk_ubinfo = ubasetup( bp, 1 );
	bn = bp->b_blkno;
	dn = minor(bp->b_dev);
	cn = bn/12;
	sn = bn%12;
	RKADDR->rkda = (dn<<13) | (cn<<4) | sn;
	RKADDR->rkba = rk_ubinfo;
	RKADDR->rkwc = -(bp->b_bcount>>1);
	com = ((rk_ubinfo & 0x30000) >> 8 ) | IENABLE | GO;
	if(bp->b_flags & B_READ)
		com |= RCOM; else
		com |= WCOM;
	RKADDR->rkcs = com;
/*
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	com = bp->b_bcount>>6;
	dk_wds[DK_N] += com;
*/
}

rkintr()
{
	register struct buf *bp;

	if (rktab.b_active == NULL)
		return;
	dk_busy &= ~(1<<DK_N);
	bp = rktab.b_actf;
	rktab.b_active = NULL;
	if (RKADDR->rkcs < 0) {		/* error bit */
		deverror(bp, RKADDR->rker, RKADDR->rkds);
		RKADDR->rkcs = RESET|GO;
		while((RKADDR->rkcs&CTLRDY) == 0)
			;
		if (++rktab.b_errcnt <= 10) {
			ubafree(rk_ubinfo);
			rkstart();
			return;
		}
		bp->b_flags |= B_ERROR;
	}
	rktab.b_errcnt = 0;
	rktab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	ubafree(rk_ubinfo);
	iodone(bp);
	rkstart();
}

rkread(dev)
dev_t dev;
{

	physio(rkstrategy, &rrkbuf, dev, B_READ, minphys);
}

rkwrite(dev)
dev_t dev;
{

	physio(rkstrategy, &rrkbuf, dev, B_WRITE, minphys);
}
