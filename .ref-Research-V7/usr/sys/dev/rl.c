/*
 *  RL disk driver
 */

#define DK_N	2
#include "../h/param.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/systm.h"

#define NRLBLK 10240
#define RLCYLSZ 10240
#define RLSECSZ 256

#define RESET 013
#define STAT 03
#define GETSTAT 04
#define WCOM 012
#define RCOM 014
#define SEEK 06
#define SEEKHI 5
#define SEEKLO 1
#define RDHDR 010
#define IENABLE 0100
#define CRDY 0200
#define OPI 02000
#define CRCERR 04000
#define TIMOUT 010000
#define NXM 020000
#define DE  040000

struct device
{
	int rlcs, rlba, rlda, rlmp;
};

#define RLADDR	((struct device *)0174400)
#define RL_CNT 1
struct	buf	rrlbuf;
struct	buf	rltab;

struct 
{
	int	cn[4];		/* location of heads for each drive */
	int	dn;		/* drive number */
	int	com;		/* read or write command word */
	int	chn;		/* cylinder and head number */
	unsigned int	bleft;	/* bytes left to be transferred */
	unsigned int	bpart;	/* number of bytes transferred */
	int	sn;		/* sector number */
	union {
		int	w[2];
		long	l;
	} addr;			/* address of memory for transfer */

}	rl = {-1,-1,-1,-1};

rlstrategy(bp)
register struct buf *bp;
{
	if(bp->b_blkno >= NRLBLK) {
		if(bp->b_blkno == NRLBLK && bp->b_flags&B_READ)
			bp->b_resid = bp->b_bcount;
		else {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
		}
		iodone(bp);
		return;
	}
	bp->av_forw = NULL;
	spl5();
	if(rltab.b_actf == NULL)
		rltab.b_actf = bp;
	else
		rltab.b_actl->av_forw = bp;
	rltab.b_actl = bp;
	if(rltab.b_active == NULL)
		rlstart();
	spl0();
}

rlstart()
{

	register struct buf *bp;

	if ((bp = rltab.b_actf) == NULL)
		return;
	rltab.b_active++;
	rl.dn = minor(bp->b_dev);
	rl.chn = bp->b_blkno/20;
	rl.sn = (bp->b_blkno%20) << 1;
	rl.bleft = bp->b_bcount;
	rl.addr.w[0] = bp->b_xmem & 3;
	rl.addr.w[1] = (int)bp->b_un.b_addr;
	rl.com = (rl.dn << 8) | IENABLE;
	if (bp->b_flags & B_READ)
		rl.com |= RCOM;
	else
		rl.com |= WCOM;
	rlio();
}

rlintr()
{
	register struct buf *bp;
	register struct device *rp;
	register int status;

	rp = RLADDR;
	if (rltab.b_active == NULL) {
/*
		logstray(rp);
*/
		return;
	}
	bp = rltab.b_actf;
	dk_busy &= ~(1<<DK_N);
	if (rp->rlcs < 0) {		/* error bit */
		if (rp->rlcs & 036000) {
			if(rltab.b_errcnt > 2)
				deverror(bp, rp->rlcs, rp->rlda);
		}
		if (rp->rlcs & 040000) {
			rp->rlda = STAT;
			rp->rlcs = (rl.dn << 8) | GETSTAT;
			while ((rp->rlcs & CRDY) == 0)
				;
			status = rp->rlmp;
			if(rltab.b_errcnt > 2)
				deverror(bp, status, rp->rlda);
			rp->rlda = RESET;
			rp->rlcs = (rl.dn << 8) | GETSTAT;
			while ((rp->rlcs & CRDY) == 0)
				;
			if(status & 01000) {
				rlstart();
				return;
			}
		}
		if (++rltab.b_errcnt <= 10) {
			rl.cn[rl.dn] = -1;
			rlstart();
			return;
		}
		else {
			bp->b_flags |= B_ERROR;
			rl.bpart = rl.bleft;
		}
	}

	if ((rl.bleft -= rl.bpart) > 0) {
		rl.addr.l += rl.bpart;
		rl.sn=0;
		rl.chn++;
		rlio();
		return;
	}
	rltab.b_active = NULL;
	rltab.b_errcnt = 0;
	rltab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	iodone(bp);
	rlstart();
}

rlio()
{

	register struct device *rp;
	register dif;
	register int head;

	rp = RLADDR;
	dk_busy |= 1<<DK_N;
	dk_numb[DK_N] += 1;
	head = rl.bpart>>6;
	dk_wds[DK_N] += head;
	if (rl.cn[rl.dn] < 0) {
		rp->rlcs = (rl.dn << 8) | RDHDR;
		while ((rp->rlcs&CRDY) == 0)
			;
		rl.cn[rl.dn] = (rp->rlmp&077700) >> 6;
	}
	dif =(rl.cn[rl.dn] >> 1) - (rl.chn >>1);
	head = (rl.chn & 1) << 4;
	if (dif < 0)
		rp->rlda = (-dif <<7) | SEEKHI | head;
	else
		rp->rlda = (dif << 7) | SEEKLO | head;
	rp->rlcs = (rl.dn << 8) | SEEK;
	rl.cn[rl.dn] = rl.chn;
	if (rl.bleft < (rl.bpart = RLCYLSZ - (rl.sn * RLSECSZ)))
		rl.bpart = rl.bleft;
	while ((rp->rlcs&CRDY) == 0)
		;
	rp->rlda = (rl.chn << 6) | rl.sn;
	rp->rlba = rl.addr.w[1];
	rp->rlmp = -(rl.bpart >> 1);
	rp->rlcs = rl.com | rl.addr.w[0] << 4;
}

rlread(dev)
{

	physio(rlstrategy, &rrlbuf, dev, B_READ);
}

rlwrite(dev)
{

	physio(rlstrategy, &rrlbuf, dev, B_WRITE);
}
