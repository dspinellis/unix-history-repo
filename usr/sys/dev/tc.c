#

/*
 * TC-11 DECtape driver
 */

#include "../h/param.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/user.h"

struct device {
	int	tccsr;
	union wb {
		int	w;
		char	b[2];
	} tccm;
	int	tcwc;
	int	tcba;
	int	tcdt;
};

struct	buf	tctab;
char	tcper[8];

#define	TCADDR	((struct device *) 0177340)
#define	NTCBLK	578

#define	TAPERR	0100000
#define	TREV	04000
#define	READY	0200
#define	IENABLE	0100
#define	UPS	0200
#define	ENDZ	0100000
#define	BLKM	02000
#define	ILGOP	010000
#define	SELERR	04000

#define	SAT	0
#define	RNUM	02
#define	RDATA	04
#define	SST	010
#define	WDATA	014
#define	GO	01

#define	SFORW	1
#define	SREV	2
#define	SIO	3

tcclose(dev)
{
	bflush(dev);
	tcper[dev&07] = 0;
}

tcstrategy(bp)
register struct buf *bp;
{

	if(bp->b_flags&B_PHYS)
		mapalloc(bp);
	bp->b_resid = 0;
	if(bp->b_blkno >= NTCBLK || tcper[minor(bp->b_dev)&07]) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl6();
	if (tctab.b_actf==0)
		tctab.b_actf = bp;
	else
		tctab.b_actl->av_forw = bp;
	tctab.b_actl = bp;
	if (tctab.b_active==0)
		tcstart();
	spl0();
}

tcstart()
{
	register struct buf *bp;
	register int com;
	register union wb *tccmp;

loop:
	tccmp = &TCADDR->tccm;
	if ((bp = tctab.b_actf) == 0)
		return;
	if(tcper[minor(bp->b_dev)&07]) {
		if((tctab.b_actf = bp->av_forw) == 0)
			tccmp->b[0] = SAT|GO;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	if ((tccmp->b[1]&07) != minor(bp->b_dev))
		tccmp->b[0] = SAT|GO;
	tctab.b_errcnt = 20;
	tctab.b_active = SFORW;
	com = (minor(bp->b_dev)<<8) | IENABLE|RNUM|GO;
	if ((TCADDR->tccsr & UPS) == 0) {
		com |= TREV;
		tctab.b_active = SREV;
	}
	tccmp->w = com;
}

tcintr()
{
	register struct buf *bp;
	register union wb *tccmp;
	register int *tcdtp;

	tccmp = &TCADDR->tccm;
	tcdtp = &TCADDR->tccsr;
	bp = tctab.b_actf;
	if (tccmp->w&TAPERR) {
		if((*tcdtp&(ENDZ|BLKM)) == 0)
			deverror(bp, *tcdtp, 0);
		if(*tcdtp & (ILGOP|SELERR)) {
			tcper[bp->b_dev&07]++;
			tctab.b_errcnt = 0;
		}
		tccmp->w &= ~TAPERR;
		if (--tctab.b_errcnt == 0) {
			bp->b_flags |= B_ERROR;
			goto done;
		}
		if (tccmp->w&TREV) {
		setforw:
			tctab.b_active = SFORW;
			tccmp->w &= ~TREV;
		} else {
		setback:
			tctab.b_active = SREV;
			tccmp->w |= TREV;
		}
		tccmp->b[0] = IENABLE|RNUM|GO;
		return;
	}
	tcdtp = &TCADDR->tcdt;
	switch (tctab.b_active) {

	case SIO:
	done:
		tctab.b_active = 0;
		if (tctab.b_actf = bp->av_forw)
			tcstart();
		else
			TCADDR->tccm.b[0] = SAT|GO;
		iodone(bp);
		return;

	case SFORW:
		if (*tcdtp > bp->b_blkno)
			goto setback;
		if (*tcdtp < bp->b_blkno)
			goto setforw;
		*--tcdtp = (int)bp->b_un.b_addr;	/* core address */
		*--tcdtp = -(bp->b_bcount>>1);
		tccmp->b[0] = ((bp->b_xmem & 03) << 4) | IENABLE|GO
		    | (bp->b_flags&B_READ?RDATA:WDATA);
		tctab.b_active = SIO;
		return;

	case SREV:
		if (*tcdtp+3 > bp->b_blkno)
			goto setback;
		goto setforw;
	}
}
