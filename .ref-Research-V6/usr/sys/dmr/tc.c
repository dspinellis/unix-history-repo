#
/*
 */

/*
 * TC-11 DECtape driver
 */

#include "../param.h"
#include "../conf.h"
#include "../buf.h"
#include "../user.h"

struct {
	int	tccsr;
	int	tccm;
	int	tcwc;
	int	tcba;
	int	tcdt;
};

struct	devtab	tctab;
char	tcper[8];

#define	TCADDR	0177340
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

tcstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;

	bp = abp;
	if(bp->b_flags&B_PHYS)
		mapalloc(bp);
	if(bp->b_blkno >= NTCBLK || tcper[bp->b_dev&07]) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	spl6();
	if (tctab.d_actf==0)
		tctab.d_actf = bp;
	else
		tctab.d_actl->av_forw = bp;
	tctab.d_actl = bp;
	if (tctab.d_active==0)
		tcstart();
	spl0();
}

tcstart()
{
	register struct buf *bp;
	register int *tccmp, com;

loop:
	tccmp = &TCADDR->tccm;
	if ((bp = tctab.d_actf) == 0)
		return;
	if(tcper[bp->b_dev&07]) {
		if((tctab.d_actf = bp->av_forw) == 0)
			(*tccmp).lobyte = SAT|GO;
		bp->b_flags =| B_ERROR;
		iodone(bp);
		goto loop;
	}
	if (((*tccmp).hibyte&07) != bp->b_dev.d_minor)
		(*tccmp).lobyte = SAT|GO;
	tctab.d_errcnt = 20;
	tctab.d_active = SFORW;
	com = (bp->b_dev.d_minor<<8) | IENABLE|RNUM|GO;
	if ((TCADDR->tccsr & UPS) == 0) {
		com =| TREV;
		tctab.d_active = SREV;
	}
	*tccmp = com;
}

tcintr()
{
	register struct buf *bp;
	register int *tccmp;
	register int *tcdtp;

	tccmp = &TCADDR->tccm;
	tcdtp = &TCADDR->tccsr;
	bp = tctab.d_actf;
	if (*tccmp&TAPERR) {
		if((*tcdtp&(ENDZ|BLKM)) == 0)
			deverror(bp, *tcdtp, 0);
		if(*tcdtp & (ILGOP|SELERR)) {
			tcper[bp->b_dev&07]++;
			tctab.d_errcnt = 0;
		}
		*tccmp =& ~TAPERR;
		if (--tctab.d_errcnt  <= 0) {
			bp->b_flags =| B_ERROR;
			goto done;
		}
		if (*tccmp&TREV) {
		setforw:
			tctab.d_active = SFORW;
			*tccmp =& ~TREV;
		} else {
		setback:
			tctab.d_active = SREV;
			*tccmp =| TREV;
		}
		(*tccmp).lobyte = IENABLE|RNUM|GO;
		return;
	}
	tcdtp = &TCADDR->tcdt;
	switch (tctab.d_active) {

	case SIO:
	done:
		tctab.d_active = 0;
		if (tctab.d_actf = bp->av_forw)
			tcstart();
		else
			TCADDR->tccm.lobyte = SAT|GO;
		iodone(bp);
		return;

	case SFORW:
		if (*tcdtp > bp->b_blkno)
			goto setback;
		if (*tcdtp < bp->b_blkno)
			goto setforw;
		*--tcdtp = bp->b_addr;		/* core address */
		*--tcdtp = bp->b_wcount;
		tccmp->lobyte = ((bp->b_xmem & 03) << 4) | IENABLE|GO
		    | (bp->b_flags&B_READ?RDATA:WDATA);
		tctab.d_active = SIO;
		return;

	case SREV:
		if (*tcdtp+3 > bp->b_blkno)
			goto setback;
		goto setforw;
	}
}
