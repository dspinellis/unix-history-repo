#
/*
 * TC-11 DECtape driver
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/user.h"

struct {
	char	lbyte;
	char	hbyte;
};

struct {
	int	tccsr;
	int	tccm;
	int	tcwc;
	int	tcba;
	int	tcdt;
};

#define	TCADDR	0177340
#define	NTCBLK	578
#define	JTC	2

#define	TAPERR	0100000
#define	TREV	04000
#define	READY	0200
#define	IENABLE	0100
#define	UPS	0200

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
}

tcstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;

	bp = abp;
	bp->av_forw = 0;
	spl6();
	if (devtab[JTC].d_actf==0)
		devtab[JTC].d_actf = bp;
	else
		devtab[JTC].d_actl->av_forw = bp;
	devtab[JTC].d_actl = bp;
	if (devtab[JTC].d_active==0)
		tcstart();
	spl0();
}

tcstart()
{
	register struct buf *bp;
	register int *tccmp, com;

	if ((bp = devtab[JTC].d_actf) == 0)
		return;
	tccmp = &TCADDR->tccm;
	if (((*tccmp).hbyte&07) != bp->b_dev.d_minor)
		(*tccmp).lbyte = SAT|GO;
	devtab[JTC].d_errcnt = 20;
	devtab[JTC].d_active = SFORW;
	com = (bp->b_dev.d_minor<<8) | IENABLE|RNUM|GO;
	if ((TCADDR->tccsr & UPS) == 0) {
		com =| TREV;
		devtab[JTC].d_active = SREV;
	}
	*tccmp = com;
}

tcintr()
{
	register struct buf *bp;
	struct buf *sbp;
	register int *tccmp;
	register int *tcdtp;

	tccmp = &TCADDR->tccm;
	bp = devtab[JTC].d_actf;
	if (*tccmp&TAPERR) {
		*tccmp =& ~TAPERR;
		if (--devtab[JTC].d_errcnt  <= 0) {
			bp->b_flags =| B_ERROR;
			goto done;
		}
		if (*tccmp&TREV) {
		setforw:
			devtab[JTC].d_active = SFORW;
			*tccmp =& ~TREV;
		} else {
		setback:
			devtab[JTC].d_active = SREV;
			*tccmp =| TREV;
		}
		(*tccmp).lbyte = IENABLE|RNUM|GO;
		return;
	}
	tcdtp = &TCADDR->tcdt;
	switch (devtab[JTC].d_active) {

	case SIO:
	done:
		devtab[JTC].d_active = 0;
		sbp = bp;
		if (devtab[JTC].d_actf = bp->av_forw)
			tcstart();
		else
			TCADDR->tccm.lbyte = SAT|GO;
		iodone(sbp);
		return;

	case SFORW:
		if (*tcdtp > bp->b_blkno)
			goto setback;
		if (*tcdtp < bp->b_blkno)
			goto setforw;
		*--tcdtp = bp->b_addr;		/* core address */
		*--tcdtp = bp->b_wcount;
		tccmp->lbyte = (bp->b_flags&B_XMEM) | IENABLE|GO
		    | (bp->b_flags&B_READ?RDATA:WDATA);
		devtab[JTC].d_active = SIO;
		return;

	case SREV:
		if (*tcdtp+3 > bp->b_blkno)
			goto setback;
		goto setforw;
	}
}
