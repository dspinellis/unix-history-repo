#
/*
 * RK disk driver
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"

struct buf *rk_q[NRK];
int	*rk_ap;

struct {
	int rkds;
	int rker;
	int rkcs;
	int rkwc;
	int rkba;
	int rkda;
};

#define	RKADDR	0177400
#define	NRKBLK	4872
#define	JRK	1	/* temp */

#define	RESET	0
#define	GO	01
#define	SEEK	010
#define	DRESET	014
#define	IENABLE	0100
#define	DRY	0200
#define	ARDY	0100
#define	WLO	020000
#define	CTLRDY	0200
#define	SEEKCMP 020000

#define	B_SEEK	02000

rkstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register *qc, *ql;
	int d;

	bp = abp;
	if (bp->b_blkno >= NRKBLK
	 && (bp->b_blkno >= NRKBLK*3 || bp->b_dev.d_major==JRK)) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	bp->b_flags =& ~B_SEEK;
	if (bp->b_dev.d_major==JRK)
		d = bp->b_dev.d_minor;
	else
		d = bp->b_blkno%3;
	spl5();
	if ((ql = *(qc = &rk_q[d])) == NULL) {
		*qc = bp;
		if (RKADDR->rkcs&CTLRDY)
			rkstart();
		goto ret;
	}
	while ((qc = ql->av_forw) != NULL) {
		if (ql->b_blkno<bp->b_blkno
		 && bp->b_blkno<qc->b_blkno
		 || ql->b_blkno>bp->b_blkno
		 && bp->b_blkno>qc->b_blkno) {
			ql->av_forw = bp;
			bp->av_forw = qc;
			goto ret;
		}
		ql = qc;
	}
	ql->av_forw = bp;
    ret:
	spl0();
}

rkstart()
{
	register struct buf *bp;
	struct buf *sbp;
	int *qp;

	for (qp = rk_q; qp < &rk_q[NRK];) {
		if ((bp = *qp++) && (bp->b_flags&B_SEEK)==0) {
			sbp = bp;
			RKADDR->rkda = rkaddr(bp);
			bp = sbp;
			rkcommand(IENABLE|SEEK|GO);
			if (RKADDR->rkcs<0) {	/* error bit */
				bp->b_flags =| B_ERROR;
				*--qp = bp->av_forw;
				iodone(bp);
				rkerror();
			} else
				bp->b_flags =| B_SEEK;
		}
	}
}

rkaddr(bp)
struct buf *bp;
{
	register struct buf *p;
	register int b;
	int d;

	p = bp;
	b = p->b_blkno;
	if (p->b_dev.d_major==JRK)
		d = p->b_dev.d_minor;
	else {
		d = b%3;
		b =/ 3;
	}
	return(d<<13 | (b/12)<<4 | b%12);
}

rkintr()
{
	register struct buf *bp;

	if (RKADDR->rkcs < 0) {		/* error bit */
		if (RKADDR->rker&WLO || ++devtab[JRK].d_errcnt>10)
			rkpost(B_ERROR);
		rkerror();
	}
	if (RKADDR->rkcs&SEEKCMP) {
		rk_ap = &rk_q[(RKADDR->rkds>>13) & 07];
		devstart(*rk_ap, &RKADDR->rkda, rkaddr(*rk_ap));
	} else
		rkpost(0);
}

rkpost(errbit)
{
	register struct buf *bp;

	if (rk_ap) {
		bp = *rk_ap;
		bp->b_flags =| B_DONE | errbit;
		*rk_ap = bp->av_forw;
		rk_ap = NULL;
		iodone(bp);
		devtab[JRK].d_errcnt = 0;
		rkstart();
	}
}

rkerror()
{
	int *qp;
	register struct buf *bp;

	rkcommand(IENABLE|RESET|GO);
	for (qp = rk_q; qp < &rk_q[NRK];)
		if ((bp = *qp++) != NULL && bp->b_flags&B_SEEK) {
			RKADDR->rkda = rkaddr(bp);
			while ((RKADDR->rkds&(DRY|ARDY)) == DRY);
			rkcommand(IENABLE|DRESET|GO);
		}
}

rkcommand(com)
{
	RKADDR->rkcs = com;
	while((RKADDR->rkcs&CTLRDY)==0);
}
