#
/*
 */

/*
 * TJU16 tape driver
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"

struct {
	int	htcs1;
	int	htwc;
	int	htba;
	int	htfc;
	int	htcs2;
	int	htds;
	int	hter;
	int	htas;
	int	htck;
	int	htdb;
	int	htmr;
	int	htdt;
	int	htsn;
	int	httc;
	int	htbae;	/* 11/70 bus extension */
};

struct	devtab	httab;
struct	buf	rhtbuf;

#define	NUNIT	8

char	h_openf[NUNIT];
char	*h_blkno[NUNIT];
char	*h_nxrec[NUNIT];

#define	HTADDR	0172440

#define	GO	01
#define	NOP	0
#define	WEOF	026
#define	SFORW	030
#define	SREV	032
#define	ERASE	024
#define	REW	06
#define	CLR	010
#define	P800	01300		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	CRDY	0200
#define	EOF	04
#define	DRY	0200
#define	MOL	010000
#define	PIP	020000
#define	ERR	040000

#define	SSEEK	1
#define	SIO	2

htopen(dev, flag)
{
	register unit;

	unit = dev.d_minor&077;
	if (unit >= NUNIT || h_openf[unit])
		u.u_error = ENXIO;
	else {
		h_openf[unit]++;
		h_blkno[unit] = 0;
		h_nxrec[unit] = 65535;
		hcommand(dev, NOP);
	}
}

htclose(dev, flag)
{
	register int unit;

	unit = dev.d_minor&077;
	h_openf[unit] = 0;
	if (flag) {
		hcommand(dev, WEOF);
		hcommand(dev, WEOF);
	}
	hcommand(dev, REW);
}

hcommand(dev, com)
{
	register unit;
	extern lbolt;

	unit = dev.d_minor;
	while (httab.d_active || (HTADDR->htcs1 & CRDY)==0)
		sleep(&lbolt, 1);
	HTADDR->htcs2 = (unit>>3)&07;
	while((HTADDR->htds&DRY) == 0)
		sleep(&lbolt, 1);
	if(unit >= 64)
		HTADDR->httc = P800 | (unit&07); else
		HTADDR->httc = P1600 | (unit&07);
	while((HTADDR->htds&(MOL|PIP)) != MOL)
		sleep(&lbolt, 1);
	HTADDR->htcs1 = com | GO;
}

htstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register char **p;

	bp = abp;
	p = &h_nxrec[bp->b_dev.d_minor&077];
	if (*p <= bp->b_blkno) {
		if (*p < bp->b_blkno) {
			bp->b_flags =| B_ERROR;
			iodone(bp);
			return;
		}
		if (bp->b_flags&B_READ) {
			clrbuf(bp);
			iodone(bp);
			return;
		}
	}
	if ((bp->b_flags&B_READ)==0)
		*p = bp->b_blkno + 1;
	bp->av_forw = 0;
	spl5();
	if (httab.d_actf==0)
		httab.d_actf = bp;
	else
		httab.d_actl->av_forw = bp;
	httab.d_actl = bp;
	if (httab.d_active==0)
		htstart();
	spl0();
}

htstart()
{
	register struct buf *bp;
	register int unit;
	register char *blkno;

    loop:
	if ((bp = httab.d_actf) == 0)
		return;
	unit = bp->b_dev.d_minor;
	HTADDR->htcs2 = (unit>>3)&07;
	if(unit >= 64)
		HTADDR->httc = P800 | (unit&07); else
		HTADDR->httc = P1600 | (unit&07);
	unit =& 077;
	blkno = h_blkno[unit];
	if (h_openf[unit] < 0 || (HTADDR->htcs1 & CRDY)==0) {
		bp->b_flags =| B_ERROR;
		httab.d_actf = bp->av_forw;
		iodone(bp);
		goto loop;
	}
	if (blkno != bp->b_blkno) {
		httab.d_active = SSEEK;
		if (blkno < bp->b_blkno) {
			HTADDR->htfc = blkno - bp->b_blkno;
			HTADDR->htcs1 = SFORW|IENABLE|GO;
		} else {
			if (bp->b_blkno == 0)
				HTADDR->htcs1 = REW|IENABLE|GO;
			else {
				HTADDR->htfc = bp->b_blkno - blkno;
				HTADDR->htcs1 = SREV|IENABLE|GO;
			}
		}
		return;
	}
	httab.d_active = SIO;
	rhstart(bp, &HTADDR->htfc, bp->b_wcount<<1, &HTADDR->htbae);
}

htintr()
{
	register struct buf *bp;
	register int unit;

	if ((bp = httab.d_actf)==0)
		return;
	unit = bp->b_dev.d_minor&077;
	if (HTADDR->htcs1 & ERR) {
/*
		deverror(bp, HTADDR->hter, 0);
 */
		if(HTADDR->htds&EOF) {
			if(bp != &rhtbuf && h_openf[unit])
				h_openf[unit] = -1;
		}
		HTADDR->htcs1 = ERR|CLR|GO;
		if ((HTADDR->htds&DRY)!=0 && httab.d_active==SIO) {
			if (++httab.d_errcnt < 10) {
				h_blkno[unit]++;
				httab.d_active = 0;
				htstart();
				return;
			}
		}
		bp->b_flags =| B_ERROR;
		httab.d_active = SIO;
	}
	if (httab.d_active == SIO) {
		httab.d_errcnt = 0;
		h_blkno[unit]++;
		httab.d_actf = bp->av_forw;
		httab.d_active = 0;
		iodone(bp);
		bp->b_resid = HTADDR->htfc;
	} else
		h_blkno[unit] = bp->b_blkno;
	htstart();
}

htread(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_READ);
	u.u_count = -rhtbuf.b_resid;
}

htwrite(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_WRITE);
	u.u_count = 0;
}

htphys(dev)
{
	register unit, a;

	unit = dev.d_minor;
	a = lshift(u.u_offset, -9);
	h_blkno[unit] = a;
	h_nxrec[unit] = ++a;
}
