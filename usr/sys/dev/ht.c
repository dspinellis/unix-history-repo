/*
 * TJU16 tape driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/file.h"
#include "../h/user.h"

struct	device
{
	int	htcs1;
	int	htwc;
	caddr_t	htba;
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
	int	htcs3;
};

struct	buf	httab;
struct	buf	rhtbuf;
struct	buf	chtbuf;

#define	NUNIT	1
#define	INF	1000000

char	h_flags[NUNIT];
char	h_openf[NUNIT];
daddr_t	h_blkno[NUNIT];
daddr_t	h_nxrec[NUNIT];

#define	HTADDR	((struct device *)0172440)

#define	GO	01
#define	WCOM	060
#define	RCOM	070
#define	NOP	0
#define	WEOF	026
#define	SFORW	030
#define	SREV	032
#define	ERASE	024
#define	REW	06
#define	DCLR	010
#define P800	01300		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	RDY	0200
#define	TM	04
#define	DRY	0200
#define EOT	02000
#define CS	02000
#define COR	0100000
#define PES	040
#define WRL	04000
#define MOL	010000
#define ERR	040000
#define FCE	01000
#define	TRE	040000
#define HARD	064023	/* UNS|OPI|NEF|FMT|RMR|ILR|ILF */

#define	CLR	040	/* controller clear (in cs2) */

#define	NED	010000

#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define SRETRY	4
#define SCOM	5
#define SOK	6

#define H_WRITTEN 1
htopen(dev, flag)
{
	register unit, ds;

	httab.b_flags |= B_TAPE;
	unit = minor(dev) & 077;
	if (unit >= NUNIT || h_openf[unit]) {
		u.u_error = ENXIO;
		return;
	}
	h_blkno[unit] = 0;
	h_nxrec[unit] = INF;
	h_flags[unit] = 0;
	ds = hcommand(dev, NOP);
	if ((ds&MOL)==0 || (flag && (ds&WRL)))
		u.u_error = ENXIO;
	if (u.u_error==0)
		h_openf[unit]++;
}

htclose(dev, flag)
{
	register int unit;

	unit = minor(dev) & 077;
	if (flag == FWRITE || ((flag&FWRITE) && (h_flags[unit]&H_WRITTEN))) {
		hcommand(dev, WEOF);
		hcommand(dev, WEOF);
		hcommand(dev, SREV);
	}
	if ((minor(dev)&0200) == 0)
		hcommand(dev, REW);
	h_openf[unit] = 0;
}

hcommand(dev, com)
{
	register struct buf *bp;

	bp = &chtbuf;
	spl5();
	while(bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	spl0();
	bp->b_dev = dev;
	bp->b_resid = com;
	bp->b_blkno = 0;
	bp->b_flags = B_BUSY|B_READ;
	htstrategy(bp);
	iowait(bp);
	if(bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags = 0;
	return(bp->b_resid);
}

htstrategy(bp)
register struct buf *bp;
{
	register daddr_t *p;

	if(bp != &chtbuf) {
		p = &h_nxrec[minor(bp->b_dev)&077];
		if(bp->b_blkno > *p) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			iodone(bp);
			return;
		}
		if(bp->b_blkno == *p && bp->b_flags&B_READ) {
			clrbuf(bp);
			bp->b_resid = bp->b_bcount;
			iodone(bp);
			return;
		}
		if ((bp->b_flags&B_READ)==0) {
			*p = bp->b_blkno + 1;
			h_flags[minor(bp->b_dev)&077] |= H_WRITTEN;
		}
	}
	bp->av_forw = NULL;
	spl5();
	if (httab.b_actf == NULL)
		httab.b_actf = bp;
	else
		httab.b_actl->av_forw = bp;
	httab.b_actl = bp;
	if (httab.b_active==0)
		htstart();
	spl0();
}

htstart()
{
	register struct buf *bp;
	register unit, den;
	daddr_t blkno;

    loop:
	if ((bp = httab.b_actf) == NULL)
		return;
	unit = minor(bp->b_dev) & 0177;
	HTADDR->htcs2 = ((unit>>3)&07);
	den = P1600 | (unit&07);
	if(unit > 077)
		den = P800 | (unit&07);
	if((HTADDR->httc&03777) != den)
		HTADDR->httc = den;
	if (HTADDR->htcs2 & NED || (HTADDR->htds&MOL)==0)
		goto abort;
	unit &= 077;
	blkno = h_blkno[unit];
	if (bp == &chtbuf) {
		if (bp->b_resid==NOP) {
			bp->b_resid = HTADDR->htds;
			goto next;
		}
		httab.b_active = SCOM;
		HTADDR->htfc = 0;
		HTADDR->htcs1 = bp->b_resid|IENABLE|GO;
		return;
	}
	if (h_openf[unit] < 0 || bp->b_blkno > h_nxrec[unit])
		goto abort;
	if (blkno == bp->b_blkno) {
		httab.b_active = SIO;
		HTADDR->htba = bp->b_un.b_addr;
		if(cputype == 70)
			HTADDR->htbae = bp->b_xmem;
		HTADDR->htfc = -bp->b_bcount;
		HTADDR->htwc = -(bp->b_bcount>>1);
		den = ((bp->b_xmem&3) << 8) | IENABLE | GO;
		if(bp->b_flags & B_READ)
			den |= RCOM;
		else {
			if(HTADDR->htds & EOT) {
				bp->b_resid = bp->b_bcount;
				goto next;
			}
			den |= WCOM;
		}
		HTADDR->htcs1 = den;
	} else {
		if (blkno < bp->b_blkno) {
			httab.b_active = SSFOR;
			HTADDR->htfc = blkno - bp->b_blkno;
			HTADDR->htcs1 = SFORW|IENABLE|GO;
		} else {
			httab.b_active = SSREV;
			HTADDR->htfc = bp->b_blkno - blkno;
			HTADDR->htcs1 = SREV|IENABLE|GO;
		}
	}
	return;

    abort:
	bp->b_flags |= B_ERROR;

    next:
	httab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

htintr()
{
	register struct buf *bp;
	register int unit, state;
	int	err;

	if ((bp = httab.b_actf)==NULL)
		return;
	unit = minor(bp->b_dev) & 077;
	state = httab.b_active;
	httab.b_active = 0;
	if (HTADDR->htcs1&TRE) {
		err = HTADDR->hter;
		if (HTADDR->htcs2&077400 || (err&HARD))
			state = 0;
		if (bp == &rhtbuf)
			err &= ~FCE;
		if ((bp->b_flags&B_READ) && (HTADDR->htds&PES))
			err &= ~(CS|COR);
		if((HTADDR->htds&MOL) == 0) {
			if(h_openf[unit])
				h_openf[unit] = -1;
		}
		else if(HTADDR->htds&TM) {
			HTADDR->htwc = -(bp->b_bcount>>1);
			h_nxrec[unit] = bp->b_blkno;
			state = SOK;
		}
		else if(state && err == 0)
			state = SOK;
		if(httab.b_errcnt > 4)
			deverror(bp, HTADDR->hter, HTADDR->htcs2);
		htinit();
		if (state==SIO && ++httab.b_errcnt < 10) {
			httab.b_active = SRETRY;
			h_blkno[unit]++;
			HTADDR->htfc = -1;
			HTADDR->htcs1 = SREV|IENABLE|GO;
			return;
		}
		if (state!=SOK) {
			bp->b_flags |= B_ERROR;
			state = SIO;
		}
	} else if (HTADDR->htcs1 < 0) {	/* SC */
		if(HTADDR->htds & ERR)
			htinit();
	}
	switch(state) {
	case SIO:
	case SOK:
		h_blkno[unit]++;

	case SCOM:
		httab.b_errcnt = 0;
		httab.b_actf = bp->av_forw;
		iodone(bp);
		bp->b_resid = (-HTADDR->htwc)<<1;
		break;

	case SRETRY:
		if((bp->b_flags&B_READ)==0) {
			httab.b_active = SSFOR;
			HTADDR->htcs1 = ERASE|IENABLE|GO;
			return;
		}

	case SSFOR:
	case SSREV:
		if(HTADDR->htds & TM) {
			if(state == SSREV) {
				h_nxrec[unit] = bp->b_blkno - HTADDR->htfc;
				h_blkno[unit] = h_nxrec[unit];
			} else {
				h_nxrec[unit] = bp->b_blkno + HTADDR->htfc - 1;
				h_blkno[unit] = h_nxrec[unit]+1;
			}
		} else
			h_blkno[unit] = bp->b_blkno;
		break;

	default:
		return;
	}
	htstart();
}

htinit()
{
	register ocs2;
	register omttc;
	
	omttc = HTADDR->httc & 03777;	/* preserve old slave select, dens, format */
	ocs2 = HTADDR->htcs2 & 07;	/* preserve old unit */

	HTADDR->htcs2 = CLR;
	HTADDR->htcs2 = ocs2;
	HTADDR->httc = omttc;
	HTADDR->htcs1 = DCLR|GO;
}

htread(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_READ);
}

htwrite(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_WRITE);
}

htphys(dev)
{
	register unit;
	daddr_t a;

	unit = minor(dev) & 077;
	if(unit < NUNIT) {
		a = u.u_offset >> 9;
		h_blkno[unit] = a;
		h_nxrec[unit] = a+1;
	}
}
