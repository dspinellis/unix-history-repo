/*	ht.c	4.4	%G%	*/

#include "ht.h"
#if NHT > 0
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
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/mba.h"
#include "../h/vm.h"
#include "../h/cmap.h"

struct	device
{
	int	htcs1;
	int	htds;
	int	hter;
	int	htmr;
	int	htas;
	int	htfc;
	int	htdt;
	int	htck;
	int	htsn;
	int	httc;
};

struct	buf	httab;
struct	buf	rhtbuf;
struct	buf	chtbuf;

#define	INF	1000000

char	h_openf[NHT];
daddr_t	h_blkno[NHT];
char	h_flags[NHT];
daddr_t	h_nxrec[NHT];

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
#define	P800	01700		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	RDY	0200
#define	TM	04
#define	DRY	0200
#define	EOT	02000
#define	CS	02000
#define	COR	0100000
#define	PES	040
#define	WRL	04000
#define	MOL	010000
#define	ERR	040000
#define	FCE	01000
#define	TRE	040000
#define	HARD	064023	/* UNS|OPI|NEF|FMT|RMR|ILR|ILF */

#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define	SRETRY	4
#define	SCOM	5
#define	SOK	6

#define	H_WRITTEN 1

htopen(dev, flag)
{
	register unit, ds;

	if ((mbaact&(1<<HTMBANUM)) == 0)
		mbainit(HTMBANUM);
	httab.b_flags |= B_TAPE;
	unit = minor(dev) & 03;
	if (unit >= NHT || h_openf[unit]) {
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

	unit = minor(dev) & 03;
	if (flag == FWRITE || ((flag&FWRITE) && (h_flags[unit]&H_WRITTEN))) {
		(void) hcommand(dev, WEOF);
		(void) hcommand(dev, WEOF);
		(void) hcommand(dev, SREV);
	}
	if((minor(dev)&4) == 0) /* no 4 -> rewind */
		(void) hcommand(dev, REW);
	h_openf[unit] = 0;
}

hcommand(dev, com)
{
	register struct buf *bp;

	bp = &chtbuf;
	(void) spl5();
	while(bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	(void) spl0();
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
		p = &h_nxrec[minor(bp->b_dev)&03];
		if(dbtofsb(bp->b_blkno) > *p) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			iodone(bp);
			return;
		}
		if(dbtofsb(bp->b_blkno) == *p && bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);
			iodone(bp);
			return;
		}
		if ((bp->b_flags&B_READ)==0) {
			*p = dbtofsb(bp->b_blkno) + 1;
			h_flags[minor(bp->b_dev)&03] |=  H_WRITTEN;
		}
	}
	bp->av_forw = NULL;
	(void) spl5();
	if (httab.b_actf == NULL)
		httab.b_actf = bp;
	else
		httab.b_actl->av_forw = bp;
	httab.b_actl = bp;
	if (httab.b_active==0)
		htstart();
	(void) spl0();
}

htstart()
{
	register struct buf *bp;
	register unit, den;
	daddr_t blkno;
	register struct device *htp = mbadev(HTMBA,0);

    loop:
	if ((bp = httab.b_actf) == NULL)
		return;
	unit = minor(bp->b_dev);
	den = P800 | (unit&03);
	if(unit >= 8)
		den = P1600 | (unit&03);
	if((htp->httc&03777) != den)
		htp->httc = den;
	unit &= 03;
	blkno = h_blkno[unit];
	if (bp == &chtbuf) {
		if (bp->b_resid==NOP) {
			bp->b_resid = htp->htds & 0xffff;
			goto next;
		}
		httab.b_active = SCOM;
		htp->htfc = 0;
		htp->htcs1 = bp->b_resid|GO;
		return;
	}
	if (h_openf[unit] < 0 || dbtofsb(bp->b_blkno) > h_nxrec[unit])
		goto abort;
	if (blkno == dbtofsb(bp->b_blkno)) {
		httab.b_active = SIO;
		htp->htfc = -bp->b_bcount;
		mbastart(bp, (int *)htp);
	} else {
		if (blkno < dbtofsb(bp->b_blkno)) {
			httab.b_active = SSFOR;
			htp->htfc = blkno - dbtofsb(bp->b_blkno);
			htp->htcs1 = SFORW|GO;
		} else {
			httab.b_active = SSREV;
			htp->htfc = dbtofsb(bp->b_blkno) - blkno;
			htp->htcs1 = SREV|GO;
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

/*ARGSUSED*/
htintr(mbastat, as)
{
	register struct buf *bp;
	register int unit, state;
	int err;
	register struct device *htp = mbadev(HTMBA,0);

	if ((bp = httab.b_actf)==NULL)
		return;
	unit = minor(bp->b_dev) & 03;
	state = httab.b_active;
	httab.b_active = 0;
	if (htp->htds&(ERR|EOT|TM) || mbastat & MBAEBITS) {
		err = htp->hter & 0xffff;
		if ((mbastat & MBAEBITS) || (err&HARD))
			state = 0;
		if (bp == &rhtbuf)
			err &= ~FCE;
		if ((bp->b_flags&B_READ) && (htp->htds&PES))
			err &= ~(CS|COR);
		if(htp->htds&EOT || (htp->htds&MOL)==0) {
			if(h_openf[unit])
				h_openf[unit] = -1;
		}
		else if(htp->htds&TM) {
			htp->htfc = 0;
			h_nxrec[unit] = dbtofsb(bp->b_blkno);
			state = SOK;
		}
		else if(state && err == 0)
			state = SOK;
		if(httab.b_errcnt > 4)
			deverror(bp, htp->hter, mbastat);
		HTMBA->mba_cr &= ~MBAIE;
		htp->htcs1 = DCLR|GO;
		HTMBA->mba_cr |= MBAIE;
		if (state==SIO && ++httab.b_errcnt < 10) {
			httab.b_active = SRETRY;
			h_blkno[unit]++;
			htp->htfc = -1;
			htp->htcs1 = SREV|GO;
			return;
		}
		if (state!=SOK) {
			bp->b_flags |= B_ERROR;
			state = SIO;
		}
	} else if (htp->htcs1 < 0) {	/* SC */
		if(htp->htds & ERR) {
			HTMBA->mba_cr &= ~MBAIE;
			htp->htcs1 = DCLR|GO;
			HTMBA->mba_cr |= MBAIE;
		}
	}
	switch(state) {
	case SIO:
	case SOK:
		h_blkno[unit]++;

	case SCOM:
		httab.b_errcnt = 0;
		httab.b_actf = bp->av_forw;
		bp->b_resid = - (htp->htfc & 0xffff);
		if (bp->b_flags & B_READ)
			bp->b_resid += bp->b_bcount;
		iodone(bp);
		break;

	case SRETRY:
		if((bp->b_flags&B_READ)==0) {
			httab.b_active = SSFOR;
			htp->htcs1 = ERASE|GO;
			return;
		}

	case SSFOR:
	case SSREV:
#define blk dbtofsb(bp->b_blkno)
		if(htp->htds & TM) {
			if(state == SSREV) {
				h_nxrec[unit] = blk - (htp->htfc&0xffff);
				h_blkno[unit] = h_nxrec[unit];
			} else {
				h_nxrec[unit] = blk + (htp->htfc&0xffff) - 1;
				h_blkno[unit] = blk + (htp->htfc & 0xffff);
			}
		} else
			h_blkno[unit] = blk;
		break;
#undef blk

	default:
		return;
	}
	htstart();
}

htread(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_READ, minphys);
}

htwrite(dev)
{
	htphys(dev);
	physio(htstrategy, &rhtbuf, dev, B_WRITE, minphys);
}

htphys(dev)
{
	register unit;
	daddr_t a;

	unit = minor(dev) & 03;
	if(unit < NHT) {
		a = u.u_offset >> 9;
		h_blkno[unit] = dbtofsb(a);
		h_nxrec[unit] = dbtofsb(a)+1;
	}
}

#define	HTADDR	((struct device *)(HTPHYSMBA + 0x400))
#define	HTMAP ((struct pte *) (HTPHYSMBA + 0x800))

#define	DBSIZE	20

twall(start, num)
	char *start;
	int num;
{
	int blk;

	HTADDR->httc = P800;	/* set 800 bpi mode */
	HTADDR->htcs1 = DCLR | GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		twrite(start, blk);
		start += blk*NBPG;
		num -= blk;
	}
}

twrite(buf, num)
char *buf;
{
	register struct pte *hpte = HTMAP;
	register int i;

	twait();
	HTADDR->htfc = -(num*NBPG);
	for (i = 0; i < num; i++)
		*(int *)hpte++ = (btop(buf)+i) | PG_V;
	((struct mba_regs *)PHYSMBA1)->mba_sr = -1;
	((struct mba_regs *)PHYSMBA1)->mba_bcr = -(num*NBPG);
	((struct mba_regs *)PHYSMBA1)->mba_var = 0;
	HTADDR->htcs1 = WCOM | GO;
}

twait()
{
	register s;

	do
		s = HTADDR->htds;
	while ((s & RDY) == 0);
}

rewind()
{

	twait();
	HTADDR->htcs1 = REW | GO;
}

teof()
{

	twait();
	HTADDR->htcs1 = WEOF | GO;
}
#endif
