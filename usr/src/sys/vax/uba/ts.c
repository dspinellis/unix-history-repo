/*	ts.c	4.6	81/03/09	*/

#include "ts.h"
#if NTS > 0
/*
 * TS11 tape driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/file.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/uba.h"
#include "../h/vm.h"

struct	device {
	u_short	tsdb;
	u_short	tssr;
};

struct	buf	tstab;
struct	buf	rtsbuf;
struct	buf	ctsbuf;

#define	INF	1000000000

u_short	ts_uba;
int	ts_ubinfo;
char	ts_flags;
char	ts_openf;
daddr_t	ts_blkno;
daddr_t	ts_nxrec;

/* status message */
struct	sts {
	u_short	s_sts;
	u_short	len;
	u_short rbpcr;
	u_short	xs0;
	u_short	xs1;
	u_short	xs2;
	u_short	xs3;
};

/* Error codes in stat 0 */
#define	TMK	0100000
#define	RLS	040000
#define	ONL	0100
#define	WLE	04000

/* command message */
struct cmd {
	u_short	c_cmd;
	u_short	c_loba;
	u_short	c_hiba;
	u_short	c_size;
};

#define	ACK	0100000
#define	CVC	040000
#define	IE	0200
#define	READ	01
#define	REREAD	01001

#define	SETCHR	04

#define	WRITE	05
#define	REWRITE	01005

#define	SFORW	010
#define	SREV	0410
#define	REW	02010

#define	WTM	011

#define	GSTAT	017

/* characteristics data */
struct charac {
	u_short	char_loba;
	u_short	char_hiba;
	u_short	char_size;
	u_short	char_mode;
};

/* All the packets, collected */
struct tsmesg {
	struct	cmd ts_cmd;
	struct	sts ts_sts;
	struct	charac ts_char;
	int	align;		/* Should force alignment */
} ts;

/* Bits in (unibus) status register */
#define	SC	0100000
#define	SSR	0200
#define	OFL	0100
#define	NBA	02000

/* states */
#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define SRETRY	4
#define SCOM	5
#define SOK	6

#define H_WRITTEN 1

tsopen(dev, flag)
{
	register struct device *tsaddr = TSADDR;
	static struct tsmesg *ubaddr;

	tstab.b_flags |= B_TAPE;
	if (ts_openf) {
		u.u_error = ENXIO;
		return;
	}
	if (ubaddr==0 || tsaddr->tssr&(OFL|NBA) || (tsaddr->tssr&SSR)==0) {
		long i = 0;
		tsaddr->tssr = 0;
		while ((tsaddr->tssr & SSR)==0) {
			if (++i > 1000000) {
				printf("Tape unready\n");
				u.u_error = ENXIO;
				return;
			}
		}
	}
	if (tsaddr->tssr&OFL) {
		printf("Tape offline\n");
		u.u_error = ENXIO;
		return;
	}
	if (tsaddr->tssr&NBA) {
		ctsbuf.b_un.b_addr = (caddr_t) &ts;
		ctsbuf.b_bcount = sizeof(ts);
		if (ubaddr == 0)
			ubaddr = (struct tsmesg *)ubasetup(&ctsbuf, 0);
		ts_uba = (u_short)((long)ubaddr + (((long)ubaddr >> 16) & 03));
		ts.ts_char.char_loba = (int)&ubaddr->ts_sts;
		ts.ts_char.char_hiba = (u_short)((long)&ubaddr->ts_sts >> 16) & 03;
		ts.ts_char.char_size = sizeof(ts.ts_sts);
		ts.ts_char.char_mode = 0400;		/* Stop on 2 tape marks */
		ts.ts_cmd.c_cmd = ACK + 04;	/* write characteristics */
		ts.ts_cmd.c_loba = (int)&ubaddr->ts_char;
		ts.ts_cmd.c_hiba = (u_short)((long)&ubaddr->ts_sts >> 16) & 03;
		ts.ts_cmd.c_size = sizeof(ts.ts_sts);
		tsaddr->tsdb = ts_uba;
	}
	ts_blkno = 0;
	ts_nxrec = INF;
	ts_flags = 0;
	if (u.u_error==0)
		ts_openf++;
}

tsclose(dev, flag)
{

	if (flag == FWRITE || ((flag&FWRITE) && (ts_flags&H_WRITTEN))) {
		tscommand(WTM);
		tscommand(WTM);
		tscommand(SREV);
	}
	if ((minor(dev)&4) == 0)
		tscommand(REW);
	ts_openf = 0;
}

tscommand(com)
{
	register struct buf *bp;

	bp = &ctsbuf;
	spl5();
	while(bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	spl0();
	bp->b_resid = com;
	bp->b_blkno = 0;
	bp->b_flags = B_BUSY|B_READ;
	tsstrategy(bp);
	iowait(bp);
	if(bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags = 0;
	return(bp->b_resid);
}

tsstrategy(bp)
register struct buf *bp;
{
	register daddr_t *p;

	if(bp != &ctsbuf) {
		p = &ts_nxrec;
		if(dbtofsb(bp->b_blkno) > *p) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			iodone(bp);
			return;
		}
		if(dbtofsb(bp->b_blkno) == *p && bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			iodone(bp);
			return;
		}
		if ((bp->b_flags&B_READ)==0) {
			*p = dbtofsb(bp->b_blkno) + 1;
			ts_flags |= H_WRITTEN;
		}
	}
	bp->av_forw = NULL;
	spl5();
	if (tstab.b_actf == NULL)
		tstab.b_actf = bp;
	else
		tstab.b_actl->av_forw = bp;
	tstab.b_actl = bp;
	if (tstab.b_active==0)
		tsstart();
	spl0();
}

tsstart()
{
	register struct buf *bp;
	register struct device *tsaddr = TSADDR;
	daddr_t blkno;

    loop:
	if ((bp = tstab.b_actf) == NULL)
		return;
	blkno = ts_blkno;
	if (ts_openf < 0 || dbtofsb(bp->b_blkno) > ts_nxrec)
		goto abort;
	if (bp == &ctsbuf) {
		tstab.b_active = SCOM;
		ts.ts_cmd.c_cmd = ACK+CVC+IE+bp->b_resid;
		ts.ts_cmd.c_loba = 1;		/* count always 1 */
	} else if (blkno == dbtofsb(bp->b_blkno)) {
		tstab.b_active = SIO;
		ts_ubinfo = ubasetup(bp, 1);
		ts.ts_cmd.c_loba = (u_short)ts_ubinfo;
		ts.ts_cmd.c_hiba = (u_short)(ts_ubinfo >> 16) & 03;
		ts.ts_cmd.c_size = bp->b_bcount;
		if(bp->b_flags & B_READ)
			ts.ts_cmd.c_cmd = ACK+CVC+IE+READ;
		else
			ts.ts_cmd.c_cmd = ACK+CVC+IE+WRITE;
	} else {
		if (blkno < dbtofsb(bp->b_blkno)) {
			tstab.b_active = SSFOR;
			ts.ts_cmd.c_cmd = ACK+CVC+IE+SFORW;
			ts.ts_cmd.c_loba = dbtofsb(bp->b_blkno) - blkno;
		} else {
			tstab.b_active = SSREV;
			ts.ts_cmd.c_cmd = ACK+CVC+IE+SREV;
			ts.ts_cmd.c_loba = blkno - dbtofsb(bp->b_blkno);
		}
	}
	tsaddr->tsdb = ts_uba;
	return;

    abort:
	bp->b_flags |= B_ERROR;

    next:
	tstab.b_active = 0;
	tstab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

tsintr()
{
	register struct buf *bp;
	register struct device *tsaddr = TSADDR;
	register err, errclass, state;

	if ((bp = tstab.b_actf)==NULL)
		return;
	state = tstab.b_active;
	tstab.b_active = 0;
	err = tsaddr->tssr & 016;
	if ((tsaddr->tssr & SC) == 0)
		err = 0;
	errclass = 0;
	switch (err) {
	case 014:		/* unrecoverable */
	case 016:		/* fatal */
	case 002:		/* attention (shouldn't happen) */
	case 012:		/* "recoverable", but shouldn't happen */
		errclass = 2;
		break;

	case 0:			/* all OK */
		break;

	case 004:		/* status alert */
		if (ts.ts_sts.xs0&RLS && bp==&rtsbuf)	/* short record */
			break;
		if (ts.ts_sts.xs0 & TMK) {		/* tape mark */
			ts.ts_sts.rbpcr = bp->b_bcount;
			break;
		}
		errclass = 1;
		break;

	case 010:		/* recoverable, tape moved */
		if (state==SIO && ++bp->b_errcnt < 10) {
			ts.ts_cmd.c_cmd |= 01000;	/* redo bit */
			tstab.b_active = SIO;
			tsaddr->tsdb = ts_uba;
			return;
		}
		errclass = 1;
		break;

	case 006:		/* Function reject */
		if (state==SIO && ts.ts_sts.xs0 & WLE)
			printf("Tape needs a ring\n");
		if ((ts.ts_sts.xs0&ONL) == 0)		/* tape offline */
			printf("Tape offline\n");
		errclass = 2;
	}
	if (errclass)
		printf("tp: %o %o %o %o %o %o %o %o\n", tsaddr->tssr,
		  ts.ts_sts.s_sts, ts.ts_sts.len, ts.ts_sts.rbpcr,
		  ts.ts_sts.xs0, ts.ts_sts.xs1, ts.ts_sts.xs2, ts.ts_sts.xs3);
	switch(state) {
	case SIO:
		ts_blkno++;
		ubarelse(&ts_ubinfo);
	case SCOM:
		tstab.b_errcnt = 0;
		tstab.b_actf = bp->av_forw;
		bp->b_resid = ts.ts_sts.rbpcr;
		iodone(bp);
		break;

	case SSFOR:
	case SSREV:
		ts_blkno = dbtofsb(bp->b_blkno);
		break;

	default:
		printf("Unknown tape interrupt\n");
		errclass = 2;
		break;
	}
	if (errclass > 1) {
		while (bp = tstab.b_actf) {
			bp->b_flags |= B_ERROR;
			iodone(bp);
			tstab.b_actf = bp->av_forw;
		}
	}
	tsstart();
}

tsread(dev)
{
	tsphys(dev);
	physio(tsstrategy, &rtsbuf, dev, B_READ, minphys);
}

tswrite(dev)
{
	tsphys(dev);
	physio(tsstrategy, &rtsbuf, dev, B_WRITE, minphys);
}

tsphys(dev)
{
	register unit;
	daddr_t a;

	a = u.u_offset >> 9;
	ts_blkno = dbtofsb(a);
	ts_nxrec = dbtofsb(a)+1;
}

#define	UBMAP	(int *)0xf30800

int dtsinfo;
struct tsmesg dts;

twall(start, num)
{
	register struct device *tsaddr = TSPHYS;
	register int *ubap = UBMAP;
	register int p, i;

	tsinit();
	/* dump mem */
	p = PG_V;
	i = 0;
	while (i<num) {
		*(ubap) = p|i++;
		*(ubap+1) = p|i;
		dts.ts_cmd.c_loba = 0;
		dts.ts_cmd.c_hiba = 0;
		dts.ts_cmd.c_size = NBPG;
		dts.ts_cmd.c_cmd = ACK+CVC+WRITE;
		tsaddr->tsdb = dtsinfo;
		twait();
	}
	printf("done\n");
}

tsinit()
{
	register struct device *tsaddr = TSPHYS;
	register struct tsmesg *tsm;
	register int *ubap = UBMAP;
	register i;

	tsaddr->tssr = 0;
	while ((tsaddr->tssr&SSR)==0)
		;
	i = (int)&dts;
	i &= 0xefffff;
	dtsinfo = ((i&0777)|02000);
	tsm = (struct tsmesg *)dtsinfo;
	i >>= 9;
	i |= PG_V;
	*(ubap+2) = i;
	*(ubap+3) = i+1;
	dts.ts_cmd.c_cmd = ACK + 04;
	dts.ts_cmd.c_loba = (int)&tsm->ts_char;
	dts.ts_cmd.c_hiba = 0;
	dts.ts_cmd.c_size = sizeof(dts.ts_char);
	dts.ts_char.char_loba = (int)&tsm->ts_sts;
	dts.ts_char.char_hiba = 0;
	dts.ts_char.char_size = sizeof(dts.ts_sts);
	dts.ts_char.char_mode = 0400;
	tsaddr->tsdb = dtsinfo;
	twait();
}

teof()
{

	dtscommand(WTM);
}

rewind()
{

	dtscommand(REW);
}

dtscommand(com)
{
	register struct device *tsaddr = TSPHYS;

	dts.ts_cmd.c_cmd = ACK+CVC+com;
	dts.ts_cmd.c_loba = 1;
	tsaddr->tsdb = dtsinfo;
	twait();
}

twait()
{
	register struct device *tsaddr = TSPHYS;
	register i;

	while ((tsaddr->tssr&SSR)==0)
		;
	i = tsaddr->tssr;
	if (i&SC)
		printf("tssr %x ", i);
}
#endif
