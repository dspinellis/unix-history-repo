/*	rk.c	4.6	%G%	*/

#include "rk.h"
#if NRK > 0
/*
 * RK disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/uba.h"
#include "../h/dk.h"

#define NCYL 815
#define NSECT 22
#define NTRK 3
#define NBLK (NTRK*NSECT*NCYL)

/* rkcs1 */
#define CCLR	0100000		/* controller clear */
#define	DI	040000		/* drive interrupt */
#define	CTO	04000		/* controller timeout */
#define	CDT	02000		/* drive type (rk07/rk06) */
#define	RDY	0200		/* controller ready */
#define	IEN	0100		/* interrupt enable */


/* rkcs2 */
#define	DLT	0100000		/* data late */
#define	WCE	040000		/* write check */
#define	UPE	020000		/* unibus parity */
#define	NED	010000		/* non-existant drive */
#define	NEM	04000		/* non-existant memory */
#define	PGE	02000		/* software error */
#define	MDS	01000		/* multiple drive select */
#define	UFE	0400		/* unit field error */
#define	SCLR	040		/* subsystem clear */
#define	cs2abort	(NED|NEM|PGE|UFE)

/* rkds */
#define	SVAL	0100000		/* status valid */
#define	CDA	040000		/* current drive attention */
#define	PIP	020000		/* positioning in progress */
#define	WRL	04000		/* write lock */
#define	DDT	0400		/* disk drive type */
#define	DRDY	0200		/* drive ready */
#define	VV	0100		/* volume valid */
#define	DROT	040		/* drive off track */
#define	SPLS	020		/* speed loss */
#define	ACLO	010		/* ac low */
#define	OFFSET	04		/* offset mode */
#define	DRA	01		/* drive available */
#define	dsabort		(ACLO|SPLS)


/* commands */
#define SELECT 0
#define PACK 2
#define DCLR 4
#define	RESET	012
#define	WCOM	022
#define	RCOM	020
#define	GO	01
#define	DRESET	012

struct	device
{
	short rkcs1;
	short rkwc;
	unsigned short rkba;
	short rkda;
	short rkcs2;
	short rkds;
	short rker;
	short rkatt;
	short rkcyl;
	short rkdb;
	short rkmr1;
	short rkecps;
	short rkecpt;
	short rkmr2;
	short rkmr3;
} ;

struct	buf	rktab;
struct	buf	rrkbuf;

struct	devsize {
	unsigned int nblocks;
	int	cyloff;
} rk_sizes [] ={
	9614, 0,	/* 0 - 145 */
	6600, 146,	/* 146 - 245 */
	37554, 246,	/* 246 - 815 */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	53790,	0,
};

rkstrategy(bp)
register struct buf *bp;
{
register dn, sz;

	dn = minor(bp->b_dev);
	sz = ((bp->b_bcount+511)>>9);
	if (dn > (NRK<<3) || sz+bp->b_blkno > rk_sizes[dn&07].nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = (struct buf *)NULL;
	spl5();
	if(rktab.b_actf == NULL)
		rktab.b_actf = bp;
	else
		rktab.b_actl->av_forw = bp;
	rktab.b_actl = bp;
	if(rktab.b_active == NULL)
		rkstart();
	spl0();
}

int rk_info;
int tcn, ttn, tsn;

rkstart()
{
	register struct buf *bp;
	register com;
	register struct device *rkaddr = RKADDR;
	daddr_t bn;
	int dn, cn, sn, tn;

	if ((bp = rktab.b_actf) == NULL)
		return;
	rktab.b_active++;
	rk_info = ubasetup(bp, 1);
	bn = bp->b_blkno;
	dn = minor(bp->b_dev);
	cn = bn/(NTRK*NSECT);
	cn += rk_sizes[dn&07].cyloff;
	dn >>= 3;
	if (dn != (rkaddr->rkcs2&07)) {
		rkaddr->rkcs2 = dn;
		rkaddr->rkcs1 = CDT | GO;
		while ((rkaddr->rkcs1&RDY)==0)
			;
	}
	if ((rkaddr->rkds & VV) == 0) {
		rkaddr->rkcs1 = PACK | CDT | GO;
		while ((rkaddr->rkcs1&RDY)==0)
			;
	}
	tn = bn%(NTRK*NSECT);
	tn = tn/NSECT;
	sn = bn%NSECT;
	rkaddr->rkcs2 = dn;
	rkaddr->rkcyl = cn;
	rkaddr->rkda = (tn << 8) | sn;
	ttn = tn; tcn = cn; tsn = sn;
	rkaddr->rkba = rk_info;
	rkaddr->rkwc = -(bp->b_bcount>>1);
	com = ((rk_info &0x30000) >> 8) | CDT | IEN | GO;
	if(bp->b_flags & B_READ)
		com |= RCOM; else
		com |= WCOM;
	rkaddr->rkcs1 = com;
	dk_busy |= 1<<RKDK_N;
	dk_xfer[RKDK_N] += 1;
	com = bp->b_bcount>>6;
	dk_wds[RKDK_N] += com;
}

rkintr()
{
	register struct buf *bp;
	register d, x;
	register struct device *rkaddr = RKADDR;
	int ds, er;

	if (rktab.b_active == NULL)
		return;
	dk_busy &= ~(1<<RKDK_N);
	bp = rktab.b_actf;
	rktab.b_active = NULL;
	if (rkaddr->rkcs1 < 0) {		/* error bit */
		d = (minor(bp->b_dev)>>3);
		x = 1;
		if (rkaddr->rkcs1&DI) {
			printf("rkintr: DI\n");
		}
		if (rkaddr->rkds&CDA)
			printf("rkintr: CDA\n");
		if ((rkaddr->rkds&CDA) || (rkaddr->rkcs1&DI)) {
			er = (unsigned short)rkaddr->rker;
			ds = (unsigned short)rkaddr->rkds;
			rkaddr->rkcs1 = CDT | DCLR | GO;
		} else {
			if ((rkaddr->rkds&SVAL)==0) {
				x = 0x8000 - rkselect(rkaddr, d);
				printf("rkintr: no SVAL, delay %d\n", x);
			}
			er = (unsigned short)rkaddr->rker;
			ds = (unsigned short)rkaddr->rkds;
		}
		if (rkaddr->rkds&dsabort) {
			printf("rk %d is down\n", d);
			rktab.b_errcnt = 10;
		}
		if (rkaddr->rkcs2&cs2abort) {
			printf("cs2 abort %o\n", rkaddr->rkcs2);
			rktab.b_errcnt = 10;
		}
		if (rktab.b_errcnt >= 10) {
			deverror(bp, er, ds);
			printf("cn %d tn %d sn %d\n", tcn, ttn, tsn);
		}
		rkaddr->rkcs1 = CDT | DCLR | GO;
		while ((rkaddr->rkcs1&RDY)==0)
			;
		rkaddr->rkcs2 = SCLR;
		while ((rkaddr->rkcs1&RDY)==0)
			;
		if ((x=rkselect(rkaddr, d)) == 0) {
			printf("after clears\n");
			goto bad;
		}
		rkaddr->rkcs1 = CDT | RESET | GO;
		while (rkaddr->rkds & PIP)
			;
		if (++rktab.b_errcnt <= 10) {
			ubarelse(&rk_info);
			rkstart();
			return;
		}
bad:
		bp->b_flags |= B_ERROR;
	}
	rktab.b_errcnt = 0;
	rktab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	ubarelse(&rk_info);
	iodone(bp);
	rkstart();
}


rkselect(rkaddr, d)
register struct device *rkaddr;
{
	rkaddr->rkcs2 = d;
	rkaddr->rkcs1 = CDT|GO;
	return(rkwait(rkaddr));
}

rkwait(rkaddr)
register struct device *rkaddr;
{
register t;

	for(t=0x8000; t && ((rkaddr->rkds&DRDY)==0); t--)
		;
	if (t==0)
		printf("rk not ready\n");
	return(t);
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

rkdump()
{

	printf("don't know how to dump to rk (yet)\n");
}
#endif
