int	printsw;
int	slow1 = 1;
int	slow2 = 1;
int	slow3 = 1;
int	slow4 = 1;
/*	%H%	3.1	%G%	*/

/*
 * Emulex UNIBUS disk driver with overlapped seeks and ECC recovery.
 *
 * This driver has been tested on a SC-11B Controller, configured
 * with the following internal switch settings:
 *	SW1-1	5/19 surfaces	(off, 19 surfaces on Ampex 9300)
 *	SW1-2	chksum enable	(off, checksum disabled)
 *	SW1-3	volume select	(off, 815 cylinders)
 *	SW1-4	sector select	(on, 32 sectors)
 *	SW1-5	unused		(off)
 *	SW1-6	port select	(on, single port)
 *	SW1-7	npr delay	(off, disable)
 *	SW1-8	ecc test mode	(off, disable)
 * and top mounted switches:
 *	SW2-1	extend opcodes	(off=open, disable)
 *	SW2-2	extend diag	(off=open, disable)
 *	SW2-3	4 wd dma burst	(off=open, disable)
 *	SW2-4	unused		(off=open)
 *
 * The controller transfers data much more rapidly with SW2-3 set,
 * but we have previously experienced problems with it set this way.
 * We intend to try this again in the near future.
 *
 *	wnj	June 14, 1980
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/vm.h"

/*
 * Define number of drives, and range of sampling information to be used.
 *
 * Normally, DK_N .. DK_N+NUP-1 gather individual drive stats,
 * and DK_N+NUP gathers controller transferring stats.
 *
 * If DK_N+NUP > DK_NMAX, then transfer stats are divided per drive.
 * If DK_NMAX is yet smaller, some drives are not monitored.
 */
#define	DK_N	1
#define	DK_NMAX	2

#define	ushort	unsigned short

struct	device
{
	ushort	upcs1;		/* control and status register 1 */
	short	upwc;		/* word count register */
	ushort	upba;		/* UNIBUS address register */
	ushort	upda;		/* desired address register */
	ushort	upcs2;		/* control and status register 2 */
	ushort	upds;		/* drive Status */
	ushort	uper1;		/* error register 1 */
	ushort	upas;		/* attention summary */
	ushort	upla;		/* look ahead */
	ushort	updb;		/* data buffer */
	ushort	upmr;		/* maintenance */ 
	ushort	updt;		/* drive type */
	ushort	upsn;		/* serial number */
	ushort	upof;		/* offset register */
	ushort	updc;		/* desired cylinder address register */
	ushort	upcc;		/* current cylinder */
	ushort	uper2;		/* error register 2 */
	ushort	uper3;		/* error register 3 */
	ushort	upec1;		/* burst error bit position */
	ushort	upec2;		/* burst error bit pattern */
};

#define	UPADDR	((struct device *)(UBA0_DEV + 0176700))

#define	NUP	2		/* Number of drives this installation */

#define	NSECT	32
#define	NTRAC	19

/*
 * Constants controlling on-cylinder SEARCH usage.
 *
 * We assume that it takes SDIST sectors of time to set up a transfer.
 * If a drive is on-cylinder, and between SDIST and SDIST+RDIST sectors
 * from the first sector to be transferred, then we just perform the
 * transfer.  SDIST represents interrupt latency, RDIST the amount
 * of rotation which is tolerable to avoid another interrupt.
 */
#define	SDIST	4		/* 4 sectors ~~= 2 msec */
#define	RDIST	6		/* 6 sectors ~~= 3 msec */

/*
 * To fill a 300M drive:
 *	A is designed to be used as a root.
 *	B is suitable for a swap area.
 *	H is the primary storage area.
 * On systems with RP06'es, we normally use only 291346 blocks of the H
 * area, and use DEF or G to cover the rest of the drive.  The C system
 * covers the whole drive and can be used for pack-pack copying.
 */
struct	size
{
	daddr_t	nblocks;
	int	cyloff;
} up_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	494912,	0,		/* C=cyl 0 thru 814 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
	81472,	681,		/* F=cyl 681 thru 814 */
	153824,	562,		/* G=cyl 562 thru 814 */
	445664,	82,		/* H=cyl 82 thru 814 */
/* Later, and more safely for H area...
	291346,	82,		/* H=cyl 82 thru 561 */
};

/*
 * The following defines are used in offset positioning
 * when trying to recover disk errors, with the constants being
 * +/- microinches.  Note that header compare inhibit (HCI) is not
 * tried (this makes sense only during read, in any case.)
 *
 * ARE ALL THESE IMPLEMENTED ON 9300?
 */
#define	P400	020
#define	M400	0220
#define	P800	040
#define	M800	0240
#define	P1200	060
#define	M1200	0260
#define	HCI	020000

int	up_offset[16] =
{
	P400, M400, P400, M400,
	P800, M800, P800, M800,
	P1200, M1200, P1200, M1200,
	0, 0, 0, 0,
};

/*
 * Each drive has a table uputab[i].  On this table are sorted the
 * pending requests implementing an elevator algorithm (see dsort.c.)
 * In the upustart() routine, each drive is independently advanced
 * until it is on the desired cylinder for the next transfer and near
 * the desired sector.  The drive is then chained onto the uptab
 * table, and the transfer is initiated by the upstart() routine.
 * When the transfer is completed the driver reinvokes the upustart()
 * routine to set up the next transfer.
 */
struct	buf	uptab;
struct	buf	uputab[NUP];

struct	buf	rupbuf;			/* Buffer for raw i/o */

/* Drive commands, placed in upcs1 */
#define	GO	01		/* Go bit, set in all commands */
#define	PRESET	020		/* Preset drive at init or after errors */
#define	OFFSET	014		/* Offset heads to try to recover error */
#define	RTC	016		/* Return to center-line after OFFSET */
#define	SEARCH	030		/* Search for cylinder+sector */
#define	RECAL	06		/* Recalibrate, needed after seek error */
#define	DCLR	010		/* Drive clear, after error */
#define	WCOM	060		/* Write */
#define	RCOM	070		/* Read */

/* Other bits of upcs1 */
#define	IE	0100		/* Controller wide interrupt enable */
#define	TRE	040000		/* Transfer error */

/* Drive status bits of upds */
#define	PIP	020000		/* Positioning in progress */
#define	ERR	040000		/* Error has occurred, DCLR necessary */
#define	VV	0100		/* Volume is valid, set by PRESET */
#define	DPR	0400		/* Drive has been preset */
#define	MOL	010000		/* Drive is online, heads loaded, etc */
#define	DRY	0200		/* Drive ready */

/* Bits of uper1 */
#define	DCK	0100000		/* Ecc error occurred */
#define	ECH	0100		/* Ecc error was unrecoverable */
#define	WLE	04000		/* Attempt to write read-only drive */

/* Bits of upof; the offset bits above are also in this register */
#define	FMT22	010000		/* 16 bits/word, must be always set */

#define	b_cylin b_resid

int	up_ubinfo;		/* Information about UBA usage saved here */
/*
 * The EMULEX controller balks if accessed quickly after
 * certain operations.  The exact timing has not yet been
 * determined, but delays are known to be needed when changing
 * the selected drive (by writing in upcs2), and thought to be
 * needed after operations like PRESET and DCLR.  The following
 * variables control the delay, DELAY(n) is approximately n usec.
 */
int	idelay = 500;		/* Delay after PRESET or DCLR */
int	sdelay = 500;		/* Delay after selecting drive in upcs2 */

#define	DELAY(N)		{ register int d; d = N; while (--d > 0); }
 
int	nwaitcs2;		/* How many sdelay loops ? */
int	neasycs2;		/* How many sdelay loops not needed ? */

#ifdef INTRLVE
daddr_t dkblock();
#endif
 
/*
 * Queue an i/o request for a drive, checking first that it is in range.
 *
 * A unit start is issued if the drive is inactive, causing
 * a SEARCH for the correct cylinder/sector.  If the drive is
 * already nearly on the money and the controller is not transferring
 * we kick it to start the transfer.
 */
upstrategy(bp)
register struct buf *bp;
{
	register struct buf *dp;
	register unit, xunit;
	long sz, bn;

	xunit = minor(bp->b_dev) & 077;
	sz = bp->b_bcount;
	sz = (sz+511) >> 9;		/* transfer size in 512 byte sectors */
	unit = dkunit(bp);
	if (unit >= NUP ||
	    bp->b_blkno < 0 ||
	    (bn = dkblock(bp))+sz > up_sizes[xunit&07].nblocks) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->b_cylin = bn/(NSECT*NTRAC) + up_sizes[xunit&07].cyloff;
	dp = &uputab[unit];
	(void) spl5();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		upustart(unit);
		if (uptab.b_actf && uptab.b_active == 0)
			upstart();
	}
	(void) spl0();
}

/*
 * Start activity on specified drive; called when drive is inactive
 * and new transfer request arrives and also when upas indicates that
 * a SEARCH command is complete.
 */
upustart(unit)
register unit;
{
	register struct buf *bp, *dp;
	register struct device *upaddr = UPADDR;
	daddr_t bn;
	int sn, cn, csn;

	if (printsw&1) printf("upustart\n");
	if (slow1) DELAY(idelay);
	upaddr->upas = 1<<unit;
	if (slow4) DELAY(idelay);
	upaddr->upcs1 = IE;
	if (slow4) DELAY(idelay);
	if (unit >= NUP) {
		printf("stray upustart\n");		/* can't happen */
		return;
	}

	if (unit+DK_N <= DK_NMAX)
		dk_busy &= ~(1<<(unit+DK_N));
	dp = &uputab[unit];
	if((bp=dp->b_actf) == NULL)
		return;
	if (slow1) DELAY(idelay);
	if ((upaddr->upcs2 & 07) != unit) {
		upaddr->upcs2 = unit;
		DELAY(sdelay);
		nwaitcs2++;
	} else
		neasycs2++;
	if (slow2) DELAY(idelay);
	if((upaddr->upds & VV) == 0) {
		upaddr->upcs1 = IE|PRESET|GO;
		DELAY(idelay);
		upaddr->upof = FMT22;
	}
	/*
	 * Don't SEARCH twice on same drive; avoids looping.
	 */
	if(dp->b_active)
		goto done;
	dp->b_active++;
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL))
		goto done;

	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(NSECT*NTRAC);
	sn = (sn+NSECT-SDIST)%NSECT;

	if(cn - upaddr->updc)
		goto search;
	csn = (upaddr->upla>>6) - sn - 1;
	if(csn < 0)
		csn += NSECT;
	if(csn > NSECT-RDIST)
		goto done;

search:
	upaddr->updc = cn;
	upaddr->upda = sn;
	upaddr->upcs1 = IE|SEARCH|GO;
	unit += DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_numb[unit]++;
	}
	return;

done:
	dp->b_forw = NULL;
	if(uptab.b_actf == NULL)
		uptab.b_actf = dp;
	else
		uptab.b_actl->b_forw = dp;
	uptab.b_actl = dp;
}

/*
 * Start a transfer; call from top level at spl5() or on interrupt.
 *
 * Pick a drive off the queue of ready drives and perform
 * the first transfer in its queue.
 */
upstart()
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *upaddr;
	daddr_t bn;
	int dn, sn, tn, cn;

	if (printsw&2) printf("upstart\n");
loop:
	if ((dp = uptab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		uptab.b_actf = dp->b_forw;
		goto loop;
	}
	uptab.b_active++;
	unit = minor(bp->b_dev) & 077;
	dn = dkunit(bp);
	bn = dkblock(bp);
	cn = up_sizes[unit&07].cyloff;
	cn += bn/(NSECT*NTRAC);
	sn = bn%(NSECT*NTRAC);
	tn = sn/NSECT;
	sn = sn%NSECT;

	upaddr = UPADDR;
	if (slow3) DELAY(idelay);
	if ((upaddr->upcs2 & 07) != dn) {
		upaddr->upcs2 = dn;
		DELAY(sdelay);
		nwaitcs2++;
	} else
		neasycs2++;
	up_ubinfo = ubasetup(bp, 1);
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL)) {
		uptab.b_active = 0;
		uptab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		ubafree(up_ubinfo), up_ubinfo = 0;
		goto loop;
	}
	if(uptab.b_errcnt >= 16) {
		upaddr->upof = up_offset[uptab.b_errcnt & 017] | FMT22;
		upaddr->upcs1 = OFFSET|GO;
		DELAY(idelay);
		while(upaddr->upds & PIP)
			DELAY(25);
	}
	upaddr->updc = cn;
	upaddr->upda = (tn << 8) + sn;
	upaddr->upba = up_ubinfo;
	upaddr->upwc = -bp->b_bcount / sizeof (short);
	if (bp->b_flags & B_READ)
		upaddr->upcs1 = IE|GO|RCOM;
	else
		upaddr->upcs1 = IE|GO|WCOM;

	unit = dn+DK_N;
	if (NUP+DK_N == DK_NMAX)
		unit = NUP+DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_numb[unit]++;
		dk_wds[unit] += bp->b_bcount>>6;
	}
}

/*
 * Handle a device interrupt.
 *
 * If the transferring drive needs attention, service it
 * retrying on error or beginning next transfer.
 * Service all other ready drives, calling ustart to transfer
 * their blocks to the ready queue in uptab, and then restart
 * the controller if there is anything to do.
 */
upintr()
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *upaddr = UPADDR;
	int as = upaddr->upas & 0377;

	if (printsw&4) printf("upintr\n");
	if(uptab.b_active) {
		dp = uptab.b_actf;
		bp = dp->b_actf;
		unit = dkunit(bp);
		if (DK_N+NUP == DK_NMAX)
			dk_busy &= ~(1<<(DK_N+NUP));
		else if (DK_N+unit <= DK_NMAX)
			dk_busy &= ~(1<<(DK_N+unit));
		if (upaddr->upcs1 & TRE) {
			if ((upaddr->upcs2 & 07) != unit) {
				upaddr->upcs2 = unit;
				DELAY(sdelay);
				nwaitcs2++;
			} else
				neasycs2++;
			while((upaddr->upds & DRY) == 0)
				DELAY(25);
			if(++uptab.b_errcnt > 28 || upaddr->uper1&WLE)
				bp->b_flags |= B_ERROR;
			else
				uptab.b_active = 0;
			if(uptab.b_errcnt > 27)
				deverror(bp, upaddr->upcs2, upaddr->uper1);
			if ((upaddr->uper1&(DCK|ECH)) == DCK) {
				if (upecc(upaddr, bp))
					return;
			}
			upaddr->upcs1 = TRE|IE|DCLR|GO;
			DELAY(idelay);
			if((uptab.b_errcnt&07) == 4) {
				upaddr->upcs1 = RECAL|GO|IE;
				DELAY(idelay);
				while(upaddr->upds & PIP)
					DELAY(25);
			}
		}
		if(uptab.b_active) {
			if(uptab.b_errcnt) {
				upaddr->upcs1 = RTC|GO;
				DELAY(idelay);
				while(upaddr->upds & PIP)
					DELAY(25);
			}
			uptab.b_active = 0;
			uptab.b_errcnt = 0;
			uptab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = (-upaddr->upwc * 2);
			iodone(bp);
			if(dp->b_actf)
				upustart(unit);
		}
		as &= ~(1<<unit);
		ubafree(up_ubinfo), up_ubinfo = 0;
	} else {
		if (upaddr->upcs1 & TRE) {
			upaddr->upcs1 = TRE;
			DELAY(idelay);
		}
		if (slow4) DELAY(idelay);
		if(as == 0)
			upaddr->upcs1 = IE;
		if (slow4) DELAY(idelay);
	}
	for(unit=0; unit<NUP; unit++)
		if(as & (1<<unit))
			upustart(unit);
	upstart();
}

upread(dev)
{

	physio(upstrategy, &rupbuf, dev, B_READ, minphys);
}

upwrite(dev)
{

	physio(upstrategy, &rupbuf, dev, B_WRITE, minphys);
}

upecc(up, bp)
register struct device *up;
register struct buf *bp;
{
	struct uba_regs *ubp = (struct uba_regs *)UBA0;
	register int i, off;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o;
	extern char buffers[NBUF][BSIZE];
	int bn, cn, tn, sn;

	if (printsw&8) printf("upecc\n");
	/*
	 * Npf is number of page frames (= disk blocks) completed before ecc.
	 */
	npf = btop((UPADDR->upwc * sizeof(short)) + bp->b_bcount) - 1;
	reg = btop(up_ubinfo&0xffff) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("%D ", bp->b_blkno+npf);
	prdev("ECC", bp->b_dev);
	mask = up->upec2;
	if (mask == 0) {
		up->upof = FMT22;
		DELAY(idelay);
		return (0);
	}
	i = up->upec1 - 1;
	bit = i&017;
	i = (i&~017)>>3;
	byte = i + o;
	if (byte & 1) {
		byte--;
		bit += 8;
	}
	i += (int)ptob(reg);
	for (off = 0; off <= 32; off += 16) {
		if (i <= bp->b_bcount) {
			addr = ptob(ubp->uba_map[reg+btop(byte)].pg_pfnum)+
			    (byte & PGOFSET);
			putmemw(addr, getmemw(addr)^(mask<<bit));
		}
		byte += sizeof (short);
		i += sizeof (short);
		bit -= 16;
	}
	uptab.b_active++;
	if (up->upwc == 0)
		return (0);
	up->upcs1 = DCLR|GO;
	DELAY(idelay);
	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(NSECT*NTRAC);
	tn = sn/NSECT;
	sn %= NSECT;
	sn += npf + 1;
	cn += sn/NSECT;
	sn %= NSECT;
	up->updc = cn;
	up->upda = ((i/NSECT)<<8) + (i%NSECT);
	up->upba = (int)ptob(reg+1)|((int)bp->b_un.b_addr&PGOFSET);
	up->upcs1 = IE|GO|RCOM;
	return (1);
}
