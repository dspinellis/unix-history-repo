int	csdel0 = 30;
int	csdel2 = 0;
int	asdel = 500;
int	softas;
/*	%H%	3.10	%G%	*/

/*
 * Emulex UNIBUS disk driver with overlapped seeks and ECC recovery.
 *
 * NB: This device is very sensitive: be aware that the code is the way
 *     it is for good reason and that there are delay loops here which may
 *     have to be lengthened if your processor is faster and which should
 *     probably be shortened if your processor is slower.
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
#define	SDIST	3		/* 2-3 sectors 1-1.5 msec */
#define	RDIST	6		/* 5-6 sectors 2.5-3 msec */

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
#define	RDY	020		/* Transfer terminated */

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
int	sdelay = 150;		/* Delay after selecting drive in upcs2 */

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
		(void) upustart(unit);
		if (uptab.b_actf && uptab.b_active == 0)
			(void) upstart();
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
	int didie = 0;

	if (unit >= NUP)
		goto out;
	if (uptab.b_active) {
		softas |= 1<<unit;
		return;
	}
	/*
	 * Whether or not it was before, this unit is no longer busy.
	 * Check to see if there is (still or now) a request in this
	 * drives queue, and if there is, select this unit.
	 */
	if (unit+DK_N <= DK_NMAX)
		dk_busy &= ~(1<<(unit+DK_N));
	dp = &uputab[unit];
	if ((bp = dp->b_actf) == NULL)
		goto out;
	if ((upaddr->upcs2 & 07) != unit) {
		upaddr->upcs2 = unit;
		DELAY(sdelay);
		nwaitcs2++;
	} else
		neasycs2++;
	/*
	 * If we have changed packs or just initialized,
	 * the the volume will not be valid; if so, clear
	 * the drive, preset it and put in 16bit/word mode.
	 */
	if ((upaddr->upds & VV) == 0) {
		upaddr->upcs1 = IE|DCLR|GO;
		DELAY(idelay);
		upaddr->upcs1 = IE|PRESET|GO;
		DELAY(idelay);
		upaddr->upof = FMT22;
		didie = 1;
	}
	/*
	 * We are called from upstrategy when a new request arrives
	 * if we are not already active (with dp->b_active == 0),
	 * and we then set dp->b_active to 1 if we are to SEARCH
	 * for the desired cylinder, or 2 if we are on-cylinder.
	 * If we SEARCH then we will later be called from upintr()
	 * when the search is complete, and will link this disk onto
	 * the uptab.  We then set dp->b_active to 2 so that upintr()
	 * will not call us again.
	 *
	 * NB: Other drives clear the bit in the attention status
	 * (i.e. upas) register corresponding to the drive when they
	 * place the drive on the ready (i.e. uptab) queue.  This does
	 * not work with the Emulex, as the controller hangs the UBA
	 * of the VAX shortly after the upas register is set, for
	 * reasons unknown.  This only occurs in multi-spindle configurations,
	 * but to avoid the problem we use the fact that dp->b_active is
	 * 2 to replace the clearing of the upas bit.
	 */
	if (dp->b_active)
		goto done;
	dp->b_active = 1;
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL))
		goto done;	/* Will redetect error in upstart() soon */

	/*
	 * Do enough of the disk address decoding to determine
	 * which cylinder and sector the request is on.
	 * Then compute the number of the sector SDIST sectors before
	 * the one where the transfer is to start, this being the
	 * point where we wish to attempt to begin the transfer,
	 * allowing approximately SDIST/2 msec for interrupt latency
	 * and preparation of the request.
	 *
	 * If we are on the correct cylinder and the desired sector
	 * lies between SDIST and SDIST+RDIST sectors ahead of us, then
	 * we don't bother to SEARCH but just begin the transfer asap.
	 */
	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(NSECT*NTRAC);
	sn = (sn+NSECT-SDIST)%NSECT;

	if (cn - upaddr->updc)
		goto search;		/* Not on-cylinder */
	csn = (upaddr->upla>>6) - sn - 1;
	if (csn < 0)
		csn += NSECT;
	if (csn > NSECT-RDIST)
		goto done;

search:
	upaddr->updc = cn;
	upaddr->upda = sn;
	upaddr->upcs1 = IE|SEARCH|GO;
	didie = 1;
	/*
	 * Mark this unit busy.
	 */
	unit += DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_numb[unit]++;
	}
	if (csdel0) DELAY(csdel0);
	goto out;

done:
	/*
	 * This unit is ready to go.  Make active == 2 so
	 * we won't get called again (by upintr() because upas&(1<<unit))
	 * and link us onto the chain of ready disks.
	 */
	dp->b_active = 2;
	dp->b_forw = NULL;
	if (uptab.b_actf == NULL)
		uptab.b_actf = dp;
	else
		uptab.b_actl->b_forw = dp;
	uptab.b_actl = dp;

out:
	return (didie);
}

/*
 * Start a transfer; call from top level at spl5() or on interrupt.
 */
upstart()
{
	register struct buf *bp, *dp;
	register unit;
	register struct device *upaddr;
	daddr_t bn;
	int dn, sn, tn, cn, cmd;

loop:
	if (csdel2) DELAY(csdel2);
	/*
	 * Pick a drive off the queue of ready drives, and
	 * perform the first transfer on its queue.
	 *
	 * Looping here is completely for the sake of drives which
	 * are not present and on-line, for which we completely clear the
	 * request queue.
	 */
	if ((dp = uptab.b_actf) == NULL)
		return (0);
	if ((bp = dp->b_actf) == NULL) {
		uptab.b_actf = dp->b_forw;
		goto loop;
	}
	/*
	 * Mark the controller busy, and multi-part disk address.
	 * Select the unit on which the i/o is to take place.
	 */
	uptab.b_active++;
	unit = minor(bp->b_dev) & 077;
	dn = dkunit(bp);
	bn = dkblock(bp);
	cn = up_sizes[unit&07].cyloff;
	cn += bn/(NSECT*NTRAC);
	sn = bn%(NSECT*NTRAC);
	tn = sn/NSECT;
	sn %= NSECT;
	upaddr = UPADDR;
	if ((upaddr->upcs2 & 07) != dn) {
		upaddr->upcs2 = dn;
		DELAY(sdelay);
		nwaitcs2++;
	} else
		neasycs2++;
	up_ubinfo = ubasetup(bp, 1);	/* In a funny place for delay... */
	/*
	 * If drive is not present and on-line, then
	 * get rid of this with an error and loop to get
	 * rid of the rest of its queued requests.
	 * (Then on to any other ready drives.)
	 */
	if ((upaddr->upds & (DPR|MOL)) != (DPR|MOL)) {
		uptab.b_active = 0;
		uptab.b_errcnt = 0;
		dp->b_actf = bp->av_forw;
		dp->b_active = 0;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		ubafree(up_ubinfo), up_ubinfo = 0;	/* A funny place ... */
		goto loop;
	}
	/*
	 * If this is a retry, then with the 16'th retry we
	 * begin to try offsetting the heads to recover the data.
	 */
	if (uptab.b_errcnt >= 16) {
		upaddr->upof = up_offset[uptab.b_errcnt & 017] | FMT22;
		upaddr->upcs1 = IE|OFFSET|GO;
		DELAY(idelay);
		while (upaddr->upds & PIP)
			DELAY(25);
	}
	/*
	 * Now set up the transfer, retrieving the high
	 * 2 bits of the UNIBUS address from the information
	 * returned by ubasetup() for the cs1 register bits 8 and 9.
	 */
	upaddr->updc = cn;
	upaddr->upda = (tn << 8) + sn;
	upaddr->upba = up_ubinfo;
	upaddr->upwc = -bp->b_bcount / sizeof (short);
	cmd = (up_ubinfo >> 8) & 0x300;
	if (bp->b_flags & B_READ)
		cmd |= IE|RCOM|GO;
	else
		cmd |= IE|WCOM|GO;
	upaddr->upcs1 = cmd;
	/*
	 * This is a controller busy situation.
	 * Record in dk slot NUP+DK_N (after last drive)
	 * unless there aren't that many slots reserved for
	 * us in which case we record this as a drive busy
	 * (if there is room for that).
	 */
	unit = dn+DK_N;
	if (NUP+DK_N == DK_NMAX)
		unit = NUP+DK_N;
	if (unit <= DK_NMAX) {
		dk_busy |= 1<<unit;
		dk_numb[unit]++;
		dk_wds[unit] += bp->b_bcount>>6;
	}
	return (1);
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
	int osoftas;
	int needie = 1;

	if (uptab.b_active) {
		/*
		 * The drive is transferring, thus the hardware
		 * (say the designers) will only interrupt when the transfer
		 * completes; check for it anyways.
		 */
		if ((upaddr->upcs1 & RDY) == 0) {
			printf("!RDY: cs1 %o, ds %o, wc %d\n", upaddr->upcs1,
			    upaddr->upds, upaddr->upwc);
printf("as=%d act %d %d %d\n", as, uptab.b_active, uputab[0].b_active, uputab[1].b_active);
		}
		/*
		 * Mark controller or drive not busy, and check for an
		 * error condition which may have resulted from the transfer.
		 */
		dp = uptab.b_actf;
		bp = dp->b_actf;
		unit = dkunit(bp);
		if (DK_N+NUP == DK_NMAX)
			dk_busy &= ~(1<<(DK_N+NUP));
		else if (DK_N+unit <= DK_NMAX)
			dk_busy &= ~(1<<(DK_N+unit));
		if (upaddr->upcs1 & TRE) {
			/*
			 * An error occurred, indeed.  Select this unit
			 * to get at the drive status (a SEARCH may have
			 * intervened to change the selected unit), and
			 * wait for the command which caused the interrupt
			 * to complete (DRY).
			 *
			 * WHY IS THE WAIT NECESSARY?
			 */
			if ((upaddr->upcs2 & 07) != unit) {
				upaddr->upcs2 = unit;
				DELAY(sdelay);
				nwaitcs2++;
			} else
				neasycs2++;
			while ((upaddr->upds & DRY) == 0)
				DELAY(25);
			/*
			 * After 28 retries (16 w/o servo offsets, and then
			 * 12 with servo offsets), or if we encountered
			 * an error because the drive is write-protected,
			 * give up.  Print an error message on the last 2
			 * retries before a hard failure.
			 */
			if (++uptab.b_errcnt > 28 || upaddr->uper1&WLE)
				bp->b_flags |= B_ERROR;
			else
				uptab.b_active = 0;	/* To force retry */
			if (uptab.b_errcnt > 27)
				deverror(bp, upaddr->upcs2, upaddr->uper1);
			/*
			 * If this was a correctible ECC error, let upecc
			 * do the dirty work to correct it.  If upecc
			 * starts another READ for the rest of the data
			 * then it returns 1 (having set uptab.b_active).
			 * Otherwise we are done and fall through to
			 * finish up.
			 */
			if ((upaddr->uper1&(DCK|ECH))==DCK && upecc(upaddr, bp))
				return;
			/*
			 * Clear the drive and, every 4 retries, recalibrate
			 * to hopefully help clear up seek positioning problems.
			 */
			upaddr->upcs1 = TRE|IE|DCLR|GO;
			DELAY(idelay);
			needie = 0;
			if ((uptab.b_errcnt&07) == 4) {
				upaddr->upcs1 = RECAL|GO|IE;
				DELAY(idelay);
				while(upaddr->upds & PIP)
					DELAY(25);
			}
		}
		/*
		 * If we are still noted as active, then no
		 * (further) retries are necessary.  
		 *
		 * Make sure the correct unit is selected,
		 * return it to centerline if necessary, and mark
		 * this i/o complete, starting the next transfer
		 * on this drive with the upustart routine (if any).
		 */
		if (uptab.b_active) {
			if ((upaddr->upcs2 & 07) != unit) {
				upaddr->upcs2 = unit;
				DELAY(sdelay);
				nwaitcs2++;
			} else
				neasycs2++;
			if (uptab.b_errcnt >= 16) {
				upaddr->upcs1 = RTC|GO|IE;
				DELAY(idelay);
				while (upaddr->upds & PIP)
					DELAY(25);
				needie = 0;
			}
			uptab.b_active = 0;
			uptab.b_errcnt = 0;
			uptab.b_actf = dp->b_forw;
			dp->b_active = 0;
			dp->b_errcnt = 0;
			dp->b_actf = bp->av_forw;
			bp->b_resid = (-upaddr->upwc * sizeof(short));
			iodone(bp);
			if(dp->b_actf)
				if (upustart(unit))
					needie = 0;
		}
		as &= ~(1<<unit);
		softas &= ~(1<<unit);
		ubafree(up_ubinfo), up_ubinfo = 0;
	} else {
		if (upaddr->upcs1 & TRE) {
			upaddr->upcs1 = TRE;
			DELAY(idelay);
		}
	}
	/*
	 * If we have a unit with an outstanding SEARCH,
	 * and the hardware indicates the unit requires attention,
	 * the bring the drive to the ready queue.
	 * Finally, if the controller is not transferring
	 * start it if any drives are now ready to transfer.
	 */
	as |= softas;
	osoftas = softas;
	softas = 0;
	for (unit = 0; unit < NUP; unit++)
		if ((as|osoftas) & (1<<unit)) {
			if (as & (1<<unit)) {
				upaddr->upas = 1<<unit;
				if (asdel) DELAY(asdel);
			}
			if (upustart(unit))
				needie = 0;
		}
	if (uptab.b_actf && uptab.b_active == 0)
		if (upstart())
			needie = 0;
out:
	if (needie) {
		upaddr->upcs1 = IE;
	}
}

upread(dev)
{

	physio(upstrategy, &rupbuf, dev, B_READ, minphys);
}

upwrite(dev)
{

	physio(upstrategy, &rupbuf, dev, B_WRITE, minphys);
}

/*
 * Correct an ECC error, and restart the i/o to complete
 * the transfer if necessary.  This is quite complicated because
 * the transfer may be going to an odd memory address base and/or
 * across a page boundary.
 */
upecc(up, bp)
register struct device *up;
register struct buf *bp;
{
	struct uba_regs *ubp = (struct uba_regs *)UBA0;
	register int i;
	caddr_t addr;
	int reg, bit, byte, npf, mask, o, cmd, ubaddr;
	int bn, cn, tn, sn;

	/*
	 * Npf is the number of sectors transferred before the sector
	 * containing the ECC error, and reg is the UBA register
	 * mapping (the first part of) the transfer.
	 * O is offset within a memory page of the first byte transferred.
	 */
	npf = btop((up->upwc * sizeof(short)) + bp->b_bcount) - 1;
	reg = btop(up_ubinfo&0x3ffff) + npf;
	o = (int)bp->b_un.b_addr & PGOFSET;
	printf("%D ", bp->b_blkno+npf);
	prdev("ECC", bp->b_dev);
	mask = up->upec2;
	if (mask == 0) {
		up->upof = FMT22;		/* == RTC ???? */
		DELAY(idelay);
		return (0);
	}
	/*
	 * Flush the buffered data path, and compute the
	 * byte and bit position of the error.  The variable i
	 * is the byte offset in the transfer, the variable byte
	 * is the offset from a page boundary in main memory.
	 */
	ubp->uba_dpr[(up_ubinfo>>28)&0x0f] |= BNE;
	i = up->upec1 - 1;		/* -1 makes 0 origin */
	bit = i&07;
	i = (i&~07)>>3;
	byte = i + o;
	/*
	 * Correct while possible bits remain of mask.  Since mask
	 * contains 11 bits, we continue while the bit offset is > -11.
	 * Also watch out for end of this block and the end of the whole
	 * transfer.
	 */
	while (i < 512 && (int)ptob(npf)+i < bp->b_bcount && bit > -11) {
		addr = ptob(ubp->uba_map[reg+btop(byte)].pg_pfnum)+
		    (byte & PGOFSET);
		putmemc(addr, getmemc(addr)^(mask<<bit));
		byte++;
		i++;
		bit -= 8;
	}
	uptab.b_active++;	/* Either complete or continuing... */
	if (up->upwc == 0)
		return (0);
	/*
	 * Have to continue the transfer... clear the drive,
	 * and compute the position where the transfer is to continue.
	 * We have completed npf+1 sectors of the transfer already;
	 * restart at offset o of next sector (i.e. in UBA register reg+1).
	 */
	up->upcs1 = TRE|IE|DCLR|GO;
	DELAY(idelay);
	bn = dkblock(bp);
	cn = bp->b_cylin;
	sn = bn%(NSECT*NTRAC) + npf + 1;
	tn = sn/NSECT;
	sn %= NSECT;
	cn += tn/NTRAC;
	tn %= NTRAC;
	up->updc = cn;
	up->upda = (tn << 8) | sn;
	ubaddr = (int)ptob(reg+1) + o;
	up->upba = ubaddr;
	cmd = (ubaddr >> 8) & 0x300;
	cmd |= IE|GO|RCOM;
	up->upcs1 = cmd;
	return (1);
}
