/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)wd.c	5.8 (Berkeley) %G%
 */

#include "wd.h"
#if	NWD > 0

#include "param.h"
#include "dkbad.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "disk.h"
#include "buf.h"
#include "vm.h"
#include "uio.h"
#include "machine/pte.h"
#include "machine/isa/isa_device.h"
#include "icu.h"
#include "wdreg.h"
#include "syslog.h"

#define	RETRIES		5	/* number of retries before giving up */
#define	MAXTRANSFER	256	/* max size of transfer in page clusters */

#define WDUNIT(dev)	((minor(dev) & 070) >> 3)

#define b_cylin	b_resid		/* cylinder number for doing IO to */
				/* shares an entry in the buf struct */

/*
 * Drive states.  Used for open and format operations.
 * States < OPEN (> 0) are transient, during an open operation.
 * OPENRAW is used for unlabeled disks, and for floppies, to inhibit
 * bad-sector forwarding.
 */
#define RAWDISK		8		/* raw disk operation, no translation*/
#define ISRAWSTATE(s)	(RAWDISK&(s))	/* are we in a raw state? */
#define DISKSTATE(s)	(~RAWDISK&(s))	/* are we in a given state regardless
					   of raw or cooked mode? */

#define	CLOSED		0		/* disk is closed. */
					/* "cooked" disk states */
#define	WANTOPEN	1		/* open requested, not started */
#define	RECAL		2		/* doing restore */
#define	RDLABEL		3		/* reading pack label */
#define	RDBADTBL	4		/* reading bad-sector table */
#define	OPEN		5		/* done with open */

#define	WANTOPENRAW	(WANTOPEN|RAWDISK)	/* raw WANTOPEN */
#define	RECALRAW	(RECAL|RAWDISK)	/* raw open, doing restore */
#define	OPENRAW		(OPEN|RAWDISK)	/* open, but unlabeled disk or floppy */


/*
 * The structure of a disk drive.
 */
struct	disk {
	struct disklabel dk_dd;			/* device configuration data */
	long	dk_bc;				/* byte count left */
	short	dk_skip;			/* blocks already transferred */
	char	dk_unit;			/* physical unit number */
	char	dk_sdh;				/* sdh prototype */
	char	dk_state;			/* control state */
	u_char	dk_status;			/* copy of status reg. */
	u_char	dk_error;			/* copy of error reg. */
	short	dk_open;			/* open/closed refcnt */
};

/*
 * This label is used as a default when initializing a new or raw disk.
 * It really only lets us access the first track until we know more.
 */
struct disklabel dflt_sizes = {
	DISKMAGIC, DTYPE_ST506,
	{
		512,		/* sector size */
		36,		/* # of sectors per track */
		15,		/* # of tracks per cylinder */
		1224,		/* # of cylinders per unit */
		36*15,		/* # of sectors per cylinder */
		1224*15*36,	/* # of sectors per unit */
		0		/* write precomp cylinder (none) */
	},
	21600,	0,	/* A=root filesystem */
	21600,	40,
	660890, 0,	/* C=whole disk */
	216000,	80,
	0,	0,
	0,	0,
	0,	0,
	399600,	480
};

static	struct	dkbad	dkbad[NWD];
struct	disk	wddrives[NWD] = {0};	/* table of units */
struct	buf	wdtab = {0};
struct	buf	wdutab[NWD] = {0};	/* head of queue per drive */
struct	buf	rwdbuf[NWD] = {0};	/* buffers for raw IO */
long	wdxfer[NWD] = {0};		/* count of transfers */
int	writeprotected[NWD] = { 0 };
int	wdprobe(), wdattach(), wdintr();
struct	isa_driver wddriver = {
	wdprobe, wdattach, "wd",
};
#include "dbg.h"

static wdc;
/*
 * Probe routine
 */
wdprobe(dvp)
	struct isa_device *dvp;
{
wdc = dvp->id_iobase;

#ifdef lint
	wdintr(0);
#endif
	outb(wdc+wd_error, 0x5a) ;	/* error register not writable */
	/*wdp->wd_cyl_hi = 0xff ;/* only two bits of cylhi are implemented */
	outb(wdc+wd_cyl_lo, 0xa5) ;	/* but all of cyllo are implemented */
	if(inb(wdc+wd_error) != 0x5a /*&& wdp->wd_cyl_hi == 3*/
	   && inb(wdc+wd_cyl_lo) == 0xa5)
		return(1) ;
	return (0);
}

/*
 * attach each drive if possible.
 */
wdattach(dvp)
	struct isa_device *dvp;
{
	int unit = dvp->id_unit;

	outb(wdc+wd_ctlr,12);
	DELAY(1000);
	outb(wdc+wd_ctlr,8);
}

/* Read/write routine for a buffer.  Finds the proper unit, range checks
 * arguments, and schedules the transfer.  Does not wait for the transfer
 * to complete.  Multi-page transfers are supported.  All I/O requests must
 * be a multiple of a sector in length.
 */
wdstrategy(bp)
	register struct buf *bp;	/* IO operation to perform */
{
	register struct buf *dp;
	register struct disk *du;	/* Disk unit to do the IO.	*/
	long nblocks, cyloff, blknum;
	int	unit = WDUNIT(bp->b_dev), xunit = minor(bp->b_dev) & 7;
	int	s;

	if ((unit >= NWD) || (bp->b_blkno < 0)) {
		printf("wdstrat: unit = %d, blkno = %d, bcount = %d\n",
			unit, bp->b_blkno, bp->b_bcount);
		pg("wd:error in wdstrategy");
		bp->b_flags |= B_ERROR;
		goto bad;
	}
	if (writeprotected[unit] && (bp->b_flags & B_READ) == 0) {
		printf("wd%d: write protected\n", unit);
		goto bad;
	}
	du = &wddrives[unit];
	if (DISKSTATE(du->dk_state) != OPEN)
		goto q;
	/*
	 * Convert DEV_BSIZE "blocks" to sectors.
	 * Note: doing the conversions this way limits the partition size
	 * to about 8 million sectors (1-8 Gb).
	 */
	blknum = (unsigned long) bp->b_blkno * DEV_BSIZE / du->dk_dd.dk_secsize;
	if (((u_long) bp->b_blkno * DEV_BSIZE % du->dk_dd.dk_secsize != 0) ||
	    bp->b_bcount >= MAXTRANSFER * CLBYTES) {
		bp->b_flags |= B_ERROR;
		goto bad;
	}
	nblocks = du->dk_dd.dk_partition[xunit].nblocks;
	cyloff = du->dk_dd.dk_partition[xunit].cyloff;
	if (blknum + (bp->b_bcount / du->dk_dd.dk_secsize) > nblocks) {
		if (blknum == nblocks)
			bp->b_resid = bp->b_bcount;
		else
			bp->b_flags |= B_ERROR;
		goto bad;
	}
	bp->b_cylin = blknum / du->dk_dd.dk_secpercyl + cyloff;
q:
	dp = &wdutab[unit];
	s = splhigh();
	disksort(dp, bp);
	if (dp->b_active == 0)
		wdustart(du);		/* start drive if idle */
	if (wdtab.b_active == 0)
		wdstart(s);		/* start IO if controller idle */
	splx(s);
	return;

bad:
	bp->b_error = EINVAL;
	biodone(bp);
}

/* Routine to queue a read or write command to the controller.  The request is
 * linked into the active list for the controller.  If the controller is idle,
 * the transfer is started.
 */
wdustart(du)
	register struct disk *du;
{
	register struct buf *bp, *dp;

	dp = &wdutab[du->dk_unit];
	if (dp->b_active)
		return;
	bp = dp->b_actf;
	if (bp == NULL)
		return;	
	dp->b_forw = NULL;
	if (wdtab.b_actf  == NULL)		/* link unit into active list */
		wdtab.b_actf = dp;
	else
		wdtab.b_actl->b_forw = dp;
	wdtab.b_actl = dp;
	dp->b_active = 1;		/* mark the drive as busy */
}

/*
 * Controller startup routine.  This does the calculation, and starts
 * a single-sector read or write operation.  Called to start a transfer,
 * or from the interrupt routine to continue a multi-sector transfer.
 * RESTRICTIONS:
 * 1.	The transfer length must be an exact multiple of the sector size.
 */

static wd_sebyse;

wdstart()
{
	register struct disk *du;	/* disk unit for IO */
	register struct buf *bp;
	struct buf *dp;
	register struct bt_bad *bt_ptr;
	long	blknum, pagcnt, cylin, head, sector;
	long	secpertrk, secpercyl, addr, i;
	int	minor_dev, unit, s;

loop:
	dp = wdtab.b_actf;
	if (dp == NULL)
		return;
	bp = dp->b_actf;
	if (bp == NULL) {
		wdtab.b_actf = dp->b_forw;
		goto loop;
	}
	unit = WDUNIT(bp->b_dev);
	du = &wddrives[unit];
	if (DISKSTATE(du->dk_state) <= RDLABEL) {
		if (wdcontrol(bp)) {
			dp->b_actf = bp->av_forw;
			goto loop;	/* done */
		}
		return;
	}
	minor_dev = minor(bp->b_dev) & 7;
	secpertrk = du->dk_dd.dk_nsectors;
	secpercyl = du->dk_dd.dk_secpercyl;
	/*
	 * Convert DEV_BSIZE "blocks" to sectors.
	 */
	blknum = (unsigned long) bp->b_blkno * DEV_BSIZE / du->dk_dd.dk_secsize
		+ du->dk_skip;
#ifdef	WDDEBUG
	if (du->dk_skip == 0) {
		dprintf(DDSK,"\nwdstart %d: %s %d@%d; map ", unit,
			(bp->b_flags & B_READ) ? "read" : "write",
			bp->b_bcount, blknum);
	} else {
		dprintf(DDSK," %d)%x", du->dk_skip, inb(wdc+wd_altsts));
	}
#endif

	addr = (int) bp->b_un.b_addr;
	if(du->dk_skip==0) du->dk_bc = bp->b_bcount;
	cylin = blknum / secpercyl;
	head = (blknum % secpercyl) / secpertrk;
	sector = blknum % secpertrk;
	if (DISKSTATE(du->dk_state) == OPEN)
		cylin += du->dk_dd.dk_partition[minor_dev].cyloff;

	/* 
	 * See if the current block is in the bad block list.
	 * (If we have one, and not formatting.)
	 */
	if (DISKSTATE(du->dk_state) == OPEN && wd_sebyse)
	    for (bt_ptr = dkbad[unit].bt_bad; bt_ptr->bt_cyl != -1; bt_ptr++) {
		if (bt_ptr->bt_cyl > cylin)
			/* Sorted list, and we passed our cylinder. quit. */
			break;
		if (bt_ptr->bt_cyl == cylin &&
				bt_ptr->bt_trksec == (head << 8) + sector) {
			/*
			 * Found bad block.  Calculate new block addr.
			 * This starts at the end of the disk (skip the
			 * last track which is used for the bad block list),
			 * and works backwards to the front of the disk.
			 */
#ifdef	WDDEBUG
			    dprintf(DDSK,"--- badblock code -> Old = %d; ",
				blknum);
#endif
			blknum = du->dk_dd.dk_secperunit - du->dk_dd.dk_nsectors
				- (bt_ptr - dkbad[unit].bt_bad) - 1;
			cylin = blknum / secpercyl;
			head = (blknum % secpercyl) / secpertrk;
			sector = blknum % secpertrk;
#ifdef	WDDEBUG
			    dprintf(DDSK, "new = %d\n", blknum);
#endif
			break;
		}
	}
	sector += 1;	/* sectors begin with 1, not 0 */

	wdtab.b_active = 1;		/* mark controller active */

	if(du->dk_skip==0 || wd_sebyse) {
	if(wdtab.b_errcnt && (bp->b_flags & B_READ) == 0) du->dk_bc += 512;
	while ((inb(wdc+wd_status) & WDCS_BUSY) != 0) ;
	/*while ((inb(wdc+wd_status) & WDCS_DRQ)) inb(wdc+wd_data);*/
	outb(wdc+wd_precomp, 0xff);
	/*wr(wdc+wd_precomp, du->dk_dd.dk_precompcyl / 4);*/
	/*if (bp->b_flags & B_FORMAT) {
		wr(wdc+wd_sector, du->dk_dd.dk_gap3);
		wr(wdc+wd_seccnt, du->dk_dd.dk_nsectors);
	} else {*/
	if(wd_sebyse)
		outb(wdc+wd_seccnt, 1);
	else
		outb(wdc+wd_seccnt, ((du->dk_bc +511) / 512));
	outb(wdc+wd_sector, sector);

	outb(wdc+wd_cyl_lo, cylin);
	outb(wdc+wd_cyl_hi, cylin >> 8);

	/* Set up the SDH register (select drive).     */
	outb(wdc+wd_sdh, WDSD_IBM | (unit<<4) | (head & 0xf));
	while ((inb(wdc+wd_status) & WDCS_READY) == 0) ;

	/*if (bp->b_flags & B_FORMAT)
		wr(wdc+wd_command, WDCC_FORMAT);
	else*/
		outb(wdc+wd_command,
			(bp->b_flags & B_READ)? WDCC_READ : WDCC_WRITE);
#ifdef	WDDEBUG
	dprintf(DDSK,"sector %d cylin %d head %d addr %x sts %x\n",
	    sector, cylin, head, addr, inb(wdc+wd_altsts));
#endif
}
		
	/* If this is a read operation, just go away until it's done.	*/
	if (bp->b_flags & B_READ) return;

	/* Ready to send data?	*/
	while ((inb(wdc+wd_status) & WDCS_DRQ) == 0)
		nulldev();		/* So compiler won't optimize out */

	/* ASSUMES CONTIGUOUS MEMORY */
	outsw (wdc+wd_data, addr+du->dk_skip*512, 256);
	du->dk_bc -= 512;
}

/*
 * these are globally defined so they can be found
 * by the debugger easily in the case of a system crash
 */
daddr_t wd_errsector;
daddr_t wd_errbn;
unsigned char wd_errstat;

/* Interrupt routine for the controller.  Acknowledge the interrupt, check for
 * errors on the current operation, mark it done if necessary, and start
 * the next request.  Also check for a partially done transfer, and
 * continue with the next chunk if so.
 */
wdintr()
{
	register struct	disk *du;
	register struct buf *bp, *dp;
	int status;
	char partch ;
static shit[32];
static wd_haderror;

	/* Shouldn't need this, but it may be a slow controller.	*/
	while ((status = inb(wdc+wd_status)) & WDCS_BUSY)
		nulldev();
	if (!wdtab.b_active) {
		printf("wd: extra interrupt\n");
		return;
	}

#ifdef	WDDEBUG
	dprintf(DDSK,"I ");
#endif
	dp = wdtab.b_actf;
	bp = dp->b_actf;
	du = &wddrives[WDUNIT(bp->b_dev)];
	partch = "abcdefgh"[minor(bp->b_dev)&7] ;
	if (DISKSTATE(du->dk_state) <= RDLABEL) {
		if (wdcontrol(bp))
			goto done;
		return;
	}
	if (status & (WDCS_ERR | WDCS_ECCCOR)) {
		wd_errstat = inb(wdc+wd_error);		/* save error status */
#ifdef	WDDEBUG
		printf("status %x error %x\n", status, wd_errstat);
#endif
		if(wd_sebyse == 0) {
			wd_haderror = 1;
			goto outt;
		}
		/*if (bp->b_flags & B_FORMAT) {
			du->dk_status = status;
			du->dk_error = wdp->wd_error;
			bp->b_flags |= B_ERROR;
			goto done;
		}*/
		
		wd_errsector = (bp->b_cylin * du->dk_dd.dk_secpercyl) +
			(((unsigned long) bp->b_blkno * DEV_BSIZE /
			    du->dk_dd.dk_secsize) % du->dk_dd.dk_secpercyl) +
			du->dk_skip;
		wd_errbn = bp->b_blkno
			+ du->dk_skip * du->dk_dd.dk_secsize / DEV_BSIZE ;
		if (status & WDCS_ERR) {
			if (++wdtab.b_errcnt < RETRIES) {
				wdtab.b_active = 0;
				/*while ((inb(wdc+wd_status) & WDCS_DRQ))
				insw(wdc+wd_data, &shit, sizeof(shit)/2);*/
			} else {
				printf("wd%d%c: ", du->dk_unit, partch);
				printf(
				"hard %s error, sn %d bn %d status %b error %b\n",
					(bp->b_flags & B_READ)? "read":"write",
					wd_errsector, wd_errbn, status, WDCS_BITS,
					wd_errstat, WDERR_BITS);
				bp->b_flags |= B_ERROR;	/* flag the error */
			}
		} else
			log(LOG_WARNING,"wd%d%c: soft ecc sn %d bn %d\n",
				du->dk_unit, partch, wd_errsector,
				wd_errbn);
	}
outt:

	/*
	 * If this was a successful read operation, fetch the data.
	 */
	if (((bp->b_flags & (B_READ | B_ERROR)) == B_READ) && wdtab.b_active) {
		int chk, dummy;

		chk = min(256,du->dk_bc/2);
		/* Ready to receive data?	*/
		while ((inb(wdc+wd_status) & WDCS_DRQ) == 0)
			nulldev();

/*dprintf(DDSK,"addr %x\n", (int)bp->b_un.b_addr + du->dk_skip * 512);*/
		insw(wdc+wd_data,(int)bp->b_un.b_addr + du->dk_skip * 512 ,chk);
		du->dk_bc -= 2*chk;
		while (chk++ < 256) insw (wdc+wd_data,&dummy,1);
	}

	wdxfer[du->dk_unit]++;
	if (wdtab.b_active) {
		if ((bp->b_flags & B_ERROR) == 0) {
			du->dk_skip++;		/* Add to successful sectors. */
			if (wdtab.b_errcnt) {
				log(LOG_WARNING, "wd%d%c: ",
						du->dk_unit, partch);
				log(LOG_WARNING,
			"soft %s error, sn %d bn %d error %b retries %d\n",
				    (bp->b_flags & B_READ) ? "read" : "write",
				    wd_errsector, wd_errbn, wd_errstat,
				    WDERR_BITS, wdtab.b_errcnt);
			}
			wdtab.b_errcnt = 0;

			/* see if more to transfer */
			/*if (du->dk_skip < (bp->b_bcount + 511) / 512) {*/
			if (du->dk_bc > 0 && wd_haderror == 0) {
				wdstart();
				return;		/* next chunk is started */
			} else if (wd_haderror && wd_sebyse == 0) {
				du->dk_skip = 0;
				wd_haderror = 0;
				wd_sebyse = 1;
				wdstart();
				return;		/* redo xfer sector by sector */
			}
		}

done:
		wd_sebyse = 0;
		/* done with this transfer, with or without error */
		wdtab.b_actf = dp->b_forw;
		wdtab.b_errcnt = 0;
		du->dk_skip = 0;
		dp->b_active = 0;
		dp->b_actf = bp->av_forw;
		dp->b_errcnt = 0;
		bp->b_resid = 0;
		biodone(bp);
	}
	wdtab.b_active = 0;
	if (dp->b_actf)
		wdustart(du);		/* requeue disk if more io to do */
	if (wdtab.b_actf)
		wdstart();		/* start IO on next drive */
}

/*
 * Initialize a drive.
 */
wdopen(dev, flags)
	dev_t	dev;
	int	flags;
{
	register unsigned int unit;
	register struct buf *bp;
	register struct disk *du;
	struct dkbad *db;
	int i, error = 0;

	unit = WDUNIT(dev);
	if (unit >= NWD) return (ENXIO) ;
	du = &wddrives[unit];
	if (du->dk_open){
		du->dk_open++ ;
		return(0);	/* already is open, don't mess with it */
	}
#ifdef THE_BUG
	if (du->dk_state && DISKSTATE(du->dk_state) <= OPEN)
		return(0);
#endif
	du->dk_unit = unit;
	wdutab[unit].b_actf = NULL;
	/*if (flags & O_NDELAY)
		du->dk_state = WANTOPENRAW;
	else*/
		du->dk_state = WANTOPEN;
	/*
	 * Use the default sizes until we've read the label,
	 * or longer if there isn't one there.
	 */
	du->dk_dd = dflt_sizes;

	/*
	 * Recal, read of disk label will be done in wdcontrol
	 * during first read operation.
	 */
	bp = geteblk(512);
	bp->b_dev = dev & 0xff00;
	bp->b_blkno = bp->b_bcount = 0;
	bp->b_flags = B_READ;
	wdstrategy(bp);
	biowait(bp);
	if (bp->b_flags & B_ERROR) {
		u.u_error = 0; 	/* XXX */
		error = ENXIO;
		du->dk_state = CLOSED;
		goto done;
	}
	if (du->dk_state == OPENRAW) {
		du->dk_state = OPENRAW;
		goto done;
	}
	/*
	 * Read bad sector table into memory.
	 */
	i = 0;
	do {
		u.u_error = 0;				/* XXX */
		bp->b_flags = B_BUSY | B_READ;
		bp->b_blkno = du->dk_dd.dk_secperunit - du->dk_dd.dk_nsectors
			+ i;
		if (du->dk_dd.dk_secsize > DEV_BSIZE)
			bp->b_blkno *= du->dk_dd.dk_secsize / DEV_BSIZE;
		else
			bp->b_blkno /= DEV_BSIZE / du->dk_dd.dk_secsize;
		bp->b_bcount = du->dk_dd.dk_secsize;
		bp->b_cylin = du->dk_dd.dk_ncylinders - 1;
		wdstrategy(bp);
		biowait(bp);
	} while ((bp->b_flags & B_ERROR) && (i += 2) < 10 &&
		i < du->dk_dd.dk_nsectors);
	db = (struct dkbad *)(bp->b_un.b_addr);
#define DKBAD_MAGIC 0x4321
	if ((bp->b_flags & B_ERROR) == 0 && db->bt_mbz == 0 &&
	    db->bt_flag == DKBAD_MAGIC) {
		dkbad[unit] = *db;
		du->dk_state = OPEN;
	} else {
		printf("wd%d: %s bad-sector file\n", unit,
		    (bp->b_flags & B_ERROR) ? "can't read" : "format error in");
		u.u_error = 0;				/* XXX */
		/*error = ENXIO ;*/
		du->dk_state = OPENRAW;
	}
done:
	bp->b_flags = B_INVAL | B_AGE;
	brelse(bp);
	if (error == 0)
		du->dk_open = 1;
	return (error);
}

/*
 * Implement operations other than read/write.
 * Called from wdstart or wdintr during opens and formats.
 * Uses finite-state-machine to track progress of operation in progress.
 * Returns 0 if operation still in progress, 1 if completed.
 */
wdcontrol(bp)
	register struct buf *bp;
{
	register struct disk *du;
	register unit;
	unsigned char  stat;
	int s, cnt;
	extern int bootdev, cyloffset;

	du = &wddrives[WDUNIT(bp->b_dev)];
	unit = du->dk_unit;
	switch (DISKSTATE(du->dk_state)) {

	tryagainrecal:
	case WANTOPEN:			/* set SDH, step rate, do restore */
#ifdef	WDDEBUG
		dprintf(DDSK,"wd%d: recal ", unit);
#endif
		s = splbio();		/* not called from intr level ... */

#ifdef notdef
		/* some compaq controllers require this ... */
		outb(wdc+wd_sdh, WDSD_IBM | (unit << 4) 
			+ du->dk_dd.dk_ntracks-1);
		outb(wdc+wd_seccnt, du->dk_dd.dk_nsectors);
		outb(wdc+wd_command, 0x91);
		while ((stat = inb(wdc+wd_status)) & WDCS_BUSY) nulldev();
#endif

		outb(wdc+wd_sdh, WDSD_IBM | (unit << 4));
		wdtab.b_active = 1;
		outb(wdc+wd_command, WDCC_RESTORE | WD_STEP);
		du->dk_state++;
		splx(s);
		return(0);

	case RECAL:
		if ((stat = inb(wdc+wd_status)) & WDCS_ERR) {
			printf("wd%d: recal", du->dk_unit);
			if (unit == 0) {
				printf(": status %b error %b\n",
					stat, WDCS_BITS,
					inb(wdc+wd_error), WDERR_BITS);
				if (++wdtab.b_errcnt < RETRIES)
					goto tryagainrecal;
			}
			goto badopen;
		}
		wdtab.b_errcnt = 0;
		if (ISRAWSTATE(du->dk_state)) {
			du->dk_state = OPENRAW;
			return(1);
		}
retry:
#ifdef	WDDEBUG
		dprintf(DDSK,"rdlabel ");
#endif
if( cyloffset < 0 || cyloffset > 2048) cyloffset=0;
		/*
		 * Read in sector 0 to get the pack label and geometry.
		 */
		outb(wdc+wd_precomp, 0xff);/* sometimes this is head bit 3 */
		outb(wdc+wd_seccnt, 1);
		outb(wdc+wd_sector, 1);
		/*if (bp->b_dev == bootdev) {
			(wdc+wd_cyl_lo = cyloffset & 0xff;
			(wdc+wd_cyl_hi = cyloffset >> 8;
		} else {
			(wdc+wd_cyl_lo = 0;
			(wdc+wd_cyl_hi = 0;
		}*/
		outb(wdc+wd_cyl_lo, (cyloffset & 0xff));
		outb(wdc+wd_cyl_hi, (cyloffset >> 8));
		outb(wdc+wd_sdh, WDSD_IBM | (unit << 4));
		outb(wdc+wd_command, WDCC_READ);
		du->dk_state = RDLABEL;
		return(0);

	case RDLABEL:
		if ((stat = inb(wdc+wd_status)) & WDCS_ERR) {
			if (++wdtab.b_errcnt < RETRIES)
				goto retry;
			printf("wd%d: read label", unit);
			goto badopen;
		}

		insw(wdc+wd_data, bp->b_un.b_addr, 256);

		if (((struct disklabel *)
		    (bp->b_un.b_addr + LABELOFFSET))->dk_magic == DISKMAGIC) {
		       du->dk_dd =
			 * (struct disklabel *) (bp->b_un.b_addr + LABELOFFSET);
		} else {
			printf("wd%d: bad disk label\n", du->dk_unit);
			du->dk_state = OPENRAW;
		}

		s = splbio();		/* not called from intr level ... */
		while ((stat = inb(wdc+wd_status)) & WDCS_BUSY) nulldev();
		outb(wdc+wd_sdh, WDSD_IBM | (unit << 4) 
			+ du->dk_dd.dk_ntracks-1);
		outb(wdc+wd_cyl_lo, du->dk_dd.dk_ncylinders);
		outb(wdc+wd_cyl_hi, du->dk_dd.dk_ncylinders>>8);
		outb(wdc+wd_seccnt, du->dk_dd.dk_nsectors);
		outb(wdc+wd_command, 0x91);
		while ((stat = inb(wdc+wd_status)) & WDCS_BUSY) nulldev();
		outb(wdc+wd_seccnt, 0);
		splx(s);

		if (du->dk_state == RDLABEL)
			du->dk_state = RDBADTBL;
		/*
		 * The rest of the initialization can be done
		 * by normal means.
		 */
		return(1);

	default:
		panic("wdcontrol %x", du->dk_state );
	}
	/* NOTREACHED */

badopen:
	printf(": status %b error %b\n",
		stat, WDCS_BITS, inb(wdc+wd_error), WDERR_BITS);
	du->dk_state = OPENRAW;
	return(1);
}

wdclose(dev)
	dev_t dev;
{	struct disk *du;

	du = &wddrives[WDUNIT(dev)];
	du->dk_open-- ;
	/*if (du->dk_open == 0) du->dk_state = CLOSED ; does not work */
}

wdioctl(dev,cmd,addr,flag)
	dev_t dev;
	caddr_t addr;
{
	int unit = WDUNIT(dev);
	register struct disk *du;
	int error = 0;
	struct uio auio;
	struct iovec aiov;
	/*int wdformat();*/

	du = &wddrives[unit];

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)addr = du->dk_dd;
		break;

	case DIOCGDINFOP:
		*(struct disklabel **)addr = &(du->dk_dd);
		break;

#ifdef notyet
	case DIOCWFORMAT:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else {
			register struct format_op *fop;

			fop = (struct format_op *)addr;
			aiov.iov_base = fop->df_buf;
			aiov.iov_len = fop->df_count;
			auio.uio_iov = &aiov;
			auio.uio_iovcnt = 1;
			auio.uio_resid = fop->df_count;
			auio.uio_segflg = 0;
			auio.uio_offset =
				fop->df_startblk * du->dk_dd.dk_secsize;
			error = physio(wdformat, &rwdbuf[unit], dev, B_WRITE,
				minphys, &auio);
			fop->df_count -= auio.uio_resid;
			fop->df_reg[0] = du->dk_status;
			fop->df_reg[1] = du->dk_error;
		}
		break;
#endif

	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

/*wdformat(bp)
	struct buf *bp;
{

	bp->b_flags |= B_FORMAT;
	return (wdstrategy(bp));
}*/

/*
 * Routines to do raw IO for a unit.
 */
wdread(dev, uio)			/* character read routine */
	dev_t dev;
	struct uio *uio;
{
	int unit = WDUNIT(dev) ;

	if (unit >= NWD) return(ENXIO);
	return(physio(wdstrategy, &rwdbuf[unit], dev, B_READ, minphys, uio));
}


wdwrite(dev, uio)			/* character write routine */
	dev_t dev;
	struct uio *uio;
{
	int unit = WDUNIT(dev) ;

	if (unit >= NWD) return(ENXIO);
	return(physio(wdstrategy, &rwdbuf[unit], dev, B_WRITE, minphys, uio));
}

wdsize(dev)
	dev_t dev;
{
	register unit = WDUNIT(dev) ;
	register xunit = minor(dev) & 07;
	register struct disk *du;
	register val ;

	return(21600);
#ifdef notdef
	if (unit >= NWD) return(-1);
	if (wddrives[unit].dk_state == 0) /*{
		val = wdopen (dev, 0) ;
		if (val < 0) return (val) ;
	}*/	return (-1) ;
	du = &wddrives[unit];
	return((int)((u_long)du->dk_dd.dk_partition[xunit].nblocks *
		du->dk_dd.dk_secsize / 512));
#endif
}

wddump(dev)			/* dump core after a system crash */
	dev_t dev;
{
#ifdef notyet
	register struct disk *du;	/* disk unit to do the IO */
	register struct wd1010 *wdp = (struct wd1010 *) VA_WD;
	register struct bt_bad *bt_ptr;
	long	num;			/* number of sectors to write */
	int	unit, xunit;
	long	cyloff, blknum, blkcnt;
	long	cylin, head, sector;
	long	secpertrk, secpercyl, nblocks, i;
	register char *addr;
	char	*end;
	extern	int dumplo, totalclusters;
	static  wddoingadump = 0 ;

	addr = (char *) PA_RAM;		/* starting address */
	/* size of memory to dump */
	num = totalclusters * CLSIZE - PA_RAM / PGSIZE;
	unit = WDUNIT(dev) ;		/* eventually support floppies? */
	xunit = minor(dev) & 7;		/* file system */
	/* check for acceptable drive number */
	if (unit >= NWD) return(ENXIO);

	du = &wddrives[unit];
	/* was it ever initialized ? */
	if (du->dk_state < OPEN) return (ENXIO) ;

	/* Convert to disk sectors */
	num = (u_long) num * PGSIZE / du->dk_dd.dk_secsize;

	/* check if controller active */
	/*if (wdtab.b_active) return(EFAULT); */
	if (wddoingadump) return(EFAULT);

	secpertrk = du->dk_dd.dk_nsectors;
	secpercyl = du->dk_dd.dk_secpercyl;
	nblocks = du->dk_dd.dk_partition[xunit].nblocks;
	cyloff = du->dk_dd.dk_partition[xunit].cyloff;

	/* check transfer bounds against partition size */
	if ((dumplo < 0) || ((dumplo + num) >= nblocks))
		return(EINVAL);

	/*wdtab.b_active = 1;		/* mark controller active for if we
					   panic during the dump */
	wddoingadump = 1  ;  i = 100000 ;
	while ((wdp->wd_status & WDCS_BUSY) && (i-- > 0)) nulldev() ;
	inb(wdc+wd_sdh = du->dk_sdh ;
	inb(wdc+wd_command = WDCC_RESTORE | WD_STEP;
	while (inb(wdc+wd_status & WDCS_BUSY) nulldev() ;
	
	blknum = dumplo;
	while (num > 0) {
#ifdef notdef
		if (blkcnt > MAXTRANSFER) blkcnt = MAXTRANSFER;
		if ((blknum + blkcnt - 1) / secpercyl != blknum / secpercyl)
			blkcnt = secpercyl - (blknum % secpercyl);
			    /* keep transfer within current cylinder */
#endif

		/* compute disk address */
		cylin = blknum / secpercyl;
		head = (blknum % secpercyl) / secpertrk;
		sector = blknum % secpertrk;
		sector++;		/* origin 1 */
		cylin += cyloff;

		/* 
		 * See if the current block is in the bad block list.
		 * (If we have one.)
		 */
	    		for (bt_ptr = dkbad[unit].bt_bad;
				bt_ptr->bt_cyl != -1; bt_ptr++) {
			if (bt_ptr->bt_cyl > cylin)
				/* Sorted list, and we passed our cylinder.
					quit. */
				break;
			if (bt_ptr->bt_cyl == cylin &&
				bt_ptr->bt_trksec == (head << 8) + sector) {
			/*
			 * Found bad block.  Calculate new block addr.
			 * This starts at the end of the disk (skip the
			 * last track which is used for the bad block list),
			 * and works backwards to the front of the disk.
			 */
				blknum = (du->dk_dd.dk_secperunit)
					- du->dk_dd.dk_nsectors
					- (bt_ptr - dkbad[unit].bt_bad) - 1;
				cylin = blknum / secpercyl;
				head = (blknum % secpercyl) / secpertrk;
				sector = blknum % secpertrk;
				break;
			}

		/* select drive.     */
		inb(wdc+wd_sdh = du->dk_sdh | (head&07);
		while ((inb(wdc+wd_status & WDCS_READY) == 0) nulldev();

		/* transfer some blocks */
		inb(wdc+wd_sector = sector;
		inb(wdc+wd_seccnt = 1;
		inb(wdc+wd_cyl_lo = cylin;
		if (du->dk_dd.dk_ntracks > 8) { 
			if (head > 7)
				inb(wdc+wd_precomp = 0;	/* set 3rd head bit */
			else
				inb(wdc+wd_precomp = 0xff;	/* set 3rd head bit */
		}
		inb(wdc+wd_cyl_hi = cylin >> 8;
#ifdef notdef
		/* lets just talk about this first...*/
		printf ("sdh 0%o sector %d cyl %d addr 0x%x\n",
			wdp->wd_sdh, wdp->wd_sector,
			wdp->wd_cyl_hi*256+wdp->wd_cyl_lo, addr) ;
		for (i=10000; i > 0 ; i--)
			;
		continue;
#endif
		inb(wdc+wd_command = WDCC_WRITE;
		
		/* Ready to send data?	*/
		while ((inb(wdc+wd_status & WDCS_DRQ) == 0) nulldev();
		if (inb(wdc+wd_status & WDCS_ERR) return(EIO) ;

		end = (char *)addr + du->dk_dd.dk_secsize;
		for (; addr < end; addr += 8) {
			wdp->wd_data = addr[0];
			wdp->wd_data = addr[1];
			wdp->wd_data = addr[2];
			wdp->wd_data = addr[3];
			wdp->wd_data = addr[4];
			wdp->wd_data = addr[5];
			wdp->wd_data = addr[6];
			wdp->wd_data = addr[7];
		}
		if (inb(wdc+wd_status & WDCS_ERR) return(EIO) ;
		/* Check data request (should be done).         */
		if (inb(wdc+wd_status & WDCS_DRQ) return(EIO) ;

		/* wait for completion */
		for ( i = 1000000 ; inb(wdc+wd_status & WDCS_BUSY ; i--) {
				if (i < 0) return (EIO) ;
				nulldev () ;
		}
		/* error check the xfer */
		if (inb(wdc+wd_status & WDCS_ERR) return(EIO) ;
		/* update block count */
		num--;
		blknum++ ;
#ifdef	WDDEBUG
if (num % 100 == 0) printf(".") ;
#endif
	}
	return(0);
#endif
}
#endif
