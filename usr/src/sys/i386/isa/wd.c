/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)wd.c	7.4 (Berkeley) %G%
 */

/* TODO:peel out buffer at low ipl,
   speed improvement, rewrite to clean code from garbage artifacts */


#include "wd.h"
#if	NWD > 0

#include <sys/param.h>
#include <sys/dkbad.h>
#include <sys/systm.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/disklabel.h>
#include <sys/buf.h>
#include <sys/uio.h>
#include <sys/syslog.h>

#include <i386/isa/isa_device.h>
#include <i386/isa/icu.h>
#include <i386/isa/wdreg.h>
#include <vm/vm.h>

#define	RETRIES		5	/* number of retries before giving up */
#define	MAXTRANSFER	32	/* max size of transfer in page clusters */

#define wdctlr(dev)	((minor(dev) & 0x80) >> 7)
#define wdunit(dev)	((minor(dev) & 0x60) >> 5)
#define wdpart(dev)	((minor(dev) & 0x1f))

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
	struct disklabel dk_dd;	/* device configuration data */
	long	dk_bc;		/* byte count left */
	short	dk_skip;	/* blocks already transferred */
	char	dk_unit;	/* physical unit number */
	char	dk_state;	/* control state */
	u_char	dk_status;	/* copy of status reg. */
	u_char	dk_error;	/* copy of error reg. */
	short	dk_open;	/* open/closed refcnt */
        u_long  dk_copenpart;   /* character units open on this drive */
        u_long  dk_bopenpart;   /* block units open on this drive */
        u_long  dk_openpart;    /* all units open on this drive */
	short	dk_wlabel;	/* label writable? */
};

/*
 * This label is used as a default when initializing a new or raw disk.
 * It really only lets us access the first track until we know more.
 */
struct disklabel dflt_sizes = {
	DISKMAGIC, DTYPE_ST506, 0, "default", "",
		512,		/* sector size */
		17,		/* # of sectors per track */
		8,		/* # of tracks per cylinder */
		766,		/* # of cylinders per unit */
		17*8,		/* # of sectors per cylinder */
		766*8*17,	/* # of sectors per unit */
		0,		/* # of spare sectors per track */
		0,		/* # of spare sectors per cylinder */
		0,		/* # of alt. cylinders per unit */
		3600,		/* rotational speed */
		1,		/* hardware sector interleave */
		0,		/* sector 0 skew, per track */
		0,		/* sector 0 skew, per cylinder */
		0,		/* head switch time, usec */
		0,		/* track-to-track seek, usec */
		0,		/* generic flags */
		0,0,0,0,0,
		0,0,0,0,0,
		DISKMAGIC,
		0,
		8,
		8192,
		8192,
	
	{{21600,	0, 0,0,0,0},	/* A=root filesystem */
	{21600,	40, 0,0,0,0},
	{660890, 0, 0,0,0,0},	/* C=whole disk */
	{216000,	80, 0,0,0,0},
	{0,	0, 0,0,0,0},
	{0,	0, 0,0,0,0},
	{0,	0, 0,0,0,0},
	{399600,	480, 0,0,0,0}}
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
	/* XXX sorry, needs to be better */
	outb(wdc+wd_error, 0x5a) ;	/* error register not writable */
	outb(wdc+wd_cyl_lo, 0xa5) ;	/* but all of cyllo are implemented */
	if(inb(wdc+wd_error) != 0x5a && inb(wdc+wd_cyl_lo) == 0xa5)
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
	register struct partition *p;
	long maxsz, sz;
	int	unit = wdunit(bp->b_dev);
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
#ifdef old
	/*
	 * Convert DEV_BSIZE "blocks" to sectors.
	 * Note: doing the conversions this way limits the partition size
	 * to about 8 million sectors (1-8 Gb).
	 */
	blknum = (unsigned long) bp->b_blkno * DEV_BSIZE / du->dk_dd.d_secsize;
	if (((u_long) bp->b_blkno * DEV_BSIZE % du->dk_dd.d_secsize != 0) ||
	    bp->b_bcount >= MAXTRANSFER * CLBYTES) {
		bp->b_flags |= B_ERROR;
		goto bad;
	}
	nblocks = du->dk_dd.d_partitions[part].p_size;
	cyloff = du->dk_dd.d_partitions[part].p_offset;
	if (blknum + (bp->b_bcount / du->dk_dd.d_secsize) > nblocks) {
		if (blknum == nblocks)
			bp->b_resid = bp->b_bcount;
		else
			bp->b_flags |= B_ERROR;
		goto bad;
	}
	bp->b_cylin = blknum / du->dk_dd.d_secpercyl + cyloff;
#else
        /*
         * Determine the size of the transfer, and make sure it is
         * within the boundaries of the partition.
         */
        p = &du->dk_dd.d_partitions[wdpart(bp->b_dev)];
        maxsz = p->p_size;
        sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
        if (bp->b_blkno + p->p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
            bp->b_blkno + p->p_offset + sz > LABELSECTOR &&
#endif
            (bp->b_flags & B_READ) == 0 && du->dk_wlabel == 0) {
                bp->b_error = EROFS;
                goto bad;
        }
        if (bp->b_blkno < 0 || bp->b_blkno + sz > maxsz) {
                /* if exactly at end of disk, return an EOF */
                if (bp->b_blkno == maxsz) {
                        bp->b_resid = bp->b_bcount;
                        biodone(bp);
                        return;
                }
                /* or truncate if part of it fits */
                sz = maxsz - bp->b_blkno;
                if (sz <= 0)
                        goto bad;
                bp->b_bcount = sz << DEV_BSHIFT;
        }
        bp->b_cylin = (bp->b_blkno + p->p_offset) / du->dk_dd.d_secpercyl;
#endif
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
	int	unit, s;

loop:
	dp = wdtab.b_actf;
	if (dp == NULL)
		return;
	bp = dp->b_actf;
	if (bp == NULL) {
		wdtab.b_actf = dp->b_forw;
		goto loop;
	}
	unit = wdunit(bp->b_dev);
	du = &wddrives[unit];
	if (DISKSTATE(du->dk_state) <= RDLABEL) {
		if (wdcontrol(bp)) {
			dp->b_actf = bp->av_forw;
			goto loop;	/* done */
		}
		return;
	}
	secpertrk = du->dk_dd.d_nsectors;
	secpercyl = du->dk_dd.d_secpercyl;
	/*
	 * Convert DEV_BSIZE "blocks" to sectors.
	 */
	blknum = (unsigned long) bp->b_blkno * DEV_BSIZE / du->dk_dd.d_secsize
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
		cylin += du->dk_dd.d_partitions[wdpart(bp->b_dev)].p_offset
				/ secpercyl;

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
			blknum = du->dk_dd.d_secperunit - du->dk_dd.d_nsectors
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
	while ((inb(wdc+wd_status) & WDCS_DRQ) == 0);

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
wdintr(unit)
{
	register struct	disk *du;
	register struct buf *bp, *dp;
	int status;
	char partch ;
	static wd_haderror;

	/* Shouldn't need this, but it may be a slow controller.	*/
	while ((status = inb(wdc+wd_status)) & WDCS_BUSY) ;
	if (!wdtab.b_active) {
		printf("wd: extra interrupt\n");
		return;
	}

#ifdef	WDDEBUG
	dprintf(DDSK,"I ");
#endif
	dp = wdtab.b_actf;
	bp = dp->b_actf;
	du = &wddrives[wdunit(bp->b_dev)];
	partch = wdpart(bp->b_dev) + 'a';
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
		
		wd_errsector = (bp->b_cylin * du->dk_dd.d_secpercyl) +
			(((unsigned long) bp->b_blkno * DEV_BSIZE /
			    du->dk_dd.d_secsize) % du->dk_dd.d_secpercyl) +
			du->dk_skip;
		wd_errbn = bp->b_blkno
			+ du->dk_skip * du->dk_dd.d_secsize / DEV_BSIZE ;
		if (status & WDCS_ERR) {
			if (++wdtab.b_errcnt < RETRIES) {
				wdtab.b_active = 0;
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
		while ((inb(wdc+wd_status) & WDCS_DRQ) == 0) ;

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
wdopen(dev, flags, fmt)
        dev_t dev;
        int flags, fmt;
{
	register unsigned int unit;
	register struct buf *bp;
	register struct disk *du;
        int part = wdpart(dev), mask = 1 << part;
        struct partition *pp;
	struct dkbad *db;
	int i, error = 0;

	unit = wdunit(dev);
	if (unit >= NWD) return (ENXIO) ;
	du = &wddrives[unit];
#ifdef notdef
	if (du->dk_open){
		du->dk_open++ ;
		return(0);	/* already is open, don't mess with it */
	}
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
	bp->b_bcount = 0;
	bp->b_blkno = LABELSECTOR;
	bp->b_flags = B_READ;
	wdstrategy(bp);
	biowait(bp);
	if (bp->b_flags & B_ERROR) {
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
		bp->b_flags = B_BUSY | B_READ;
		bp->b_blkno = du->dk_dd.d_secperunit - du->dk_dd.d_nsectors
			+ i;
		if (du->dk_dd.d_secsize > DEV_BSIZE)
			bp->b_blkno *= du->dk_dd.d_secsize / DEV_BSIZE;
		else
			bp->b_blkno /= DEV_BSIZE / du->dk_dd.d_secsize;
		bp->b_bcount = du->dk_dd.d_secsize;
		bp->b_cylin = du->dk_dd.d_ncylinders - 1;
		wdstrategy(bp);
		biowait(bp);
	} while ((bp->b_flags & B_ERROR) && (i += 2) < 10 &&
		i < du->dk_dd.d_nsectors);
	db = (struct dkbad *)(bp->b_un.b_addr);
#define DKBAD_MAGIC 0x4321
	if ((bp->b_flags & B_ERROR) == 0 && db->bt_mbz == 0 &&
	    db->bt_flag == DKBAD_MAGIC) {
		dkbad[unit] = *db;
		du->dk_state = OPEN;
	} else {
		printf("wd%d: %s bad-sector file\n", unit,
		    (bp->b_flags & B_ERROR) ? "can't read" : "format error in");
		error = ENXIO ;
		du->dk_state = OPENRAW;
	}
done:
	bp->b_flags = B_INVAL | B_AGE;
	brelse(bp);
	if (error == 0)
		du->dk_open = 1;

        /*
         * Warn if a partion is opened
         * that overlaps another partition which is open
         * unless one is the "raw" partition (whole disk).
         */
#define RAWPART         8               /* 'x' partition */     /* XXX */
        if ((du->dk_openpart & mask) == 0 && part != RAWPART) {
		int	start, end;

                pp = &du->dk_dd.d_partitions[part];
                start = pp->p_offset;
                end = pp->p_offset + pp->p_size;
                for (pp = du->dk_dd.d_partitions;
                     pp < &du->dk_dd.d_partitions[du->dk_dd.d_npartitions];
			pp++) {
                        if (pp->p_offset + pp->p_size <= start ||
                            pp->p_offset >= end)
                                continue;
                        if (pp - du->dk_dd.d_partitions == RAWPART)
                                continue;
                        if (du->dk_openpart & (1 << (pp -
					du->dk_dd.d_partitions)))
                                log(LOG_WARNING,
                                    "wd%d%c: overlaps open partition (%c)\n",
                                    unit, part + 'a',
                                    pp - du->dk_dd.d_partitions + 'a');
                }
        }
        if (part >= du->dk_dd.d_npartitions)
                return (ENXIO);
        du->dk_openpart |= mask;
        switch (fmt) {
        case S_IFCHR:
                du->dk_copenpart |= mask;
                break;
        case S_IFBLK:
                du->dk_bopenpart |= mask;
                break;
        }
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

	du = &wddrives[wdunit(bp->b_dev)];
	unit = du->dk_unit;
	switch (DISKSTATE(du->dk_state)) {

	tryagainrecal:
	case WANTOPEN:			/* set SDH, step rate, do restore */
#ifdef	WDDEBUG
		dprintf(DDSK,"wd%d: recal ", unit);
#endif
		s = splbio();		/* not called from intr level ... */
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

		/* some compaq controllers require this ... */
		wdsetctlr(bp->b_dev, du);

		wdtab.b_errcnt = 0;
		if (ISRAWSTATE(du->dk_state)) {
			du->dk_state = OPENRAW;
			return(1);
		}
retry:
#ifdef	WDDEBUG
		dprintf(DDSK,"rdlabel ");
#endif
if( cyloffset < 0 || cyloffset > 8192) cyloffset=0;
		/*
		 * Read in sector LABELSECTOR to get the pack label
		 * and geometry.
		 */
		outb(wdc+wd_precomp, 0xff);/* sometimes this is head bit 3 */
		outb(wdc+wd_seccnt, 1);
		outb(wdc+wd_sector, LABELSECTOR+1);
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
		    (bp->b_un.b_addr + LABELOFFSET))->d_magic == DISKMAGIC) {
		       du->dk_dd =
			 * (struct disklabel *) (bp->b_un.b_addr + LABELOFFSET);
		} else {
			printf("wd%d: bad disk label\n", du->dk_unit);
			du->dk_state = OPENRAW;
		}

		s = splbio();		/* not called from intr level ... */
		while ((stat = inb(wdc+wd_status)) & WDCS_BUSY);

		wdsetctlr(bp->b_dev, du);

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
		panic("wdcontrol");
	}
	/* NOTREACHED */

badopen:
	printf(": status %b error %b\n",
		stat, WDCS_BITS, inb(wdc+wd_error), WDERR_BITS);
	du->dk_state = OPENRAW;
	return(1);
}

wdsetctlr(dev, du) dev_t dev; struct disk *du; {
	int stat;

	outb(wdc+wd_cyl_lo, du->dk_dd.d_ncylinders);
	outb(wdc+wd_cyl_hi, (du->dk_dd.d_ncylinders)>>8);
	outb(wdc+wd_sdh, WDSD_IBM | (wdunit(dev) << 4) + du->dk_dd.d_ntracks-1);
	outb(wdc+wd_seccnt, du->dk_dd.d_nsectors);
	outb(wdc+wd_command, 0x91);

	while ((stat = inb(wdc+wd_status)) & WDCS_BUSY) ;
	stat = inb(wdc+wd_error);
	return(stat);
}

/* ARGSUSED */
wdclose(dev, flags, fmt)
        dev_t dev;
        int flags, fmt;
{
	register struct disk *du;

	du = &wddrives[wdunit(dev)];
	du->dk_open-- ;
	/*if (du->dk_open == 0) du->dk_state = CLOSED ; does not work */
}

wdioctl(dev,cmd,addr,flag)
	dev_t dev;
	caddr_t addr;
{
	int unit = wdunit(dev);
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

        case DIOCGPART:
                ((struct partinfo *)addr)->disklab = &du->dk_dd;
                ((struct partinfo *)addr)->part =
                    &du->dk_dd.d_partitions[wdpart(dev)];
                break;

        case DIOCSDINFO:
                if ((flag & FWRITE) == 0)
                        error = EBADF;
                else
                        error = setdisklabel(&du->dk_dd,
					(struct disklabel *)addr,
                         0 /*(dk->dk_state == OPENRAW) ? 0 : dk->dk_openpart*/);
                /*if (error == 0 && dk->dk_state == OPENRAW &&
                    vdreset_drive(vddinfo[unit]))
                        dk->dk_state = OPEN;*/
		wdsetctlr(dev, du);
                break;

        case DIOCWLABEL:
                if ((flag & FWRITE) == 0)
                        error = EBADF;
                else
                        du->dk_wlabel = *(int *)addr;
                break;

        case DIOCWDINFO:
                if ((flag & FWRITE) == 0)
                        error = EBADF;
                else if ((error = setdisklabel(&du->dk_dd, (struct disklabel *)addr,
                  0/*(dk->dk_state == OPENRAW) ? 0 : dk->dk_openpart*/)) == 0) {
                        int wlab;

                        /*if (error == 0 && dk->dk_state == OPENRAW &&
                            vdreset_drive(vddinfo[unit]))
                                dk->dk_state = OPEN; */
			wdsetctlr(dev, du);

                        /* simulate opening partition 0 so write succeeds */
                        /* dk->dk_openpart |= (1 << 0);            /* XXX */
                        wlab = du->dk_wlabel;
                        du->dk_wlabel = 1;
                        error = writedisklabel(dev, wdstrategy, &du->dk_dd,wdpart(dev));
                        /*dk->dk_openpart = dk->dk_copenpart | dk->dk_bopenpart;*/
                        du->dk_wlabel = wlab;
                }
                break;

#ifdef notyet
	case DIOCGDINFOP:
		*(struct disklabel **)addr = &(du->dk_dd);
		break;

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
				fop->df_startblk * du->dk_dd.d_secsize;
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
	int unit = wdunit(dev) ;

	if (unit >= NWD) return(ENXIO);
	return(physio(wdstrategy, &rwdbuf[unit], dev, B_READ, minphys, uio));
}


wdwrite(dev, uio)			/* character write routine */
	dev_t dev;
	struct uio *uio;
{
	int unit = wdunit(dev) ;

	if (unit >= NWD) return(ENXIO);
	return(physio(wdstrategy, &rwdbuf[unit], dev, B_WRITE, minphys, uio));
}

wdsize(dev)
	dev_t dev;
{
	register unit = wdunit(dev);
	register part = wdpart(dev);
	register struct disk *du;
	register val ;

	if (unit >= NWD) return(-1);
	if (wddrives[unit].dk_state == 0) {
		val = wdopen (dev, 0);
		if (val < 0)
			return (-1);
	}
	du = &wddrives[unit];
	return((int)((u_long)du->dk_dd.d_partitions[part].p_size *
		du->dk_dd.d_secsize / 512));
}

extern        char *vmmap;            /* poor name! */

wddump(dev)			/* dump core after a system crash */
	dev_t dev;
{
	register struct disk *du;	/* disk unit to do the IO */
	register struct bt_bad *bt_ptr;
	long	num;			/* number of sectors to write */
	int	unit, part;
	long	cyloff, blknum, blkcnt;
	long	cylin, head, sector, stat;
	long	secpertrk, secpercyl, nblocks, i;
	char *addr;
	extern	int Maxmem;
	static  wddoingadump = 0 ;
	extern CMAP1;
	extern char CADDR1[];

	
#ifdef ARGO
outb(0x461,0);	/* disable failsafe timer */
#endif
	addr = (char *) 0;		/* starting address */
	/* size of memory to dump */
	num = Maxmem;
	unit = wdunit(dev);		/* eventually support floppies? */
	part = wdpart(dev);		/* file system */
	/* check for acceptable drive number */
	if (unit >= NWD) return(ENXIO);

	du = &wddrives[unit];
	/* was it ever initialized ? */
	if (du->dk_state < OPEN) return (ENXIO) ;

	/* Convert to disk sectors */
	num = (u_long) num * NBPG / du->dk_dd.d_secsize;

	/* check if controller active */
	/*if (wdtab.b_active) return(EFAULT); */
	if (wddoingadump) return(EFAULT);

	secpertrk = du->dk_dd.d_nsectors;
	secpercyl = du->dk_dd.d_secpercyl;
	nblocks = du->dk_dd.d_partitions[part].p_size;
	cyloff = du->dk_dd.d_partitions[part].p_offset / secpercyl;

/*pg("xunit %x, nblocks %d, dumplo %d num %d\n", part,nblocks,dumplo,num);*/
	/* check transfer bounds against partition size */
	if ((dumplo < 0) || ((dumplo + num) > nblocks))
		return(EINVAL);

	/*wdtab.b_active = 1;		/* mark controller active for if we
					   panic during the dump */
	wddoingadump = 1  ;  i = 100000 ;
	while ((inb(wdc+wd_status) & WDCS_BUSY) && (i-- > 0)) ;
	outb(wdc+wd_sdh, WDSD_IBM | (unit << 4));
	outb(wdc+wd_command, WDCC_RESTORE | WD_STEP);
	while (inb(wdc+wd_status) & WDCS_BUSY) ;

	/* some compaq controllers require this ... */
	wdsetctlr(dev, du);
	
	blknum = dumplo;
	while (num > 0) {
#ifdef notdef
		if (blkcnt > MAXTRANSFER) blkcnt = MAXTRANSFER;
		if ((blknum + blkcnt - 1) / secpercyl != blknum / secpercyl)
			blkcnt = secpercyl - (blknum % secpercyl);
			    /* keep transfer within current cylinder */
#endif
		pmap_enter(kernel_pmap, vmmap, addr, VM_PROT_READ, TRUE);

		/* compute disk address */
		cylin = blknum / secpercyl;
		head = (blknum % secpercyl) / secpertrk;
		sector = blknum % secpertrk;
		cylin += cyloff;

#ifdef notyet
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
				blknum = (du->dk_dd.d_secperunit)
					- du->dk_dd.d_nsectors
					- (bt_ptr - dkbad[unit].bt_bad) - 1;
				cylin = blknum / secpercyl;
				head = (blknum % secpercyl) / secpertrk;
				sector = blknum % secpertrk;
				break;
			}

#endif
		sector++;		/* origin 1 */

		/* select drive.     */
		outb(wdc+wd_sdh, WDSD_IBM | (unit<<4) | (head & 0xf));
		while ((inb(wdc+wd_status) & WDCS_READY) == 0) ;

		/* transfer some blocks */
		outb(wdc+wd_sector, sector);
		outb(wdc+wd_seccnt,1);
		outb(wdc+wd_cyl_lo, cylin);
		outb(wdc+wd_cyl_hi, cylin >> 8);
#ifdef notdef
		/* lets just talk about this first...*/
		pg ("sdh 0%o sector %d cyl %d addr 0x%x",
			inb(wdc+wd_sdh), inb(wdc+wd_sector),
			inb(wdc+wd_cyl_hi)*256+inb(wdc+wd_cyl_lo), addr) ;
#endif
#ifdef ODYSSEUS
if(cylin < 46 || cylin > 91)pg("oops");
#endif
#ifdef PRIAM
if(cylin < 40 || cylin > 79)pg("oops");
#endif
		outb(wdc+wd_command, WDCC_WRITE);
		
		/* Ready to send data?	*/
		while ((inb(wdc+wd_status) & WDCS_DRQ) == 0) ;
		if (inb(wdc+wd_status) & WDCS_ERR) return(EIO) ;

		outsw (wdc+wd_data, CADDR1+((int)addr&(NBPG-1)), 256);
		(int) addr += 512;

		if (inb(wdc+wd_status) & WDCS_ERR) return(EIO) ;
		/* Check data request (should be done).         */
		if (inb(wdc+wd_status) & WDCS_DRQ) return(EIO) ;

		/* wait for completion */
		for ( i = 1000000 ; inb(wdc+wd_status) & WDCS_BUSY ; i--) {
				if (i < 0) return (EIO) ;
		}
		/* error check the xfer */
		if (inb(wdc+wd_status) & WDCS_ERR) return(EIO) ;
		/* update block count */
		num--;
		blknum++ ;
if (num % 100 == 0) printf(".") ;
	}
	return(0);
}
#endif
