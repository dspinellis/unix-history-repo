/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)wd.c	7.1 (Berkeley) %G%
 */

/*  device driver for winchester disk  */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dkbad.h"
#include "../h/disk.h"
#include "../isa/atio.h"
#include "../isa/wdreg.h"
#include "saio.h"

#define	NWD		2	/* number of hard disk units supported, max 2 */
#define	RETRIES		5	/* number of retries before giving up */

int noretries = 0;
int wdquiet = 0;
#ifdef	WDDEBUG
int wdinfoflag = 0;
#endif

#ifdef	SMALL
extern struct disklabel disklabel;
#else
struct disklabel wdsizes[NWD];
extern struct disklabel *dlp;
#endif
int cyloffset;

/*
 * Record for the bad block forwarding code.
 * This is initialized to be empty until the bad-sector table
 * is read from the disk.
 */
#define TRKSEC(trk,sec)	((trk << 8) + sec)

struct	dkbad	dkbad[NWD];

wdopen(io)
	register struct iob *io;
{
        register struct disklabel *dd;
	int unit, partition;

	unit = minor_unit(minor(io->i_ino.i_dev));
	partition = minor_partition(minor(io->i_ino.i_dev));
#ifdef SMALL
        dd = &disklabel;
#else
        dd = &wdsizes[unit];
	if (partition >= 8)
                _stop("Invalid partition number");
#endif
        if (wdinit(io))
                _stop("wd initialization error");
}

wdstrategy(io,func)
	register struct iob *io;
{
	register int iosize;    /* number of sectors to do IO for this loop */
	register daddr_t sector;
	int nblocks, cyloff;
	int unit, partition;
	char *address;
	register struct disklabel *dd;

	unit = minor_unit(minor(io->i_ino.i_dev));
	partition = minor_partition(minor(io->i_ino.i_dev));
	if ((unsigned)unit >= NWD) {
		printf("wd: unit %d\n", unit);
		return(-1);
	}
#ifdef	SMALL
	dd = &disklabel;
#else
	dd = &wdsizes[unit];
#endif
        iosize = io->i_cc / dd->dk_secsize;
	/*
	 * Convert PGSIZE "blocks" to sectors.
	 * Note: doing the conversions this way limits the partition size
	 * to about 8 million sectors (1-8 Gb).
	 */
	sector = (unsigned long) io->i_bn * DEV_BSIZE / dd->dk_secsize;
	nblocks = dd->dk_partition[partition].nblocks;
	cyloff = dd->dk_partition[partition].cyloff;
        if (iosize < 0 || sector + iosize > nblocks || sector < 0) {
#ifdef WDDEBUG
		printf("bn = %d; sectors = %d; partition = %d; fssize = %d\n",
			io->i_bn, iosize, partition, nblocks);
#endif
                printf("wdstrategy - I/O out of filesystem boundaries\n");
		return(-1);
	}
	if (io->i_bn * DEV_BSIZE % dd->dk_secsize) {
		printf("wdstrategy - transfer starts in midsector\n");
		return(-1);
	}
        if (io->i_cc % dd->dk_secsize) {
		printf("wd: transfer of partial sector\n");
		return(-1);
	}

	address = io->i_ma;
	sector += cyloff * dd->dk_secpercyl;
        while (iosize > 0) {
                if (wdio(func, unit, sector, address))
                        return(-1);
		iosize--;
		sector++;
                address += dd->dk_secsize;
        }
        return(io->i_cc);
}

/* 
 * Routine to do a one-sector I/O operation, and wait for it
 * to complete.
 */
wdio(func, unit, blknm, addr)
        short *addr;
{
	struct disklabel *dd;
	int wdc = IO_WD0;
	struct bt_bad *bt_ptr;
        int    i;
	int retries = 0;
        long    cylin, head, sector;
        u_char opcode;

#ifdef	SMALL
	dd = &disklabel;
#else
	dd = &wdsizes[unit];
#endif
        if (func == WRITE)
                opcode = WDCC_WRITE;
        else
                opcode = WDCC_READ;

        /* Calculate data for output.           */
        cylin = blknm / dd->dk_secpercyl;
        head = (blknm % dd->dk_secpercyl) / dd->dk_nsectors;
        sector = blknm % dd->dk_nsectors + 1;

#ifdef notyet
	/* 
	 * See if the current block is in the bad block list.
	 */
	if (blknm > 7)	/* should be BBSIZE */
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
#ifdef WDDEBUG
			if (wdinfoflag)
			    printf("--- badblock code -> Old = %d; ",
				blknm);
#endif
			blknm = dd->dk_secperunit - dd->dk_nsectors
				- (bt_ptr - dkbad[unit].bt_bad) - 1;
			cylin = blknm / dd->dk_secpercyl;
			head = (blknm % dd->dk_secpercyl) / dd->dk_nsectors;
			sector = blknm % dd->dk_nsectors;
#ifdef WDDEBUG
			if (wdinfoflag)
			    printf("new = %d\n", blknm);
#endif
			break;
		}
	}

#endif
retry:
printf("sec %d sdh %x cylin %d\n", sector
	, WDSD_IBM | (unit<<4) | (head & 0xf), cylin);
	outb(wdc+wd_precomp, 0xff);
	outb(wdc+wd_seccnt, 1);
	outb(wdc+wd_sector, sector);
	outb(wdc+wd_cyl_lo, cylin);
	outb(wdc+wd_cyl_hi, cylin >> 8);

	/* Set up the SDH register (select drive).     */
	outb(wdc+wd_sdh, WDSD_IBM | (unit<<4) | (head & 0xf));
	while ((inb(wdc+wd_altsts) & WDCS_READY) == 0) ;

	outb(wdc+wd_command, opcode);
	while (opcode == WDCC_READ && (inb(wdc+wd_altsts) & WDCS_BUSY))
		;
	/* Did we get an error?         */
	if (opcode == WDCC_READ && (inb(wdc+wd_altsts) & WDCS_ERR))
		goto error;

	/* Ready to remove data?        */
	while ((inb(wdc+wd_altsts) & WDCS_DRQ) == 0) ;

	if (opcode == WDCC_READ)
		insw(wdc+wd_data,addr,256);
	else	outsw(wdc+wd_data,addr,256);

	/* Check data request (should be done).         */
	if (inb(wdc+wd_altsts) & WDCS_DRQ) goto error;

	while (opcode == WDCC_WRITE && (inb(wdc+wd_altsts) & WDCS_BUSY)) ;

	if (inb(wdc+wd_altsts) & WDCS_ERR) goto error;

/*for (i=0; i < 256 ; i++){
	if((i%20) == 0) printf("\n");
	printf("%x ", addr[i]);
}
i=getchar();*/

        return (0);
error:
	wddumpregs(wdc);
	if (++retries < RETRIES)
		goto retry;
	if (!wdquiet)
	    printf("wd%d: hard %s error: sector %d, status %b error %b\n", unit,
		opcode == WDCC_READ? "read" : "write", blknm, 
		inb(wdc+wd_status), WDCS_BITS, inb(wdc+wd_error), WDERR_BITS);
	return (-1);
}

wdinit(io)
	struct iob *io;
{
	int wdc = IO_WD0;
	struct disklabel *dd;
        unsigned int   unit;
	struct dkbad *db;
	int i, errcnt = 0;
	char buf[512];
	static open[NWD];

	/* reset controller */
	outb(wdc+wd_ctlr, 4); wait(10); outb(wdc+wd_ctlr, 0);
	wdwait();

	unit = minor_unit(minor(io->i_ino.i_dev));
#ifdef	SMALL
	dd = &disklabel;
	outb(wdc+wd_command, WDCC_RESTORE | WD_STEP);
	wdwait();
#else
	dd = &wdsizes[unit];
	if (open[unit]) return(0);
/*
code to tell disk controller geometry of disk
not currently used

	outb(wdc+wd_sdh, 0xa7);
wdwait();
	outb(wdc+wd_seccnt, 17);
	outb(wdc+wd_cyl_lo, 0);
	outb(wdc+wd_command, 0x91);
	wdwait();*/

tryagainrecal:
	/* set SDH, step rate, do restore */
	outb(wdc+wd_sdh, WDSD_IBM | (unit << 4));
	wdwait();
	outb(wdc+wd_command, WDCC_RESTORE | WD_STEP);
	wdwait();
	if ((i = inb(wdc+wd_status)) & WDCS_ERR) {
		printf("wd%d: recal status %b error %b\n",
			unit, i, WDCS_BITS, inb(wdc+wd_error), WDERR_BITS);
		wddumpregs(wdc);
		if (++errcnt < 10)
			goto tryagainrecal;
		return(-1);
	}
#endif	SMALL
#ifndef	SMALL
	errcnt = 0;
retry:
cyloffset = 290;
	/*
	 * Read in sector 0 to get the pack label and geometry.
	 */
	outb(wdc+wd_precomp, 0xff);	/* sometimes this is head bit 3 */
	outb(wdc+wd_seccnt, 1);
	outb(wdc+wd_sector, 1);
	outb(wdc+wd_cyl_lo, (cyloffset & 0xff));
	outb(wdc+wd_cyl_hi, (cyloffset >> 8));
	outb(wdc+wd_sdh, WDSD_IBM | (unit << 4));
	wdwait();
	outb(wdc+wd_command, WDCC_READ);
	wdwait();
	if ((i = inb(wdc+wd_status)) & WDCS_ERR) {
		wddumpregs(wdc);
		if (++errcnt < RETRIES)
			goto retry;
		if (!wdquiet)
		    printf("wd%d: reading label, status %b error %b\n",
			unit, i, WDCS_BITS, inb(wdc+wd_error), WDERR_BITS);
		return(-1);
	}

	/* Ready to remove data?        */
	while ((inb(wdc+wd_altsts) & WDCS_DRQ) == 0) ;

	i = insw(wdc+wd_data, buf, 256);

	/*printf("magic %x,insw %x, %x\n",
	((struct disklabel *) (buf + LABELOFFSET))->dk_magic, i, buf);*/
	if (((struct disklabel *) (buf + LABELOFFSET))->dk_magic == DISKMAGIC) {
		*dd = * (struct disklabel *) (buf + LABELOFFSET);
		open[unit] = 1;
	} else {
		if (!wdquiet)
			printf("wd%d: bad disk label\n", unit);
		if (io->i_flgs & F_FILE) return(-1);
		dkbad[unit].bt_bad[0].bt_cyl = -1;
		dd->dk_secpercyl = 1999999 ; dd->dk_nsectors = 17 ;
		dd->dk_secsize = 512;
		outb(wdc+wd_precomp, 0xff);	/* force head 3 bit off */
/**dd = *dlp;
open[unit] = 1;*/
		return (0) ;
	}
	dkbad[unit].bt_bad[0].bt_cyl = -1;
	outb(wdc+wd_precomp, dd->dk_precompcyl / 4);
#endif	SMALL

#ifdef notyet
	/*
	 * Read bad sector table into memory.
	 */
	i = 0;
	do {
		int blknm = dd->dk_secperunit - dd->dk_nsectors + i;
		errcnt = wdio(READ, unit, blknm, buf);
	} while (errcnt && (i += 2) < 10 && i < dd->dk_nsectors);
	db = (struct dkbad *)(buf);
	if (errcnt == 0 && db->bt_mbz == 0 && db->bt_flag == DKBAD_MAGIC)
		dkbad[unit] = *db;
	else {
		if (!wdquiet)
			printf("wd%d: error in bad-sector file\n", unit);
		dkbad[unit].bt_bad[0].bt_cyl = -1;
	}
#endif
	return(0);
}

wdwait()
{
	register wdc = IO_WD0;
	register i = 0;
	
	while (inb(wdc+wd_altsts) & WDCS_BUSY)
		;
	while ((inb(wdc+wd_altsts) & WDCS_READY) == 0)
		if (i++ > 100000)
			return(-1);
	return(0);
}


wddumpregs(wdc){

printf("err %x ", inb(wdc+wd_error));
printf("seccnt %d ", inb(wdc+wd_seccnt));
printf("sector %d ", inb(wdc+wd_sector));
printf("cyl %d:", inb(wdc+wd_cyl_lo));
printf("%d ", inb(wdc+wd_cyl_hi));
printf("sdh %x ", inb(wdc+wd_sdh));
printf("sts %x ", inb(wdc+wd_status));
printf("alt %x ", inb(wdc+wd_altsts));
printf("dig %x\n", inb(wdc+wd_digin));

}
