#include "fd.h"
#if NFD > 0
/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Don Ahn.
 *
 * %sccs.include.386.c%
 *
 *	@(#)fd.c	5.3 (Berkeley) %G%
 */

/****************************************************************************/
/*                               fd driver                                  */
/****************************************************************************/
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
#include "machine/isa/fdreg.h"
#include "icu.h"

#define	FDUNIT(s)	((s)&1)
#define	FDTYPE(s)	(((s)>>1)&7)

#define b_cylin b_resid
#define b_step b_resid
#define FDBLK 512
#define NUMTYPES 4

struct fd_type {
	int	sectrac;		/* sectors per track         */
	int	secsize;		/* size code for sectors     */
	int	datalen;		/* data len when secsize = 0 */
	int	gap;			/* gap len between sectors   */
	int	tracks;			/* total num of tracks       */
	int	size;			/* size of disk in sectors   */
	int	steptrac;		/* steps per cylinder        */
	int	trans;			/* transfer speed code       */
};

struct fd_type fd_types[NUMTYPES] = {
 	{ 18,2,0xFF,0x1B,80,2880,1,0 },	/* 1.44 meg HD 3.5in floppy    */
	{ 15,2,0xFF,0x1B,80,2400,1,0 },	/* 1.2 meg HD floppy           */
	{ 9,2,0xFF,0x23,40,720,2,1 },	/* 360k floppy in 1.2meg drive */
	{ 9,2,0xFF,0x2A,40,720,1,1 },	/* 360k floppy in DD drive     */
};

struct fd_u {
	int type;		/* Drive type (HD, DD     */
	int active;		/* Drive activity boolean */
	int motor;		/* Motor on flag          */
	struct buf head;	/* Head of buf chain      */
	struct buf rhead;	/* Raw head of buf chain  */
	int reset;
} fd_unit[NFD];


extern int hz;

/* state needed for current transfer */
static fdc;	/* floppy disk controller io base register */
int	fd_dmachan = 2;
static int fd_skip;
static int fd_state;
static int fd_retry;
static int fd_drive;
static int fd_track = -1;
static int fd_status[7];

/*
	make sure bounce buffer for DMA is aligned since the DMA chip
	doesn't roll over properly over a 64k boundary
*/
extern struct buf *dma_bounce[];

/****************************************************************************/
/*                      autoconfiguration stuff                             */
/****************************************************************************/
int fdprobe(), fdattach(), fd_turnoff();

struct	isa_driver fddriver = {
	fdprobe, fdattach, "fd",
};

fdprobe(dev)
struct isa_device *dev;
{
	return 1;
}

fdattach(dev)
struct isa_device *dev;
{	int	s;

	fdc = dev->id_iobase;
	/* Set transfer to 500kbps */
	outb(fdc+fdctl,0);
	fd_turnoff(0);
}

int
fdsize(dev)
dev_t	dev;
{
	return(2400);
}

/****************************************************************************/
/*                               fdstrategy                                 */
/****************************************************************************/
fdstrategy(bp)
	register struct buf *bp;	/* IO operation to perform */
{
	register struct buf *dp,*dp0,*dp1;
	long nblocks,blknum;
 	int	unit, type, s;

 	unit = FDUNIT(minor(bp->b_dev));
 	type = FDTYPE(minor(bp->b_dev));

#ifdef FDTEST
printf("fdstrat%d, blk = %d, bcount = %d, addr = %x|",
	unit, bp->b_blkno, bp->b_bcount,bp->b_un.b_addr);
#endif
	if ((unit >= NFD) || (bp->b_blkno < 0)) {
		printf("fdstrat: unit = %d, blkno = %d, bcount = %d\n",
			unit, bp->b_blkno, bp->b_bcount);
		pg("fd:error in fdstrategy");
		bp->b_error = EINVAL;
		bp->b_flags |= B_ERROR;
		goto bad;
	}
	/*
	 * Set up block calculations.
	 */
	blknum = (unsigned long) bp->b_blkno * DEV_BSIZE/FDBLK;
 	nblocks = fd_types[type].size;
	if (blknum + (bp->b_bcount / FDBLK) > nblocks) {
		if (blknum == nblocks) {
			bp->b_resid = bp->b_bcount;
		} else {
			bp->b_error = ENOSPC;
			bp->b_flags |= B_ERROR;
		}
		goto bad;
	}
 	bp->b_cylin = blknum / (fd_types[type].sectrac * 2);
	dp = &fd_unit[unit].head;
	dp0 = &fd_unit[0].head;
	dp1 = &fd_unit[1].head;
	dp->b_step = (fd_types[fd_unit[unit].type].steptrac);
	s = splbio();
	disksort(dp, bp);
	if ((dp0->b_active == 0)&&(dp1->b_active == 0)) {
#ifdef FDDEBUG
printf("T|");
#endif
		dp->b_active = 1;
		fd_drive = unit;
		fd_track = -1;  /* force seek on first xfer */
		untimeout(fd_turnoff,unit);
		fdstart(unit);		/* start drive if idle */
	}
	splx(s);
	return;

bad:
	biodone(bp);
}

/****************************************************************************/
/*                            motor control stuff                           */
/****************************************************************************/
set_motor(unit,reset)
int unit,reset;
{
	int m0,m1;
	m0 = fd_unit[0].motor;
	m1 = fd_unit[1].motor;
	outb(fdc+fdout,unit | (reset ? 0 : 0xC)  | (m0 ? 16 : 0) | (m1 ? 32 : 0));
}

fd_turnoff(unit)
int unit;
{
	fd_unit[unit].motor = 0;
	if (unit) set_motor(0,0);
	else set_motor(1,0);
}

fd_turnon(unit)
int unit;
{
	fd_unit[unit].motor = 1;
	set_motor(unit,0);
}

/****************************************************************************/
/*                             fdc in/out                                   */
/****************************************************************************/
int
in_fdc()
{
	int i;
	while ((i = inb(fdc+fdsts) & (NE7_DIO|NE7_RQM)) != (NE7_DIO|NE7_RQM))
		if (i == NE7_RQM) return -1;
	return inb(fdc+fddata);
}

dump_stat()
{
	int i;
	for(i=0;i<7;i++) {
		fd_status[i] = in_fdc();
		if (fd_status[i] < 0) break;
	}
printf("FD bad status :%X %X %X %X %X %X %X\n",
	fd_status[0], fd_status[1], fd_status[2], fd_status[3],
	fd_status[4], fd_status[5], fd_status[6] );
}

out_fdc(x)
int x;
{
	int r,errcnt;
	static int maxcnt = 0;
	errcnt = 0;
	do {
		r = (inb(fdc+fdsts) & (NE7_DIO|NE7_RQM));
		if (r== NE7_RQM) break;
		if (r==(NE7_DIO|NE7_RQM)) {
			dump_stat(); /* error: direction. eat up output */
#ifdef FDOTHER
printf("%X\n",x);
#endif
		}
		/* printf("Error r = %d:",r); */
		errcnt++;
	} while (1);
	if (errcnt > maxcnt) {
		maxcnt = errcnt;
#ifdef FDOTHER
printf("New MAX = %d\n",maxcnt);
#endif
	}
	outb(fdc+fddata,x);
}

/* see if fdc responding */
int
check_fdc()
{
	int i;
	for(i=0;i<100;i++) {
		if (inb(fdc+fdsts)& NE7_RQM) return 0;
	}
	return 1;
}

/****************************************************************************/
/*                           fdopen/fdclose                                 */
/****************************************************************************/
fdopen(dev, flags)
	dev_t	dev;
	int	flags;
{
 	int unit = FDUNIT(minor(dev));
 	int type = FDTYPE(minor(dev));
	int s;

	/* check bounds */
	if (unit >= NFD) return(ENXIO);
	if (type >= NUMTYPES) return(ENXIO);
/*
	if (check_fdc()) return(EBUSY);
*/

	/* Set proper disk type, only allow one type */
	return 0;
}

fdclose(dev)
	dev_t dev;
{
}

/****************************************************************************/
/*                            fdread/fdwrite                                */
/****************************************************************************/
/*
 * Routines to do raw IO for a unit.
 */
fdread(dev, uio)			/* character read routine */
dev_t dev;
struct uio *uio;
{
 	int unit = FDUNIT(minor(dev)) ;
	if (unit >= NFD) return(ENXIO);
	return(physio(fdstrategy,&fd_unit[unit].rhead,dev,B_READ,minphys,uio));
}

fdwrite(dev, uio)			/* character write routine */
dev_t dev;
struct uio *uio;
{
 	int unit = FDUNIT(minor(dev)) ;
	if (unit >= NFD) return(ENXIO);
	return(physio(fdstrategy,&fd_unit[unit].rhead,dev,B_WRITE,minphys,uio));
}

/****************************************************************************/
/*                                 fdstart                                  */
/****************************************************************************/
fdstart(unit)
int unit;
{
	register struct buf *dp,*bp;
	int s;

#ifdef FDTEST
printf("st%d|",unit);
#endif 
	s = splbio();
	if (!fd_unit[unit].motor) {
		fd_turnon(unit);
		/* Wait for 1 sec */
		timeout(fdstart,unit,hz);
		/*DELAY(1000000);*/
	}else
		 {
		/* make sure drive is selected as well as on */
		/*set_motor(unit,0);*/

		dp = &fd_unit[unit].head;
		bp = dp->b_actf;
		fd_retry = 0;
		if (fd_unit[unit].reset) fd_state = 1;
		else {
			/* DO a RESET */
			fd_unit[unit].reset = 1;
			fd_state = 5;
		}
		fd_skip = 0;
#ifdef FDDEBUG
printf("Seek %d %d\n", bp->b_cylin, dp->b_step);
#endif
		if (bp->b_cylin != fd_track) {
		/* Seek necessary, never quite sure where head is at! */
		out_fdc(15);	/* Seek function */
		out_fdc(unit);	/* Drive number */
		out_fdc(bp->b_cylin * dp->b_step);
		} else fdintr(0);
	}
	splx(s);
}

fd_timeout(x)
int x;
{
	int i,j;
	struct buf *dp,*bp;

	dp = &fd_unit[fd_drive].head;
	bp = dp->b_actf;

	out_fdc(0x4);
	out_fdc(fd_drive);
	i = in_fdc();
	printf("Timeout drive status %X\n",i);

	out_fdc(0x8);
	i = in_fdc();
	j = in_fdc();
	printf("ST0 = %X, PCN = %X\n",i,j);

	if (bp) badtrans(dp,bp);
}

/****************************************************************************/
/*                                 fdintr                                   */
/****************************************************************************/
fdintr(vec)
int vec;
{
	register struct buf *dp,*bp;
	struct buf *dpother;
	int read,head,trac,sec,i,s,sectrac,cyl;
	unsigned long blknum;
	struct fd_type *ft;

#ifdef FDTEST
	printf("state %d, vec %d, dr %d|",fd_state,vec,fd_drive);
#endif

	dp = &fd_unit[fd_drive].head;
	bp = dp->b_actf;
	read = bp->b_flags & B_READ;
 	ft = &fd_types[FDTYPE(bp->b_dev)];

	switch (fd_state) {
	case 1 : /* SEEK DONE, START DMA */
		/* Make sure seek really happened*/
		if (vec) {
			out_fdc(0x8);
			i = in_fdc();
			cyl = in_fdc();
			if (!(i&0x20) || (cyl != bp->b_cylin*dp->b_step)) {
printf("Stray int ST0 = %X, PCN = %X:",i,cyl);
				return;
			}
		}

		fd_track = bp->b_cylin;
		at_dma(read,bp->b_un.b_addr+fd_skip,FDBLK, fd_dmachan);
		blknum = (unsigned long)bp->b_blkno*DEV_BSIZE/FDBLK
			+ fd_skip/FDBLK;
		sectrac = ft->sectrac;
		sec = blknum %  (sectrac * 2);
		head = sec / sectrac;
		sec = sec % sectrac + 1;

		if (read)  out_fdc(0xE6);	/* READ */
		else out_fdc(0xC5);		/* WRITE */
		out_fdc(head << 2 | fd_drive);	/* head & unit */
		out_fdc(fd_track);		/* track */
		out_fdc(head);
		out_fdc(sec);			/* sector XXX +1? */
		out_fdc(ft->secsize);		/* sector size */
		out_fdc(sectrac);		/* sectors/track */
		out_fdc(ft->gap);		/* gap size */
		out_fdc(ft->datalen);		/* data length */
		fd_state = 2;
		/* XXX PARANOIA */
		untimeout(fd_timeout,2);
		timeout(fd_timeout,2,hz);
		break;
	case 2 : /* IO DONE, post-analyze */
		untimeout(fd_timeout,2);
		for(i=0;i<7;i++) {
			fd_status[i] = in_fdc();
		}
		if (fd_status[0]&0xF8) {
#ifdef FDOTHER
printf("status0 err %d:",fd_status[0]);
#endif
			goto retry;
		}
/*
		if (fd_status[1]){
			printf("status1 err %d:",fd_status[0]);
			goto retry;
		}
		if (fd_status[2]){
			printf("status2 err %d:",fd_status[0]);
			goto retry;
		}
*/
		/* All OK */
		if (!kernel_space(bp->b_un.b_addr+fd_skip)) {
			/* RAW transfer */
			if (read) bcopy(dma_bounce[fd_dmachan]->b_un.b_addr,
				bp->b_un.b_addr+fd_skip, FDBLK);
		}
		fd_skip += FDBLK;
		if (fd_skip >= bp->b_bcount) {
#ifdef FDTEST
printf("DONE %d|", bp->b_blkno);
#endif
			/* ALL DONE */
			fd_skip = 0;
			bp->b_resid = 0;
			dp->b_actf = bp->av_forw;
			biodone(bp);
			nextstate(dp);

		} else {
#ifdef FDDEBUG
printf("next|");
#endif
			/* set up next transfer */
			blknum = (unsigned long)bp->b_blkno*DEV_BSIZE/FDBLK
				+ fd_skip/FDBLK;
			fd_state = 1;
			bp->b_cylin = (blknum / (ft->sectrac * 2));
			if (bp->b_cylin != fd_track) {
#ifdef FDTEST
printf("Seek|");
#endif
				/* SEEK Necessary */
				out_fdc(15);	/* Seek function */
				out_fdc(fd_drive);/* Drive number */
				out_fdc(bp->b_cylin * dp->b_step);
				break;
			} else fdintr(0);
		}
		break;
	case 3:
#ifdef FDOTHER
printf("Seek %d %d\n", bp->b_cylin, dp->b_step);
#endif
		/* Seek necessary */
		out_fdc(15);	/* Seek function */
		out_fdc(fd_drive);/* Drive number */
		out_fdc(bp->b_cylin * dp->b_step);
		fd_state = 1;
		break;
	case 4:
		out_fdc(3); /* specify command */
		out_fdc(0xDF);
		out_fdc(2);
		out_fdc(7);	/* Recalibrate Function */
		out_fdc(fd_drive);
		fd_state = 3;
		break;
	case 5:
#ifdef FDOTHER
		printf("**RESET**\n");
#endif
		/* Try a reset, keep motor on */
		set_motor(fd_drive,1);
		set_motor(fd_drive,0);
		outb(fdc+fdctl,ft->trans);
		fd_retry++;
		fd_state = 4;
		break;
	default:
		printf("Unexpected FD int->");
		out_fdc(0x8);
		i = in_fdc();
		sec = in_fdc();
		printf("ST0 = %X, PCN = %X\n",i,sec);
		out_fdc(0x4A); 
		out_fdc(fd_drive);
		for(i=0;i<7;i++) {
			fd_status[i] = in_fdc();
		}
	printf("intr status :%X %X %X %X %X %X %X ",
		fd_status[0], fd_status[1], fd_status[2], fd_status[3],
		fd_status[4], fd_status[5], fd_status[6] );
		break;
	}
	return;
retry:
	switch(fd_retry) {
	case 0: case 1:
	case 2: case 3:
		break;
	case 4:
		fd_retry++;
		fd_state = 5;
		fdintr(0);
		return;
	case 5: case 6: case 7:
		break;
	default:
		printf("FD err %X %X %X %X %X %X %X\n",
		fd_status[0], fd_status[1], fd_status[2], fd_status[3],
		fd_status[4], fd_status[5], fd_status[6] );
		badtrans(dp,bp);
		return;
	}
	fd_state = 1;
	fd_retry++;
	fdintr(0);
}

badtrans(dp,bp)
struct buf *dp,*bp;
{

	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	bp->b_resid = bp->b_bcount - fd_skip;
	dp->b_actf = bp->av_forw;
	fd_skip = 0;
	biodone(bp);
	nextstate(dp);

}

/*
	nextstate : After a transfer is done, continue processing
	requests on the current drive queue.  If empty, go to
	the other drives queue.  If that is empty too, timeout
	to turn off the current drive in 5 seconds, and go
	to state 0 (not expecting any interrupts).
*/

nextstate(dp)
struct buf *dp;
{
	struct buf *dpother;
	
	dpother = &fd_unit[fd_drive ? 0 : 1].head;
	if (dp->b_actf) fdstart(fd_drive);
	else if (dpother->b_actf) {
#ifdef FDTEST
printf("switch|");
#endif
		untimeout(fd_turnoff,fd_drive);
		timeout(fd_turnoff,fd_drive,5*hz);
		fd_drive = 1 - fd_drive;
		dp->b_active = 0;
		dpother->b_active = 1;
		fdstart(fd_drive);
	} else {
#ifdef FDTEST
printf("off|");
#endif
		untimeout(fd_turnoff,fd_drive);
		timeout(fd_turnoff,fd_drive,5*hz);
		fd_state = 0;
		dp->b_active = 0;
	}
}
#endif
