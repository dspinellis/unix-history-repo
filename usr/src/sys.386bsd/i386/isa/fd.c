/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Don Ahn.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)fd.c	7.4 (Berkeley) 5/25/91
 */

#include "fd.h"
#if NFD > 0

#include "param.h"
#include "dkbad.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "ioctl.h"
#include "buf.h"
#include "uio.h"
#include "i386/isa/isa_device.h"
#include "i386/isa/fdreg.h"
#include "i386/isa/icu.h"
#include "i386/isa/rtc.h"
#undef NFD
#define NFD 2

#define	FDUNIT(s)	((s>>3)&1)
#define	FDTYPE(s)	((s)&7)

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


struct buf fdtab, fdutab[NFD];	/* controller activity */
extern int hz;

/* state needed for current transfer */
static fdc;	/* floppy disk controller io base register */
int	fd_dmachan;
static int fd_skip;
static int fd_state;
static int fd_retry;
static int fd_drive;
static int fd_hddrv;
static int fd_track = -1;
static int fd_status[7];

/****************************************************************************/
/*                      autoconfiguration stuff                             */
/****************************************************************************/
int fdprobe(), fdattach(), fd_turnoff();

struct	isa_driver fddriver = {
	fdprobe, fdattach, "fd",
};

/*
 * probe for existance of controller
 */
fdprobe(dev)
struct isa_device *dev;
{
	fdc = dev->id_iobase;

	/* see if it can handle a command */
	if (out_fdc(NE7CMD_SPECIFY) < 0) {
		fdc = 0;
		return(0);
	}
	out_fdc(0xDF);
	out_fdc(2);
	return 1;
}

/*
 * wire controller into system, look for floppy units
 */
fdattach(dev)
struct isa_device *dev;
{
	int	i, hdr;
	unsigned fdt,st0, cyl;

	fd_dmachan = dev->id_drq;

	fdt = rtcin(RTC_FDISKETTE);
	hdr = 0;

	/* check for each floppy drive */
	for (i = 0; i < NFD; i++) {
		/* is there a unit? */
		if ((fdt & 0xf0) == RTCFDT_NONE)
			continue;

#ifdef notyet
		/* select it */
		fd_turnon(i);
		DELAY(10000);
		out_fdc(NE7CMD_RECAL);	/* Recalibrate Function */
		out_fdc(i);
		DELAY(10000);

		/* anything responding */
		out_fdc(NE7CMD_SENSEI);
		st0 = in_fdc();
		cyl = in_fdc();
		if (st0 & 0xd0)
			continue;

#endif
		/* yes, announce it */
		if (!hdr)
			printf(" drives ");
		else
			printf(", ");
		printf("%d: ", i);

		if ((fdt & 0xf0) == RTCFDT_12M) {
			printf("1.2M");
			fd_unit[i].type = 1;
		}
		if ((fdt & 0xf0) == RTCFDT_144M) {
			printf("1.44M");
			fd_unit[i].type = 0;
		}

		fdt <<= 4;
		fd_turnoff(i);
		hdr = 1;
	}

	/* Set transfer to 500kbps */
	outb(fdc+fdctl,0);
	fd_turnoff(0);
}

int
fdsize(dev)
dev_t	dev;
{
	return(0);
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
 	/*type = FDTYPE(minor(bp->b_dev));*/
	type = fd_unit[unit].type;

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
	dp->b_step = (fd_types[fd_unit[unit].type].steptrac);
	s = splbio();
	disksort(dp, bp);
	if (dp->b_active == 0) {
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
	outb(fdc+fdout, (unit&FDO_FDSEL)
		| (reset ? 0 : (FDO_FRST|FDO_FDMAEN))
		| (m0 ? FDO_MOEN0 : 0)
		| (m1 ? FDO_MOEN1 : 0));
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
	int i, j = 100000;
	while ((i = inb(fdc+fdsts) & (NE7_DIO|NE7_RQM)) != (NE7_DIO|NE7_RQM) && j-- > 0)
		if (i == NE7_RQM) return -1;
	if (j <= 0)
		return(-1);
	return inb(fdc+fddata);
}

out_fdc(x)
int x;
{
	int i = 100000;

	while ((inb(fdc+fdsts) & NE7_DIO) && i-- > 0);
	while ((inb(fdc+fdsts) & NE7_RQM) == 0 && i-- > 0);
	if (i <= 0) return (-1);
	outb(fdc+fddata,x);
	return (0);
}

static fdopenf;
/****************************************************************************/
/*                           fdopen/fdclose                                 */
/****************************************************************************/
Fdopen(dev, flags)
	dev_t	dev;
	int	flags;
{
 	int unit = FDUNIT(minor(dev));
 	/*int type = FDTYPE(minor(dev));*/
	int s;

	fdopenf = 1;
	/* check bounds */
	if (unit >= NFD) return(ENXIO);
	/*if (type >= NUMTYPES) return(ENXIO);*/

	/* Set proper disk type, only allow one type */
	return 0;
}

fdclose(dev, flags)
	dev_t dev;
{
	return(0);
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
	dp = &fd_unit[unit].head;
	bp = dp->b_actf;
	s = splbio();
	if (!fd_unit[unit].motor) {
		fd_turnon(unit);
#ifdef notdef
		if ((bp->b_flags & B_READ) == 0) {
			/* Wait for 1 sec */
#endif
			timeout(fdstart,unit,hz);
		/*}*/
	} else
		 {
		/* make sure drive is selected as well as on */

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
		out_fdc(NE7CMD_SEEK);	/* Seek function */
		out_fdc(unit);	/* Drive number */
		out_fdc(bp->b_cylin * dp->b_step);
		fd_state = 6;
		} else {
		fd_state = 1;
		fdintr(0xff);
		}
	}
	splx(s);
}

fd_timeout(x)
int x;
{
	int st0, st3, cyl;
	struct buf *dp,*bp;

	dp = &fd_unit[fd_drive].head;
	bp = dp->b_actf;

	out_fdc(NE7CMD_SENSED);
	out_fdc(fd_hddrv);
	st3 = in_fdc();

	out_fdc(NE7CMD_SENSEI);
	st0 = in_fdc();
	cyl = in_fdc();
printf("fd%d: Operation timeout ST0 %b cyl %d ST3 %b\n", fd_drive,
st0, NE7_ST0BITS, cyl, st3, NE7_ST3BITS);

	if (bp) {
		fd_state = 4;
		fdintr(fd_drive);
	}
}

/****************************************************************************/
/*                                 fdintr                                   */
/****************************************************************************/
fdintr(unit)
{
	register struct buf *dp,*bp;
	struct buf *dpother;
	int read,head,trac,sec,i,s,sectrac,cyl,st0;
	unsigned long blknum;
	struct fd_type *ft;

#ifdef FDTEST
	printf("state %d, unit %d, dr %d|",fd_state,unit,fd_drive);
#endif

	if (!fdopenf) return;
	dp = &fd_unit[fd_drive].head;
	bp = dp->b_actf;
	read = bp->b_flags & B_READ;
 	/*ft = &fd_types[FDTYPE(bp->b_dev)];*/
 	ft = &fd_types[fd_unit[fd_drive].type];

	switch (fd_state) {
	case 1 : /* SEEK DONE, START DMA */
		/* Make sure seek really happened*/
		if (unit != 0xff) {
			int descyl = bp->b_cylin * dp->b_step;
			out_fdc(NE7CMD_SENSEI);
			i = in_fdc();
			cyl = in_fdc();
			if (cyl != descyl) {
printf("fd%d: Seek to cyl %d failed; am at cyl %d (ST0 = %b)\n", fd_drive,
descyl, cyl, i, NE7_ST0BITS);
fd_state = 4;
				return;
			}
		}

		fd_track = bp->b_cylin;
		isa_dmastart(bp->b_flags, bp->b_un.b_addr+fd_skip,
			FDBLK, fd_dmachan);
		blknum = (unsigned long)bp->b_blkno*DEV_BSIZE/FDBLK
			+ fd_skip/FDBLK;
		sectrac = ft->sectrac;
		sec = blknum %  (sectrac * 2);
		head = sec / sectrac;
		sec = sec % sectrac + 1;
fd_hddrv = ((head&1)<<2)+fd_drive;

		if (read)  out_fdc(NE7CMD_READ);	/* READ */
		else out_fdc(NE7CMD_WRITE);		/* WRITE */
		out_fdc(head << 2 | fd_drive);	/* head & unit */
		out_fdc(fd_track);		/* track */
		out_fdc(head);
		out_fdc(sec);			/* sector XXX +1? */
		out_fdc(ft->secsize);		/* sector size */
		out_fdc(sectrac);		/* sectors/track */
		out_fdc(ft->gap);		/* gap size */
		out_fdc(ft->datalen);		/* data length */
		fd_state = 2;
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
		/* All OK */
		isa_dmadone(bp->b_flags, bp->b_un.b_addr+fd_skip,
			FDBLK, fd_dmachan);
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
				out_fdc(NE7CMD_SEEK);	/* Seek function */
				out_fdc(fd_drive);/* Drive number */
				out_fdc(bp->b_cylin * dp->b_step);
			fd_state = 6;
				break;
			} else fdintr(0xff);
		}
		break;
	case 3:
		out_fdc(NE7CMD_SENSEI);
		st0 = in_fdc();
		cyl = in_fdc();
		if (cyl != 0)
			printf("fd%d: recal failed ST0 %b cyl %d\n", fd_drive,
				st0, NE7_ST0BITS, cyl);

		/* Seek necessary */
		out_fdc(NE7CMD_SEEK);	/* Seek function */
		out_fdc(fd_drive);/* Drive number */
		out_fdc(bp->b_cylin * dp->b_step);
		fd_state = 6;
		break;
	case 4:
		out_fdc(NE7CMD_SPECIFY); /* specify command */
		out_fdc(0xDF);
		out_fdc(2);
		out_fdc(NE7CMD_RECAL);	/* Recalibrate Function */
		out_fdc(fd_drive);
		fd_state = 7;
		break;
	case 5:
#ifdef FDOTHER
		printf("**RESET**\n");
#endif
		/* Try a reset, keep motor on */
		set_motor(fd_drive,1);
		DELAY(100);
		set_motor(fd_drive,0);
		outb(fdc+fdctl,ft->trans);
		fd_retry++;
		fd_state = 4;
		break;
	case 6:
		/* allow heads to settle */
		timeout(fdintr,fd_drive,hz/30);
		fd_state = 1;
		return;
		break;
		
	case 7:
		/* allow heads to settle */
		timeout(fdintr,fd_drive,hz/3);
		fd_state = 3;
		return;
		break;
		
	default:
		printf("Unexpected FD int->");
		out_fdc(NE7CMD_SENSEI);
		st0 = in_fdc();
		cyl = in_fdc();
		printf("ST0 = %lx, PCN = %lx\n",i,sec);
		out_fdc(0x4A); 
		out_fdc(fd_drive);
		for(i=0;i<7;i++) {
			fd_status[i] = in_fdc();
		}
	printf("intr status :%lx %lx %lx %lx %lx %lx %lx ",
		fd_status[0], fd_status[1], fd_status[2], fd_status[3],
		fd_status[4], fd_status[5], fd_status[6] );
		break;
	}
	return;
retry:
	switch(fd_retry) {
	case 0: case 1:
	case 2:
		break;
	case 3:
	case 4:
	case 5:
		fd_retry++;
		fd_state = 4;
		fdintr(0xff);
		return;
	case 6:
		fd_retry++;
		fd_state = 5;
		fdintr(0xff);
		return;
	case 7:
		break;
	default:
/*printf("fd%d: hard error (ST0 %b ST1 %b ST2 %b ST3 %b cyl %d hd %d sec %d)\n",
		fd_drive, fd_status[0], NE7_ST0BITS, fd_status[1], NE7_ST1BITS,
		fd_status[2], NE7_ST2BITS,  fd_status[3], NE7_ST3BITS, 
		fd_status[4], fd_status[5], fd_status[6]);*/
printf("fd%d: hard error (ST0 %b ", fd_drive, fd_status[0], NE7_ST0BITS);
printf(" ST1 %b ", fd_status[1], NE7_ST1BITS);
printf(" ST2 %b ", fd_status[2], NE7_ST2BITS);
printf(" ST3 %b ", fd_status[3], NE7_ST3BITS);
printf("cyl %d hd %d sec %d)\n", fd_status[4], fd_status[5], fd_status[6]);
		badtrans(dp,bp);
		return;
	}
	fd_state = 1;
	fd_retry++;
	fdintr(0xff);
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
	
	if (dp->b_actf) fdstart(fd_drive);
	else {
		untimeout(fd_turnoff,fd_drive);
		timeout(fd_turnoff,fd_drive,hz);
		fd_state = 0;
		dp->b_active = 0;
	}
}
#endif
