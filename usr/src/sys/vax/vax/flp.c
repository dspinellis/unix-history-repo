/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)flp.c	7.4 (Berkeley) %G%
 */

#if VAX780
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/conf.h"
#include "sys/user.h"
#include "sys/buf.h"

#include "cons.h"
#include "../include/cpu.h"
#include "flp.h"
#include "../include/mtpr.h"

struct {
	short	fl_state;		/* open and busy flags */
	short	fl_active;		/* driver state flag */
	struct	buf *fl_buf;		/* buffer we're using */
	unsigned char *fl_xaddr;	/* transfer address */
	short	fl_errcnt;
} fltab;

/*ARGSUSED*/
flopen(dev, flag)
	dev_t dev;
	int flag;
{
	struct buf *geteblk();

	if (cpu != VAX_780)
		return (ENXIO);
	if (fltab.fl_state != 0)
		return (ENXIO);
	fltab.fl_state = FL_OPEN;
	fltab.fl_buf = geteblk(512);
	fltab.fl_active = FL_IDLE;
	return (0);
}

/*ARGSUSED*/
flclose(dev, flag)
	dev_t dev;
	int flag;
{

	brelse(fltab.fl_buf);
	fltab.fl_state = 0;
}

/*ARGSUSED*/
flrw(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct buf *bp;
	register int i;
	int error;

	/*
	 * Assume one block read/written for each call - 
	 * and enforce this by checking for block size of 128.
	 * Use the b_blkno field to address
	 * physical, 128-byte blocks (u.u_offset/128).
	 * This is checked for validity, and is further interpreted as:
	 *
	 *	track# * (sectors/track) + sector #
	 */
	if (uio->uio_resid == 0) 
		return (0);
	(void) spl4();
	while (fltab.fl_state & FL_BUSY)
		sleep((caddr_t)&fltab, PRIBIO);
	fltab.fl_state |= FL_BUSY;
	(void) spl0();

	bp = fltab.fl_buf;
	error = 0;
	while ((i = imin(RXBYSEC, uio->uio_resid)) > 0) {
		bp->b_blkno = uio->uio_offset>>7;
		if (bp->b_blkno >= MAXSEC || (uio->uio_offset & 0177) != 0) {
			error = ENXIO;
			break;
		}
		if (uio->uio_rw == UIO_WRITE) {
			error = uiomove(bp->b_un.b_addr, i, uio);
			if (error)
				break;
		}
		bp->b_flags = uio->uio_rw == UIO_WRITE ? B_WRITE : B_READ;
		(void) spl4(); 
		flstart();
		while ((bp->b_flags & B_DONE) == 0)
			sleep((caddr_t)bp, PRIBIO);	
		(void) spl0();
		if (bp->b_flags & B_ERROR) {
			error = EIO;
			break;
		}
		if (uio->uio_rw == UIO_READ) {
			error = uiomove(bp->b_un.b_addr, i, uio);
			if (error)
				break;
		}
	}
	fltab.fl_state &= ~FL_BUSY;
	wakeup((caddr_t)&fltab);
	return (error);
}

flstart()
{
	register struct buf *bp;

	bp = fltab.fl_buf;
	fltab.fl_active = FL_MAND;
	fltab.fl_errcnt = 0;
	fltab.fl_xaddr = (unsigned char *) bp->b_un.b_addr;
	bp->b_resid = 0;
	bp->b_bcount = RXBYSEC; /* always transfer a full sector */

	if ((mfpr(TXCS) & TXCS_RDY) == 0)
		/* not ready to receive order */
		return;
	/*
	 * Wake up floppy LSI software with command
	 */
	fltab.fl_active = FL_SEC;
	if ((bp->b_flags&B_READ) == B_READ)
		mtpr(TXDB, FL_RS);
	else
		mtpr(TXDB, FL_WS);
}

/*
 * See if we want to transmit something
 * to the floppy - and do it
 */
conxfl()
{
	register int databyte;
	register struct buf *bp;

	bp = fltab.fl_buf;
	switch (fltab.fl_active) {

	case FL_MAND:		/* send command */
		if ((bp->b_flags&B_READ) == B_READ)
			mtpr(TXDB,FL_RS);
		else
			mtpr(TXDB,  FL_WS);
		fltab.fl_active = FL_SEC;
		break;

	case FL_SEC:		/* send sector address */
		databyte = (int)bp->b_blkno % RXSTRK + 1;
		mtpr(TXDB, FL_DATA | databyte);
		fltab.fl_active = FL_TRACK;
		break;

	case FL_TRACK:		/* send track address */
		databyte = (int)bp->b_blkno / RXSTRK;
		mtpr(TXDB , FL_DATA | databyte);
		if ((bp->b_flags&B_READ) == B_READ)
			/* prepare to receive complete */
			fltab.fl_active = FL_COM;
		else
			/* prepare to send data */
			fltab.fl_active = FL_DAX;
		break;

	case FL_DAX:
		databyte = *(fltab.fl_xaddr++);
		mtpr(TXDB, FL_DATA | databyte);
		if (--bp->b_bcount == 0)
			fltab.fl_active = FL_COM;
		break;

	case FL_CAN:		/* give cancel order */
		mtpr(TXDB, FL_CANCEL);
		if (++fltab.fl_errcnt <= FLERRS) {
			/* If error count permits, retry order */
			fltab.fl_active = FL_MAND;
			bp->b_bcount = RXBYSEC;
			fltab.fl_xaddr = (unsigned char *) bp->b_un.b_addr;
		} else {
			/*
			 * We're really stupid today - call it an
			 * error and give up
			 */
			bp->b_flags |= B_ERROR | B_DONE;
			bp->b_resid = -RXBYSEC;
			fltab.fl_active = FL_IDLE;
			wakeup((caddr_t)bp);
		}
	}
}

cnrfl(c)
	int c;
{
	register int datum;
	register struct buf *bp;

	datum = c;
	bp = fltab.fl_buf;
	if (datum == FL_PERR) {
		/*
		 * Got a protocol error - cancel the
		 * current function and try again if error count isn't
		 * too great.  First, though, make sure that an actual
		 * transaction is in progress (so a spurious error from
		 * the LSI won't screw us up too much!
		 */
		if (fltab.fl_active != FL_IDLE)
			fltab.fl_active = FL_CAN;
	} else switch(fltab.fl_active ) {

	case FL_DAR:		/* expecting a datum */
		if ((c&RXDB_ID) != FL_DATA)
			goto error;
		*(fltab.fl_xaddr++) = (c & RXDB_DATA);
		if (--bp->b_bcount==0) {
			fltab.fl_active = FL_IDLE;
			bp->b_flags |= B_DONE;
			wakeup((caddr_t)bp);
		}
		break;

	case FL_COM:		/* expecting a "function complete" */
		if ((c&RXDB_ID)!= FL_FFC || (c&FL_ERR) == FL_ERR){
error:
			bp->b_flags |= B_ERROR | B_DONE;
			bp->b_resid = -bp->b_bcount;
			fltab.fl_active = FL_IDLE;
			wakeup((caddr_t)bp);
		} else if ((bp->b_flags&B_READ) == B_READ)
			/* got function complete, now get data */
			fltab.fl_active = FL_DAR;
		else {
			/* got function complete on write - finish up */
			fltab.fl_active = FL_IDLE;
			bp->b_flags |= B_DONE;
				wakeup((caddr_t)bp);
		}
		break;
	}
}
#endif
