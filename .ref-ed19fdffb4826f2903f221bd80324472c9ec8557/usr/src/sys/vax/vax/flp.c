/*	flp.c	4.5	81/03/09	*/

#if VAX780
#include "../h/flp.h"
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mtpr.h"
#include "../h/buf.h"
#include "../h/cons.h"
#include "../h/cpu.h"

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

#if VAX750
	if (cpu != VAX_780) {
		u.u_error = ENXIO;
		return;
	}
#endif
	if (fltab.fl_state != 0) {
		u.u_error = ENXIO;
		return;
	}
	fltab.fl_state = FL_OPEN;
	fltab.fl_buf = geteblk();
	fltab.fl_active = FL_IDLE;
}

/*ARGSUSED*/
flclose(dev, flag)
	dev_t dev;
	int flag;
{

	brelse(fltab.fl_buf);
	fltab.fl_state = 0;
}

flstrategy(rw)
	int rw;
{
	register struct buf *bp;
	register unsigned i;

	/*
	 * Assume one block read/written for each call - 
	 * and enforce this by checking for block size of 128.
	 * Use the b_blkno field to address
	 * physical, 128-byte blocks (u.u_offset/128).
	 * This is checked for validity, and is further interpreted as:
	 *
	 *	track# * (sectors/track) + sector #
	 */
	if (u.u_count == 0) 
		return;
	(void) spl4();
	while (fltab.fl_state & FL_BUSY)
		sleep((caddr_t)&fltab, PRIBIO);
	fltab.fl_state |= FL_BUSY;
	(void) spl0();

	bp = fltab.fl_buf;
	while ((i = min(RXBYSEC, u.u_count)) != 0) {
		bp->b_blkno = u.u_offset>>7;
		if (bp->b_blkno >= MAXSEC || (u.u_offset & 0177) != 0) {
			/* block number out of range */
			/* or offset in middle of block */
			u.u_error = ENXIO;
			break;	
		}
		if (rw == B_WRITE) {
			iomove(bp->b_un.b_addr, i, B_WRITE);
			if (u.u_error != 0)
				break;
		}
		bp->b_flags = rw;
		(void) spl4(); 
		flstart();
		while ((bp->b_flags & B_DONE) == 0)
			sleep((caddr_t)bp, PRIBIO);	
		(void) spl0();
		if (bp->b_flags & B_ERROR) {
			u.u_error = EIO;
			break;
		}
		if (rw == B_READ) {
			iomove(bp->b_un.b_addr, i, B_READ);
			if (u.u_error != 0)
				break;
		}
	}
	u.u_count = bp->b_resid;
	fltab.fl_state &= ~FL_BUSY;
	wakeup((caddr_t)&fltab);
}

/*ARGSUSED*/
flread(dev)
	dev_t dev;
{

	flstrategy(B_READ);
}

/*ARGSUSED*/
flwrite(dev)
	dev_t dev;
{

	flstrategy(B_WRITE);
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
