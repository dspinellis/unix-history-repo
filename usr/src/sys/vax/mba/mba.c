/*	mba.c	4.25	82/03/31	*/

#include "mba.h"
#if NMBA > 0
/*
 * Massbus driver, arbitrates a massbus among attached devices.
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dk.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/mbareg.h"
#include "../h/mbavar.h"
#include "../h/mtpr.h"
#include "../h/vm.h"

char	mbsr_bits[] = MBSR_BITS;
/*
 * Start activity on a massbus device.
 * We are given the device's mba_device structure and activate
 * the device via the unit start routine.  The unit start
 * routine may indicate that it is finished (e.g. if the operation
 * was a ``sense'' on a tape drive), that the (multi-ported) unit
 * is busy (we will get an interrupt later), that it started the
 * unit (e.g. for a non-data transfer operation), or that it has
 * set up a data transfer operation and we should start the massbus adaptor.
 */
mbustart(mi)
	register struct mba_device *mi;
{
	register struct buf *bp;	/* i/o operation at head of queue */
	register struct mba_hd *mhp;	/* header for mba device is on */

loop:
	/*
	 * Get the first thing to do off device queue.
	 */
	bp = mi->mi_tab.b_actf;
	if (bp == NULL)
		return;
	/*
	 * Let the drivers unit start routine have at it
	 * and then process the request further, per its instructions.
	 */
	switch ((*mi->mi_driver->md_ustart)(mi)) {

	case MBU_NEXT:		/* request is complete (e.g. ``sense'') */
		mi->mi_tab.b_active = 0;
		mi->mi_tab.b_errcnt = 0;
		mi->mi_tab.b_actf = bp->av_forw;
		iodone(bp);
		goto loop;

	case MBU_DODATA:	/* all ready to do data transfer */
		/*
		 * Queue the device mba_device structure on the massbus
		 * mba_hd structure for processing as soon as the
		 * data path is available.
		 */
		mhp = mi->mi_hd;
		mi->mi_forw = NULL;
		if (mhp->mh_actf == NULL)
			mhp->mh_actf = mi;
		else
			mhp->mh_actl->mi_forw = mi;
		mhp->mh_actl = mi;
		/*
		 * If data path is idle, start transfer now.
		 * In any case the device is ``active'' waiting for the
		 * data to transfer.
		 */
		mi->mi_tab.b_active = 1;
		if (mhp->mh_active == 0)
			mbstart(mhp);
		return;

	case MBU_STARTED:	/* driver started a non-data transfer */
		/*
		 * Mark device busy during non-data transfer
		 * and count this as a ``seek'' on the device.
		 */
		if (mi->mi_dk >= 0) {
			dk_seek[mi->mi_dk]++;
			dk_busy |= (1 << mi->mi_dk);
		}
		mi->mi_tab.b_active = 1;
		return;

	case MBU_BUSY:		/* dual port drive busy */
		/*
		 * We mark the device structure so that when an
		 * interrupt occurs we will know to restart the unit.
		 */
		mi->mi_tab.b_flags |= B_BUSY;
		return;

	default:
		panic("mbustart");
	}
}

/*
 * Start an i/o operation on the massbus specified by the argument.
 * We peel the first operation off its queue and insure that the drive
 * is present and on-line.  We then use the drivers start routine
 * (if any) to prepare the drive, setup the massbus map for the transfer
 * and start the transfer.
 */
mbstart(mhp)
	register struct mba_hd *mhp;
{
	register struct mba_device *mi;
	struct buf *bp;
	register struct mba_regs *mbp;
	register int com;

loop:
	/*
	 * Look for an operation at the front of the queue.
	 */
	if ((mi = mhp->mh_actf) == NULL) {
		return;
	}
	if ((bp = mi->mi_tab.b_actf) == NULL) {
		mhp->mh_actf = mi->mi_forw;
		goto loop;
	}
	/*
	 * If this device isn't present and on-line, then
	 * we screwed up, and can't really do the operation.
	 * Only check for non-tapes because tape drivers check
	 * ONLINE themselves and because TU78 registers are
	 * different.
	 */
	if ((mi->mi_drv->mbd_dt & MBDT_TAP) == 0)
	if ((mi->mi_drv->mbd_ds & MBDS_DREADY) != MBDS_DREADY) {
		printf("%s%d: not ready\n", mi->mi_driver->md_dname,
		    dkunit(bp));
		mi->mi_tab.b_actf = bp->av_forw;
		mi->mi_tab.b_errcnt = 0;
		mi->mi_tab.b_active = 0;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		goto loop;
	}
	/*
	 * We can do the operation; mark the massbus active
	 * and let the device start routine setup any necessary
	 * device state for the transfer (e.g. desired cylinder, etc
	 * on disks).
	 */
	mhp->mh_active = 1;
	if (mi->mi_driver->md_start) {
		if ((com = (*mi->mi_driver->md_start)(mi)) == 0)
			com = (bp->b_flags & B_READ) ?
			    MB_RCOM|MB_GO : MB_WCOM|MB_GO;
	} else
		com = (bp->b_flags & B_READ) ? MB_RCOM|MB_GO : MB_WCOM|MB_GO;

	/*
	 * Setup the massbus control and map registers and start
	 * the transfer.
	 */
	mbp = mi->mi_mba;
	mbp->mba_sr = -1;	/* conservative */
	mbp->mba_var = mbasetup(mi);
	mbp->mba_bcr = -bp->b_bcount;
	mi->mi_drv->mbd_cs1 = com;
	if (mi->mi_dk >= 0) {
		dk_busy |= 1 << mi->mi_dk;
		dk_xfer[mi->mi_dk]++;
		dk_wds[mi->mi_dk] += bp->b_bcount >> 6;
	}
}

/*
 * Take an interrupt off of massbus mbanum,
 * and dispatch to drivers as appropriate.
 */
mbintr(mbanum)
	int mbanum;
{
	register struct mba_hd *mhp = &mba_hd[mbanum];
	register struct mba_regs *mbp = mhp->mh_mba;
	register struct mba_device *mi;
	register struct buf *bp;
	register int drive;
	int mbasr, as;
	
	/*
	 * Read out the massbus status register
	 * and attention status register and clear
	 * the bits in same by writing them back.
	 */
	mbasr = mbp->mba_sr;
	mbp->mba_sr = mbasr;
#if VAX750
	if (mbasr&MBSR_CBHUNG) {
		printf("mba%d: control bus hung\n", mbanum);
		panic("cbhung");
	}
#endif
	/* note: the mbd_as register is shared between drives */
	as = mbp->mba_drv[0].mbd_as & 0xff;
	mbp->mba_drv[0].mbd_as = as;

	/*
	 * If the mba was active, process the data transfer
	 * complete interrupt; otherwise just process units which
	 * are now finished.
	 */
	if (mhp->mh_active) {
		/*
		 * Clear attention status for drive whose data
		 * transfer related operation completed,
		 * and give the dtint driver
		 * routine a chance to say what is next.
		 */
		mi = mhp->mh_actf;
		as &= ~(1 << mi->mi_drive);
		dk_busy &= ~(1 << mi->mi_dk);
		bp = mi->mi_tab.b_actf;
		switch ((*mi->mi_driver->md_dtint)(mi, mbasr)) {

		case MBD_DONE:		/* all done, for better or worse */
			/*
			 * Flush request from drive queue.
			 */
			mi->mi_tab.b_errcnt = 0;
			mi->mi_tab.b_actf = bp->av_forw;
			iodone(bp);
			/* fall into... */
		case MBD_RETRY:		/* attempt the operation again */
			/*
			 * Dequeue data transfer from massbus queue;
			 * if there is still a i/o request on the device
			 * queue then start the next operation on the device.
			 * (Common code for DONE and RETRY).
			 */
			mhp->mh_active = 0;
			mi->mi_tab.b_active = 0;
			mhp->mh_actf = mi->mi_forw;
			if (mi->mi_tab.b_actf)
				mbustart(mi);
			break;

		case MBD_RESTARTED:	/* driver restarted op (ecc, e.g.)
			/*
			 * Note that mhp->mh_active is still on.
			 */
			break;

		default:
			panic("mbintr");
		}
	}
	/*
	 * Service drives which require attention
	 * after non-data-transfer operations.
	 */
	while (drive = ffs(as)) {
		drive--;		/* was 1 origin */
		as &= ~(1 << drive);
		mi = mhp->mh_mbip[drive];
		if (mi == NULL)
			continue;
		/*
		 * If driver has a handler for non-data transfer
		 * interrupts, give it a chance to tell us what to do.
		 */
		if (mi->mi_driver->md_ndint) {
			mi->mi_tab.b_active = 0;
			switch ((*mi->mi_driver->md_ndint)(mi)) {

			case MBN_DONE:		/* operation completed */
				mi->mi_tab.b_errcnt = 0;
				bp = mi->mi_tab.b_actf;
				mi->mi_tab.b_actf = bp->av_forw;
				iodone(bp);
				/* fall into common code */
			case MBN_RETRY:		/* operation continues */
				if (mi->mi_tab.b_actf)
					mbustart(mi);
				break;
			case MBN_SKIP:		/* ignore unsol. interrupt */
				break;
			default:
				panic("mbintr");
			}
		} else
			/*
			 * If there is no non-data transfer interrupt
			 * routine, then we should just
			 * restart the unit, leading to a mbstart() soon.
			 */
			mbustart(mi);
	}
	/*
	 * If there is an operation available and
	 * the massbus isn't active, get it going.
	 */
	if (mhp->mh_actf && !mhp->mh_active)
		mbstart(mhp);
	/* THHHHATS all folks... */
}

/*
 * Setup the mapping registers for a transfer.
 */
mbasetup(mi)
	register struct mba_device *mi;
{
	register struct mba_regs *mbap = mi->mi_mba;
	struct buf *bp = mi->mi_tab.b_actf;
	register int npf;
	unsigned v;
	register struct pte *pte, *io;
	int o;
	struct proc *rp;

	v = btop(bp->b_un.b_addr);
	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = btoc(bp->b_bcount + o);
	rp = bp->b_flags&B_DIRTY ? &proc[2] : bp->b_proc;
	if ((bp->b_flags & B_PHYS) == 0)
		pte = &Sysmap[btop(((int)bp->b_un.b_addr)&0x7fffffff)];
	else if (bp->b_flags & B_UAREA)
		pte = &rp->p_addr[v];
	else if (bp->b_flags & B_PAGET)
		pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
	else
		pte = vtopte(rp, v);
	io = mbap->mba_map;
	while (--npf >= 0) {
		if (pte->pg_pfnum == 0)
			panic("mba, zero entry");
		*(int *)io++ = pte++->pg_pfnum | PG_V;
	}
	*(int *)io++ = 0;
	return (o);
}

#if notdef
/*
 * Init and interrupt enable a massbus adapter.
 */
mbainit(mp)
	struct mba_regs *mp;
{

	mp->mba_cr = MBCR_INIT;
	mp->mba_cr = MBCR_IE;
}
#endif
#endif
