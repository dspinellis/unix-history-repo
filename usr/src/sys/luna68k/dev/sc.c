/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sc.c	7.3 (Berkeley) %G%
 */

/*
 * sc.c -- FUJITSU MB89352 SCSI Protocole Controller (SPC) Device Driver
 *
 * remaked by A.Fujita,		Mar-22-1992
 * remaked again by A.Fujita,	Apr-16-1992
 */

#define DEBUG_FUNC

#include "sc.h"
#if NSC > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>

#include <luna68k/dev/device.h>
#include <luna68k/dev/scsireg.h>
#include <luna68k/dev/scsivar.h>

/*
 * SC Driver Options
 */

#define	QUADBYTES	/* 4 bytes access to SPC DREG Reg. */
#define	NODISCONNECT	/* not used SCSI DISCONNECT Ops. */
#undef	XFER_ENABLE	/* using interrupt for DREG access */


#define SCSI_IPL	2
#define SCSI_ID		7

extern char *hexstr();

int	scinit(), scstart(), scintr();
void	screset();
struct	driver scdriver = {
	scinit, "sc", scstart, (int (*)()) 0, scintr, (int (*)()) 0
};

struct	scsi_softc scsi_softc[NSC];


#define	SC_TIMEOUT	0x01400000	/* (20971520) */


/*
 * for DEBUG
 */

char *
scsi_status(stat)
	u_char stat;
{
	if ((stat & 0x1e) == 0)
		return("Good");
	else if ((stat & 0x1e) == STS_CHECKCOND)
		return("Check Condition");
	else if ((stat & 0x1e) == STS_CONDMET)
		return("Condition Met");
	else if ((stat & 0x1e) == STS_BUSY)
		return("Busy");
	else if ((stat & 0x1e) == STS_INTERMED)
		return("Intermediate status sent");
	else if ((stat & 0x1e) == STS_EXT)
		return("Extended status valid");
	else
		return("Unknown Status");
}

#ifdef DEBUG_FUNC

char *
scsi_command(cmd)
	u_char cmd;
{
	if (cmd == CMD_TEST_UNIT_READY)
		return("TEST_UNIT_READY");
	else if (cmd == CMD_REQUEST_SENSE)
		return("REQUEST_SENSE");
	else if (cmd == CMD_INQUIRY)
		return("INQUIRY");
	else if (cmd == CMD_READ)
		return("READ");
	else if (cmd == CMD_WRITE)
		return("WRITE");
	else if (cmd == CMD_READ_EXT)
		return("READ EXT");
	else if (cmd == CMD_WRITE_EXT)
		return("WRITE_EXT");
	else if (cmd == CMD_READ_CAPACITY)
		return("READ_CAPACITY");
	else
		return(hexstr(cmd, 2));
}

char *
scsi_mesg(mesg)
	u_char mesg;
{
	if (mesg == MSG_CMD_COMPLETE)
		return("Command Complete");
	else if (mesg == MSG_EXT_MESSAGE)
		return("Extended Message");
	else if (mesg == MSG_SAVE_DATA_PTR)
		return("Save Data Pointer");
	else if (mesg == MSG_RESTORE_PTR)
		return("Restore Pointer");
	else if (mesg == MSG_DISCONNECT)
		return("Disconnect");
	else if (mesg == MSG_INIT_DETECT_ERROR)
		return("Initiator Detected Error");
	else if (mesg == MSG_ABORT)
		return("Abort");
	else if (mesg == MSG_REJECT)
		return("Message Reject");
	else if (mesg == MSG_NOOP)
		return("No Operation");
	else if (mesg == MSG_PARITY_ERROR)
		return("Message Parity Error");
	else if (mesg == MSG_BUS_DEVICE_RESET)
		return("Bus Device Reset");
	else if (mesg == MSG_IDENTIFY)
		return("Identify");
	else if (mesg == MSG_IDENTIFY_DR)
		return("Identify (Disconnect)");
	else
		return("Unknown Message");
}

char *
phase_name(phase)
	u_char phase;
{
	if (phase == DATA_OUT_PHASE)
		return("Data Out");
	else if (phase == DATA_IN_PHASE)
		return("Data In");
	else if (phase == CMD_PHASE)
		return("Command");
	else if (phase == STATUS_PHASE)
		return("Status");
	else if (phase == BUS_FREE_PHASE)
		return("Bus Free");
	else if (phase == ARB_SEL_PHASE)
		return("Arbitration/Select");
	else if (phase == MESG_OUT_PHASE)
		return("Message Out");
	else if (phase == MESG_IN_PHASE)
		return("Message In");
	else
		return("Unknown");
}
#endif

/*
 * Initialize SPC & Data Structure
 */

int
scinit(hc)
	register struct hp_ctlr *hc;
{
	register struct scsi_softc *hs = &scsi_softc[hc->hp_unit];
	register int i;

	hc->hp_ipl    = SCSI_IPL;
	hs->sc_hc     = hc;
	hs->sc_sq.dq_forw = hs->sc_sq.dq_back = &hs->sc_sq;
	hs->sc_wq.dq_forw = hs->sc_wq.dq_back = &hs->sc_wq;

	hs->sc_flags  = 0;
	hs->sc_phase  = BUS_FREE_PHASE;

	hs->sc_stat   = 0;
	hs->sc_msg[0] = 0;

	scsi_init_buf();

	screset(hc->hp_unit);

	return(1);
}

void
screset(unit)
	register int unit;
{
	register struct scsi_softc *hs = &scsi_softc[unit];
	volatile register struct scsidevice *hd =
				(struct scsidevice *)hs->sc_hc->hp_addr;

	printf("sc%d: ", unit);

	/*
	 * Disable interrupts then reset the FUJI chip.
	 */

	hd->scsi_sctl = SCTL_DISABLE | SCTL_CTRLRST;
	hd->scsi_scmd = 0;
	hd->scsi_pctl = 0;
	hd->scsi_temp = 0;
	hd->scsi_tch  = 0;
	hd->scsi_tcm  = 0;
	hd->scsi_tcl  = 0;
	hd->scsi_ints = 0;

	/* We can use Asynchronous Transfer only */
	printf("async");

	/*
	 * Configure MB89352 with its SCSI address, all
	 * interrupts enabled & appropriate parity.
	 */
	hd->scsi_bdid = SCSI_ID;
	hd->scsi_sctl = SCTL_DISABLE | SCTL_ABRT_ENAB|
			SCTL_PARITY_ENAB | SCTL_RESEL_ENAB |
			SCTL_INTR_ENAB;
	printf(", parity");

	DELAY(400);
	hd->scsi_sctl &= ~SCTL_DISABLE;

	printf(", scsi id %d\n", SCSI_ID);
}


/*
 * SPC Arbitration/Selection routine
 */

int
issue_select(hd, target, flags)
	volatile register struct scsidevice *hd;
	u_char target;
	int flags;
{
#ifndef NODISCONNECT
	if (flags & DQ_DISCONNECT) {
		hd->scsi_scmd = SCMD_SET_ATN;
	}
#endif

	hd->scsi_pctl = 0;
	hd->scsi_temp = (1 << SCSI_ID) | (1 << target);

	/* select timeout is hardcoded to 2ms */
	hd->scsi_tch = 0;
	hd->scsi_tcm = 32;
	hd->scsi_tcl = 4;

	hd->scsi_scmd = SCMD_SELECT;

	return (1);
}


/*
 * SPC Manual Transfer routines
 */

/* not yet */


/*
 * SPC Program Transfer routines
 */

int
ixfer_start(hd, len, phase)
	volatile register struct scsidevice *hd;
	register int len;
	register u_char phase;
{
	register int wait = 0;

	hd->scsi_sdgc = 0;

	hd->scsi_tch  = ((len & 0xff0000) >> 16);
	hd->scsi_tcm  = ((len & 0x00ff00) >>  8);
	hd->scsi_tcl  =  (len & 0x0000ff);
	hd->scsi_pctl = phase;
	hd->scsi_scmd = SCMD_XFR | SCMD_PROG_XFR;

	while ((hd->scsi_ssts & SSTS_BUSY) == 0) {
		if (wait > SC_TIMEOUT) {
			panic("ixfer_start: too long wait");
		}
		wait++;
		DELAY(1);
	}
}

int
ixfer_out(hd, len, buf)
	volatile register struct scsidevice *hd;
	register int len;
	register u_char *buf;
{
	u_char *t = buf;
	register int wait = 0;
#ifdef QUADBYTES
	register int qwait = 0;
	register int l_len = len >> 3;
	register u_long * l_buf = (u_long *) buf;

	for(; l_len > 0; l_len--) {
		while ((hd->scsi_ssts & SSTS_DREG_EMPTY) == 0) {
			if (qwait > SC_TIMEOUT) {
				printf("ixfer_out: quad time out\n");
				printf("ixfer_out: %d bytes sended\n",
				       (((u_char *) l_buf) - t));
				printf("ixfer_out: TC = %d\n",
				       ( hd->scsi_tch << 16 ) |
				       ( hd->scsi_tcm <<  8 ) |
				       ( hd->scsi_tcl ));
				return(-1);
			}
			qwait++;
			DELAY(1);
		}
		*((u_long *) &hd->scsi_dreg) = *l_buf++;
		*((u_long *) &hd->scsi_dreg) = *l_buf++;
	}

	len &= 0x07;
	buf = (u_char *) l_buf;
#endif
	for(; len > 0; len--) {
		while (hd->scsi_ssts & SSTS_DREG_FULL) {
			if (wait > SC_TIMEOUT) {
				printf("ixfer_out: time out\n");
				printf("ixfer_out: %d bytes sended\n",
				       (buf - t));
				return(-1);
			}
			wait++;
			DELAY(1);
		}
		hd->scsi_dreg = *buf++;
	}

#ifdef QUADBYTES
	return(qwait);
#else
	return(wait);
#endif
}

int
ixfer_in(hd, len, buf)
	volatile register struct scsidevice *hd;
	register int len;
	register u_char *buf;
{
	u_char *t = buf;
	register int wait = 0;
#ifdef QUADBYTES
	register int qwait = 0;
	register int l_len = len >> 3;
	register u_long * l_buf = (u_long *) buf;

	for(; l_len > 0; l_len--) {
		while ((hd->scsi_ssts & SSTS_DREG_FULL) == 0) {
			if (qwait > SC_TIMEOUT) {
				printf("ixfer_in: quad time out\n");
				printf("ixfer_in: %d bytes recieved\n",
				       (((u_char *) l_buf) - t));
				return(-1);
			}
			qwait++;
			DELAY(1);
		}
		*l_buf++ = *((u_long *) &hd->scsi_dreg);
		*l_buf++ = *((u_long *) &hd->scsi_dreg);
	}

	len &= 0x07;
	buf = (u_char *) l_buf;
#endif
	for (; len > 0; len--) {
		while (hd->scsi_ssts & SSTS_DREG_EMPTY) {
			if (wait > SC_TIMEOUT) {
				printf("ixfer_in: time out\n");
				printf("ixfer_in: %d bytes recieved\n",
				       (buf - t));
				return(-1);
			}
			wait++;
			DELAY(1);
		}
		*buf++ = hd->scsi_dreg;
	}


#ifdef QUADBYTES
	return(qwait);
#else
	return(wait);
#endif
}


#ifdef XFER_ENABLE
/*
 * SPC Interrupt base Transfer Routines
 */

int
txfer_start(hd, len, phase)
	volatile register struct scsidevice *hd;
	register int len;
	register u_char phase;
{
	register int wait = 0;

	hd->scsi_sdgc = SDGC_XFER_ENAB;		/* for interrupt */

	hd->scsi_tch  = ((len & 0xff0000) >> 16);
	hd->scsi_tcm  = ((len & 0x00ff00) >>  8);
	hd->scsi_tcl  =  (len & 0x0000ff);
	hd->scsi_pctl = phase;
	hd->scsi_scmd = SCMD_XFR | SCMD_PROG_XFR;

	while ((hd->scsi_ssts & SSTS_BUSY) == 0) {
		if (wait > SC_TIMEOUT) {
			panic("ixfer_start: too long wait");
		}
		wait++;
		DELAY(1);
	}
}

int
txfer_in(ctlr)
	register int ctlr;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd = (struct scsidevice *) hs->sc_hc->hp_addr;
	register struct scsi_queue *dq = hs->sc_sq.dq_forw;
#ifdef QUADBYTES
	register u_long *lp;

	if (hd->scsi_ssts & SSTS_DREG_FULL) {
		lp = (u_long *) dq->dq_xferp;

		*lp++ = *((u_long *) &hd->scsi_dreg);
		*lp++ = *((u_long *) &hd->scsi_dreg);

		dq->dq_xferp = (u_char *) lp;
		dq->dq_xfercnt -= 8;

		goto xfer_done;
	}
#endif

	*dq->dq_xferp++ = hd->scsi_dreg;
	dq->dq_xfercnt--;

 xfer_done:
#ifdef DEBUGPRINT
	if (dq->dq_xfercnt == 0) {
		dbgprintf("txfer_in: ");
		dbgprintf("dq->dq_bp->b_un.b_addr = 0x%s, ", hexstr(dq->dq_bp->b_un.b_addr, 8));
		dbgprintf("dq->dq_xferp = 0x%s :", hexstr(dq->dq_xferp, 8));
		dbgprintf("done\n");
	}
#endif
}
#endif

/*
 * SCSI Job Handler
 */

int
scstart(ctlr)
	int ctlr;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd =
		(struct scsidevice *) hs->sc_hc->hp_addr;
	register struct scsi_queue *dq = hs->sc_sq.dq_forw;

	dq->dq_imax =  0;
	dq->dq_imin = -1;
	dq->dq_omax =  0;
	dq->dq_omin = -1;

	hs->sc_flags  = 0;
	hs->sc_phase  = ARB_SEL_PHASE;

	hs->sc_stat   = 0;
	hs->sc_msg[0] = 0;

#ifdef DEBUGPRINT
	dbgprintf("\n");
	dbgprintf("scstart: ID = %d\n", dq->dq_slave);
	dbgprintf("scstart: cdb[0] = %s\n", scsi_command(dq->dq_cdb->cdb[0]));
	dbgprintf("scstart: cdb[1] = 0x%s\n", hexstr(dq->dq_cdb->cdb[1], 2));
	dbgprintf("scstart: cdb[2] = 0x%s\n", hexstr(dq->dq_cdb->cdb[2], 2));
	dbgprintf("scstart: cdb[3] = 0x%s\n", hexstr(dq->dq_cdb->cdb[3], 2));
	dbgprintf("scstart: cdb[4] = 0x%s\n", hexstr(dq->dq_cdb->cdb[4], 2));
	dbgprintf("scstart: cdb[5] = 0x%s\n", hexstr(dq->dq_cdb->cdb[5], 2));
	if (dq->dq_cdb->cdb[0] & 0xE0) {
		dbgprintf("scstart: cdb[6] = 0x%s\n", hexstr(dq->dq_cdb->cdb[6], 2));
		dbgprintf("scstart: cdb[7] = 0x%s\n", hexstr(dq->dq_cdb->cdb[7], 2));
		dbgprintf("scstart: cdb[8] = 0x%s\n", hexstr(dq->dq_cdb->cdb[8], 2));
		dbgprintf("scstart: cdb[9] = 0x%s\n", hexstr(dq->dq_cdb->cdb[9], 2));
	}
	dbgprintf("scstart: bp->b_bcount = %d\n", dq->dq_bp->b_bcount);
	dbgprintf("scstart: %s\n", phase_name(hs->sc_phase));
#endif

	issue_select(hd, dq->dq_slave, dq->dq_flags);

	return(1);
}

int
_scintr()
{
	register struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
	register int ctlr;

	for (ctlr = 0; ctlr < NSC; ctlr++) {
		hs = &scsi_softc[ctlr];
		hd = (struct scsidevice *) hs->sc_hc->hp_addr;

#ifdef XFER_ENABLE
		if (((hd->scsi_psns & PHASE) == DATA_IN_PHASE) &&
		    (hd->scsi_serr & SERR_XFER_OUT))
			txfer_in(ctlr);
#endif

		if (hd->scsi_ints != 0)
			scintr(ctlr);
	}

	return;
}

int
scintr(ctlr)
	register int ctlr;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd = (struct scsidevice *) hs->sc_hc->hp_addr;
	register struct scsi_queue *dq = hs->sc_sq.dq_forw;
	register u_char ints, temp;
	register int i, slave;
	int wait, len;
	u_char *buf;

	ints = hd->scsi_ints;

#ifdef DEBUGPRINT
	dbgprintf("scintr: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x",
	       ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
	if (hs->sc_phase == CMD_PHASE)
		dbgprintf("   [%s]", scsi_command(dq->dq_cdb->cdb[0]));
	if (hs->sc_phase & PHASE_MSG)
		dbgprintf("   [%s]", scsi_mesg(hs->sc_msg[0]));
	dbgprintf("\n");
#endif

	if (ints & INTS_DISCON) {
		if (hs->sc_msg[0] == MSG_CMD_COMPLETE) {
			hd->scsi_ints = ints;

			if (hs->sc_lock != NULL) {
				*(hs->sc_lock) = SC_IO_COMPLETE;
			} else {
				(dq->dq_driver->d_intr)(dq->dq_unit, hs->sc_stat);
			}

			return;
#ifndef NODISCONNECT
		} else if (hs->sc_msg[0] == MSG_DISCONNECT) {
#ifdef DEBUGPRINT
			dbgprintf("scintr: DISCONNECT : ctlr = %d, slave = %d, cdb = %s\n",
			       dq->dq_ctlr, dq->dq_slave, scsi_command(dq->dq_cdb->cdb[0]));
#endif
			       
			hd->scsi_ints = ints;

			scpend(dq);
			
			dq = hs->sc_sq.dq_forw;
			
			if (dq != &hs->sc_sq)
				(dq->dq_driver->d_start)(dq->dq_unit);

			return;
#endif
		} else
			goto abort;

#ifndef NODISCONNECT
	} else if (ints & INTS_RESEL) {
		temp = hd->scsi_temp & ~(1 << SCSI_ID);
		for (slave = 0; temp != 1; slave++) {
			temp >>= 1;
		}

		hd->scsi_ints = ints;

		scrschdl(ctlr, slave);

		dq = hs->sc_sq.dq_forw;
#ifdef DEBUGPRINT
		dbgprintf("\n");
		dbgprintf("scintr: RESELECT : ctlr = %d, slave = %d, cdb = %s\n",
		       dq->dq_ctlr, dq->dq_slave, scsi_command(dq->dq_cdb->cdb[0]));
#endif
#endif
	} else if (ints & INTS_CMD_DONE) {
		if (hs->sc_phase == BUS_FREE_PHASE)
			goto abort;
		else if (hs->sc_phase  == MESG_IN_PHASE) {
			hd->scsi_scmd = SCMD_RST_ACK;

			 if ((hs->sc_msg[0] == MSG_CMD_COMPLETE) ||
			     (hs->sc_msg[0] == MSG_DISCONNECT)) {
				 hd->scsi_ints = ints;
				 
				 hs->sc_phase  = BUS_FREE_PHASE;

				 return;
			 }
		}
		if (hs->sc_flags & SC_SEL_TIMEOUT)
			hs->sc_flags &= ~SC_SEL_TIMEOUT;
	} else if (ints & INTS_SRV_REQ) {
		if (hs->sc_phase != MESG_IN_PHASE)
			goto abort;
	} else if (ints & INTS_TIMEOUT) {
		if (hs->sc_phase == ARB_SEL_PHASE) {
			if (hs->sc_flags & SC_SEL_TIMEOUT) {
				hd->scsi_ints = ints;
				hs->sc_flags &= ~SC_SEL_TIMEOUT;
				/* Such SCSI Device is not conected. */

				if (hs->sc_lock != NULL) {
					*(hs->sc_lock) = SC_DEV_NOT_FOUND;
				} else {
					(dq->dq_driver->d_intr)(dq->dq_unit, SC_DEV_NOT_FOUND);
				}

				return;
			} else {
				/* wait more 250 usec */
				hs->sc_flags |= SC_SEL_TIMEOUT;
				hd->scsi_temp = 0;
				hd->scsi_tch  = 0;
				hd->scsi_tcm  = 0x06;
				hd->scsi_tcl  = 0x40;
				hd->scsi_ints = ints;
				return;
			}
		} else
			goto abort;
	} else
		goto abort;

	hd->scsi_ints = ints;

	/*
	 * Next SCSI Transfer
	 */

	wait = SC_TIMEOUT;
	while ((hd->scsi_psns & PSNS_REQ) == 0) {
		if (wait < 0) {
/*			hd->scsi_scmd = SCMD_SET_ATN;	*/
			hd->scsi_scmd = SCMD_RST;
			DELAY(40);			/* wait 25 micro sec */
			hd->scsi_scmd = 0;

			wait = SC_TIMEOUT;
			while (wait-- > 0)
				DELAY(1);

			if (hs->sc_lock != NULL) {
				*(hs->sc_lock) = SC_IO_TIMEOUT;
			} else {
				(dq->dq_driver->d_intr)(dq->dq_unit, SC_IO_TIMEOUT);
			}

			return;
		}
		DELAY(1);
		wait--;
	}

	hs->sc_phase = hd->scsi_psns & PHASE;

#ifdef DEBUGPRINT
	dbgprintf("scintr: %s\n", phase_name(hs->sc_phase));
#endif

	if ((hs->sc_phase == DATA_OUT_PHASE) || (hs->sc_phase == DATA_IN_PHASE)) {
		len = ( hs->sc_lock != NULL ? hs->sc_len : dq->dq_bp->b_bcount );
		buf = ( hs->sc_lock != NULL ? hs->sc_buf : (u_char *) dq->dq_bp->b_un.b_addr );
	} else if (hs->sc_phase == CMD_PHASE) {
		len = ( hs->sc_lock != NULL ? hs->sc_cdblen : dq->dq_cdb->len );
		buf = ( hs->sc_lock != NULL ? hs->sc_cdb    : dq->dq_cdb->cdb );
	} else if (hs->sc_phase == STATUS_PHASE) {
		len = 1;
		buf = &hs->sc_stat;
	} else {
		if (hs->sc_phase == MESG_OUT_PHASE) {
#ifndef NODISCONNECT
			hs->sc_msg[0] = MSG_IDENTIFY_DR;
#else
			hs->sc_msg[0] = MSG_IDENTIFY;
#endif
		}
		len = 1;
		buf = hs->sc_msg;
	}

#ifdef XFER_ENABLE
	if ((hs->sc_lock == NULL) && (hs->sc_phase == DATA_IN_PHASE)) {
		dq->dq_xferp   = buf;
		dq->dq_xfercnt = len;
		txfer_start(hd, len, hs->sc_phase);
		return;
	}
#endif

	ixfer_start(hd, len, hs->sc_phase);
	if (hs->sc_phase & PHASE_IO) {
		if ((wait = ixfer_in(hd, len, buf)) == -1) {
			goto time_out;
		}
		if (dq->dq_imin == -1)
			dq->dq_imin = wait;
		else
			dq->dq_imin = min(wait, dq->dq_imin);
		dq->dq_imax = max(wait, dq->dq_imax);
	} else {
		if ((wait = ixfer_out(hd, len, buf)) == -1) {
			goto time_out;
		}
		if (dq->dq_omin == -1)
			dq->dq_omin = wait;
		else 
			dq->dq_omin = min(wait, dq->dq_omin);
		dq->dq_omax = max(wait, dq->dq_omax);
	}

	return;

 time_out:
	scabort(hs, hd);
	printf("scintr: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Current Status\n",
	       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
	
	if (hs->sc_lock != NULL) {
		*(hs->sc_lock) = SC_IO_TIMEOUT;
	} else {
		(dq->dq_driver->d_intr)(dq->dq_unit, SC_IO_TIMEOUT);
	}

	return;

	/*
	 * SCSI Abort
	 */
 abort:

	/* SCSI IO failed */
	scabort(hs, hd);
	hd->scsi_ints = ints;

	if (hs->sc_lock != NULL) {
		*(hs->sc_lock) = SC_IO_FAILED;
	} else {
		(dq->dq_driver->d_intr)(dq->dq_unit, SC_IO_FAILED);
	}

	return;
}

int
scabort(hs, hd)
	register struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
{
	int len;
	u_char junk;

#ifdef DEBUGPRINT
	dbgprintall();
	printf("\n");
#endif

	printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Current Status\n",
	       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);

	if (hd->scsi_ints != 0)
		hd->scsi_ints = hd->scsi_ints;
	printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Reset INTS reg.\n",
	       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);

	if (hd->scsi_psns == 0 || (hd->scsi_ssts & SSTS_INITIATOR) == 0)
		/* no longer connected to scsi target */
		return;

	/* get the number of bytes remaining in current xfer + fudge */
	len = (hd->scsi_tch << 16) | (hd->scsi_tcm << 8) | hd->scsi_tcl;
	printf("scabort: Current xfer count = %d\n", len);

	/* for that many bus cycles, try to send an abort msg */
	for (len += 1024; (hd->scsi_ssts & SSTS_INITIATOR) && --len >= 0; ) {
/*
		hd->scsi_scmd = SCMD_SET_ATN;
		printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Set ATN\n",
		       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
 */
		while ((hd->scsi_psns & PSNS_REQ) == 0) {
			printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Wait for REQ\n",
			       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
			if (! (hd->scsi_ssts & SSTS_INITIATOR))
				goto out;
			DELAY(1);
		}
/*
		if ((hd->scsi_psns & PHASE) == MESG_OUT_PHASE) {
			hd->scsi_scmd = SCMD_RST_ATN;
			printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Reset ATN\n",
			       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
		}
 */
		hd->scsi_pctl = hs->sc_phase = hd->scsi_psns & PHASE;
		printf("scabort: Phase = %s\n", phase_name(hs->sc_phase));

		if (hd->scsi_psns & PHASE_IO) {
			/* one of the input phases - read & discard a byte */
			hd->scsi_scmd = SCMD_SET_ACK;
			printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Set ACK\n",
			       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);

			while (hd->scsi_psns & PSNS_REQ) {
				printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Wait for REQ\n",
				       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
				DELAY(1);
			}

			junk = hd->scsi_temp;
			printf("scabort: TEMP = 0x%s\n", hexstr(junk, 2));
		} else {
			/* one of the output phases - send an abort msg */
			hd->scsi_temp = MSG_ABORT;
			hd->scsi_scmd = SCMD_SET_ACK;
			printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Set ACK\n",
			       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);

			while (hd->scsi_psns & PSNS_REQ) {
				printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Wait for REQ\n",
				       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
				DELAY(1);
			}
		}

		hd->scsi_scmd = SCMD_RST_ACK;
		printf("scabort: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Reset ACK\n",
		       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);
	}
out:
	/*
	 * Either the abort was successful & the bus is disconnected or
	 * the device didn't listen.  If the latter, announce the problem.
	 * Either way, reset the card & the SPC.
	 */
	if (len < 0 && hs)
		printf("sc%d: abort failed.  phase=0x%x, ssts=0x%x\n",
			hs->sc_hc->hp_unit, hd->scsi_psns, hd->scsi_ssts);

	while (hd->scsi_ints == 0)
		DELAY(1);

	hd->scsi_ints = hd->scsi_ints;

	printf("scintr: INTS 0x%x, SSTS 0x%x, PCTL 0x%x, PSNS 0x%x   Current Status\n",
	       hd->scsi_ints, hd->scsi_ssts, hd->scsi_pctl, hd->scsi_psns);

	printf("scabort: SCSI abort operation is done\n");
}


/*
 * SPC device queue handling
 */

int
screq(dq)
	register struct scsi_queue *dq;
{
	register struct scsi_softc *hs = &scsi_softc[dq->dq_ctlr];
	register struct scsi_queue *hq = &hs->sc_sq;

	insque(dq, hq->dq_back);

	if (dq->dq_back == hq) {
#ifdef QUE_DEBUG
		printf("screq: slave = %d, command = %s\n",
		       hq->dq_forw->dq_slave,
		       scsi_command(hq->dq_forw->dq_cdb->cdb[0]));
#endif
		return(1);
	}

	return(0);
}

#ifndef NODISCONNECT
int
scpend(dq)
	register struct scsi_queue *dq;
{
	register struct scsi_softc *hs = &scsi_softc[dq->dq_ctlr];
	register struct scsi_queue *hq = &hs->sc_sq;
	register struct scsi_queue *wq = &hs->sc_wq;

	remque(dq);

	insque(dq, wq->dq_back);
}

int
scrschdl(ctlr, slave)
	register int ctlr;
	register int slave;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	register struct scsi_queue *wq = &hs->sc_wq;
	register struct scsi_queue *hq = &hs->sc_sq;
	register struct scsi_queue *dq;

	for (dq = wq->dq_forw; dq != wq; dq = dq->dq_forw) {
		if (dq->dq_slave == slave)
			goto found;
	}

	return(0);

 found:
	remque(dq);
	insque(dq, hq);

	return(1);
}
#endif

int
scfree(dq)
	register struct scsi_queue *dq;
{
	register struct scsi_softc *hs = &scsi_softc[dq->dq_ctlr];
	register struct scsi_queue *hq = &hs->sc_sq;
	int status = hs->sc_stat;

	remque(dq);

	hs->sc_flags  = 0;
	hs->sc_phase  = BUS_FREE_PHASE;

	hs->sc_stat   = 0;
	hs->sc_msg[0] = 0;

	if ((dq = hq->dq_forw) != hq) {
#ifdef QUE_DEBUG
		printf("scfree: slave = %d, command = %s\n",
		       dq->dq_slave,
		       scsi_command(dq->dq_cdb->cdb[0]));
#endif
		(dq->dq_driver->d_start)(dq->dq_unit);
	}

	return(status);
}

/*
 * SCSI common interface
 */

int scsi_lock[NSC];

int
scsi_result(unit, stat)
	int unit, stat;
{
#ifdef SCSI_DEBUG
	printf("scsi_result: stat = %s\n", scsi_status(stat));
#endif
	if (stat < 0)
		scsi_lock[unit] = stat;
	else
		scsi_lock[unit] = SC_IO_COMPLETE;
}

struct	driver scsi_driver = {
	(int (*)()) 0, "scsi", (int (*)()) 0, (int (*)()) 0, scsi_result, (int (*)()) 0
};

#define SCSI_BUF 8

struct buf scsi_buf[SCSI_BUF];

int
scsi_init_buf()
{
	register struct buf *rbp = &scsi_buf[0];
	register int i;

	rbp->av_forw = rbp->av_back = rbp;

	for(i = 0; i < SCSI_BUF; i++)
		scsi_free_buf(&scsi_buf[i]);
}

int
scsi_free_buf(bp)
	register struct buf *bp;
{
	register struct buf *rbp = &scsi_buf[0];

	bp->av_forw = rbp;
	bp->av_back = rbp->av_back;

	rbp->av_back->av_forw = bp;
	rbp->av_back = bp;
}

struct buf *
scsi_get_buf()
{
	register struct buf *rbp = &scsi_buf[0];
	register struct buf *bp = rbp->av_forw;

	if (bp == rbp)
		return((struct buf *) 0);

	bp->av_forw->av_back = rbp;
	rbp->av_forw = bp->av_forw;

	return(bp);
}

struct scsi_queue scsi_entry[NSC];

int
scsi_immed_command(ctlr, slave, lun, cdb, buf, len)
	int ctlr, slave, lun;
	struct scsi_fmt_cdb *cdb;
	u_char *buf;
	unsigned len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd =
		(struct scsidevice *) hs->sc_hc->hp_addr;
	register struct scsi_queue *dq = &scsi_entry[ctlr];
	register struct buf *bp;
	int s, status, wait = 30;

#ifdef SCSI_DEBUG
	printf("scsi_immed_command( %d, %d, %d, cdb(%d,%s), buf, %d): Start\n",
	       ctlr, slave, lun, cdb->len, scsi_command(cdb->cdb[0]), len);
#endif

	if ((bp = scsi_get_buf()) == 0) {
		return(SC_BUSY);
	}

	s = splbio();

	bp->b_flags = B_BUSY;
	bp->b_bcount = len;
	bp->b_un.b_addr = (caddr_t) buf;

	dq->dq_unit   = ctlr;
	dq->dq_ctlr   = ctlr;
	dq->dq_slave  = slave;
	dq->dq_driver = &scsi_driver;
	dq->dq_cdb    = cdb;
	dq->dq_bp     = bp;

	scsi_lock[ctlr] = SC_IN_PROGRESS;
	if (screq(dq))
		scstart(ctlr);

	splx(s);

	while (scsi_lock[ctlr] == SC_IN_PROGRESS) {
		if (wait < 0) {
			scabort(hs, hd);

			s = splbio();
			status = scfree(dq);
			splx(s);

			bp->b_flags = 0;

			return(SC_IO_FAILED);
		}

		DELAY(100000);
		wait--;
	}

	s = splbio();
	status = scfree(dq);
	splx(s);

	if (scsi_lock[ctlr] < 0)
		status = scsi_lock[ctlr];

	scsi_free_buf(bp);

#ifdef SCSI_DEBUG
		printf("scsi_immed_command: Status -- 0x%x\n", status);
#endif
	return(status);
}

int
scsi_test_unit_rdy(ctlr, slave, lun)
	int ctlr, slave, lun;
{
	static struct scsi_fmt_cdb cdb = { 6, CMD_TEST_UNIT_READY };
	int stat;

	while ((stat = scsi_immed_command(ctlr, slave, lun,
					  &cdb, (u_char *) 0, 0)) == SC_BUSY) {
		DELAY(10000);
	}

	return(stat);
}

int
scsi_request_sense(ctlr, slave, lun, buf, len)
	int ctlr, slave, lun;
	u_char *buf;
	unsigned len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd = 
		(struct scsidevice *) hs->sc_hc->hp_addr;
	static struct scsi_fmt_cdb req_cmd = { 6, CMD_REQUEST_SENSE };
	int s, status, lock;

#ifdef REQ_DEBUG
	printf("scsi_request_sense( %d, %d, %d, buf, %d) -- Start\n",
	       ctlr, slave, lun, len);
#endif

        req_cmd.cdb[1] = lun;
        req_cmd.cdb[4] = len;

	if (hd->scsi_ssts & (SSTS_INITIATOR|SSTS_TARGET|SSTS_BUSY))
		return(0);

	s = splbio();

	hs->sc_flags  = 0;
	hs->sc_phase  = ARB_SEL_PHASE;

	hs->sc_cdb    = req_cmd.cdb;
	hs->sc_cdblen = req_cmd.len;
	hs->sc_buf    = buf;
	hs->sc_len    = len;

	hs->sc_stat   = 0;
	hs->sc_msg[0] = 0;

	lock = SC_IN_PROGRESS;
	hs->sc_lock   = &lock;

	issue_select(hd, slave, 0);

	spl0();

	while ((lock == SC_IN_PROGRESS) || (lock == SC_DISCONNECTED))
		DELAY(10);

	splbio();

	hs->sc_flags  = 0;
	hs->sc_phase  = BUS_FREE_PHASE;

	hs->sc_cdb    = NULL;
	hs->sc_cdblen = 0;
	hs->sc_buf    = NULL;
	hs->sc_len    = 0;
	hs->sc_lock   = NULL;

	status = hs->sc_stat;

	hs->sc_stat   = 0;
	hs->sc_msg[0] = 0;

	splx(s);

	if (lock == SC_IO_COMPLETE) {
#ifdef REQ_DEBUG
		printf("scsi_request_sense: Status -- 0x%x\n", status);
#endif
		return(status);
	} else {
		return(lock);
	}
}
#endif
