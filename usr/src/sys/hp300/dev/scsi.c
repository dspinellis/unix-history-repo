/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
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
 *	@(#)scsi.c	8.2 (Berkeley) 1/12/94
 */

#ifndef DEBUG
#define DEBUG
#endif
/*
 * HP9000/3xx 98658 SCSI host adaptor driver.
 */
#include "scsi.h"
#if NSCSI > 0

#ifndef lint
static char rcsid[] = "$Header: /usr/src/sys/hp300/dev/RCS/scsi.c,v 1.2 92/04/10 20:48:29 mike Exp $";
#endif

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>

#include <hp/dev/device.h>

#include <hp300/dev/scsivar.h>
#include <hp300/dev/scsireg.h>
#include <hp300/dev/dmavar.h>

#include <machine/cpu.h>
#include <hp300/hp300/isr.h>

/*
 * SCSI delays
 * In u-seconds, primarily for state changes on the SPC.
 */
#define	SCSI_CMD_WAIT	1000	/* wait per step of 'immediate' cmds */
#define	SCSI_DATA_WAIT	1000	/* wait per data in/out step */
#define	SCSI_INIT_WAIT	50000	/* wait per step (both) during init */

extern void isrlink();
extern void _insque();
extern void _remque();

int	scsiinit(), scsigo(), scsiintr(), scsixfer();
void	scsistart(), scsidone(), scsifree(), scsireset();
struct	driver scsidriver = {
	scsiinit, "scsi", (int (*)())scsistart, scsigo, scsiintr,
	(int (*)())scsidone,
};

struct	scsi_softc scsi_softc[NSCSI];
struct	isr scsi_isr[NSCSI];

int scsi_cmd_wait = SCSI_CMD_WAIT;
int scsi_data_wait = SCSI_DATA_WAIT;
int scsi_init_wait = SCSI_INIT_WAIT;

int scsi_nosync = 1;		/* inhibit sync xfers if 1 */
int scsi_pridma = 0;		/* use "priority" dma */

#ifdef DEBUG
int	scsi_debug = 0;
#define WAITHIST
#endif

#ifdef WAITHIST
#define MAXWAIT	1022
u_int	ixstart_wait[MAXWAIT+2];
u_int	ixin_wait[MAXWAIT+2];
u_int	ixout_wait[MAXWAIT+2];
u_int	mxin_wait[MAXWAIT+2];
u_int	mxin2_wait[MAXWAIT+2];
u_int	cxin_wait[MAXWAIT+2];
u_int	fxfr_wait[MAXWAIT+2];
u_int	sgo_wait[MAXWAIT+2];
#define HIST(h,w) (++h[((w)>MAXWAIT? MAXWAIT : ((w) < 0 ? -1 : (w))) + 1]);
#else
#define HIST(h,w)
#endif

#define	b_cylin		b_resid

static void
scsiabort(hs, hd, where)
	register struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
	char *where;
{
	int len;
	int maxtries;	/* XXX - kludge till I understand whats *supposed* to happen */
	int startlen;	/* XXX - kludge till I understand whats *supposed* to happen */
	u_char junk;

	printf("scsi%d: abort from %s: phase=0x%x, ssts=0x%x, ints=0x%x\n",
		hs->sc_hc->hp_unit, where, hd->scsi_psns, hd->scsi_ssts,
		hd->scsi_ints);

	hd->scsi_ints = hd->scsi_ints;
	hd->scsi_csr = 0;
	if (hd->scsi_psns == 0 || (hd->scsi_ssts & SSTS_INITIATOR) == 0)
		/* no longer connected to scsi target */
		return;

	/* get the number of bytes remaining in current xfer + fudge */
	len = (hd->scsi_tch << 16) | (hd->scsi_tcm << 8) | hd->scsi_tcl;

	/* for that many bus cycles, try to send an abort msg */
	for (startlen = (len += 1024); (hd->scsi_ssts & SSTS_INITIATOR) && --len >= 0; ) {
		hd->scsi_scmd = SCMD_SET_ATN;
		maxtries = 1000;
		while ((hd->scsi_psns & PSNS_REQ) == 0) {
			if (! (hd->scsi_ssts & SSTS_INITIATOR))
				goto out;
			DELAY(1);
			if (--maxtries == 0) {
				printf("-- scsiabort gave up after 1000 tries (startlen = %d len = %d)\n",
					startlen, len);
				goto out2;
			}

		}
out2:
		if ((hd->scsi_psns & PHASE) == MESG_OUT_PHASE)
			hd->scsi_scmd = SCMD_RST_ATN;
		hd->scsi_pctl = hd->scsi_psns & PHASE;
		if (hd->scsi_psns & PHASE_IO) {
			/* one of the input phases - read & discard a byte */
			hd->scsi_scmd = SCMD_SET_ACK;
			if (hd->scsi_tmod == 0)
				while (hd->scsi_psns & PSNS_REQ)
					DELAY(1);
			junk = hd->scsi_temp;
		} else {
			/* one of the output phases - send an abort msg */
			hd->scsi_temp = MSG_ABORT;
			hd->scsi_scmd = SCMD_SET_ACK;
			if (hd->scsi_tmod == 0)
				while (hd->scsi_psns & PSNS_REQ)
					DELAY(1);
		}
		hd->scsi_scmd = SCMD_RST_ACK;
	}
out:
	/*
	 * Either the abort was successful & the bus is disconnected or
	 * the device didn't listen.  If the latter, announce the problem.
	 * Either way, reset the card & the SPC.
	 */
	if (len < 0 && hs)
		printf("scsi%d: abort failed.  phase=0x%x, ssts=0x%x\n",
			hs->sc_hc->hp_unit, hd->scsi_psns, hd->scsi_ssts);

	if (! ((junk = hd->scsi_ints) & INTS_RESEL)) {
		hd->scsi_sctl |= SCTL_CTRLRST;
		DELAY(1);
		hd->scsi_sctl &=~ SCTL_CTRLRST;
		hd->scsi_hconf = 0;
		hd->scsi_ints = hd->scsi_ints;
	}
}

/*
 * XXX Set/reset long delays.
 *
 * if delay == 0, reset default delays
 * if delay < 0,  set both delays to default long initialization values
 * if delay > 0,  set both delays to this value
 *
 * Used when a devices is expected to respond slowly (e.g. during
 * initialization).
 */
void
scsi_delay(delay)
	int delay;
{
	static int saved_cmd_wait, saved_data_wait;

	if (delay) {
		saved_cmd_wait = scsi_cmd_wait;
		saved_data_wait = scsi_data_wait;
		if (delay > 0)
			scsi_cmd_wait = scsi_data_wait = delay;
		else
			scsi_cmd_wait = scsi_data_wait = scsi_init_wait;
	} else {
		scsi_cmd_wait = saved_cmd_wait;
		scsi_data_wait = saved_data_wait;
	}
}

int
scsiinit(hc)
	register struct hp_ctlr *hc;
{
	register struct scsi_softc *hs = &scsi_softc[hc->hp_unit];
	register struct scsidevice *hd = (struct scsidevice *)hc->hp_addr;
	
	if ((hd->scsi_id & ID_MASK) != SCSI_ID)
		return(0);
	hc->hp_ipl = SCSI_IPL(hd->scsi_csr);
	hs->sc_hc = hc;
	hs->sc_dq.dq_unit = hc->hp_unit;
	hs->sc_dq.dq_driver = &scsidriver;
	hs->sc_sq.dq_forw = hs->sc_sq.dq_back = &hs->sc_sq;
	scsi_isr[hc->hp_unit].isr_intr = scsiintr;
	scsi_isr[hc->hp_unit].isr_ipl = hc->hp_ipl;
	scsi_isr[hc->hp_unit].isr_arg = hc->hp_unit;
	isrlink(&scsi_isr[hc->hp_unit]);
	scsireset(hc->hp_unit);
	/*
	 * XXX scale initialization wait according to CPU speed.
	 * Should we do this for all wait?  Should we do this at all?
	 */
	scsi_init_wait *= cpuspeed;
	return(1);
}

void
scsireset(unit)
	register int unit;
{
	register struct scsi_softc *hs = &scsi_softc[unit];
	volatile register struct scsidevice *hd =
				(struct scsidevice *)hs->sc_hc->hp_addr;
	u_int i;

	if (hs->sc_flags & SCSI_ALIVE)
		scsiabort(hs, hd, "reset");
		
	printf("scsi%d: ", unit);

	hd->scsi_id = 0xFF;
	DELAY(100);
	/*
	 * Disable interrupts then reset the FUJI chip.
	 */
	hd->scsi_csr  = 0;
	hd->scsi_sctl = SCTL_DISABLE | SCTL_CTRLRST;
	hd->scsi_scmd = 0;
	hd->scsi_tmod = 0;
	hd->scsi_pctl = 0;
	hd->scsi_temp = 0;
	hd->scsi_tch  = 0;
	hd->scsi_tcm  = 0;
	hd->scsi_tcl  = 0;
	hd->scsi_ints = 0;

	if ((hd->scsi_id & ID_WORD_DMA) == 0) {
		hs->sc_flags |= SCSI_DMA32;
		printf("32 bit dma, ");
	}

	/* Determine Max Synchronous Transfer Rate */
	if (scsi_nosync)
		i = 3;
	else
		i = SCSI_SYNC_XFER(hd->scsi_hconf);
	switch (i) {
		case 0:
			hs->sc_sync = TMOD_SYNC | 0x3e; /* 250 nsecs */
			printf("250ns sync");
			break;
		case 1:
			hs->sc_sync = TMOD_SYNC | 0x5e; /* 375 nsecs */
			printf("375ns sync");
			break;
		case 2:
			hs->sc_sync = TMOD_SYNC | 0x7d; /* 500 nsecs */
			printf("500ns sync");
			break;
		case 3:
			hs->sc_sync = 0;
			printf("async");
			break;
		}

	/*
	 * Configure the FUJI chip with its SCSI address, all
	 * interrupts enabled & appropriate parity.
	 */
	i = (~hd->scsi_hconf) & 0x7;
	hs->sc_scsi_addr = 1 << i;
	hd->scsi_bdid = i;
	if (hd->scsi_hconf & HCONF_PARITY)
		hd->scsi_sctl = SCTL_DISABLE | SCTL_ABRT_ENAB |
				SCTL_SEL_ENAB | SCTL_RESEL_ENAB |
				SCTL_INTR_ENAB | SCTL_PARITY_ENAB;
	else {
		hd->scsi_sctl = SCTL_DISABLE | SCTL_ABRT_ENAB |
				SCTL_SEL_ENAB | SCTL_RESEL_ENAB |
				SCTL_INTR_ENAB;
		printf(", no parity");
	}
	hd->scsi_sctl &=~ SCTL_DISABLE;

	printf(", scsi id %d\n", i);
	hs->sc_flags |= SCSI_ALIVE;
}

static void
scsierror(hs, hd, ints)
	register struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
	u_char ints;
{
	int unit = hs->sc_hc->hp_unit;
	char *sep = "";

	printf("scsi%d: ", unit);
	if (ints & INTS_RST) {
		DELAY(100);
		if (hd->scsi_hconf & HCONF_SD)
			printf("spurious RST interrupt");
		else
			printf("hardware error - check fuse");
		sep = ", ";
	}
	if ((ints & INTS_HARD_ERR) || hd->scsi_serr) {
		if (hd->scsi_serr & SERR_SCSI_PAR) {
			printf("%sparity err", sep);
			sep = ", ";
		}
		if (hd->scsi_serr & SERR_SPC_PAR) {
			printf("%sSPC parity err", sep);
			sep = ", ";
		}
		if (hd->scsi_serr & SERR_TC_PAR) {
			printf("%sTC parity err", sep);
			sep = ", ";
		}
		if (hd->scsi_serr & SERR_PHASE_ERR) {
			printf("%sphase err", sep);
			sep = ", ";
		}
		if (hd->scsi_serr & SERR_SHORT_XFR) {
			printf("%ssync short transfer err", sep);
			sep = ", ";
		}
		if (hd->scsi_serr & SERR_OFFSET) {
			printf("%ssync offset error", sep);
			sep = ", ";
		}
	}
	if (ints & INTS_TIMEOUT)
		printf("%sSPC select timeout error", sep);
	if (ints & INTS_SRV_REQ)
		printf("%sspurious SRV_REQ interrupt", sep);
	if (ints & INTS_CMD_DONE)
		printf("%sspurious CMD_DONE interrupt", sep);
	if (ints & INTS_DISCON)
		printf("%sspurious disconnect interrupt", sep);
	if (ints & INTS_RESEL)
		printf("%sspurious reselect interrupt", sep);
	if (ints & INTS_SEL)
		printf("%sspurious select interrupt", sep);
	printf("\n");
}

static int
issue_select(hd, target, our_addr)
	volatile register struct scsidevice *hd;
	u_char target, our_addr;
{
	if (hd->scsi_ssts & (SSTS_INITIATOR|SSTS_TARGET|SSTS_BUSY))
		return (1);

	if (hd->scsi_ints & INTS_DISCON)
		hd->scsi_ints = INTS_DISCON;

	hd->scsi_pctl = 0;
	hd->scsi_temp = (1 << target) | our_addr;
	/* select timeout is hardcoded to 2ms */
	hd->scsi_tch = 0;
	hd->scsi_tcm = 32;
	hd->scsi_tcl = 4;

	hd->scsi_scmd = SCMD_SELECT;
	return (0);
}

static int
wait_for_select(hd)
	volatile register struct scsidevice *hd;
{
	u_char ints;

	while ((ints = hd->scsi_ints) == 0)
		DELAY(1);
	hd->scsi_ints = ints;
	return (!(hd->scsi_ssts & SSTS_INITIATOR));
}

static int
ixfer_start(hd, len, phase, wait)
	volatile register struct scsidevice *hd;
	int len;
	u_char phase;
	register int wait;
{

	hd->scsi_tch = len >> 16;
	hd->scsi_tcm = len >> 8;
	hd->scsi_tcl = len;
	hd->scsi_pctl = phase;
	hd->scsi_tmod = 0; /*XXX*/
	hd->scsi_scmd = SCMD_XFR | SCMD_PROG_XFR;

	/* wait for xfer to start or svc_req interrupt */
	while ((hd->scsi_ssts & SSTS_BUSY) == 0) {
		if (hd->scsi_ints || --wait < 0) {
#ifdef DEBUG
			if (scsi_debug)
				printf("ixfer_start fail: i%x, w%d\n",
				       hd->scsi_ints, wait);
#endif
			HIST(ixstart_wait, wait)
			return (0);
		}
		DELAY(1);
	}
	HIST(ixstart_wait, wait)
	return (1);
}

static int
ixfer_out(hd, len, buf)
	volatile register struct scsidevice *hd;
	int len;
	register u_char *buf;
{
	register int wait = scsi_data_wait;

	for (; len > 0; --len) {
		while (hd->scsi_ssts & SSTS_DREG_FULL) {
			if (hd->scsi_ints || --wait < 0) {
#ifdef DEBUG
				if (scsi_debug)
					printf("ixfer_out fail: l%d i%x w%d\n",
					       len, hd->scsi_ints, wait);
#endif
				HIST(ixout_wait, wait)
				return (len);
			}
			DELAY(1);
		}
		hd->scsi_dreg = *buf++;
	}
	HIST(ixout_wait, wait)
	return (0);
}

static void
ixfer_in(hd, len, buf)
	volatile register struct scsidevice *hd;
	int len;
	register u_char *buf;
{
	register int wait = scsi_data_wait;

	for (; len > 0; --len) {
		while (hd->scsi_ssts & SSTS_DREG_EMPTY) {
			if (hd->scsi_ints || --wait < 0) {
				while (! (hd->scsi_ssts & SSTS_DREG_EMPTY)) {
					*buf++ = hd->scsi_dreg;
					--len;
				}
#ifdef DEBUG
				if (scsi_debug)
					printf("ixfer_in fail: l%d i%x w%d\n",
					       len, hd->scsi_ints, wait);
#endif
				HIST(ixin_wait, wait)
				return;
			}
			DELAY(1);
		}
		*buf++ = hd->scsi_dreg;
	}
	HIST(ixin_wait, wait)
}

static int
mxfer_in(hd, len, buf, phase)
	volatile register struct scsidevice *hd;
	register int len;
	register u_char *buf;
	register u_char phase;
{
	register int wait = scsi_cmd_wait;
	register int i;

	hd->scsi_tmod = 0;
	for (i = 0; i < len; ++i) {
		/*
		 * manual sez: reset ATN before ACK is sent.
		 */
		if (hd->scsi_psns & PSNS_ATN)
			hd->scsi_scmd = SCMD_RST_ATN;
		/*
		 * wait for the request line (which says the target
		 * wants to give us data).  If the phase changes while
		 * we're waiting, we're done.
		 */
		while ((hd->scsi_psns & PSNS_REQ) == 0) {
			if (--wait < 0) {
				HIST(mxin_wait, wait)
				return (-1);
			}
			if ((hd->scsi_psns & PHASE) != phase ||
			    (hd->scsi_ssts & SSTS_INITIATOR) == 0)
				goto out;

			DELAY(1);
		}
		/*
		 * set ack (which says we're ready for the data, wait for
		 * req to go away (target says data is available), grab the
		 * data, then reset ack (say we've got the data).
		 */
		hd->scsi_pctl = phase;
		hd->scsi_scmd = SCMD_SET_ACK;
		while (hd->scsi_psns & PSNS_REQ) {
			if (--wait < 0) {
				HIST(mxin_wait, wait)
				return (-2);
			}
			DELAY(1);
		}
		*buf++ = hd->scsi_temp;
		hd->scsi_scmd = SCMD_RST_ACK;
	}
out:
	HIST(mxin_wait, wait)
	/*
	 * Wait for manual transfer to finish.
	 * Avoids occasional "unexpected phase" errors in finishxfer
	 * formerly addressed by per-slave delays.
	 */
	wait = scsi_cmd_wait;
	while ((hd->scsi_ssts & SSTS_ACTIVE) == SSTS_INITIATOR) {
		if (--wait < 0)
			break;
		DELAY(1);
	}
	HIST(mxin2_wait, wait)
	return (i);
}

/*
 * SCSI 'immediate' command:  issue a command to some SCSI device
 * and get back an 'immediate' response (i.e., do programmed xfer
 * to get the response data).  'cbuf' is a buffer containing a scsi
 * command of length clen bytes.  'buf' is a buffer of length 'len'
 * bytes for data.  The transfer direction is determined by the device
 * (i.e., by the scsi bus data xfer phase).  If 'len' is zero, the
 * command must supply no data.  'xferphase' is the bus phase the
 * caller expects to happen after the command is issued.  It should
 * be one of DATA_IN_PHASE, DATA_OUT_PHASE or STATUS_PHASE.
 */
static int
scsiicmd(hs, target, cbuf, clen, buf, len, xferphase)
	struct scsi_softc *hs;
	int target;
	u_char *cbuf;
	int clen;
	u_char *buf;
	int len;
	u_char xferphase;
{
	volatile register struct scsidevice *hd =
				(struct scsidevice *)hs->sc_hc->hp_addr;
	u_char phase, ints;
	register int wait;

	/* select the SCSI bus (it's an error if bus isn't free) */
	if (issue_select(hd, target, hs->sc_scsi_addr))
		return (-1);
	if (wait_for_select(hd))
		return (-1);
	/*
	 * Wait for a phase change (or error) then let the device
	 * sequence us through the various SCSI phases.
	 */
	hs->sc_stat[0] = 0xff;
	hs->sc_msg[0] = 0xff;
	phase = CMD_PHASE;
	while (1) {
		wait = scsi_cmd_wait;
		switch (phase) {

		case CMD_PHASE:
			if (ixfer_start(hd, clen, phase, wait))
				if (ixfer_out(hd, clen, cbuf))
					goto abort;
			phase = xferphase;
			break;

		case DATA_IN_PHASE:
			if (len <= 0)
				goto abort;
			wait = scsi_data_wait;
			if (ixfer_start(hd, len, phase, wait) ||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY))
				ixfer_in(hd, len, buf);
			phase = STATUS_PHASE;
			break;

		case DATA_OUT_PHASE:
			if (len <= 0)
				goto abort;
			wait = scsi_data_wait;
			if (ixfer_start(hd, len, phase, wait)) {
				if (ixfer_out(hd, len, buf))
					goto abort;
			}
			phase = STATUS_PHASE;
			break;

		case STATUS_PHASE:
			wait = scsi_data_wait;
			if (ixfer_start(hd, sizeof(hs->sc_stat), phase, wait) ||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY))
				ixfer_in(hd, sizeof(hs->sc_stat), hs->sc_stat);
			phase = MESG_IN_PHASE;
			break;

		case MESG_IN_PHASE:
			if (ixfer_start(hd, sizeof(hs->sc_msg), phase, wait) ||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY)) {
				ixfer_in(hd, sizeof(hs->sc_msg), hs->sc_msg);
				hd->scsi_scmd = SCMD_RST_ACK;
			}
			phase = BUS_FREE_PHASE;
			break;

		case BUS_FREE_PHASE:
			goto out;

		default:
			printf("scsi%d: unexpected phase %d in icmd from %d\n",
				hs->sc_hc->hp_unit, phase, target);
			goto abort;
		}
		/* wait for last command to complete */
		while ((ints = hd->scsi_ints) == 0) {
			if (--wait < 0) {
				HIST(cxin_wait, wait)
				goto abort;
			}
			DELAY(1);
		}
		HIST(cxin_wait, wait)
		hd->scsi_ints = ints;
		if (ints & INTS_SRV_REQ)
			phase = hd->scsi_psns & PHASE;
		else if (ints & INTS_DISCON)
			goto out;
		else if ((ints & INTS_CMD_DONE) == 0) {
			scsierror(hs, hd, ints);
			goto abort;
		}
	}
abort:
	scsiabort(hs, hd, "icmd");
out:
	return (hs->sc_stat[0]);
}

/*
 * Finish SCSI xfer command:  After the completion interrupt from
 * a read/write operation, sequence through the final phases in
 * programmed i/o.  This routine is a lot like scsiicmd except we
 * skip (and don't allow) the select, cmd out and data in/out phases.
 */
static void
finishxfer(hs, hd, target)
	struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
	int target;
{
	u_char phase, ints;

	/*
	 * We specified padding xfer so we ended with either a phase
	 * change interrupt (normal case) or an error interrupt (handled
	 * elsewhere).  Reset the board dma logic then try to get the
	 * completion status & command done msg.  The reset confuses
	 * the SPC REQ/ACK logic so we have to do any status/msg input
	 * operations via 'manual xfer'.
	 */
	if (hd->scsi_ssts & SSTS_BUSY) {
		int wait = scsi_cmd_wait;

		/* wait for dma operation to finish */
		while (hd->scsi_ssts & SSTS_BUSY) {
			if (--wait < 0) {
#ifdef DEBUG
				if (scsi_debug)
					printf("finishxfer fail: ssts %x\n",
					       hd->scsi_ssts);
#endif
				HIST(fxfr_wait, wait)
				goto abort;
			}
		}
		HIST(fxfr_wait, wait)
	}
	hd->scsi_scmd |= SCMD_PROG_XFR;
	hd->scsi_sctl |= SCTL_CTRLRST;
	DELAY(1);
	hd->scsi_sctl &=~ SCTL_CTRLRST;
	hd->scsi_hconf = 0;
	/*
	 * The following delay is definitely needed when trying to
	 * write on a write protected disk (in the optical jukebox anyways),
	 * but we shall see if other unexplained machine freezeups
	 * also stop occuring...  A value of 5 seems to work but
	 * 10 seems safer considering the potential consequences.
	 */
	DELAY(10);
	hs->sc_stat[0] = 0xff;
	hs->sc_msg[0] = 0xff;
	hd->scsi_csr = 0;
	hd->scsi_ints = ints = hd->scsi_ints;
	while (1) {
		phase = hd->scsi_psns & PHASE;
		switch (phase) {

		case STATUS_PHASE:
			if (mxfer_in(hd, sizeof(hs->sc_stat), hs->sc_stat,
				     phase) <= 0)
				goto abort;
			break;

		case MESG_IN_PHASE:
			if (mxfer_in(hd, sizeof(hs->sc_msg), hs->sc_msg,
				     phase) < 0)
				goto abort;
			break;

		case BUS_FREE_PHASE:
			return;

		default:
			printf("scsi%d: unexpected phase %d in finishxfer from %d\n",
				hs->sc_hc->hp_unit, phase, target);
			goto abort;
		}
		if (ints = hd->scsi_ints) {
			hd->scsi_ints = ints;
			if (ints & INTS_DISCON)
				return;
			else if (ints & ~(INTS_SRV_REQ|INTS_CMD_DONE)) {
				scsierror(hs, hd, ints);
				break;
			}
		}
		if ((hd->scsi_ssts & SSTS_INITIATOR) == 0)
			return;
	}
abort:
	scsiabort(hs, hd, "finishxfer");
	hs->sc_stat[0] = 0xfe;
}

int
scsi_test_unit_rdy(ctlr, slave, unit)
	int ctlr, slave, unit;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_TEST_UNIT_READY };

	cdb.lun = unit;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), (u_char *)0, 0,
			 STATUS_PHASE));
}

int
scsi_request_sense(ctlr, slave, unit, buf, len)
	int ctlr, slave, unit;
	u_char *buf;
	unsigned len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_REQUEST_SENSE };

	cdb.lun = unit;
	cdb.len = len;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_IN_PHASE));
}

int
scsi_immed_command(ctlr, slave, unit, cdb, buf, len, rd)
	int ctlr, slave, unit, rd;
	struct scsi_fmt_cdb *cdb;
	u_char *buf;
	u_int len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];

	cdb->cdb[1] |= unit << 5;
	return (scsiicmd(hs, slave, cdb->cdb, cdb->len, buf, len,
			 rd != 0? DATA_IN_PHASE : DATA_OUT_PHASE));
}

/*
 * The following routines are test-and-transfer i/o versions of read/write
 * for things like reading disk labels and writing core dumps.  The
 * routine scsigo should be used for normal data transfers, NOT these
 * routines.
 */
int
scsi_tt_read(ctlr, slave, unit, buf, len, blk, bshift)
	int ctlr, slave, unit;
	u_char *buf;
	u_int len;
	daddr_t blk;
	int bshift;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;
	int stat;
	int old_wait = scsi_data_wait;

	scsi_data_wait = 300000;
	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_READ_EXT;
	cdb.lun = unit;
	blk >>= bshift;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = len >> (8 + DEV_BSHIFT + bshift);
	cdb.lenl = len >> (DEV_BSHIFT + bshift);
	stat = scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_IN_PHASE);
	scsi_data_wait = old_wait;
	return (stat);
}

int
scsi_tt_write(ctlr, slave, unit, buf, len, blk, bshift)
	int ctlr, slave, unit;
	u_char *buf;
	u_int len;
	daddr_t blk;
	int bshift;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;
	int stat;
	int old_wait = scsi_data_wait;

	scsi_data_wait = 300000;

	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_WRITE_EXT;
	cdb.lun = unit;
	blk >>= bshift;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = len >> (8 + DEV_BSHIFT + bshift);
	cdb.lenl = len >> (DEV_BSHIFT + bshift);
	stat = scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_OUT_PHASE);
	scsi_data_wait = old_wait;
	return (stat);
}

int
scsireq(dq)
	register struct devqueue *dq;
{
	register struct devqueue *hq;

	hq = &scsi_softc[dq->dq_ctlr].sc_sq;
	insque(dq, hq->dq_back);
	if (dq->dq_back == hq)
		return(1);
	return(0);
}

int
scsiustart(unit)
	int unit;
{
	register struct scsi_softc *hs = &scsi_softc[unit];

	hs->sc_dq.dq_ctlr = DMA0 | DMA1;
	hs->sc_flags |= SCSI_HAVEDMA;
	if (dmareq(&hs->sc_dq))
		return(1);
	return(0);
}

void
scsistart(unit)
	int unit;
{
	register struct devqueue *dq;
	
	dq = scsi_softc[unit].sc_sq.dq_forw;
	(dq->dq_driver->d_go)(dq->dq_unit);
}

int
scsigo(ctlr, slave, unit, bp, cdb, pad)
	int ctlr, slave, unit;
	struct buf *bp;
	struct scsi_fmt_cdb *cdb;
	int pad;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	volatile register struct scsidevice *hd =
				(struct scsidevice *)hs->sc_hc->hp_addr;
	int i, dmaflags;
	u_char phase, ints, cmd;

	cdb->cdb[1] |= unit << 5;

	/* select the SCSI bus (it's an error if bus isn't free) */
	if (issue_select(hd, slave, hs->sc_scsi_addr) || wait_for_select(hd)) {
		if (hs->sc_flags & SCSI_HAVEDMA) {
			hs->sc_flags &=~ SCSI_HAVEDMA;
			dmafree(&hs->sc_dq);
		}
		return (1);
	}
	/*
	 * Wait for a phase change (or error) then let the device
	 * sequence us through command phase (we may have to take
	 * a msg in/out before doing the command).  If the disk has
	 * to do a seek, it may be a long time until we get a change
	 * to data phase so, in the absense of an explicit phase
	 * change, we assume data phase will be coming up and tell
	 * the SPC to start a transfer whenever it does.  We'll get
	 * a service required interrupt later if this assumption is
	 * wrong.  Otherwise we'll get a service required int when
	 * the transfer changes to status phase.
	 */
	phase = CMD_PHASE;
	while (1) {
		register int wait = scsi_cmd_wait;

		switch (phase) {

		case CMD_PHASE:
			if (ixfer_start(hd, cdb->len, phase, wait))
				if (ixfer_out(hd, cdb->len, cdb->cdb))
					goto abort;
			break;

		case MESG_IN_PHASE:
			if (ixfer_start(hd, sizeof(hs->sc_msg), phase, wait)||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY)) {
				ixfer_in(hd, sizeof(hs->sc_msg), hs->sc_msg);
				hd->scsi_scmd = SCMD_RST_ACK;
			}
			phase = BUS_FREE_PHASE;
			break;

		case DATA_IN_PHASE:
		case DATA_OUT_PHASE:
			goto out;

		default:
			printf("scsi%d: unexpected phase %d in go from %d\n",
				hs->sc_hc->hp_unit, phase, slave);
			goto abort;
		}
		while ((ints = hd->scsi_ints) == 0) {
			if (--wait < 0) {
				HIST(sgo_wait, wait)
				goto abort;
			}
			DELAY(1);
		}
		HIST(sgo_wait, wait)
		hd->scsi_ints = ints;
		if (ints & INTS_SRV_REQ)
			phase = hd->scsi_psns & PHASE;
		else if (ints & INTS_CMD_DONE)
			goto out;
		else {
			scsierror(hs, hd, ints);
			goto abort;
		}
	}
out:
	/*
	 * Reset the card dma logic, setup the dma channel then
	 * get the dio part of the card set for a dma xfer.
	 */
	hd->scsi_hconf = 0;
	cmd = CSR_IE;
	dmaflags = DMAGO_NOINT;
	if (scsi_pridma)
		dmaflags |= DMAGO_PRI;
	if (bp->b_flags & B_READ)
		dmaflags |= DMAGO_READ;
	if ((hs->sc_flags & SCSI_DMA32) &&
	    ((int)bp->b_un.b_addr & 3) == 0 && (bp->b_bcount & 3) == 0) {
		cmd |= CSR_DMA32;
		dmaflags |= DMAGO_LWORD;
	} else
		dmaflags |= DMAGO_WORD;
	dmago(hs->sc_dq.dq_ctlr, bp->b_un.b_addr, bp->b_bcount, dmaflags);

	if (bp->b_flags & B_READ) {
		cmd |= CSR_DMAIN;
		phase = DATA_IN_PHASE;
	} else
		phase = DATA_OUT_PHASE;
	/*
	 * DMA enable bits must be set after size and direction bits.
	 */
	hd->scsi_csr = cmd;
	hd->scsi_csr |= (CSR_DE0 << hs->sc_dq.dq_ctlr);
	/*
	 * Setup the SPC for the transfer.  We don't want to take
	 * first a command complete then a service required interrupt
	 * at the end of the transfer so we try to disable the cmd
	 * complete by setting the transfer counter to more bytes
	 * than we expect.  (XXX - This strategy may have to be
	 * modified to deal with devices that return variable length
	 * blocks, e.g., some tape drives.)
	 */
	cmd = SCMD_XFR;
	i = (unsigned)bp->b_bcount;
	if (pad) {
		cmd |= SCMD_PAD;
		/*
		 * XXX - If we don't do this, the last 2 or 4 bytes
		 * (depending on word/lword DMA) of a read get trashed.
		 * It looks like it is necessary for the DMA to complete
		 * before the SPC goes into "pad mode"???  Note: if we
		 * also do this on a write, the request never completes.
		 */
		if (bp->b_flags & B_READ)
			i += 2;
#ifdef DEBUG
		hs->sc_flags |= SCSI_PAD;
		if (i & 1)
			printf("scsi%d: odd byte count: %d bytes @ %d\n",
				ctlr, i, bp->b_cylin);
#endif
	} else
		i += 4;
	hd->scsi_tch = i >> 16;
	hd->scsi_tcm = i >> 8;
	hd->scsi_tcl = i;
	hd->scsi_pctl = phase;
	hd->scsi_tmod = 0;
	hd->scsi_scmd = cmd;
	hs->sc_flags |= SCSI_IO;
	return (0);
abort:
	scsiabort(hs, hd, "go");
	hs->sc_flags &=~ SCSI_HAVEDMA;
	dmafree(&hs->sc_dq);
	return (1);
}

void
scsidone(unit)
	register int unit;
{
	volatile register struct scsidevice *hd =
			(struct scsidevice *)scsi_softc[unit].sc_hc->hp_addr;

#ifdef DEBUG
	if (scsi_debug)
		printf("scsi%d: done called!\n", unit);
#endif
	/* dma operation is done -- turn off card dma */
	hd->scsi_csr &=~ (CSR_DE1|CSR_DE0);
}

int
scsiintr(unit)
	register int unit;
{
	register struct scsi_softc *hs = &scsi_softc[unit];
	volatile register struct scsidevice *hd =
				(struct scsidevice *)hs->sc_hc->hp_addr;
	register u_char ints;
	register struct devqueue *dq;

	if ((hd->scsi_csr & (CSR_IE|CSR_IR)) != (CSR_IE|CSR_IR))
		return (0);

	ints = hd->scsi_ints;
	if ((ints & INTS_SRV_REQ) && (hs->sc_flags & SCSI_IO)) {
		/*
		 * this should be the normal i/o completion case.
		 * get the status & cmd complete msg then let the
		 * device driver look at what happened.
		 */
#ifdef DEBUG
		int len = (hd->scsi_tch << 16) | (hd->scsi_tcm << 8) |
			  hd->scsi_tcl;
		if (!(hs->sc_flags & SCSI_PAD))
			len -= 4;
		hs->sc_flags &=~ SCSI_PAD;
#endif
		dq = hs->sc_sq.dq_forw;
		finishxfer(hs, hd, dq->dq_slave);
		hs->sc_flags &=~ (SCSI_IO|SCSI_HAVEDMA);
		dmafree(&hs->sc_dq);
		(dq->dq_driver->d_intr)(dq->dq_unit, hs->sc_stat[0]);
	} else {
		/* Something unexpected happened -- deal with it. */
		hd->scsi_ints = ints;
		hd->scsi_csr = 0;
		scsierror(hs, hd, ints);
		scsiabort(hs, hd, "intr");
		if (hs->sc_flags & SCSI_IO) {
			hs->sc_flags &=~ (SCSI_IO|SCSI_HAVEDMA);
			dmafree(&hs->sc_dq);
			dq = hs->sc_sq.dq_forw;
			(dq->dq_driver->d_intr)(dq->dq_unit, -1);
		}
	}
	return(1);
}

void
scsifree(dq)
	register struct devqueue *dq;
{
	register struct devqueue *hq;

	hq = &scsi_softc[dq->dq_ctlr].sc_sq;
	remque(dq);
	if ((dq = hq->dq_forw) != hq)
		(dq->dq_driver->d_start)(dq->dq_unit);
}

/*
 * (XXX) The following routine is needed for the SCSI tape driver
 * to read odd-size records.
 */

#include "st.h"
#if NST > 0
int
scsi_tt_oddio(ctlr, slave, unit, buf, len, b_flags, freedma)
	int ctlr, slave, unit, b_flags, freedma;
	u_char *buf;
	u_int len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb6 cdb;
	u_char iphase;
	int stat;

#ifdef DEBUG
	if (freedma && (hs->sc_flags & SCSI_HAVEDMA) == 0 ||
	    !freedma && (hs->sc_flags & SCSI_HAVEDMA))
		printf("oddio: freedma (%d) inconsistency (flags=%x)\n",
		       freedma, hs->sc_flags);
#endif
	/*
	 * First free any DMA channel that was allocated.
	 * We can't use DMA to do this transfer.
	 */
	if (freedma) {
		hs->sc_flags &=~ SCSI_HAVEDMA;
		dmafree(hs->sc_dq);
	}
	/*
	 * Initialize command block
	 */
	bzero(&cdb, sizeof(cdb));
	cdb.lun = unit;
	cdb.lbam = (len >> 16) & 0xff;
	cdb.lbal = (len >> 8) & 0xff;
	cdb.len = len & 0xff;
	if (buf == 0) {
		cdb.cmd = CMD_SPACE;
		cdb.lun |= 0x00;
		len = 0;
		iphase = MESG_IN_PHASE;
	} else if (b_flags & B_READ) {
		cdb.cmd = CMD_READ;
		iphase = DATA_IN_PHASE;
	} else {
		cdb.cmd = CMD_WRITE;
		iphase = DATA_OUT_PHASE;
	}
	/*
	 * Perform command (with very long delays)
	 */
	scsi_delay(30000000);
	stat = scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, iphase);
	scsi_delay(0);
	return (stat);
}
#endif
#endif
