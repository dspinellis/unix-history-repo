/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and the Systems
 * Programming Group of the University of Utah Computer Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: scsi.c 1.3 90/01/27$
 *
 *	@(#)scsi.c	7.2 (Berkeley) %G%
 */

/*
 * SCSI bus driver for standalone programs.
 */

#include "sys/types.h"
#include "sys/reboot.h"
#include "../dev/device.h"
#include "../dev/scsireg.h"
#include "scsivar.h"

#include "saio.h"
#include "samachdep.h"

struct	scsi_softc scsi_softc[NSCSI];

#define	scsiunit(x)	((x) >> 3)
#define	scsislave(x)	((x) & 7)

void scsireset();
int scsi_cmd_wait = 500;
int scsi_data_wait = 300000;

scsiinit()
{
	extern struct hp_hw sc_table[];
	register struct hp_hw *hw;
	register struct scsi_softc *hs;
	register int i, addr;
	static int first = 1;
	
	i = 0;
	for (hw = sc_table; i < NSCSI && hw < &sc_table[MAX_CTLR]; hw++) {
		if (hw->hw_type != SCSI)
			continue;
		hs = &scsi_softc[i];
		hs->sc_addr = hw->hw_addr;
		scsireset(i);
		if (howto & RB_ASKNAME)
			printf("scsi%d at sc%d\n", i, hw->hw_sc);
		/*
		 * Adjust devtype on first call.  This routine assumes that
		 * adaptor is in the high byte of devtype.
		 */
		if (first && ((devtype >> 24) & 0xff) == hw->hw_sc) {
			devtype = (devtype & 0x00ffffff) | (i << 24);
			first = 0;
		}
		hs->sc_alive = 1;
		i++;
	}
}

scsialive(unit)
	register int unit;
{
	unit = scsiunit(unit);
	if (unit >= NSCSI || scsi_softc[unit].sc_alive == 0)
		return (0);
	return (1);
}

void
scsireset(unit)
	register int unit;
{
	volatile register struct scsidevice *hd;
	register struct scsi_softc *hs;
	u_int i;

	unit = scsiunit(unit);
	hs = &scsi_softc[unit];
	hd = (struct scsidevice *)hs->sc_addr;
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
	else
		hd->scsi_sctl = SCTL_DISABLE | SCTL_ABRT_ENAB |
				SCTL_SEL_ENAB | SCTL_RESEL_ENAB |
				SCTL_INTR_ENAB;
	hd->scsi_sctl &=~ SCTL_DISABLE;
}


int
scsiabort(hs, hd)
	register struct scsi_softc *hs;
	volatile register struct scsidevice *hd;
{
	printf("scsi error: scsiabort\n");
	return (0);
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
		if (hd->scsi_ints || --wait < 0)
			return (0);
		DELAY(1);
	}
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
			if (hd->scsi_ints || --wait < 0)
				return (len);
			DELAY(1);
		}
		hd->scsi_dreg = *buf++;
	}
	return (0);
}

static int
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
				return (len);
			}
			DELAY(1);
		}
		*buf++ = hd->scsi_dreg;
	}
	return (len);
}

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
				(struct scsidevice *)hs->sc_addr;
	int i;
	u_char phase, ints;
	register int wait;

	/* select the SCSI bus (it's an error if bus isn't free) */
	if (issue_select(hd, target, hs->sc_scsi_addr))
		return (0);
	if (wait_for_select(hd))
		return (0);
	/*
	 * Wait for a phase change (or error) then let the device
	 * sequence us through the various SCSI phases.
	 */
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
			if (ixfer_start(hd, len, phase, wait))
				if (ixfer_out(hd, len, buf))
					goto abort;
			phase = STATUS_PHASE;
			break;

		case STATUS_PHASE:
			wait = scsi_data_wait;
			if (ixfer_start(hd, sizeof(hs->sc_stat), phase, wait) ||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY))
				ixfer_in(hd, sizeof(hs->sc_stat), &hs->sc_stat);
			phase = MESG_IN_PHASE;
			break;

		case MESG_IN_PHASE:
			if (ixfer_start(hd, sizeof(hs->sc_msg), phase, wait) ||
			    !(hd->scsi_ssts & SSTS_DREG_EMPTY)) {
				ixfer_in(hd, sizeof(hs->sc_msg), &hs->sc_msg);
				hd->scsi_scmd = SCMD_RST_ACK;
			}
			phase = BUS_FREE_PHASE;
			break;

		case BUS_FREE_PHASE:
			return (1);

		default:
			printf("unexpected scsi phase %d\n", phase);
			goto abort;
		}
		/* wait for last command to complete */
		while ((ints = hd->scsi_ints) == 0) {
			if (--wait < 0)
				goto abort;
			DELAY(1);
		}
		hd->scsi_ints = ints;
		if (ints & INTS_SRV_REQ)
			phase = hd->scsi_psns & PHASE;
		else if (ints & INTS_DISCON)
			return (1);
		else if ((ints & INTS_CMD_DONE) == 0) {
			goto abort;
		}
	}
abort:
	scsiabort(hs, hd);
	return (0);
}

int
scsi_test_unit_rdy(unit)
{
	int ctlr = scsiunit(unit);
	int slave = scsislave(unit);
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_TEST_UNIT_READY };

	if (scsiicmd(hs, slave, &cdb, sizeof(cdb), (u_char *)0, 0,
		     STATUS_PHASE) == 0)
		return (0);
		
	return (hs->sc_stat == 0);
}

int
scsi_request_sense(unit, buf, len)
	int unit;
	u_char *buf;
	unsigned len;
{
	int ctlr = scsiunit(unit);
	int slave = scsislave(unit);
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_REQUEST_SENSE };

	cdb.len = len;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_IN_PHASE));
}

int
scsi_read_capacity(unit, buf, len)
	int unit;
	u_char *buf;
	unsigned len;
{
	int ctlr = scsiunit(unit);
	int slave = scsislave(unit);
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb10 cdb = { CMD_READ_CAPACITY };

	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_IN_PHASE));
}

int
scsi_tt_read(unit, buf, len, blk, nblk)
	int unit;
	u_char *buf;
	u_int len;
	daddr_t blk;
	u_int nblk;
{
	int ctlr = scsiunit(unit);
	int slave = scsislave(unit);
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;
	int stat;

	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_READ_EXT;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = nblk >> (8 + DEV_BSHIFT);
	cdb.lenl = nblk >> DEV_BSHIFT;
	stat = scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_IN_PHASE);
	if (stat == 0)
		return (1);
	return (hs->sc_stat);
}

int
scsi_tt_write(unit, buf, len, blk, nblk)
	int unit;
	u_char *buf;
	u_int len;
	daddr_t blk;
	u_int nblk;
{
	int ctlr = scsiunit(unit);
	int slave = scsislave(unit);
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;
	int stat;

	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_WRITE_EXT;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = nblk >> (8 + DEV_BSHIFT);
	cdb.lenl = nblk >> DEV_BSHIFT;
	stat = scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len, DATA_OUT_PHASE);
	if (stat == 0)
		return (1);
	return (hs->sc_stat);
}
