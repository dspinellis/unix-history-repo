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
 *	@(#)scsi.c	7.8 (Berkeley) %G%
 */

/*
 * SCSI bus driver for standalone programs.
 */

#include <sys/param.h>
#include <sys/reboot.h>
#include <hp/dev/device.h>
#include <hp300/dev/scsireg.h>
#include <hp300/stand/scsivar.h>

#include <stand.att/saio.h>
#include <hp300/stand/samachdep.h>

struct	scsi_softc scsi_softc[NSCSI];

void scsireset();
int scsi_cmd_wait = 50000;	/* use the "real" driver init_wait value */
int scsi_data_wait = 50000;	/* use the "real" driver init_wait value */

scsiinit()
{
	extern struct hp_hw sc_table[];
	register struct hp_hw *hw;
	register struct scsi_softc *hs;
	register int i, addr;
	static int waitset = 0;
	
	i = 0;
	for (hw = sc_table; i < NSCSI && hw < &sc_table[MAXCTLRS]; hw++) {
		if (!HW_ISSCSI(hw))
			continue;
		hs = &scsi_softc[i];
		hs->sc_addr = hw->hw_kva;
		scsireset(i);
		if (howto & RB_ASKNAME)
			printf("scsi%d at sc%d\n", i, hw->hw_sc);
		hw->hw_pa = (caddr_t) i;	/* XXX for autoconfig */
		hs->sc_alive = 1;
		i++;
	}
	/*
	 * Adjust the wait values
	 */
	if (!waitset) {
		scsi_cmd_wait *= cpuspeed;
		scsi_data_wait *= cpuspeed;
		waitset = 1;
	}
}

scsialive(unit)
	register int unit;
{
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
	printf("scsi%d error: scsiabort\n", hs - scsi_softc);

	scsireset(hs - scsi_softc);
	DELAY(1000000);
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
	register int wait;
	u_char ints;

	wait = scsi_data_wait;
	while ((ints = hd->scsi_ints) == 0) {
		if (--wait < 0)
			return (1);
		DELAY(1);
	}
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
	u_char phase, ints;
	register int wait;

	/* select the SCSI bus (it's an error if bus isn't free) */
	if (issue_select(hd, target, hs->sc_scsi_addr))
		return (-2);
	if (wait_for_select(hd))
		return (-2);
	/*
	 * Wait for a phase change (or error) then let the device
	 * sequence us through the various SCSI phases.
	 */
	hs->sc_stat = -1;
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
			goto out;

		default:
			printf("scsi%d: unexpected scsi phase %d\n",
			       hs - scsi_softc, phase);
			goto abort;
		}
#ifdef SLOWSCSI
		/*
		 * XXX we have wierd transient problems with booting from
		 * slow scsi disks on fast machines.  I have never been
		 * able to pin the problem down, but a large delay here
		 * seems to always work.
		 */
		DELAY(1000);
#endif
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
			goto out;
		else if ((ints & INTS_CMD_DONE) == 0)
			goto abort;
	}
abort:
	scsiabort(hs, hd);
out:
	return (hs->sc_stat);
}

int
scsi_test_unit_rdy(ctlr, slave)
	int ctlr, slave;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_TEST_UNIT_READY };

	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), (u_char *)0, 0,
			 STATUS_PHASE));
}

int
scsi_request_sense(ctlr, slave, buf, len)
	int ctlr, slave;
	u_char *buf;
	unsigned len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb6 cdb = { CMD_REQUEST_SENSE };

	cdb.len = len;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len,
			 DATA_IN_PHASE));
}

int
scsi_read_capacity(ctlr, slave, buf, len)
	int ctlr, slave;
	u_char *buf;
	unsigned len;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	static struct scsi_cdb10 cdb = { CMD_READ_CAPACITY };

	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len,
			 DATA_IN_PHASE));
}

int
scsi_tt_read(ctlr, slave, buf, len, blk, nblk)
	int ctlr, slave;
	u_char *buf;
	u_int len;
	daddr_t blk;
	u_int nblk;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;

	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_READ_EXT;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = nblk >> (8 + DEV_BSHIFT);
	cdb.lenl = nblk >> DEV_BSHIFT;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len,
			 DATA_IN_PHASE));
}

int
scsi_tt_write(ctlr, slave, buf, len, blk, nblk)
	int ctlr, slave;
	u_char *buf;
	u_int len;
	daddr_t blk;
	u_int nblk;
{
	register struct scsi_softc *hs = &scsi_softc[ctlr];
	struct scsi_cdb10 cdb;

	bzero(&cdb, sizeof(cdb));
	cdb.cmd = CMD_WRITE_EXT;
	cdb.lbah = blk >> 24;
	cdb.lbahm = blk >> 16;
	cdb.lbalm = blk >> 8;
	cdb.lbal = blk;
	cdb.lenh = nblk >> (8 + DEV_BSHIFT);
	cdb.lenl = nblk >> DEV_BSHIFT;
	return (scsiicmd(hs, slave, &cdb, sizeof(cdb), buf, len,
			 DATA_OUT_PHASE));
}
