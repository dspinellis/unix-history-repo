/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scsi_subr.c	5.3 (Berkeley) %G%
 *
 * from: $Header: scsi_subr.c,v 1.9 92/11/19 04:18:23 torek Exp $ (LBL)
 */

/*
 * Generic SCSI host adapter driver.
 * Does almost nothing (most work is relegated to per-hba drivers).
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/device.h>

#include <dev/scsi/scsi.h>
#include <dev/scsi/scsivar.h>

/*
 * General subroutines, and scsi data.
 */

/* table of lengths of scsi commands */
const char scsicmdlen[8] = { 6, 10, 0, 0, 0, 12, 0, 0 };

/* table of lengths of scsi messages */
const signed char scsimsglen[0x24] = {
	SMLEN_DONE,		/* MSG_CMD_COMPLETE */
	SMLEN_EXTENDED,		/* MSG_EXT_MESSAGE */
	1,			/* MSG_SAVE_DATA_PTR */
	1,			/* MSG_RESTORE_PTR */
	1,			/* MSG_DISCONNECT */
	1,			/* MSG_INIT_DETECT_ERROR */
	1,			/* MSG_ABORT */
	1,			/* MSG_REJECT */
	1,			/* MSG_NOOP */
	1,			/* MSG_PARITY_ERROR */
	1,			/* MSG_LCC */
	1,			/* MSG_LCCF */
	1,			/* MSG_BUS_DEVICE_RESET */
	1,			/* MSG_ABORT_TAG */
	1,			/* MSG_CLEAR_QUEUE */
	1,			/* MSG_INITIATE_RECOVERY */
	1,			/* MSG_RELEASE_RECOVERY */
	1,			/* MSG_TERMINATE_PROCESS */
	SMLEN_UNDEF,		/* 0x12 */
	SMLEN_UNDEF,		/* 0x13 */
	SMLEN_UNDEF,		/* 0x14 */
	SMLEN_UNDEF,		/* 0x15 */
	SMLEN_UNDEF,		/* 0x16 */
	SMLEN_UNDEF,		/* 0x17 */
	SMLEN_UNDEF,		/* 0x18 */
	SMLEN_UNDEF,		/* 0x19 */
	SMLEN_UNDEF,		/* 0x1a */
	SMLEN_UNDEF,		/* 0x1b */
	SMLEN_UNDEF,		/* 0x1c */
	SMLEN_UNDEF,		/* 0x1d */
	SMLEN_UNDEF,		/* 0x1e */
	SMLEN_UNDEF,		/* 0x1f */
	2,			/* MSG_SIMPLE_QTAG */
	2,			/* MSG_HEAD_QTAG */
	2,			/* MSG_ORDERED_QTAG */
	2,			/* MSG_IGNORE_WIDE_RESID */
};

/* definition of `tg' target driver for autoconfig */
static int scsi_targmatch __P((struct device *, struct cfdata *, void *));
static void scsi_targattach __P((struct device *, struct device *, void *));
struct cfdriver tgcd =
    { NULL, "tg", scsi_targmatch, scsi_targattach,
      DV_DULL, sizeof(struct targ) };

void	scsi_targstart __P((struct device *, struct sq *, struct buf *,
			scdgo_fn, struct device *));
int	scsi_targgo __P((struct device *, int targ,
			scintr_fn, struct device *, struct buf *, int));
void	scsi_targintr __P((struct device *, int, int));
void	scsi_targrel __P((struct device *));

#define	NOBUF	((caddr_t)0)

/*
 * Perform a TEST UNIT READY immediate (polled) command
 * on the given <target,unit> pair.  Return the status byte
 * returned, or -1 for none.
 */
int
scsi_test_unit_ready(hba, targ, unit)
	struct hba_softc *hba;
	int targ, unit;
{
	struct scsi_cdb cdb;

	CDB6(&cdb)->cdb_cmd = CMD_TEST_UNIT_READY;
	CDB6(&cdb)->cdb_lun_lbah = unit << 5;
	*(short *)&CDB6(&cdb)->cdb_lbam = 0;
	*(short *)&CDB6(&cdb)->cdb_len = 0;
	return (hba->hba_driver->hd_icmd(hba, targ, &cdb, NOBUF, 0, 0));
}

/*
 * Request sense.  The sense is to be written into the given buffer.
 * The given length must be < 256.
 */
int
scsi_request_sense(hba, targ, unit, buf, len)
	struct hba_softc *hba;
	int targ, unit;
	caddr_t buf;
	int len;
{
	struct scsi_cdb cdb;

	CDB6(&cdb)->cdb_cmd = CMD_REQUEST_SENSE;
	CDB6(&cdb)->cdb_lun_lbah = unit << 5;
	*(short *)&CDB6(&cdb)->cdb_lbam = 0;
	CDB6(&cdb)->cdb_len = len;
	CDB6(&cdb)->cdb_ctrl = 0;
	return (hba->hba_driver->hd_icmd(hba, targ, &cdb, buf, len, B_READ));
}

/*
 * Called (indirectly, via config_found) from scsi_hbaattach.
 * Print target number, and if no device was configured there,
 * the hba as well.
 */
int
scsi_targprint(aux, hba)
	void *aux;
	char *hba;
{

	if (hba) {
		printf("target %d on %s", *(int *)aux, hba);
		return (UNCONF);
	}
	printf(" target %d", *(int *)aux);
	return (QUIET);
}

/*
 * Print information about a unit found on some target.
 * If the unit was not configured, `targ' is the name of the target
 * on which the unit was found.  If it was, targ is NULL and we
 * let the unit's attach routine print the INQUIRE result if
 * appropriate.
 */
static int
scsi_unitprint(aux, targ)
	void *aux;
	char *targ;
{
	register struct scsi_attach_args *sa = aux;

	if (targ) {
		printf("unit %d at %s", sa->sa_unit, targ);
		if ((sa->sa_inq_status & STS_MASK) == STS_GOOD) {
			printf(" (");
			scsi_printinq(&sa->sa_si);
			printf(")");
		}
		return (UNCONF);
	}
	printf(" unit %d", sa->sa_unit);
	return (QUIET);
}

/*
 * Generic target-match.
 */
static int
scsi_targmatch(parent, cf, aux)
	struct device *parent;
	register struct cfdata *cf;
	void *aux;
{
	int targ = *(int *)aux;

	return (cf->cf_loc[0] == targ || cf->cf_loc[0] == -1);
}

/*
 * And now, a generic `target attach' routine.
 * We assume that INQUIRY works.
 */
static void
scsi_targattach(parent, self, aux)
	struct device *parent, *self;
	void *aux;
{
	register struct targ *t = (struct targ *)self;
	register struct hba_softc *hba;
	register struct hbadriver *hd;
	register int targ, unit;
	struct scsi_attach_args sa;
	struct scsi_cdb si;

	printf("\n");
	t->t_targ = targ = *(int *)aux;
	hba = (struct hba_softc *)parent;
	hba->hba_targets[targ] = t;

	/*
	 * Probe each of the 8 units using the sequence
	 *	TEST UNIT READY
	 *	REQUEST SENSE
	 *	INQUIRY
	 * The first should not be necessary, but some SCSI devices
	 * refuse to speak until it is done.  The second is only necessary
	 * if the first returns a CHECK CONDITION status, but we do it
	 * anyway.
	 */
	hd = hba->hba_driver;
	sa.sa_targ = targ;
	CDB6(&si)->cdb_cmd = CMD_INQUIRY;
	*(short *)&CDB6(&si)->cdb_lbam = 0;
	CDB6(&si)->cdb_len = sizeof sa.sa_si;
	CDB6(&si)->cdb_ctrl = 0;
	for (unit = 0; unit < 8; unit++) {
		if (scsi_test_unit_ready(hba, targ, unit) == -1)
			continue;
		sa.sa_unit = unit;
		sa.sa_req_status = scsi_request_sense(hba, targ, unit,
		    (caddr_t)&sa.sa_sn, sizeof sa.sa_sn);
		CDB6(&si)->cdb_lun_lbah = unit << 5;
		sa.sa_inq_status = (*hd->hd_icmd)(hba, targ, &si,
		    (caddr_t)&sa.sa_si, sizeof sa.sa_si, B_READ);
		if ((sa.sa_inq_status & STS_MASK) == STS_GOOD &&
#ifdef notdef /* XXX don't know if this is a reasonable test */
		    (sa.sa_si.si_type & TYPE_QUAL_MASK) == TYPE_QUAL_NOTCONN &&
#endif
		    (sa.sa_si.si_type & TYPE_TYPE_MASK) == TYPE_NP) {
			continue;
		}
		config_found(&t->t_dev, (void *)&sa, scsi_unitprint);
	}
}

/*
 * Each unit calls scsi_establish to tell the hba and target of
 * its existence.
 */
void
scsi_establish(u, dev, unit)
	register struct unit *u;
	struct device *dev;
	register int unit;
{
	register struct targ *t;
	register struct hba_softc *hba;
	register struct hbadriver *hbd;

	u->u_dev = dev;
	t = (struct targ *)dev->dv_parent;
	hba = (struct hba_softc *)t->t_dev.dv_parent;
	hbd = hba->hba_driver;
	t->t_units[unit] = u;
	if (t->t_nunits == 0) {
		/*
		 * This is the first unit on the target.  We can
		 * probably just call the hba start code, avoiding
		 * one level of calls and queueing.  If we attach
		 * another target we will fix this in the code below.
		 */
		u->u_start = hbd->hd_start;
		u->u_go = hbd->hd_go;
		u->u_rel = hbd->hd_rel;
		u->u_updev = &hba->hba_dev;
		t->t_firstunit = unit;
	} else {
		/*
		 * This is not the only unit on the target, so we
		 * must call the target start code rather than the
		 * hba start code.  Fix the linkage on the first
		 * target too (possibly for the 2nd, 3rd, ..., time).
		 */
		t->t_units[t->t_firstunit]->u_start = scsi_targstart;
		t->t_units[t->t_firstunit]->u_updev = &t->t_dev;
		u->u_start = scsi_targstart;
		u->u_go = scsi_targgo;
		u->u_rel = scsi_targrel;
		u->u_updev = &t->t_dev;
	}
	t->t_nunits++;			/* another unit is alive */
	u->u_unit = unit;
	u->u_targ = t->t_targ;		/* record target number, */
	u->u_hba = hba;			/* hba ... */
	u->u_hbd = hbd;			/* and driver */
}

/* NO DOUBT SOME OF THE STUFF PRINTED HERE IS USELESS */
void
scsi_printinq(inq)
	register struct scsi_inquiry *inq;
{
	register int iso, ecma, ansi, t;
	static char *types[] = { "disk", "tape", "printer", "processor",
	    "WORM", "ROM disk", "scanner", "magneto-optical",
	    "jukebox", "lan" };

	if ((t = (inq->si_type & TYPE_QUAL_MASK)) != 0)
		printf("type-qual=0x%x ", t);
	t = inq->si_type & TYPE_TYPE_MASK;
	if (t < sizeof types / sizeof *types)
		printf("%s", types[t]);
	else
		printf("<type %d>", t);
	if (inq->si_qual & QUAL_RMB)
		printf(" (removable)");
	printf(" qual=0x%x", inq->si_qual & QUAL_MASK);
	iso = (inq->si_qual >> VER_ISO_SHIFT) & VER_ISO_MASK;
	ecma = (inq->si_qual >> VER_ECMA_SHIFT) & VER_ECMA_MASK;
	ansi = (inq->si_qual >> VER_ANSI_SHIFT) & VER_ANSI_MASK;
	printf(" version=<iso %d, ecma %d, ansi %d>", iso, ecma, ansi);
	if (ansi == 1 || ansi == 2) {
		char v[9], p[17], r[5];

		scsi_inq_ansi((struct scsi_inq_ansi *)inq, v, p, r);
		printf(" vendor %s, product %s, rev %s", v, p, r);
	}
}

/* copy a counted string but trim trailing blanks; make the dest a C string */
static void
scsi_str(src, dst, len)
	register char *src, *dst;
	register int len;
{

	while (src[len - 1] == ' ') {
		if (--len == 0) {
			*dst = 0;
			return;
		}
	}
	bcopy(src, dst, len);
	dst[len] = 0;
}

void
scsi_inq_ansi(si, vendor, product, rev)
	register struct scsi_inq_ansi *si;
	char *vendor, *product, *rev;
{
	register int i, len;

	/* if too short, extend with blanks */
	len = si->si_len + 5;	/* 5 fixed; len is `additional' */
	if (len < sizeof(*si))
		for (i = len; i < sizeof *si; i++)
			((char *)si)[i] = ' ';
	scsi_str(si->si_vendor, vendor, sizeof si->si_vendor);
	scsi_str(si->si_product, product, sizeof si->si_product);
	scsi_str(si->si_rev, rev, sizeof si->si_rev);
}

/*
 * Tell all the devices on the given hba that it has been reset.
 * SHOULD PROBABLY DO MORE HERE
 */
void
scsi_reset_units(hba)
	register struct hba_softc *hba;
{
	register int targ, unit;
	register struct targ *t;
	register struct unit *u;

	for (targ = 0; targ < 8; targ++) {
		if ((t = hba->hba_targets[targ]) == NULL)
			continue;
		for (unit = 0; unit < 8; unit++)
			if ((u = t->t_units[unit]) != NULL)
				(*u->u_driver->ud_reset)(u);
	}
}

/*
 * Start a unit on a target.
 * If the target is busy, just enqueue the unit;
 * once the target becomes free, we will call the hba start routine.
 * Otherwise, call the hba start routine now, and then when the hba
 * becomes free it will call the unit's dgo routine.
 */
void
scsi_targstart(self, sq, bp, dgo, dev)
	struct device *self;
	register struct sq *sq;
	struct buf *bp;
	scdgo_fn dgo;
	struct device *dev;
{
	register struct targ *t = (struct targ *)self;
	register struct hba_softc *hba;

	sq->sq_forw = NULL;
	if (t->t_head == NULL)
		t->t_head = sq;
	else
		t->t_tail->sq_forw = sq;
	t->t_tail = sq;
	if (t->t_busy == 0) {
		t->t_busy = 1;
		hba = (struct hba_softc *)t->t_dev.dv_parent;
		(*hba->hba_driver->hd_start)(&hba->hba_dev, &t->t_forw, bp,
		    dgo, dev);
	} else {
		sq->sq_bp = bp;
		sq->sq_dgo = dgo;
		sq->sq_dev = dev;
	}
}

/*
 * The unit got the bus, and wants the hba to go.
 * Remember its interrupt handler; substitute ours instead.
 */
int
scsi_targgo(self, targ, intr, dev, bp, pad)
	struct device *self;
	int targ;
	scintr_fn intr;
	struct device *dev;
	struct buf *bp;
	int pad;
{
	register struct targ *t = (struct targ *)self;
	register struct hba_softc *hba;

	t->t_intr = intr;
	t->t_intrdev = dev;
	hba = (struct hba_softc *)t->t_dev.dv_parent;
	return ((*hba->hba_driver->hd_go)(&hba->hba_dev, targ,
	    scsi_targintr, &t->t_dev, bp, pad));
}

/*
 * The hba got an interrupt.  Dequeue the unit from the target
 * (the target is already off the hba queue) and then call the
 * underlying interrupt handler.
 */
void
scsi_targintr(self, stat, resid)
	struct device *self;
	int stat, resid;
{
	register struct targ *t = (struct targ *)self;
	register struct hba_softc *hba;
	register struct sq *sq;

	sq = t->t_head;
if (sq == NULL) panic("scsi_targintr");
	t->t_head = sq = sq->sq_forw;
	(*t->t_intr)(t->t_intrdev, stat, resid);
	if (sq != NULL) {
		hba = (struct hba_softc *)t->t_dev.dv_parent;
		(*hba->hba_driver->hd_start)(&hba->hba_dev, &t->t_forw,
		    sq->sq_bp, sq->sq_dgo, sq->sq_dev);
	} else
		t->t_busy = 0;
}

/*
 * The unit decided that it needed to `give up' its hold on the bus early.
 */
void
scsi_targrel(self)
	struct device *self;
{
	register struct targ *t = (struct targ *)self;
	register struct hba_softc *hba;
	register struct sq *sq;

	hba = (struct hba_softc *)t->t_dev.dv_parent;
	sq = t->t_head;
if (sq == NULL) panic("scsi_targrel");
	/*
	 * This target is at the head of the hba queue.
	 * Remove it by calling hba bus release.  Then, if the
	 * target queue is not empty, put it back on the hba queue.
	 * (This produces round robin service.)
	 */
	(*hba->hba_driver->hd_rel)(&hba->hba_dev);
	sq = sq->sq_forw;
	if ((t->t_head = sq) != NULL)
		(*hba->hba_driver->hd_start)(&hba->hba_dev, &t->t_forw,
		    sq->sq_bp, sq->sq_dgo, sq->sq_dev);
	else
		t->t_busy = 0;
}
