/*
 * Copyright (c) 1988, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)esp.c	8.2 (Berkeley) %G%
 *
 * from: $Header: esp.c,v 1.28 93/04/27 14:40:44 torek Exp $ (LBL)
 *
 * Loosely derived from Mary Baker's devSCSIC90.c from the Berkeley
 * Sprite project, which is:
 *
 * Copyright 1988 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * from /sprite/src/kernel/dev/sun4c.md/RCS/devSCSIC90.c,v 1.4
 * 90/12/19 12:37:58 mgbaker Exp $ SPRITE (Berkeley)
 */

/*
 * Sbus ESP/DMA driver.  A single driver must be used for both devices
 * as they are physically tied to each other:  The DMA chip can only
 * be used to assist ESP SCSI transactions; the ESP interrupt enable is
 * in the DMA chip csr.
 *
 * Since DMA and SCSI interrupts are handled in the same routine, the
 * DMA device does not declare itself as an sbus device.  This saves
 * some space.
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/device.h>
#include <sys/malloc.h>

#include <dev/scsi/scsi.h>
#include <dev/scsi/scsivar.h>

#include <machine/autoconf.h>
#include <machine/cpu.h>

#include <sparc/sbus/dmareg.h>
#define ESP_PHASE_NAMES
#include <sparc/sbus/espreg.h>
#include <sparc/sbus/sbusvar.h>

#include <libkern/libkern.h>

/*
 * This driver is largely a giant state machine:
 *
 *	Given some previous SCSI state (as set up or tracked by us
 *	earlier) and the interrupt registers provided on the chips
 *	(dmacsr, espstat, espstep, and espintr), derive an action.
 *	In many cases this is just a matter of reading the target's
 *	phase and following its orders, which sets a new state.
 *
 * This sequencing is done in espact(); the state is primed in espselect().
 *
 * Data transfer is always done via DMA.  Unfortunately, there are
 * limits in the DMA and ESP chips on how much data can be moved
 * in a single operation.  The ESP chip has a 16-bit counter, so
 * it is limited to 65536 bytes.  More insidiously, while the DMA
 * chip has a 32-bit address, this is composed of a 24-bit counter
 * with an 8-bit latch, so it cannot cross a 16 MB boundary.  To
 * handle these, we program a smaller count than our caller requests;
 * when this shorter transfer is done, if the target is still up
 * for data transfer, we simply keep going (updating the DMA address)
 * as needed.
 *
 * Another state bit is used to recover from bus resets:
 *
 *	A single TEST UNIT READY is attempted on each target before any
 *	real communication begins; this TEST UNIT READY is allowed to
 *	fail in any way.  This is required for the Quantum ProDrive 100
 *	MB disks, for instance, which respond to their first selection
 *	with status phase, and for anything that insists on implementing
 *	the broken SCSI-2 synch transfer initial message.
 *
 * This is done in espclear() (which calls espselect(); functions that
 * call espselect() must check for clearing first).
 *
 * The state machines actually intermingle, as some SCSI sequences are
 * only allowed during clearing.
 */

/* per-DMA variables */
struct dma_softc {
	struct	device dc_dev;		/* base device */
	volatile struct dmareg *dc_dma;	/* register virtual address */
	int	dc_dmarev;		/* revision */
	char	*dc_dmafmt;		/* format for error messages */
};
void	dmaattach(struct device *, struct device *, void *);
struct cfdriver dmacd =
    { NULL, "dma", matchbyname, dmaattach, DV_DULL, sizeof(struct dma_softc) };

/* per-ESP variables */
struct esp_softc {
	/*
	 * External interfaces.
	 */
	struct	hba_softc sc_hba;	/* base device + hba, must be first */
#define	sc_dev	sc_hba.hba_dev
	struct	sbusdev sc_sd;		/* sbus device */
	struct	intrhand sc_ih;		/* interrupt entry */
	struct	evcnt sc_intrcnt;	/* interrupt counter */
	struct	dma_softc *sc_dc;	/* pointer to corresponding dma sc */

	/*
	 * Addresses mapped to hardware registers.
	 */
	volatile struct espreg *sc_esp;
	volatile struct dmareg *sc_dma;

	/*
	 * Copies of registers cleared/unlatched by reading.
	 * (FIFO flags is not cleared, but we want it for debugging.)
	 */
	u_long	sc_dmacsr;
	u_char	sc_espstat;
	u_char	sc_espstep;
	u_char	sc_espintr;
	u_char	sc_espfflags;

	/* miscellaneous */
	int	sc_clockfreq;		/* clock frequency */
	u_char	sc_sel_timeout;		/* select timeout */
	u_char	sc_id;			/* initiator ID (default = 7) */
	u_char	sc_needclear;		/* uncleared targets (1 bit each) */
	u_char	sc_esptype;		/* 100, 100A, 2xx (see below) */
	u_char	sc_ccf;			/* clock conversion factor */
	u_char	sc_conf1;		/* value for config reg 1 */
	u_char	sc_conf2;		/* value for config reg 2 */
	u_char	sc_conf3;		/* value for config reg 3 */
	struct	bootpath *sc_bp;	/* esp bootpath so far */

	/*
	 * Information pertaining to the current transfer,
	 * including sequencing.
	 *
	 * The size of sc_msg is the size of the ESP fifo,
	 * since we do message-in simply by allowing the fifo to fill.
	 */
	char	sc_probing;		/* used during autoconf; see below */
	char	sc_clearing;		/* true => cmd is just to clear targ */
	char	sc_state;		/* SCSI protocol state; see below */
	char	sc_sentcmd;		/* set once we get cmd out */
	char	sc_dmaactive;		/* true => doing dma */
#ifdef notyet
	u_char	sc_sync;		/* synchronous transfer stuff (?) */
#endif
	u_char	sc_stat[2];		/* status from last `status' phase */
	u_char	sc_msg[16];		/* message from device */
	u_short	sc_dmactl;		/* control to load into dma csr */
	u_long	sc_dmaaddr;		/* address for next xfer */
	int	sc_dmasize;		/* size of current xfer */
	int	sc_resid;		/* count of bytes not yet xferred */
	int	sc_targ;		/* the target involved */
	struct	scsi_cdb *sc_curcdb;	/* ptr to current command */
	/* might cdbspace eventually be per-target? */
	struct	scsi_cdb sc_cdbspace;	/* space for one command */
};

/*
 * Values for sc_esptype (used to control configuration reset, and for
 * workarounds for chip bugs).  The order is important; see espreset().
 */
#define	ESP100	0
#define	ESP100A	1
#define	ESP2XX	2

/*
 * Probe state.  0 means not probing.  While looking for each target
 * we set this to PROBE_TESTING and do a TEST UNIT READY on unit 0.
 * If selection fails, this is changed to PROBE_NO_TARGET; otherwise
 * we assume the target exists, regardless of the result of the test.
 */
#define	PROBE_TESTING	1
#define	PROBE_NO_TARGET	2

/*
 * States in sc_state.
 *
 * Note that S_SVC is rare: normally we load the SCSI command into the
 * ESP fifo and get interrupted only when the device has gone to data
 * or status phase.  If the device wants to play games, though, we end
 * up doing things differently.
 */
char *espstates[] = {
#define	S_IDLE		0	/* not doing anything */
	"idle",
#define	S_SEL		1	/* expecting select done interrupt */
	"selecting",
#define	S_SVC		2	/* expecting service req interrupt */
	"waiting for svc req",
#define	S_DI		3	/* expecting data-in done interrupt */
	"receiving data",
#define	S_DO		4	/* expecting data-out done interrupt */
	"sending data",
#define	S_STAT		5	/* expecting status done interrupt */
	"receiving status",
#define	S_MI		6	/* expecting message-in done interrupt */
	"receiving message",
#define	S_FI		7	/* expecting final disconnect interrupt */
	"waiting for disconnect"
};

/*
 * Hardware limits on transfer sizes (see comments at top).
 */
#define	ESPMAX		(64 * 1024)
#define	DMAMAX(a)	(0x01000000 - ((a) & 0x00ffffff))

/*
 * Return values from espact().
 */
#define	ACT_CONT	0	/* espact() handled everything */
#define	ACT_IO		1	/* espact() is xferring data */
#define	ACT_DONE	2	/* handled everything, and op is now done */
#define	ACT_ERROR	3	/* an error occurred, op has been trashed */
#define	ACT_RESET	4	/* please reset ESP, then do ACT_ERROR */
#define	ACT_QUICKINTR	5	/* another interrupt is expected immediately */

/* autoconfiguration driver */
void	espattach(struct device *, struct device *, void *);
struct cfdriver espcd =
    { NULL, "esp", matchbyname, espattach, DV_DULL, sizeof(struct esp_softc),
      "intr" };

/* Sbus driver */
void	espsbreset(struct device *);

/* interrupt interface */
int	espintr(void *);

/* SCSI HBA driver */
int	espicmd(struct hba_softc *, int, struct scsi_cdb *, caddr_t, int, int);
int	espdump(struct hba_softc *, int, struct scsi_cdb *, caddr_t, int);
void	espstart(struct device *, struct sq *, struct buf *,
		scdgo_fn, struct device *);
int	espgo(struct device *, int, scintr_fn, struct device *,
		struct buf *, int);
void	esprel(struct device *);
void	esphbareset(struct hba_softc *, int);
static struct hbadriver esphbadriver =
    { espicmd, espdump, espstart, espgo, esprel, esphbareset };

/* other prototypes */
static void espdoattach(int);
static void dmareset(struct esp_softc *);
static void espreset(struct esp_softc *, int);
static void esperror(struct esp_softc *, const char *);
static int espact(struct esp_softc *);
void espselect(struct esp_softc *, int, struct scsi_cdb *);

/* second arg to espreset() */
#define	RESET_ESPCHIP	0x1
#define	RESET_SCSIBUS	0x2
#define	RESET_BOTH	(RESET_ESPCHIP | RESET_SCSIBUS)

/*
 * Attach a found DMA chip.
 * The second argument is really a pointer to an sbus_attach_args.
 */
void
dmaattach(parent, dev, args)
	struct device *parent;
	struct device *dev;
	void *args;
{
	register struct dma_softc *dc = (struct dma_softc *)dev;
	register struct sbus_attach_args *sa = args;
	register volatile struct dmareg *dma;
	register int rev;
	struct esp_softc *esc;

	if (sa->sa_ra.ra_vaddr)
		dma = (volatile struct dmareg *)sa->sa_ra.ra_vaddr;
	else
		dma = (volatile struct dmareg *)
		    mapiodev(sa->sa_ra.ra_paddr, sizeof(struct dmareg));
	dc->dc_dma = dma;

	switch (rev = DMA_REV(dma->dma_csr)) {
	case DMAREV_1:
		printf(": rev 1\n");
		dc->dc_dmafmt = DMA_REV1_BITS;
		break;
	case DMAREV_2:
		printf(": rev 2\n");
		dc->dc_dmafmt = DMA_REV2_BITS;
		break;
	case DMAREV_3:
		printf(": rev 3\n");
		printf("WARNING: esp.c not yet updated for rev 3\n");
		dc->dc_dmafmt = DMA_REV3_BITS;
		break;
	default:
		printf(": unknown revision code 0x%x\n", rev);
		dc->dc_dmafmt = DMA_REV3_BITS;	/* cross fingers */
		break;
	}
	dc->dc_dmarev = rev;
	espdoattach(dc->dc_dev.dv_unit);
}

/*
 * Attach a found ESP chip.  Search for targets; attach each one found.
 * The latter must be deferred if the corresponding dma chip has not yet
 * been configured.
 */
void
espattach(parent, self, args)
	struct device *parent;
	struct device *self;
	void *args;
{
	register struct esp_softc *sc = (struct esp_softc *)self;
	register struct sbus_attach_args *sa = args;
	register volatile struct espreg *esp;
	register struct bootpath *bp;
	int node, pri, freq, t;

	if (sa->sa_ra.ra_nintr != 1) {
		printf(": expected 1 interrupt, got %d\n", sa->sa_ra.ra_nintr);
		return;
	}
	pri = sa->sa_ra.ra_intr[0].int_pri;
	printf(" pri %d", pri);
	if (sa->sa_ra.ra_vaddr)
		esp = (volatile struct espreg *)sa->sa_ra.ra_vaddr;
	else
		esp = (volatile struct espreg *)
		    mapiodev(sa->sa_ra.ra_paddr, sizeof(struct espreg));
	sc->sc_esp = esp;
	node = sa->sa_ra.ra_node;
	sc->sc_id = getpropint(node, "initiator-id", 7);
	freq = getpropint(node, "clock-frequency", -1);
	if (freq < 0)
		freq =
		    ((struct sbus_softc *)sc->sc_dev.dv_parent)->sc_clockfreq;

	/* MIGHT NEED TO RESET ESP CHIP HERE ...? */

	/*
	 * Find out whether we have a -100, -100A, or -2xx,
	 * and what speed it runs at.
	 */
	sc->sc_conf1 = sc->sc_id | ESPCONF1_PARENB;
	/* sc->sc_conf2 = 0; */
	/* sc->sc_conf3 = 0; */
	esp->esp_conf1 = sc->sc_conf1;
	esp->esp_conf2 = 0;
	esp->esp_conf2 = ESPCONF2_SCSI2 | ESPCONF2_RPE;
	if ((esp->esp_conf2 & ~ESPCONF2_RSVD) !=
	    (ESPCONF2_SCSI2 | ESPCONF2_RPE)) {
		printf(": ESP100");
		sc->sc_esptype = ESP100;
	} else {
		esp->esp_conf2 = 0;
		esp->esp_conf3 = 0;
		esp->esp_conf3 = 5;
		if (esp->esp_conf3 != 5) {	/* XXX def bits */
			printf(": ESP100A");
			sc->sc_esptype = ESP100A;
		} else {
			esp->esp_conf3 = 0;
			printf(": ESP2XX");
			sc->sc_esptype = ESP2XX;
		}
	}
	printf(", clock = %s MHz, ID = %d\n", clockfreq(freq), sc->sc_id);

	/*
	 * Set clock conversion factor and select timeout.
	 * N.B.: clock frequency is not actually used in the rest
	 * of the driver; I calculate it here for completeness only
	 * (so I can see it when debugging).
	 */
	sc->sc_clockfreq = freq;
	freq = howmany(freq, 1000 * 1000);	/* convert to MHz */
	t = ESPCCF_FROMMHZ(freq);
	if (t < ESPCCF_MIN)
		t = ESPCCF_MIN;
	sc->sc_ccf = t;
	t = ESPTIMO_REGVAL(250, t, freq);	/* timeout = 250 ms. */
	if (t >= 256)
		t = 0;
	sc->sc_sel_timeout = t;

	/*
	 * Link into sbus; set interrupt handler.
	 */
	sc->sc_sd.sd_reset = espsbreset;
	sbus_establish(&sc->sc_sd, &sc->sc_dev);
	sc->sc_ih.ih_fun = espintr;
	sc->sc_ih.ih_arg = sc;
	intr_establish(pri, &sc->sc_ih);
	evcnt_attach(&sc->sc_dev, "intr", &sc->sc_intrcnt);

#define SAME_ESP(bp, sa) \
	((bp->val[0] == sa->sa_slot && bp->val[1] == sa->sa_offset) || \
	 (bp->val[0] == -1 && bp->val[1] == sc->sc_dev.dv_unit))

	bp = sa->sa_ra.ra_bp;
	if (bp != NULL && strcmp(bp->name, "esp") == 0 && SAME_ESP(bp, sa))
		sc->sc_bp = bp + 1;
	espdoattach(sc->sc_dev.dv_unit);
}

/*
 * `Final' attach of esp occurs once esp and dma chips have been found
 * and assigned virtual addresses.  Set up the ESP SCSI data structures
 * and probe the SCSI bus.
 */
static void
espdoattach(unit)
	int unit;
{
	register struct esp_softc *sc;
	register struct dma_softc *dc;
	register struct bootpath *bp;
	register struct targ *t;
	register int targ, u;

	/* make sure we have both */
	if (espcd.cd_ndevs <= unit ||
	    dmacd.cd_ndevs <= unit ||
	    (sc = espcd.cd_devs[unit]) == NULL ||
	    (dc = dmacd.cd_devs[unit]) == NULL)
		return;
	sc->sc_dc = dc;
	sc->sc_dma = dc->dc_dma;
	sc->sc_hba.hba_driver = &esphbadriver;

	sc->sc_dma->dma_csr = 0;	/* ??? */
	espreset(sc, RESET_ESPCHIP);

	/* MAYBE THIS SHOULD BE MOVED TO scsi_subr.c? */
	for (targ = 0; targ < 8; targ++) {
		if (targ == sc->sc_id)
			continue;
		sc->sc_probing = PROBE_TESTING;
		sc->sc_clearing = 1;
		(void)scsi_test_unit_ready(&sc->sc_hba, targ, 0);
		if (sc->sc_probing != PROBE_NO_TARGET) {
			sc->sc_probing = 0;
			sc->sc_clearing = 0;
			SCSI_FOUNDTARGET(&sc->sc_hba, targ);
		}
	}
	sc->sc_probing = 0;
	sc->sc_clearing = 0;

	/*
	 * See if we booted from a unit on this target.  We could
	 * compare bp->name against the unit's name but there's no
	 * real need since a target and unit uniquely specify a
	 * scsi device.
	 */
	if ((bp = sc->sc_bp) != NULL && (u_int)(targ = bp->val[0]) < 8 &&
	    (u_int)(u = bp->val[1]) < 8 &&
	    (t = sc->sc_hba.hba_targets[targ]) != NULL && t->t_units[u] != NULL)
		bootdv = t->t_units[u]->u_dev;
}

/*
 * We are not allowed to touch the DMA "flush" and "drain" bits
 * while it is still thinking about a request (DMA_RP).
 */
#define	DMAWAIT(dma)	while ((dma)->dma_csr & DMA_RP) DELAY(1)

/*
 * Reset the DMA chip.
 */
static void
dmareset(sc)
	struct esp_softc *sc;
{
	register volatile struct dmareg *dma = sc->sc_dma;

	DMAWAIT(dma);
	dma->dma_csr |= DMA_RESET;
	DELAY(200);
	dma->dma_csr &= ~DMA_RESET;	/* ??? */
	sc->sc_state = S_IDLE;
	sc->sc_dmaactive = 0;
	if (sc->sc_dc->dc_dmarev == DMAREV_2 && sc->sc_esptype != ESP100)
		dma->dma_csr |= DMA_TURBO;
	dma->dma_csr |= DMA_IE;		/* enable interrupts */
	DELAY(200);
}

/*
 * Reset the chip and/or SCSI bus (always resets DMA).
 */
static void
espreset(sc, how)
	register struct esp_softc *sc;
	int how;
{
	register volatile struct espreg *esp = sc->sc_esp;

	dmareset(sc);
	if (how & RESET_ESPCHIP) {
		esp->esp_cmd = ESPCMD_RESET_CHIP;
		esp->esp_cmd = ESPCMD_NOP;
		/*
		 * Reload configuration registers (cleared by
		 * RESET_CHIP command).  Reloading conf2 on an
		 * ESP100 goofs it up, so out of paranoia we load
		 * only the registers that exist.
		 */
		esp->esp_conf1 = sc->sc_conf1;
		if (sc->sc_esptype > ESP100) {		/* 100A, 2XX */
			esp->esp_conf2 = sc->sc_conf2;
			if (sc->sc_esptype > ESP100A)	/* 2XX only */
				esp->esp_conf3 = sc->sc_conf3;
		}
		esp->esp_ccf = sc->sc_ccf;
		esp->esp_timeout = sc->sc_sel_timeout;
		/* We set synch offset later. */
	}
	if (how & RESET_SCSIBUS) {
		/*
		 * The chip should retain most of its parameters
		 * (including esp_ccf) across this kind of reset
		 * (see section 3.5 of Emulex documentation).
		 */
		/* turn off scsi bus reset interrupts and reset scsi bus */
		esp->esp_conf1 = sc->sc_conf1 | ESPCONF1_REPORT;
		esp->esp_cmd = ESPCMD_RESET_BUS;
		esp->esp_cmd = ESPCMD_NOP;
		DELAY(100000);	/* ??? */
		(void)esp->esp_intr;
		esp->esp_conf1 = sc->sc_conf1;
	}

	sc->sc_needclear = 0xff;
}

/*
 * Reset the SCSI bus and, optionally, all attached targets.
 */
void
esphbareset(hba, resetunits)
	struct hba_softc *hba;
	int resetunits;
{
	register struct esp_softc *sc = (struct esp_softc *)hba;

	espreset(sc, RESET_SCSIBUS);
	if (resetunits)
		scsi_reset_units(&sc->sc_hba);
}

/*
 * Reset the esp, after an Sbus reset.
 * Also resets corresponding dma chip.
 *
 * THIS ROUTINE MIGHT GO AWAY
 */
void
espsbreset(dev)
	struct device *dev;
{
	struct esp_softc *sc = (struct esp_softc *)dev;

	if (sc->sc_dc) {
		printf(" %s %s", sc->sc_dc->dc_dev.dv_xname,
		    sc->sc_dev.dv_xname);
		esphbareset(&sc->sc_hba, 1);
	}
}

/*
 * Log an error.
 */
static void
esperror(sc, err)
	register struct esp_softc *sc;
	const char *err;
{
	int stat;

	stat = sc->sc_espstat;
	printf(
"%s target %d cmd 0x%x (%s): %s:\n\
\tstat=%b (%s) step=%x dmacsr=%b fflags=%x intr=%b\n",
	    sc->sc_dev.dv_xname, sc->sc_targ, sc->sc_curcdb->cdb_bytes[0],
	    espstates[sc->sc_state], err,
	    stat, ESPSTAT_BITS, espphases[stat & ESPSTAT_PHASE],
	    sc->sc_espstep, sc->sc_dmacsr, sc->sc_dc->dc_dmafmt,
	    sc->sc_espfflags, sc->sc_espintr, ESPINTR_BITS);
}

/*
 * Issue a select, loading command into the FIFO.
 * Return nonzero on error, 0 if OK.
 * Sets state to `selecting'; espact() will sequence state FSM.
 */
void
espselect(sc, targ, cdb)
	register struct esp_softc *sc;
	register int targ;
	register struct scsi_cdb *cdb;
{
	register volatile struct espreg *esp;
	register int i, cmdlen;

	sc->sc_targ = targ;
	sc->sc_state = S_SEL;
	sc->sc_curcdb = cdb;
	sc->sc_sentcmd = 0;
	sc->sc_stat[0] = 0xff;		/* ??? */
	sc->sc_msg[0] = 0xff;		/* ??? */

	/*
	 * Try to talk to target.
	 * Synch offset 0 => asynchronous transfer.
	 */
	esp = sc->sc_esp;
	esp->esp_id = targ;
	esp->esp_syncoff = 0;

	/*
	 * Stuff the command bytes into the fifo.
	 * Select without attention since we do not do disconnect yet.
	 */
	cmdlen = SCSICMDLEN(cdb->cdb_bytes[0]);
	for (i = 0; i < cmdlen; i++)
		esp->esp_fifo = cdb->cdb_bytes[i];
	esp->esp_cmd = ESPCMD_SEL_NATN;
	/* the rest is done elsewhere */
}

/*
 * Sequence through the SCSI state machine.  Return the action to take.
 *
 * Most of the work happens here.
 *
 * There are three interrupt sources:
 *   -- ESP interrupt request (typically, some device wants something).
 *   -- DMA memory error.
 *   -- DMA byte count has reached 0 (we do not often want this one but
 *	can only turn it off in rev 2 DMA chips, it seems).
 *	DOES THIS OCCUR AT ALL HERE?  THERE IS NOTHING TO HANDLE IT!
 */
static int
espact(sc)
	register struct esp_softc *sc;
{
	register volatile struct espreg *esp;
	register volatile struct dmareg *dma;
	register int reg, i, resid, newstate;
	register struct scsi_cdb *cdb;

	dma = sc->sc_dma;
	/* check various error conditions, using as little code as possible */
	if (sc->sc_dmacsr & DMA_EP) {
		esperror(sc, "DMA error");
		DMAWAIT(dma);
		dma->dma_csr |= DMA_FLUSH;
		return (ACT_ERROR);
	}
	reg = sc->sc_espstat;
	if (reg & ESPSTAT_GE) {
		/*
		 * This often occurs when there is no target.
		 * (See DSC code below.)
		 */
		if (sc->sc_espintr & ESPINTR_DSC &&
		    sc->sc_state == S_SEL && sc->sc_probing) {
			sc->sc_probing = PROBE_NO_TARGET;
			return (ACT_RESET);
		}
esperror(sc, "DIAG: gross error (ignored)");
	}
	if (reg & ESPSTAT_PE) {
		esperror(sc, "parity error");
		return (ACT_RESET);
	}
	reg = sc->sc_espintr;
#define ERR (ESPINTR_SBR|ESPINTR_ILC|ESPINTR_RSL|ESPINTR_SAT|ESPINTR_SEL)
	if (reg & ERR) {
		if (reg & ESPINTR_SBR)
			esperror(sc, "scsi bus reset");
		else if (reg & ESPINTR_ILC)
			esperror(sc, "illegal command (driver bug)");
		else {
			printf("%s: target %d", sc->sc_dev.dv_xname,
			    sc->sc_targ);
			if (reg & ESPINTR_RSL)
				printf(" tried to reselect;");
			if (reg & ESPINTR_SAT)
				printf(" selected with ATN;");
			if (reg & ESPINTR_SEL)
				printf(" selected us as target;");
			printf("we do not allow this yet\n");
		}
		return (ACT_ERROR);
	}
#undef ERR

	esp = sc->sc_esp;

	/*
	 * Disconnect currently only allowed in `final interrupt' states.
	 */
	if (reg & ESPINTR_DSC) {
		if (sc->sc_state == S_FI)
			return (ACT_DONE);
		/*
		 * If we were doing a select just to test the existence
		 * of the target, note that it did not respond; otherwise
		 * gripe.
		 */
		if (sc->sc_state == S_SEL) {
			if (sc->sc_probing) {
				sc->sc_probing = PROBE_NO_TARGET;
				return (ACT_RESET);
			}
		}
		/* flush fifo, in case we were selecting or sending data */
		esp->esp_cmd = ESPCMD_FLUSH_FIFO;
		DELAY(1);
		printf("%s: target %d not responding\n",
		    sc->sc_dev.dv_xname, sc->sc_targ);
		return (ACT_ERROR);
	}

	/*
	 * Okay, things are moving along.
	 * What were we doing the last time we did something,
	 * and did it complete normally?
	 */
	switch (sc->sc_state) {

	case S_SEL:
		/*
		 * We were selecting.  Arbitration and select are
		 * complete (because ESPINTR_DSC was not set), but
		 * there is no guarantee the command went out.
		 */
		if ((reg & (ESPINTR_SVC|ESPINTR_CMP)) !=
		    (ESPINTR_SVC|ESPINTR_CMP)) {
			esperror(sc, "selection failed");
			return (ACT_RESET);
		}
		if (sc->sc_espstep == ESPSTEP_DONE) {
			sc->sc_sentcmd = 1;
			break;
		}
		if (sc->sc_espstep == 2) {
			/*
			 * We got something other than command phase.
			 * Just pretend things are normal; the
			 * device will ask for the command later.
			 */
esperror(sc, "DIAG: esp step 2");
		} else if (sc->sc_espstep == 3) {
			/*
			 * Device entered command phase and then exited it
			 * before we finished handing out the command.
			 * Let this happen iff we are trying to clear the
			 * target state.
			 */
esperror(sc, "DIAG: esp step 3");
			if (!sc->sc_clearing)
				return (ACT_RESET);
		} else {
			printf("%s: mysterious esp step %d\n",
			    sc->sc_dev.dv_xname, sc->sc_espstep);
			return (ACT_RESET);
		}

		/*
		 * Part of the command may still be lodged in the FIFO.
		 */
		if (ESP_NFIFO(sc->sc_espfflags)) {
			esp->esp_cmd = ESPCMD_FLUSH_FIFO;
			DELAY(1);
		}
		break;

	case S_SVC:
		/*
		 * We were waiting for phase change after stuffing the command
		 * into the FIFO.  Make sure it got out.
		 */
		if (ESP_NFIFO(sc->sc_espfflags)) {
esperror(sc, "DIAG: CMDSVC, fifo not empty");
			esp->esp_cmd = ESPCMD_FLUSH_FIFO;
			DELAY(1);
		} else
			sc->sc_sentcmd = 1;
		break;

	case S_DI:
		/*
		 * We were doing DMA data in, and expecting a
		 * transfer-count-zero interrupt or a phase change.
		 * We got that; drain the pack register and handle
		 * as for data out -- but ignore FIFO (it should be
		 * empty, except for sync mode which we are not
		 * using anyway).
		 */
		DMAWAIT(dma);
		dma->dma_csr |= DMA_DRAIN;
		DELAY(1);
		resid = 0;
		goto dma_data_done;

	case S_DO:
		/*
		 * We were doing DMA data out.  If there is data in the
		 * FIFO, it is stuff that got DMAed out but never made
		 * it to the device, so it counts as residual.
		 */
		if ((resid = ESP_NFIFO(sc->sc_espfflags)) != 0) {
			esp->esp_cmd = ESPCMD_FLUSH_FIFO;
			DELAY(1);
		}
dma_data_done:
		if (sc->sc_dmaactive == 0) {
			esperror(sc, "dma done w/o dmaactive");
			panic("espact");
		}
		sc->sc_dmaactive = 0;

		/* Finish computing residual count. */
		reg = esp->esp_tcl | (esp->esp_tch << 8);
		if (reg == 0 && (sc->sc_espstat & ESPSTAT_TC) == 0)
			reg = 65536;
		resid += reg;

		/* Compute xfer count (requested - resid). */
		i = sc->sc_dmasize - resid;
		if (i < 0) {
			printf("%s: xfer resid (%d) > xfer req (%d)\n",
			    sc->sc_dev.dv_xname, resid, sc->sc_dmasize);
			i = sc->sc_dmasize;	/* forgiving... */
		}

		/* If data came in we must flush cache. */
		if (sc->sc_state == S_DI)
			cache_flush(sc->sc_dmaaddr, i);
		sc->sc_dmaaddr += i;
		sc->sc_resid -= i;
		if ((sc->sc_espintr & ESPINTR_SVC) == 0) {
			esperror(sc, "no bus service req");
			return (ACT_RESET);
		}
		break;

	case S_STAT:
		/*
		 * The last thing we did was tell it `initiator complete'
		 * and so we expect to have gotten both the status byte
		 * and the final message byte.  It is possible that we
		 * got something else....
		 *
		 * Apparently, BUS SERVICE is set if we got just status,
		 * while FUNCTION COMPLETE is set if we got both.
		 */
		if ((reg & (ESPINTR_SVC|ESPINTR_CMP)) != ESPINTR_CMP) {
			esperror(sc, "bad status interrupt state");
			return (ACT_RESET);
		}
		reg = ESP_NFIFO(sc->sc_espfflags);
		if (reg < 2) {
			printf(
		"%s: command done but fifo count = %d; must be >= 2\n",
			    sc->sc_dev.dv_xname, reg);
			return (ACT_RESET);
		}
		/*
		 * Read the status and the first msg byte.
		 * It should be CMD_COMPLETE.  Eventually we
		 * may handle IDENTIFY, DISCONNECT, etc., as well.
		 */
		sc->sc_stat[0] = esp->esp_fifo;
		sc->sc_msg[0] = reg = esp->esp_fifo;
		esp->esp_cmd = ESPCMD_MSG_ACCEPT;
		if (reg == MSG_CMD_COMPLETE) {
			sc->sc_state = S_FI;
			return (ACT_CONT);
		}
		if (SCSIMSGLEN(reg) != 1) {
			printf("%s: target %d is naughty\n",
			    sc->sc_dev.dv_xname, sc->sc_targ);
			return (ACT_RESET);
		}
		printf("%s: warning: target %d returned msg 0x%x\n",
		    sc->sc_dev.dv_xname, sc->sc_targ, reg);
		sc->sc_state = S_FI;
		return (ACT_CONT);

	case S_MI:
		if ((reg & ESPINTR_SVC) == 0) {
			esperror(sc, "missing phase after msg in");
			return (ACT_RESET);
		}
		reg = ESP_NFIFO(sc->sc_espfflags);
		for (i = 0; i < reg; i++)
			sc->sc_msg[i] = esp->esp_fifo;
		break;

	case S_FI:
		esperror(sc, "target did not disconnect");
		return (ACT_RESET);
	}

	/*
	 * Things are still moving along.  The phase tells us
	 * what the device wants next.  Do it.
	 */
	switch (sc->sc_espstat & ESPSTAT_PHASE) {

	case ESPPHASE_DATA_OUT:
if (!sc->sc_sentcmd) esperror(sc, "DIAG: data out without command");
		if (sc->sc_dmactl & DMA_READ) {
			esperror(sc, "wrong phase (want to read)");
			return (ACT_RESET);
		}
		newstate = S_DO;
		goto do_data_xfer;

	case ESPPHASE_DATA_IN:
if (!sc->sc_sentcmd) esperror(sc, "DIAG: data in without command");
		if (!(sc->sc_dmactl & DMA_READ)) {
			esperror(sc, "wrong phase (want to write)");
			return (ACT_RESET);
		}
		newstate = S_DI;
do_data_xfer:
		if (sc->sc_resid == 0) {
			esperror(sc, "data count error");
			return (ACT_RESET);
		}

		/*
		 * Compute DMA count based on chip limits.
		 * Set DMA address and load transfer count into
		 * ESP via DMA NOP, then set DMA control, and
		 * then we can start the DMA.
		 */
		sc->sc_state = newstate;
		i = min(sc->sc_resid, ESPMAX);
		i = min(i, DMAMAX(sc->sc_dmaaddr));
		sc->sc_dmasize = i;
		dma->dma_addr = sc->sc_dmaaddr;
		esp->esp_tch = i >> 8;
		esp->esp_tcl = i;
		esp->esp_cmd = ESPCMD_DMA | ESPCMD_NOP;
		dma->dma_csr = sc->sc_dmactl;
		sc->sc_dmaactive = 1;
		esp->esp_cmd = ESPCMD_DMA | ESPCMD_XFER_INFO;
		return (ACT_IO);

	case ESPPHASE_CMD:
		/*
		 * Silly thing wants the command again.
		 * Load it into the FIFO and go to SVC state.
		 */
printf("%s: redoing command\n", sc->sc_dev.dv_xname);
		cdb = sc->sc_curcdb;
		reg = SCSICMDLEN(cdb->cdb_bytes[0]);
		for (i = 0; i < reg; i++)
			esp->esp_fifo = cdb->cdb_bytes[i];
		sc->sc_state = S_SVC;
		esp->esp_cmd = ESPCMD_XFER_INFO;
		return (ACT_CONT);

	case ESPPHASE_STATUS:
		sc->sc_state = S_STAT;
		esp->esp_cmd = ESPCMD_INIT_COMP;
		return (ACT_CONT);

	case ESPPHASE_MSG_IN:
printf("%s: accepting (& ignoring) msg from target %d\n",
    sc->sc_dev.dv_xname, sc->sc_targ);
		sc->sc_state = S_MI;
		esp->esp_cmd = ESPCMD_MSG_ACCEPT;
		return (ACT_CONT);

	default:
		esperror(sc, "bad phase");
		return (ACT_RESET);
	}
	/* NOTREACHED */
}

/*
 * Clear out target state by doing a special TEST UNIT READY.
 * Note that this calls espicmd (possibly recursively).
 */
void
espclear(sc, targ)
	register struct esp_softc *sc;
	register int targ;
{

	/* turn off needclear immediately since this calls espicmd() again */
	sc->sc_needclear &= ~(1 << targ);
	sc->sc_clearing = 1;
	(void) scsi_test_unit_ready(&sc->sc_hba, targ, 0);
	sc->sc_clearing = 0;
}

/*
 * THIS SHOULD BE ADJUSTABLE
 */
	/* name		howlong		purpose */
#define	SELECT_WAIT	300000		/* wait for select to complete */
#define	CMD_WAIT	100000		/* wait for next phase, generic */
#define	DATA_WAIT	100000		/* time to xfer data in/out */

/*
 * Send an `immediate' command, i.e., poll until the whole thing is done.
 * Return the status byte from the device, or -1 if we timed out.  We use
 * DMA to transfer the data as the fifo only moves one byte at a time.
 */
int
espicmd(hba, targ, cdb, buf, len, rw)
	struct hba_softc *hba;
	int targ;
	struct scsi_cdb *cdb;
	caddr_t buf;
	int len, rw;
{
	register struct esp_softc *sc = (struct esp_softc *)hba;
	register volatile struct espreg *esp = sc->sc_esp;
	register volatile struct dmareg *dma = sc->sc_dma;
	register int r, wait;

	/*
	 * Clear the target if necessary.
	 */
	if (sc->sc_needclear & (1 << targ) && !sc->sc_probing)
		espclear(sc, targ);

	/*
	 * Set up DMA transfer control (leaving interrupts disabled).
	 */
	sc->sc_dmactl = rw & B_READ ? DMA_ENA | DMA_READ : DMA_ENA;
	sc->sc_dmaaddr = (u_long)buf;
	sc->sc_resid = len;

	/*
	 * Disable hardware interrupts and start select sequence,
	 * then loop, calling espact() after each ``interrupt''.
	 */
	DMAWAIT(dma);		/* ??? */
	dma->dma_csr = 0;
	espselect(sc, targ, cdb);
	wait = SELECT_WAIT;
	for (;;) {
		r = dma->dma_csr;
		if (!DMA_INTR(r)) {
			if (--wait < 0) {
				esperror(sc, "timeout");
				goto reset;
			}
			DELAY(1);
			continue;
		}
		sc->sc_espstat = esp->esp_stat;
		sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
		sc->sc_espintr = esp->esp_intr;
		sc->sc_espfflags = esp->esp_fflags;
		sc->sc_dmacsr = r;
		switch (r = espact(sc)) {

		case ACT_CONT:
		case ACT_QUICKINTR:
			wait = CMD_WAIT;
			break;

		case ACT_IO:
			wait = DATA_WAIT;
			break;

		case ACT_RESET:
			sc->sc_state = S_IDLE;
			goto reset;

		case ACT_DONE:
			sc->sc_state = S_IDLE;
			return (sc->sc_stat[0]);

		case ACT_ERROR:
			sc->sc_state = S_IDLE;
			return (-1);

		default:
			panic("espicmd action");
		}
	}
reset:
	espreset(sc, RESET_ESPCHIP);		/* ??? */
	return (-1);
}

/*
 * Dump (write memory, possibly physmem).
 * SPARC higher-level dump code always provides virtual addresses,
 * so we need not do any I/O mapping here.
 */
int
espdump(hba, targ, cdb, buf, len)
	register struct hba_softc *hba;
	int targ;
	struct scsi_cdb *cdb;
	caddr_t buf;
	register int len;
{

	return (espicmd(hba, targ, cdb, buf, len, B_WRITE));
}

/*
 * Allocate resources (SCSI bus and DVMA space) for the given transfer.
 * Must be called at splbio().
 *
 * THIS SHOULD RETURN SUCCESS/FAIL INDICATION
 */
void
espstart(self, sq, bp, dgo, dev)
	struct device *self;
	register struct sq *sq;
	struct buf *bp;
	scdgo_fn dgo;
	struct device *dev;
{
	register struct esp_softc *sc = (struct esp_softc *)self;

	if (sc->sc_hba.hba_busy == 0) {
		/*
		 * Bus not busy, nothing to do here, just tell
		 * this target or unit that it has the SCSI bus.
		 */
		sc->sc_hba.hba_busy = 1;
		(*dgo)(dev, &sc->sc_cdbspace);
	} else {
		/*
		 * Bus is busy; just enqueue.
		 */
		sq->sq_dgo = dgo;
		sq->sq_dev = dev;
		sq->sq_forw = NULL;
		if (sc->sc_hba.hba_head == NULL)
			sc->sc_hba.hba_head = sq;
		else
			sc->sc_hba.hba_tail->sq_forw = sq;
		sc->sc_hba.hba_tail = sq;
	}
}

/*
 * Start buffered I/O.
 * Return 0 on success, 1 on failure.
 */
int
espgo(self, targ, intr, dev, bp, pad)
	struct device *self;
	int targ;
	scintr_fn intr;
	struct device *dev;
	register struct buf *bp;
	int pad;
{
	register struct esp_softc *sc = (struct esp_softc *)self;

	if (sc->sc_needclear & (1 << targ))
		espclear(sc, targ);

	/* Set up dma control for espact(). */
	sc->sc_dmactl = bp->b_flags & B_READ ?
	    DMA_ENA | DMA_READ | DMA_IE : DMA_ENA | DMA_IE;
	sc->sc_dmaaddr = (u_long)bp->b_un.b_addr;
	sc->sc_resid = bp->b_bcount;

	/*
	 * Enable interrupts and start selection.
	 * The rest is done in espintr() and espact().
	 */
	sc->sc_hba.hba_intr = intr;	/* remember dev done function */
	sc->sc_hba.hba_intrdev = dev;	/* and its first arg */
	sc->sc_dma->dma_csr = DMA_IE;
	espselect(sc, targ, &sc->sc_cdbspace);
	return (0);
}

/*
 * Handle interrupt.  Return 1 if taken.
 */
int
espintr(sc0)
	void *sc0;
{
	register struct esp_softc *sc = (struct esp_softc *)sc0;
	register volatile struct espreg *esp = sc->sc_esp;
	register volatile struct dmareg *dma = sc->sc_dma;
	register int r, wait;
	register struct sq *sq;

	r = dma->dma_csr;
	if (!DMA_INTR(r))
		return (0);		/* not ours */
	sc->sc_intrcnt.ev_count++;

again:
	sc->sc_espstat = esp->esp_stat;
	sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
	sc->sc_espintr = esp->esp_intr;
	sc->sc_espfflags = esp->esp_fflags;
	sc->sc_dmacsr = r;

	if (sc->sc_state == S_IDLE) {
		printf("%s: stray interrupt\n", sc->sc_dev.dv_xname);
		dma->dma_csr &= ~DMA_IE;	/* ??? */
		return (1);
	}
	switch (r = espact(sc)) {

	case ACT_CONT:		/* just return */
	case ACT_IO:
		break;

	case ACT_RESET:		/* please reset esp */
reset:
		espreset(sc, RESET_ESPCHIP);	/* ??? */
		/* FALLTHROUGH */

	case ACT_DONE:		/* this one is done, successfully */
	case ACT_ERROR:		/* this one is done due to `severe' error */
		sc->sc_state = S_IDLE;
		if (!sc->sc_hba.hba_busy)
			panic("espintr sq");
		/*
		 * This transaction is done.
		 * Call the driver's intr routine,
		 * then start the next guy if any.
		 */
		(*sc->sc_hba.hba_intr)(sc->sc_hba.hba_intrdev,
		    r == ACT_DONE ? sc->sc_stat[0] : -1, sc->sc_resid);
		if ((sq = sc->sc_hba.hba_head) != NULL) {
			sc->sc_hba.hba_head = sq->sq_forw;
			(*sq->sq_dgo)(sq->sq_dev, &sc->sc_cdbspace);
		} else
			sc->sc_hba.hba_busy = 0;
		break;

	case ACT_QUICKINTR:	/* wait a short while for another interrupt */
printf("%s: quickintr: ", sc->sc_dev.dv_xname);
		wait = 100;
		do {
			r = dma->dma_csr;
			if (DMA_INTR(r)) {
printf("got one, wait=%d\n", wait);
				goto again;
			}
		} while (--wait > 0);
printf("did not get one\n");
		break;

	default:
		panic("espintr action");
	}
	return (1);
}

/*
 * Target or unit decided to let go of the bus early.
 */
void
esprel(self)
	struct device *self;
{
	register struct esp_softc *sc = (struct esp_softc *)self;
	register struct sq *sq;

	/* if there is someone else waiting, give them a crack at it */
	if ((sq = sc->sc_hba.hba_head) != NULL)
		(*sq->sq_dgo)(sq->sq_dev, &sc->sc_cdbspace);
	else
		sc->sc_hba.hba_busy = 0;
}
