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
 *	@(#)esp.c	7.4 (Berkeley) %G%
 *
 * from: $Header: esp.c,v 1.22 92/06/17 06:59:33 torek Exp $ (LBL)
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

#ifdef DEBUG
int espdebug = 1;
#endif

/*
 * This driver is organized as a collection of state machines.  The
 * primary machine is the SCSI sequencer:
 *
 *	Given some previous SCSI state (as set up or tracked by us earlier)
 *	and the interrupt registers provided on the chips (dmacsr, espstat,
 *	espstep, and espintr), derive an action.  In many cases this is
 *	just a matter of reading the target's phase and following its orders,
 *	which sets a new state.
 *
 * This sequencing is done in espact(); the state is primed in espselect().
 *
 * There will be (update this comment when there is) another state machine
 * used to handle transfers that fall afoul of chip limits (16 bit DMA
 * counter; 24 bit address counter in 32 bit address field).
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
	struct	device	sc_dev;		/* base device */
	volatile struct dmareg *sc_dma;	/* register virtual address */
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
	struct	sbusdev sc_sd;		/* sbus device */
	struct	intrhand sc_ih;		/* interrupt entry */
	int	sc_interrupts;		/* total number of interrupts taken */
	struct	dma_softc *sc_dsc;	/* pointer to corresponding dma sc */

	/*
	 * Addresses mapped to hardware registers.
	 */
	volatile struct espreg *sc_esp;
	volatile struct dmareg *sc_dma;

	/*
	 * Copies of registers cleared/unlatched by reading.
	 */
	u_long	sc_dmacsr;
	u_char	sc_espstat;
	u_char	sc_espstep;
	u_char	sc_espintr;

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
	u_long	sc_dmaaddr;		/* addr to load into dma addr */
	int	sc_targ;		/* the target involved */
	int	sc_resid;		/* count of bytes not xferred */
	struct	scsi_cdb sc_cdb;	/* current command (not in dvma) */
};

/*
 * Values for sc_esptype (used to control configuration reset).
 * The order is important; see espreset().
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
 * Note that S_CMDSVC is rare: normally we load the SCSI command into the
 * ESP fifo and get interrupted only when the device has gone to data
 * or status phase.  If the device wants to play games, though, we end
 * up doing things differently.
 */
char *espstates[] = {
#define	S_IDLE		0	/* not doing anything */
	"idle",
#define	S_SEL		1	/* expecting select done interrupt */
	"selecting",
#define	S_CMDSVC	2	/* expecting service req interrupt */
	"waiting for service request after command",
#define	S_IOSVC		3	/* expecting service req interrupt */
	"waiting for service request after io",
#define	S_DI		4	/* expecting data-in done interrupt */
	"receiving data",
#define	S_DO		5	/* expecting data-out done interrupt */
	"sending data",
#define	S_STAT		6	/* expecting status done interrupt */
	"receiving status",
#define	S_MI		7	/* expecting message-in done interrupt */
	"receiving message",
#define	S_FI		8	/* expecting final disconnect interrupt */
	"waiting for disconnect"
};

/*
 * Return values from espact().
 */
#define	ACT_CONT	0	/* espact() handled everything */
#define	ACT_READ	1	/* target said it is sending us data */
#define	ACT_WRITE	2	/* target said it is expecting data */
#define	ACT_DONE	3	/* handled everything, and op is now done */
#define	ACT_ERROR	4	/* an error occurred, op has been trashed */
#define	ACT_RESET	5	/* please reset ESP, then do ACT_ERROR */
#define	ACT_QUICKINTR	6	/* another interrupt is expected immediately */

/* autoconfiguration driver */
void	espattach(struct device *, struct device *, void *);
struct cfdriver espcd =
    { NULL, "esp", matchbyname, espattach, DV_DULL, sizeof(struct esp_softc) };

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

/* forward declarations */
static void espdoattach(int unit);
static void espreset(struct esp_softc *);

/*
 * The transfer size is limited to 16 bits since the scsi ctrl transfer
 * counter is only 2 bytes.  A 0 value means the biggest transfer size
 * (2 ** 16) == 64k.
 */
#define MAX_TRANSFER_SIZE	(64 * 1024)

/* Return true if this transfer will cross a dma boundary */
#define CROSS_DMA(addr, len) \
    (((int)(addr) & 0xff000000) != (((int)(addr) + (len) - 1) & 0xff000000))

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
	register struct dma_softc *dsc = (struct dma_softc *)dev;
	register struct sbus_attach_args *sa = args;
	register volatile struct dmareg *dma;
	register int rev;
	struct esp_softc *esc;

	if (sa->sa_ra.ra_vaddr)
		dma = (volatile struct dmareg *)sa->sa_ra.ra_vaddr;
	else
		dma = (volatile struct dmareg *)
		    mapiodev(sa->sa_ra.ra_paddr, sizeof(struct dmareg));
	dsc->sc_dma = dma;

	switch (rev = DMA_REV(dma->dma_csr)) {
	case DMAREV_1:
		printf(": rev 1\n");
		break;
	case DMAREV_2:
		printf(": rev 2\n");
		break;
	default:
		printf(": unknown revision %d\n", rev);
		break;
	}
	espdoattach(dsc->sc_dev.dv_unit);
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
	struct dma_softc *dsc;
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
		freq = ((struct sbus_softc *)sc->sc_hba.hba_dev.dv_parent)->sc_clockfreq;

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
	sbus_establish(&sc->sc_sd, &sc->sc_hba.hba_dev);
	sc->sc_ih.ih_fun = espintr;
	sc->sc_ih.ih_arg = sc;
	intr_establish(pri, &sc->sc_ih);
	espdoattach(sc->sc_hba.hba_dev.dv_unit);
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
	register struct dma_softc *dsc;
	register int targ;

	/* make sure we have both */
	if (espcd.cd_ndevs <= unit ||
	    dmacd.cd_ndevs <= unit ||
	    (sc = espcd.cd_devs[unit]) == NULL ||
	    (dsc = dmacd.cd_devs[unit]) == NULL)
		return;
	sc->sc_dsc = dsc;
	sc->sc_dma = dsc->sc_dma;
	sc->sc_hba.hba_driver = &esphbadriver;

	espreset(sc);

	/* MAYBE THIS SHOULD BE MOVED TO scsi_subr.c? */
	for (targ = 0; targ < 8; targ++) {
		if (targ == sc->sc_id)
			continue;
		sc->sc_probing = PROBE_TESTING;
		sc->sc_clearing = 1;
		(void) scsi_test_unit_ready(&sc->sc_hba, targ, 0);
		if (sc->sc_probing != PROBE_NO_TARGET) {
			sc->sc_probing = 0;
			sc->sc_clearing = 0;
			SCSI_FOUNDTARGET(&sc->sc_hba, targ);
		}
	}
	sc->sc_probing = 0;
	sc->sc_clearing = 0;
}

/*
 * Internal DMA reset.
 */
static void
dmareset(sc)
	struct esp_softc *sc;
{
	register volatile struct dmareg *dma = sc->sc_dma;

	/* reset DMA chip */
	dma->dma_csr |= DMA_RESET;
	DELAY(200);
	dma->dma_csr &= ~DMA_RESET;	/* ??? */
	sc->sc_state = S_IDLE;
	sc->sc_dmaactive = 0;
	dma->dma_csr |= DMA_IE;		/* enable interrupts */
	DELAY(200);
}

/*
 * Reset the chip.  N.B.: this causes a SCSI bus reset!
 */
static void
espreset(sc)
	register struct esp_softc *sc;
{
	register volatile struct espreg *esp = sc->sc_esp;

	dmareset(sc);
	esp->esp_cmd = ESPCMD_RESET_CHIP;
	DELAY(200);
	esp->esp_cmd = ESPCMD_NOP;
	DELAY(200);

	/*
	 * Reload configuration registers (cleared by RESET_CHIP command).
	 * Reloading conf2 on an ESP100 goofs it up, so out of paranoia
	 * we load only the registers that exist.
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

	sc->sc_needclear = 0xff;
}

/*
 * Reset the SCSI bus and, optionally, all attached targets.
 * The chip should retain most of its parameters (including esp_ccf)
 * across this kind of reset (see section 3.5 of Emulex documentation).
 */
void
esphbareset(hba, resetunits)
	struct hba_softc *hba;
	int resetunits;
{
	register struct esp_softc *sc = (struct esp_softc *)hba;
	register volatile struct espreg *esp = sc->sc_esp;

	dmareset(sc);

	/* BEGIN ??? */
	/* turn off scsi bus reset interrupts and reset scsi bus */
	esp->esp_conf1 = sc->sc_conf1 | ESPCONF1_REPORT;
	DELAY(200);
	esp->esp_cmd = ESPCMD_RESET_BUS;
	DELAY(800);
	esp->esp_cmd = ESPCMD_NOP;
	DELAY(200);
	esp->esp_conf1 = sc->sc_conf1;
	/* END ??? */

	sc->sc_needclear = 0xff;

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

	if (sc->sc_dsc) {
		printf(" %s %s", sc->sc_dsc->sc_dev.dv_xname,
		    sc->sc_hba.hba_dev.dv_xname);
		esphbareset(&sc->sc_hba, 1);
	}
}

static void
esperror(sc, err)
	char *err;
	register struct esp_softc *sc;
{

	printf("%s: %s (target=%d): stat=%b step=%x dmacsr=%b intr=%b\n",
	    sc->sc_hba.hba_dev.dv_xname, err, sc->sc_targ,
	    sc->sc_espstat, ESPSTAT_BITS, sc->sc_espstep,
	    sc->sc_dmacsr, DMA_BITS, sc->sc_espintr, ESPINTR_BITS);
}

/*
 * An interrupt has occurred.  Sequence through the SCSI state machine.
 * Return the action to take.
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
espact(sc, esp, dma, cdb)
	register struct esp_softc *sc;
	register volatile struct espreg *esp;
	register volatile struct dmareg *dma;
	register struct scsi_cdb *cdb;
{
	register char *xname = sc->sc_hba.hba_dev.dv_xname;
	register int reg, phase, i;

	/* check various error conditions, using as little code as possible */
	if (sc->sc_dmacsr & DMA_EP) {
		esperror(sc, "DMA error");
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
esperror(sc, "DIAGNOSTIC: gross error (ignored)");
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
			printf("%s: target %d", xname, sc->sc_targ);
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
		printf("%s: target %d not responding\n",
		    xname, sc->sc_targ);
		return (ACT_ERROR);
	}

	/*
	 * Okay, things are moving along.
	 * What were we doing the last time we did something,
	 * and did it complete normally?
	 */
	phase = sc->sc_espstat & ESPSTAT_PHASE;
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
esperror(sc, "DIAGNOSTIC: esp step 2");
		} else if (sc->sc_espstep == 3) {
			/*
			 * Device entered command phase and then exited it
			 * before we finished handing out the command.
			 * Let this happen iff we are trying to clear the
			 * target state.
			 */
esperror(sc, "DIAGNOSTIC: esp step 3");
			if (!sc->sc_clearing)
				return (ACT_RESET);
		} else {
			printf("%s: mysterious esp step %d\n",
			    xname, sc->sc_espstep);
			return (ACT_RESET);
		}
		/*
		 * Part of the command may still be lodged in the FIFO.
		 */
		esp->esp_cmd = ESPCMD_FLUSH_FIFO;
		break;

	case S_CMDSVC:
		/*
		 * We were waiting for phase change after stuffing the command
		 * into the FIFO.  Make sure it got out.
		 */
		reg = ESP_NFIFO(esp);
		if (reg) {
esperror(sc, "DIAGNOSTIC: CMDSVC, fifo not empty");
printf("\tfifo count = %x\n", reg);
			esp->esp_cmd = ESPCMD_FLUSH_FIFO;
		} else
			sc->sc_sentcmd = 1;
		break;

	case S_IOSVC:
		/*
		 * We were waiting for phase change after I/O.
		 */
		break;

	case S_DI:
		/*
		 * We were doing DMA data in, and expecting a
		 * transfer-count-zero interrupt or a phase change.
		 * We got that; drain the pack register and
		 * handle as for data out.
		 */
		dma->dma_csr |= DMA_DRAIN;
		reg = 0;		/* FIFO auto flushed? */
		goto dma_data_done;

	case S_DO:
		/*
		 * We were doing DMA data out.  If there is data in the
		 * FIFO, it is stuff that got DMAed out but never made
		 * it to the device, so it counts as residual.
		 *
		 * XXX	handle DMA IO with large count or address
		 *	boundary condition by resuming here, or below?
		 */
		if ((reg = ESP_NFIFO(esp)) != 0)
			esp->esp_cmd = ESPCMD_FLUSH_FIFO;
dma_data_done:
		if (sc->sc_dmaactive == 0) {
			printf("%s: dma done while %s, dmaactive==0\n",
			    xname, espstates[sc->sc_state]);
			panic("espact");
		}
		sc->sc_dmaactive = 0;
		reg += esp->esp_tcl | (esp->esp_tch << 8);
		if (reg == 0 && (sc->sc_espstat & ESPSTAT_TC) == 0)
			reg = 65536;
		if (reg > sc->sc_resid) {
			printf("%s: xfer resid (%d) > xfer req (%d)\n",
			    xname, reg, sc->sc_resid);
			reg = sc->sc_resid;
		}
		/*
		 * If data came in we must flush cache.
		 */
		if (sc->sc_state == S_DI)
			cache_flush(sc->sc_dmaaddr, sc->sc_resid - reg);
		sc->sc_resid = reg;
		if ((sc->sc_espintr & ESPINTR_SVC) == 0) {
			printf("%s: no bus service req\n", xname);
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
		reg = ESP_NFIFO(esp);
		if (reg < 2) {
			printf(
		"%s: command done but fifo count = %d; must be >= 2\n", xname,
			    reg);
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
			    xname, sc->sc_targ);
			return (ACT_RESET);
		}
		printf("%s: warning: target %d returned msg 0x%x\n",
		    xname, sc->sc_targ, reg);
		sc->sc_state = S_FI;
		return (ACT_CONT);

	case S_MI:
		if ((reg & ESPINTR_SVC) == 0) {
			esperror(sc, "missing phase after msg in");
			return (ACT_RESET);
		}
		reg = ESP_NFIFO(esp);
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
	switch (phase) {

	case ESPPHASE_DATA_OUT:
if (!sc->sc_sentcmd) esperror(sc, "DIAGNOSTIC: data out without command");
		sc->sc_state = S_DO;
		return (ACT_WRITE);

	case ESPPHASE_DATA_IN:
if (!sc->sc_sentcmd) esperror(sc, "DIAGNOSTIC: data in without command");
		sc->sc_state = S_DI;
		return (ACT_READ);

	case ESPPHASE_CMD:
		/*
		 * Silly thing wants the command again.
		 * Load it into the FIFO and go to CMDSVC state.
		 */
printf("%s: redoing command\n", xname);
		reg = SCSICMDLEN(cdb->cdb_bytes[0]);
		for (i = 0; i < reg; i++)
			esp->esp_fifo = cdb->cdb_bytes[i];
		sc->sc_state = S_CMDSVC;
		esp->esp_cmd = ESPCMD_XFER_INFO;
		return (ACT_CONT);

	case ESPPHASE_STATUS:
		sc->sc_state = S_STAT;
		esp->esp_cmd = ESPCMD_INIT_COMP;
		return (ACT_CONT);

	case ESPPHASE_MSG_IN:
printf("%s: accepting (& ignoring) msg from target %d\n", xname, sc->sc_targ);
		sc->sc_state = S_MI;
		esp->esp_cmd = ESPCMD_MSG_ACCEPT;
		return (ACT_CONT);

	default:
		printf("%s: target %d asked for strange phase (%s)\n",
		    xname, sc->sc_targ, espphases[phase]);
		return (ACT_RESET);
	}
	/* NOTREACHED */
}

/*
 * Issue a select, loading command into the FIFO.
 * Return nonzero on error, 0 if OK.
 * Sets state to `selecting'; espact() will sequence state FSM.
 */
void
espselect(sc, esp, targ, cdb)
	register struct esp_softc *sc;
	register volatile struct espreg *esp;
	register int targ;
	register struct scsi_cdb *cdb;
{
	register int i, cmdlen = SCSICMDLEN(cdb->cdb_bytes[0]);

	sc->sc_targ = targ;
	sc->sc_state = S_SEL;
	sc->sc_sentcmd = 0;
	sc->sc_stat[0] = 0xff;		/* ??? */
	sc->sc_msg[0] = 0xff;		/* ??? */

	/*
	 * Try to talk to target.
	 * Synch offset 0 => asynchronous transfer.
	 */
	esp->esp_id = targ;
	esp->esp_syncoff = 0;

	/*
	 * Stuff the command bytes into the fifo.
	 * Select without attention since we do not do disconnect yet.
	 */
	for (i = 0; i < cmdlen; i++)
		esp->esp_fifo = cdb->cdb_bytes[i];
	esp->esp_cmd = ESPCMD_SEL_NATN;
	/* the rest is done elsewhere */
}

/*
 * THIS SHOULD BE ADJUSTABLE
 */
	/* name		howlong		purpose */
#define	SELECT_WAIT	300000		/* wait for select to complete */
#define	CMD_WAIT	1000		/* wait for next phase, generic */
#define	IO_WAIT		1000000		/* time to xfer data in/out */
#define	POSTDATA_WAIT	10000000	/* wait for next phase, after dataio */

/*
 * Transfer data out via polling.  Return success (0) iff all
 * the bytes were sent and we got an interrupt.
 *
 * This returns -1 on timeout, resid count on early interrupt,
 * but no one really cares....
 */
static int
espixfer_out(sc, esp, dma, buf, len)
	register struct esp_softc *sc;
	register volatile struct espreg *esp;
	register volatile struct dmareg *dma;
	register caddr_t buf;
	register int len;
{
	register int wait, n;

	if (CROSS_DMA(buf, len))
		panic("espixfer_out: 16MB boundary");

	/* set dma address and transfer count */
	dma->dma_addr = (int)buf;
	esp->esp_tch = len >> 8;
	esp->esp_tcl = len;

	/* load count into counter via DMA NOP */
	esp->esp_cmd = ESPCMD_DMA | ESPCMD_NOP;

	/* enable dma (but not interrupts) */
	dma->dma_csr = DMA_ENA;

	/* and go */
	esp->esp_cmd = ESPCMD_DMA | ESPCMD_XFER_INFO;

	/* wait for completion */
	for (wait = IO_WAIT; wait > 0; --wait) {
		n = dma->dma_csr;
		if (DMA_INTR(n)) {
			sc->sc_espstat = esp->esp_stat;
			sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
			sc->sc_espintr = esp->esp_intr;
			sc->sc_dmacsr = n;
			n = esp->esp_tcl | (esp->esp_tch << 8);
			if (n == 0 && (sc->sc_espstat & ESPSTAT_TC) == 0)
				n = 65536;

			return (n);
		}
		DELAY(1);
	}
	return (-1);
}

/*
 * Transfer data in via polling.
 * Return resid count on interrupt, -1 if timed out.
 */
static int
espixfer_in(sc, esp, dma, buf, len)
	register struct esp_softc *sc;
	register volatile struct espreg *esp;
	register volatile struct dmareg *dma;
	register caddr_t buf;
	register int len;
{
	register int wait, n;

	if (CROSS_DMA(buf, len))
		panic("espixfer_in: 16MB boundary");

	/* set dma address and transfer count */
	dma->dma_addr = (int)buf;
	esp->esp_tch = len >> 8;
	esp->esp_tcl = len;

	/* load count into counter via DMA NOP */
	esp->esp_cmd = ESPCMD_DMA | ESPCMD_NOP;

	/* enable dma (but not interrupts) */
	dma->dma_csr = DMA_ENA | DMA_READ;

	/* and go */
	esp->esp_cmd = ESPCMD_DMA | ESPCMD_XFER_INFO;

	/* wait for completion */
	for (wait = IO_WAIT; wait > 0; --wait) {
		n = dma->dma_csr;
		if (DMA_INTR(n)) {
			sc->sc_espstat = esp->esp_stat;
			sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
			sc->sc_espintr = esp->esp_intr;
			dma->dma_csr |= DMA_DRAIN;
			sc->sc_dmacsr = n;
			n = esp->esp_tcl | (esp->esp_tch << 8);
			if (n == 0 && (sc->sc_espstat & ESPSTAT_TC) == 0)
				n = 65536;

			cache_flush(buf, (u_int)len - n);
			return (n);
		}
		DELAY(1);
	}
	return (-1);
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
 * Send an `immediate' command, i.e., poll until the whole thing is done.
 * Return the status byte from the device, or -1 if we timed out.
 */
int
espicmd(hba, targ, cdb, buf, len, rw)
	register struct hba_softc *hba;
	int targ;
	register struct scsi_cdb *cdb;
	caddr_t buf;
	register int len;
	int rw;
{
	register struct esp_softc *sc = (struct esp_softc *)hba;
	register volatile struct espreg *esp = sc->sc_esp;
	register volatile struct dmareg *dma = sc->sc_dma;
	register int r, wait;
	char *msg;

	if ((unsigned)len > MAX_TRANSFER_SIZE) {
		printf("%s: bad length %d\n", sc->sc_hba.hba_dev.dv_xname, len);
		panic("espicmd");
	}

	/*
	 * Clear the target if necessary.
	 */
	if (sc->sc_needclear & (1 << targ) && !sc->sc_probing)
		espclear(sc, targ);

	/*
	 * Disable hardware interrupts, start select sequence.
	 * Wait for interrupt-pending bit, then call espact() to
	 * sequence the state machine.  When it tells us to do
	 * data transfer, we do programmed I/O.
	 * In any case, we loop calling espact() until done.
	 */
	dma->dma_csr = 0;	/* disable hardware interrupts */
	espselect(sc, esp, targ, cdb);
	wait = SELECT_WAIT;
loop:
	for (;;) {
		r = dma->dma_csr;
		if (!DMA_INTR(r)) {
			if (--wait < 0) {
				msg = "timeout waiting for phase change";
				goto err;
			}
			DELAY(1);
			continue;
		}
		break;
	}
	sc->sc_espstat = esp->esp_stat;
	sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
	sc->sc_espintr = esp->esp_intr;
	sc->sc_dmacsr = r;
	/*
	 * The action happens `twice around' for read and write.
	 * All the rest `goto loop' or return or some such.
	 */
	wait = CMD_WAIT;
	for (;;) {
		switch (r = espact(sc, esp, dma, cdb)) {

		case ACT_CONT:
		case ACT_QUICKINTR:
			goto loop;

		case ACT_READ:
			if (len == 0 || (rw & B_READ) == 0) {
				msg = "wrong phase";
				goto err;
			}
			r = espixfer_in(sc, esp, dma, buf, len);
			if (r < 0) {
				msg = "timeout reading from device";
				goto err;
			}
			buf += len - r;
			len = r;
			/* we did the io, expecting `generic service' */
			sc->sc_state = S_IOSVC;
			wait = POSTDATA_WAIT;
			break;

		case ACT_WRITE:
			if (len == 0 || rw & B_READ) {
				msg = "wrong phase";
				goto err;
			}
			if (espixfer_out(sc, esp, dma, buf, len)) {
				msg = "timeout writing to device";
				goto err;
			}
			sc->sc_state = S_IOSVC;
			wait = POSTDATA_WAIT;
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
err:
	printf("%s: target %d: %s (phase = %s)\n",
	    sc->sc_hba.hba_dev.dv_xname, targ, msg,
	    espphases[sc->sc_espstat & ESPSTAT_PHASE]);
reset:
	espreset(sc);		/* ??? */
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
	register struct scsi_cdb *cdb;
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
		(*dgo)(dev, &sc->sc_cdb);
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
 * Send a `dma' command, i.e., send the cdb and use DMA to send the data.
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
	register int len = bp->b_bcount;
	register u_long addr;

	if ((unsigned)len > MAX_TRANSFER_SIZE) {
		printf("%s: %s\n", sc->sc_hba.hba_dev.dv_xname,
		    len < 0 ? "negative length" : "transfer too big");
		return (1);
	}

	if (sc->sc_needclear & (1 << targ))
		espclear(sc, targ);

	/*
	 * Set dma registers later, on data transfer,
	 * but compute the contents now.
	 * COULD JUST REMEMBER bp HERE...?
	 *
	 * The DMA chip cannot cross a 16 MB address boundary.
	 * We should do this as multiple DMA transactions on a
	 * single SCSI command, but I have not written that yet.
	 */
	sc->sc_dmactl = bp->b_flags & B_READ ? DMA_ENA | DMA_READ | DMA_IE :
	    DMA_ENA | DMA_IE;
	addr = (u_long)bp->b_un.b_addr;
	/* dma chip cannot cross 16MB boundary  XXX */
	if (CROSS_DMA(addr, len))
		panic("dma crosses 16MB boundary: fix esp.c");
	sc->sc_dmaaddr = addr;
	sc->sc_resid = len;

	/*
	 * Enable interrupts and start selection.
	 * The rest is done in our interrupt handler.
	 */
	sc->sc_hba.hba_intr = intr;	/* remember dev done function */
	sc->sc_hba.hba_intrdev = dev;	/* and its first arg */
	sc->sc_dma->dma_csr = DMA_IE;
	espselect(sc, sc->sc_esp, targ, &sc->sc_cdb);
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
	sc->sc_interrupts++;

again:
	sc->sc_espstat = esp->esp_stat;
	sc->sc_espstep = esp->esp_step & ESPSTEP_MASK;
	sc->sc_espintr = esp->esp_intr;
	sc->sc_dmacsr = r;

	if (sc->sc_state == S_IDLE) {
		printf("%s: stray interrupt\n", sc->sc_hba.hba_dev.dv_xname);
		dma->dma_csr &= ~DMA_IE;	/* ??? */
		return (1);
	}
	switch (r = espact(sc, esp, dma, &sc->sc_cdb)) {

	case ACT_CONT:		/* just return */
		break;

	case ACT_READ:
	case ACT_WRITE:
		/*
		 * We have to do this ourselves since another
		 * user of espact() wants to do programmed I/O.
		 * If we already did dma, and are done, stop.
		 */
		if (sc->sc_resid == 0) {
			printf("%s: target %d sent too much data\n",
			    sc->sc_hba.hba_dev.dv_xname, sc->sc_targ);
			goto reset;
		}
		sc->sc_dmaactive = 1;
		dma->dma_addr = sc->sc_dmaaddr;
		esp->esp_tch = sc->sc_resid >> 8;
		esp->esp_tcl = sc->sc_resid;
		/* load count into counter via DMA NOP */
		esp->esp_cmd = ESPCMD_DMA | ESPCMD_NOP;
		/* enable dma */
		dma->dma_csr = sc->sc_dmactl;
		/* and go */
		esp->esp_cmd = ESPCMD_DMA | ESPCMD_XFER_INFO;
		break;

	case ACT_RESET:		/* please reset esp */
reset:
		espreset(sc);	/* ??? */
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
			(*sq->sq_dgo)(sq->sq_dev, &sc->sc_cdb);
		} else
			sc->sc_hba.hba_busy = 0;
		break;

	case ACT_QUICKINTR:	/* wait a short while for another interrupt */
printf("%s: quickintr: ", sc->sc_hba.hba_dev.dv_xname);
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
		(*sq->sq_dgo)(sq->sq_dev, &sc->sc_cdb);
	else
		sc->sc_hba.hba_busy = 0;
}
