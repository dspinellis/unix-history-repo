/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)uda.c	7.32 (Berkeley) %G%
 */

/*
 * UDA50/MSCP device driver
 */

#define	POLLSTATS

/*
 * TODO
 *	write bad block forwarding code
 */

#include "ra.h"

#if NUDA > 0

/*
 * CONFIGURATION OPTIONS.  The next three defines are tunable -- tune away!
 *
 * COMPAT_42 enables 4.2/4.3 compatibility (label mapping)
 *
 * NRSPL2 and NCMDL2 control the number of response and command
 * packets respectively.  They may be any value from 0 to 7, though
 * setting them higher than 5 is unlikely to be of any value.
 * If you get warnings about your command ring being too small,
 * try increasing the values by one.
 *
 * MAXUNIT controls the maximum unit number (number of drives per
 * controller) we are prepared to handle.
 *
 * DEFAULT_BURST must be at least 1.
 */
#define	COMPAT_42

#define	NRSPL2	5		/* log2 number of response packets */
#define NCMDL2	5		/* log2 number of command packets */
#define	MAXUNIT	8		/* maximum allowed unit number */
#define	DEFAULT_BURST	4	/* default DMA burst size */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/ioctl.h"
#include "sys/user.h"
#include "sys/map.h"
#include "sys/vm.h"
#include "sys/dkstat.h"
#include "sys/cmap.h"
#include "sys/disklabel.h"
#include "sys/syslog.h"
#include "sys/stat.h"

#include "../include/pte.h"

#include "../include/cpu.h"
#include "ubareg.h"
#include "ubavar.h"

#define	NRSP	(1 << NRSPL2)
#define	NCMD	(1 << NCMDL2)

#include "udareg.h"
#include "../vax/mscp.h"
#include "../vax/mscpvar.h"
#include "../include/mtpr.h"

/*
 * UDA communications area and MSCP packet pools, per controller.
 */
struct	uda {
	struct	udaca uda_ca;		/* communications area */
	struct	mscp uda_rsp[NRSP];	/* response packets */
	struct	mscp uda_cmd[NCMD];	/* command packets */
} uda[NUDA];

/*
 * Software status, per controller.
 */
struct	uda_softc {
	struct	uda *sc_uda;	/* Unibus address of uda struct */
	short	sc_state;	/* UDA50 state; see below */
	short	sc_flags;	/* flags; see below */
	int	sc_micro;	/* microcode revision */
	int	sc_ivec;	/* interrupt vector address */
	short	sc_ipl;		/* interrupt priority, Q-bus */
	struct	mscp_info sc_mi;/* MSCP info (per mscpvar.h) */
#ifndef POLLSTATS
	int	sc_wticks;	/* watchdog timer ticks */
#else
	short	sc_wticks;
	short	sc_ncmd;
#endif
} uda_softc[NUDA];

#ifdef POLLSTATS
struct udastats {
	int	ncmd;
	int	cmd[NCMD + 1];
} udastats = { NCMD + 1 };
#endif

/*
 * Controller states
 */
#define	ST_IDLE		0	/* uninitialised */
#define	ST_STEP1	1	/* in `STEP 1' */
#define	ST_STEP2	2	/* in `STEP 2' */
#define	ST_STEP3	3	/* in `STEP 3' */
#define	ST_SETCHAR	4	/* in `Set Controller Characteristics' */
#define	ST_RUN		5	/* up and running */

/*
 * Flags
 */
#define	SC_MAPPED	0x01	/* mapped in Unibus I/O space */
#define	SC_INSTART	0x02	/* inside udastart() */
#define	SC_GRIPED	0x04	/* griped about cmd ring too small */
#define	SC_INSLAVE	0x08	/* inside udaslave() */
#define	SC_DOWAKE	0x10	/* wakeup when ctlr init done */
#define	SC_STARTPOLL	0x20	/* need to initiate polling */

/*
 * Device to unit number and partition and back
 */
#define	UNITSHIFT	3
#define	UNITMASK	7
#define	udaunit(dev)	(minor(dev) >> UNITSHIFT)
#define	udapart(dev)	(minor(dev) & UNITMASK)
#define	udaminor(u, p)	(((u) << UNITSHIFT) | (p))

/*
 * Drive status, per drive
 */
struct ra_info {
	daddr_t	ra_dsize;	/* size in sectors */
/*	u_long	ra_type;	/* drive type */
	u_long	ra_mediaid;	/* media id */
	int	ra_state;	/* open/closed state */
	struct	ra_geom {	/* geometry information */
		u_short	rg_nsectors;	/* sectors/track */
		u_short	rg_ngroups;	/* track groups */
		u_short	rg_ngpc;	/* groups/cylinder */
		u_short	rg_ntracks;	/* ngroups*ngpc */
		u_short	rg_ncyl;	/* ra_dsize/ntracks/nsectors */
#ifdef notyet
		u_short	rg_rctsize;	/* size of rct */
		u_short	rg_rbns;	/* replacement blocks per track */
		u_short	rg_nrct;	/* number of rct copies */
#endif
	} ra_geom;
	int	ra_wlabel;	/* label sector is currently writable */
	u_long	ra_openpart;	/* partitions open */
	u_long	ra_bopenpart;	/* block partitions open */
	u_long	ra_copenpart;	/* character partitions open */
} ra_info[NRA];

/*
 * Software state, per drive
 */
#define	CLOSED		0
#define	WANTOPEN	1
#define	RDLABEL		2
#define	OPEN		3
#define	OPENRAW		4

/*
 * Definition of the driver for autoconf.
 */
int	udaprobe(), udaslave(), udaattach(), udadgo(), udaintr();
struct	uba_ctlr *udaminfo[NUDA];
struct	uba_device *udadinfo[NRA];
struct	disklabel udalabel[NRA];

u_short	udastd[] = { 0772150, 0772550, 0777550, 0 };
struct	uba_driver udadriver =
 { udaprobe, udaslave, udaattach, udadgo, udastd, "ra", udadinfo, "uda",
   udaminfo };

/*
 * More driver definitions, for generic MSCP code.
 */
int	udadgram(), udactlrdone(), udaunconf(), udaiodone();
int	udaonline(), udagotstatus(), udaioerror(), udareplace(), udabb();

struct	buf udautab[NRA];	/* per drive transfer queue */

struct	mscp_driver udamscpdriver =
 { MAXUNIT, NRA, UNITSHIFT, udautab, udalabel, udadinfo,
   udadgram, udactlrdone, udaunconf, udaiodone,
   udaonline, udagotstatus, udareplace, udaioerror, udabb,
   "uda", "ra" };

/*
 * Miscellaneous private variables.
 */
char	udasr_bits[] = UDASR_BITS;

struct	uba_device *udaip[NUDA][MAXUNIT];
				/* inverting pointers: ctlr & unit => Unibus
				   device pointer */

int	udaburst[NUDA] = { 0 };	/* burst size, per UDA50, zero => default;
				   in data space so patchable via adb */

struct	mscp udaslavereply;	/* get unit status response packet, set
				   for udaslave by udaunconf, via udaintr */

static struct uba_ctlr *probeum;/* this is a hack---autoconf should pass ctlr
				   info to slave routine; instead, we remember
				   the last ctlr argument to probe */

int	udawstart, udawatch();	/* watchdog timer */

/*
 * Externals
 */
int	wakeup();
int	hz;

/*
 * Poke at a supposed UDA50 to see if it is there.
 * This routine duplicates some of the code in udainit() only
 * because autoconf has not set up the right information yet.
 * We have to do everything `by hand'.
 */
udaprobe(reg, ctlr, um)
	caddr_t reg;
	int ctlr;
	struct uba_ctlr *um;
{
	register int br, cvec;
	register struct uda_softc *sc;
	register struct udadevice *udaddr;
	register struct mscp_info *mi;
	int timeout, tries;
#ifdef QBA
	int s;
#endif

#ifdef VAX750
	/*
	 * The UDA50 wants to share BDPs on 750s, but not on 780s or
	 * 8600s.  (730s have no BDPs anyway.)  Toward this end, we
	 * here set the `keep bdp' flag in the per-driver information
	 * if this is a 750.  (We just need to do it once, but it is
	 * easiest to do it now, for each UDA50.)
	 */
	if (cpu == VAX_750)
		udadriver.ud_keepbdp = 1;
#endif

	probeum = um;			/* remember for udaslave() */
#ifdef lint
	br = 0; cvec = br; br = cvec; udaintr(0);
#endif
	/*
	 * Set up the controller-specific generic MSCP driver info.
	 * Note that this should really be done in the (nonexistent)
	 * controller attach routine.
	 */
	sc = &uda_softc[ctlr];
	mi = &sc->sc_mi;
	mi->mi_md = &udamscpdriver;
	mi->mi_ctlr = um->um_ctlr;
	mi->mi_tab = &um->um_tab;
	mi->mi_ip = udaip[ctlr];
	mi->mi_cmd.mri_size = NCMD;
	mi->mi_cmd.mri_desc = uda[ctlr].uda_ca.ca_cmddsc;
	mi->mi_cmd.mri_ring = uda[ctlr].uda_cmd;
	mi->mi_rsp.mri_size = NRSP;
	mi->mi_rsp.mri_desc = uda[ctlr].uda_ca.ca_rspdsc;
	mi->mi_rsp.mri_ring = uda[ctlr].uda_rsp;
	mi->mi_wtab.av_forw = mi->mi_wtab.av_back = &mi->mi_wtab;

	/*
	 * More controller specific variables.  Again, this should
	 * be in the controller attach routine.
	 */
	if (udaburst[ctlr] == 0)
		udaburst[ctlr] = DEFAULT_BURST;
		
	/*
	 * Get an interrupt vector.  Note that even if the controller
	 * does not respond, we keep the vector.  This is not a serious
	 * problem; but it would be easily fixed if we had a controller
	 * attach routine.  Sigh.
	 */
	sc->sc_ivec = (uba_hd[numuba].uh_lastiv -= 4);
	udaddr = (struct udadevice *) reg;

	/*
	 * Initialise the controller (partially).  The UDA50 programmer's
	 * manual states that if initialisation fails, it should be retried
	 * at least once, but after a second failure the port should be
	 * considered `down'; it also mentions that the controller should
	 * initialise within ten seconds.  Or so I hear; I have not seen
	 * this manual myself.
	 */
#if defined(QBA) && !defined(GENERIC)
	s = spl6();
#endif
	tries = 0;
again:
	udaddr->udaip = 0;		/* start initialisation */
	timeout = todr() + 1000;	/* timeout in 10 seconds */
	while ((udaddr->udasa & UDA_STEP1) == 0)
		if (todr() > timeout)
			goto bad;
	udaddr->udasa = UDA_ERR | (NCMDL2 << 11) | (NRSPL2 << 8) | UDA_IE |
		(sc->sc_ivec >> 2);
	while ((udaddr->udasa & UDA_STEP2) == 0)
		if (todr() > timeout)
			goto bad;

	/* should have interrupted by now */
#ifdef QBA
#ifndef GENERIC
	sc->sc_ipl = br = qbgetpri();
#else
	sc->sc_ipl = br = 0x15;
#endif
#endif
	return (sizeof (struct udadevice));
bad:
	if (++tries < 2)
		goto again;
#if defined(QBA) && !defined(GENERIC)
	splx(s);
#endif
	return (0);
}

/*
 * Find a slave.  We allow wildcard slave numbers (something autoconf
 * is not really prepared to deal with); and we need to know the
 * controller number to talk to the UDA.  For the latter, we keep
 * track of the last controller probed, since a controller probe
 * immediately precedes all slave probes for that controller.  For the
 * former, we simply put the unit number into ui->ui_slave after we
 * have found one.
 *
 * Note that by the time udaslave is called, the interrupt vector
 * for the UDA50 has been set up (so that udaunconf() will be called).
 */
udaslave(ui, reg)
	register struct uba_device *ui;
	caddr_t reg;
{
	register struct uba_ctlr *um = probeum;
	register struct mscp *mp;
	register struct uda_softc *sc;
	int next = 0, timeout, tries, i;

#ifdef lint
	i = 0; i = i;
#endif
	/*
	 * Make sure the controller is fully initialised, by waiting
	 * for it if necessary.
	 */
	sc = &uda_softc[um->um_ctlr];
	if (sc->sc_state == ST_RUN)
		goto findunit;
	tries = 0;
again:
	if (udainit(ui->ui_ctlr))
		return (0);
	timeout = todr() + 1000;		/* 10 seconds */
	while (todr() < timeout)
		if (sc->sc_state == ST_RUN)	/* made it */
			goto findunit;
	if (++tries < 2)
		goto again;
	printf("uda%d: controller hung\n", um->um_ctlr);
	return (0);

	/*
	 * The controller is all set; go find the unit.  Grab an
	 * MSCP packet and send out a Get Unit Status command, with
	 * the `next unit' modifier if we are looking for a generic
	 * unit.  We set the `in slave' flag so that udaunconf()
	 * knows to copy the response to `udaslavereply'.
	 */
findunit:
	udaslavereply.mscp_opcode = 0;
	sc->sc_flags |= SC_INSLAVE;
	if ((mp = mscp_getcp(&sc->sc_mi, MSCP_DONTWAIT)) == NULL)
		panic("udaslave");		/* `cannot happen' */
	mp->mscp_opcode = M_OP_GETUNITST;
	if (ui->ui_slave == '?') {
		mp->mscp_unit = next;
		mp->mscp_modifier = M_GUM_NEXTUNIT;
	} else {
		mp->mscp_unit = ui->ui_slave;
		mp->mscp_modifier = 0;
	}
	*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
	i = ((struct udadevice *) reg)->udaip;	/* initiate polling */
	mp = &udaslavereply;
	timeout = todr() + 1000;
	while (todr() < timeout)
		if (mp->mscp_opcode)
			goto gotit;
	printf("uda%d: no response to Get Unit Status request\n",
		um->um_ctlr);
	sc->sc_flags &= ~SC_INSLAVE;
	return (0);

gotit:
	sc->sc_flags &= ~SC_INSLAVE;

	/*
	 * Got a slave response.  If the unit is there, use it.
	 */
	switch (mp->mscp_status & M_ST_MASK) {

	case M_ST_SUCCESS:	/* worked */
	case M_ST_AVAILABLE:	/* found another drive */
		break;		/* use it */

	case M_ST_OFFLINE:
		/*
		 * Figure out why it is off line.  It may be because
		 * it is nonexistent, or because it is spun down, or
		 * for some other reason.
		 */
		switch (mp->mscp_status & ~M_ST_MASK) {

		case M_OFFLINE_UNKNOWN:
			/*
			 * No such drive, and there are none with
			 * higher unit numbers either, if we are
			 * using M_GUM_NEXTUNIT.
			 */
			return (0);

		case M_OFFLINE_UNMOUNTED:
			/*
			 * The drive is not spun up.  Use it anyway.
			 *
			 * N.B.: this seems to be a common occurrance
			 * after a power failure.  The first attempt
			 * to bring it on line seems to spin it up
			 * (and thus takes several minutes).  Perhaps
			 * we should note here that the on-line may
			 * take longer than usual.
			 */
			break;

		default:
			/*
			 * In service, or something else equally unusable.
			 */
			printf("uda%d: unit %d off line: ", um->um_ctlr,
				mp->mscp_unit);
			mscp_printevent(mp);
			goto try_another;
		}
		break;

	default:
		printf("uda%d: unable to get unit status: ", um->um_ctlr);
		mscp_printevent(mp);
		return (0);
	}

	/*
	 * Does this ever happen?  What (if anything) does it mean?
	 */
	if (mp->mscp_unit < next) {
		printf("uda%d: unit %d, next %d\n",
			um->um_ctlr, mp->mscp_unit, next);
		return (0);
	}

	if (mp->mscp_unit >= MAXUNIT) {
		printf("uda%d: cannot handle unit number %d (max is %d)\n",
			um->um_ctlr, mp->mscp_unit, MAXUNIT - 1);
		return (0);
	}

	/*
	 * See if we already handle this drive.
	 * (Only likely if ui->ui_slave=='?'.)
	 */
	if (udaip[um->um_ctlr][mp->mscp_unit] != NULL) {
try_another:
		if (ui->ui_slave != '?')
			return (0);
		next = mp->mscp_unit + 1;
		goto findunit;
	}

	/*
	 * Voila!
	 */
	uda_rasave(ui->ui_unit, mp, 0);
	ui->ui_flags = 0;	/* not on line, nor anything else */
	ui->ui_slave = mp->mscp_unit;
	return (1);
}

/*
 * Attach a found slave.  Make sure the watchdog timer is running.
 * If this disk is being profiled, fill in the `wpms' value (used by
 * what?).  Set up the inverting pointer, and attempt to bring the
 * drive on line and read its label.
 */
udaattach(ui)
	register struct uba_device *ui;
{
	register int unit = ui->ui_unit;

	if (udawstart == 0) {
		timeout(udawatch, (caddr_t) 0, hz);
		udawstart++;
	}

	/*
	 * Floppies cannot be brought on line unless there is
	 * a disk in the drive.  Since an ONLINE while cold
	 * takes ten seconds to fail, and (when notyet becomes now)
	 * no sensible person will swap to one, we just
	 * defer the ONLINE until someone tries to use the drive.
	 *
	 * THIS ASSUMES THAT DRIVE TYPES ?X? ARE FLOPPIES
	 */
	if (MSCP_MID_ECH(1, ra_info[unit].ra_mediaid) == 'X' - '@') {
		printf(": floppy");
		return;
	}
	if (ui->ui_dk >= 0)
		dk_wpms[ui->ui_dk] = (60 * 31 * 256);	/* approx */
	udaip[ui->ui_ctlr][ui->ui_slave] = ui;

	if (uda_rainit(ui, 0))
		printf(": offline");
	else if (ra_info[unit].ra_state == OPEN) {
		printf(": %s, size = %d sectors",
		    udalabel[unit].d_typename, ra_info[unit].ra_dsize);
#ifdef notyet
		addswap(makedev(UDADEVNUM, udaminor(unit, 0)), &udalabel[unit]);
#endif
	}
}

/*
 * Initialise a UDA50.  Return true iff something goes wrong.
 */
udainit(ctlr)
	int ctlr;
{
	register struct uda_softc *sc;
	register struct udadevice *udaddr;
	struct uba_ctlr *um;
	int timo, ubinfo;

	sc = &uda_softc[ctlr];
	um = udaminfo[ctlr];
	if ((sc->sc_flags & SC_MAPPED) == 0) {
		/*
		 * Map the communication area and command and
		 * response packets into Unibus space.
		 */
		ubinfo = uballoc(um->um_ubanum, (caddr_t) &uda[ctlr],
			sizeof (struct uda), UBA_CANTWAIT);
		if (ubinfo == 0) {
			printf("uda%d: uballoc map failed\n", ctlr);
			return (-1);
		}
		sc->sc_uda = (struct uda *) UBAI_ADDR(ubinfo);
		sc->sc_flags |= SC_MAPPED;
	}

	/*
	 * While we are thinking about it, reset the next command
	 * and response indicies.
	 */
	sc->sc_mi.mi_cmd.mri_next = 0;
	sc->sc_mi.mi_rsp.mri_next = 0;

	/*
	 * Start up the hardware initialisation sequence.
	 */
#define	STEP0MASK	(UDA_ERR | UDA_STEP4 | UDA_STEP3 | UDA_STEP2 | \
			 UDA_STEP1 | UDA_NV)

	sc->sc_state = ST_IDLE;	/* in case init fails */
	udaddr = (struct udadevice *)um->um_addr;
	udaddr->udaip = 0;
	timo = todr() + 1000;
	while ((udaddr->udasa & STEP0MASK) == 0) {
		if (todr() > timo) {
			printf("uda%d: timeout during init\n", ctlr);
			return (-1);
		}
	}
	if ((udaddr->udasa & STEP0MASK) != UDA_STEP1) {
		printf("uda%d: init failed, sa=%b\n", ctlr,
			udaddr->udasa, udasr_bits);
		udasaerror(um, 0);
		return (-1);
	}

	/*
	 * Success!  Record new state, and start step 1 initialisation.
	 * The rest is done in the interrupt handler.
	 */
	sc->sc_state = ST_STEP1;
	udaddr->udasa = UDA_ERR | (NCMDL2 << 11) | (NRSPL2 << 8) | UDA_IE |
	    (sc->sc_ivec >> 2);
	return (0);
}

/*
 * Open a drive.
 */
/*ARGSUSED*/
udaopen(dev, flag, fmt)
	dev_t dev;
	int flag, fmt;
{
	register int unit;
	register struct uba_device *ui;
	register struct uda_softc *sc;
	register struct disklabel *lp;
	register struct partition *pp;
	register struct ra_info *ra;
	int s, i, part, mask, error = 0;
	daddr_t start, end;

	/*
	 * Make sure this is a reasonable open request.
	 */
	unit = udaunit(dev);
	if (unit >= NRA || (ui = udadinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);

	/*
	 * Make sure the controller is running, by (re)initialising it if
	 * necessary.
	 */
	sc = &uda_softc[ui->ui_ctlr];
	s = spl5();
	if (sc->sc_state != ST_RUN) {
		if (sc->sc_state == ST_IDLE && udainit(ui->ui_ctlr)) {
			splx(s);
			return (EIO);
		}
		/*
		 * In case it does not come up, make sure we will be
		 * restarted in 10 seconds.  This corresponds to the
		 * 10 second timeouts in udaprobe() and udaslave().
		 */
		sc->sc_flags |= SC_DOWAKE;
		timeout(wakeup, (caddr_t) sc, 10 * hz);
		sleep((caddr_t) sc, PRIBIO);
		if (sc->sc_state != ST_RUN) {
			splx(s);
			printf("uda%d: controller hung\n", ui->ui_ctlr);
			return (EIO);
		}
		untimeout(wakeup, (caddr_t) sc);
	}

	/*
	 * Wait for the state to settle
	 */
	ra = &ra_info[unit];
	while (ra->ra_state != OPEN && ra->ra_state != OPENRAW &&
	    ra->ra_state != CLOSED)
		if (error = tsleep((caddr_t)ra, (PZERO + 1) | PCATCH,
		    devopn, 0)) {
			splx(s);
			return (error);
		}

	/*
	 * If not on line, or we are not sure of the label, reinitialise
	 * the drive.
	 */
	if ((ui->ui_flags & UNIT_ONLINE) == 0 ||
	    (ra->ra_state != OPEN && ra->ra_state != OPENRAW))
		error = uda_rainit(ui, flag);
	splx(s);
	if (error)
		return (error);

	part = udapart(dev);
	lp = &udalabel[unit];
	if (part >= lp->d_npartitions)
		return (ENXIO);
	/*
	 * Warn if a partition is opened that overlaps another
	 * already open, unless either is the `raw' partition
	 * (whole disk).
	 */
#define	RAWPART		2	/* 'c' partition */	/* XXX */
	mask = 1 << part;
	if ((ra->ra_openpart & mask) == 0 && part != RAWPART) {
		pp = &lp->d_partitions[part];
		start = pp->p_offset;
		end = pp->p_offset + pp->p_size;
		for (pp = lp->d_partitions, i = 0;
		     i < lp->d_npartitions; pp++, i++) {
			if (pp->p_offset + pp->p_size <= start ||
			    pp->p_offset >= end || i == RAWPART)
				continue;
			if (ra->ra_openpart & (1 << i))
				log(LOG_WARNING,
				    "ra%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a', i + 'a');
		}
	}
	switch (fmt) {
	case S_IFCHR:
		ra->ra_copenpart |= mask;
		break;
	case S_IFBLK:
		ra->ra_bopenpart |= mask;
		break;
	}
	ra->ra_openpart |= mask;
	return (0);
}

/* ARGSUSED */
udaclose(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register int unit = udaunit(dev);
	register struct ra_info *ra = &ra_info[unit];
	int s, mask = (1 << udapart(dev));

	switch (fmt) {
	case S_IFCHR:
		ra->ra_copenpart &= ~mask;
		break;
	case S_IFBLK:
		ra->ra_bopenpart &= ~mask;
		break;
	}
	ra->ra_openpart = ra->ra_copenpart | ra->ra_bopenpart;

	/*
	 * Should wait for I/O to complete on this partition even if
	 * others are open, but wait for work on blkflush().
	 */
	if (ra->ra_openpart == 0) {
		s = spl5();
		while (udautab[unit].b_actf)
			sleep((caddr_t)&udautab[unit], PZERO - 1);
		splx(s);
		ra->ra_state = CLOSED;
		ra->ra_wlabel = 0;
	}
	return (0);
}

/*
 * Initialise a drive.  If it is not already, bring it on line,
 * and set a timeout on it in case it fails to respond.
 * When on line, read in the pack label.
 */
uda_rainit(ui, flags)
	register struct uba_device *ui;
	int flags;
{
	register struct uda_softc *sc = &uda_softc[ui->ui_ctlr];
	register struct disklabel *lp;
	register struct mscp *mp;
	register int unit = ui->ui_unit;
	register struct ra_info *ra;
	char *msg, *readdisklabel();
	int s, i, udastrategy();
	extern int cold;

	ra = &ra_info[unit];
	if ((ui->ui_flags & UNIT_ONLINE) == 0) {
		mp = mscp_getcp(&sc->sc_mi, MSCP_WAIT);
		mp->mscp_opcode = M_OP_ONLINE;
		mp->mscp_unit = ui->ui_slave;
		mp->mscp_cmdref = (long)&ui->ui_flags;
		*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
		ra->ra_state = WANTOPEN;
		if (!cold)
			s = spl5();
		i = ((struct udadevice *)ui->ui_addr)->udaip;

		if (cold) {
			i = todr() + 1000;
			while ((ui->ui_flags & UNIT_ONLINE) == 0)
				if (todr() > i)
					break;
		} else {
			timeout(wakeup, (caddr_t)&ui->ui_flags, 10 * hz);
			sleep((caddr_t)&ui->ui_flags, PSWP + 1);
			splx(s);
			untimeout(wakeup, (caddr_t)&ui->ui_flags);
		}
		if (ra->ra_state != OPENRAW) {
			ra->ra_state = CLOSED;
			wakeup((caddr_t)ra);
			return (EIO);
		}
	}

	lp = &udalabel[unit];
	lp->d_secsize = DEV_BSIZE;
	lp->d_secperunit = ra->ra_dsize;

	if (flags & O_NDELAY)
		return (0);
	ra->ra_state = RDLABEL;
	/*
	 * Set up default sizes until we have the label, or longer
	 * if there is none.  Set secpercyl, as readdisklabel wants
	 * to compute b_cylin (although we do not need it), and set
	 * nsectors in case diskerr is called.
	 */
	lp->d_secpercyl = 1;
	lp->d_npartitions = 1;
	lp->d_secsize = 512;
	lp->d_secperunit = ra->ra_dsize;
	lp->d_nsectors = ra->ra_geom.rg_nsectors;
	lp->d_partitions[0].p_size = lp->d_secperunit;
	lp->d_partitions[0].p_offset = 0;

	/*
	 * Read pack label.
	 */
	if ((msg = readdisklabel(udaminor(unit, 0), udastrategy, lp)) != NULL) {
		if (cold)
			printf(": %s", msg);
		else
			log(LOG_ERR, "ra%d: %s", unit, msg);
#ifdef COMPAT_42
		if (udamaptype(unit, lp))
			ra->ra_state = OPEN;
		else
			ra->ra_state = OPENRAW;
#else
		ra->ra_state = OPENRAW;
		uda_makefakelabel(ra, lp);
#endif
	} else
		ra->ra_state = OPEN;
	wakeup((caddr_t)ra);
	return (0);
}

/*
 * Copy the geometry information for the given ra from a
 * GET UNIT STATUS response.  If check, see if it changed.
 */
uda_rasave(unit, mp, check)
	int unit;
	register struct mscp *mp;
	int check;
{
	register struct ra_info *ra = &ra_info[unit];

	if (check && ra->ra_mediaid != mp->mscp_guse.guse_mediaid) {
		printf("ra%d: changed types! was %d now %d\n", unit,
			ra->ra_mediaid, mp->mscp_guse.guse_mediaid);
		ra->ra_state = CLOSED;	/* ??? */
	}
	/* ra->ra_type = mp->mscp_guse.guse_drivetype; */
	ra->ra_mediaid = mp->mscp_guse.guse_mediaid;
	ra->ra_geom.rg_nsectors = mp->mscp_guse.guse_nspt;
	ra->ra_geom.rg_ngroups = mp->mscp_guse.guse_group;
	ra->ra_geom.rg_ngpc = mp->mscp_guse.guse_ngpc;
	ra->ra_geom.rg_ntracks = ra->ra_geom.rg_ngroups * ra->ra_geom.rg_ngpc;
	/* ra_geom.rg_ncyl cannot be computed until we have ra_dsize */
#ifdef notyet
	ra->ra_geom.rg_rctsize = mp->mscp_guse.guse_rctsize;
	ra->ra_geom.rg_rbns = mp->mscp_guse.guse_nrpt;
	ra->ra_geom.rg_nrct = mp->mscp_guse.guse_nrct;
#endif
}

/*
 * Queue a transfer request, and if possible, hand it to the controller.
 *
 * This routine is broken into two so that the internal version
 * udastrat1() can be called by the (nonexistent, as yet) bad block
 * revectoring routine.
 */
udastrategy(bp)
	register struct buf *bp;
{
	register int unit;
	register struct uba_device *ui;
	register struct ra_info *ra;
	struct partition *pp;
	int p;
	daddr_t sz, maxsz;

	/*
	 * Make sure this is a reasonable drive to use.
	 */
	if ((unit = udaunit(bp->b_dev)) >= NRA ||
	    (ui = udadinfo[unit]) == NULL || ui->ui_alive == 0 ||
	    (ra = &ra_info[unit])->ra_state == CLOSED) {
		bp->b_error = ENXIO;
		goto bad;
	}

	/*
	 * If drive is open `raw' or reading label, let it at it.
	 */
	if (ra->ra_state < OPEN) {
		udastrat1(bp);
		return;
	}
	p = udapart(bp->b_dev);
	if ((ra->ra_openpart & (1 << p)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}

	/*
	 * Determine the size of the transfer, and make sure it is
	 * within the boundaries of the partition.
	 */
	pp = &udalabel[unit].d_partitions[p];
	maxsz = pp->p_size;
	if (pp->p_offset + pp->p_size > ra->ra_dsize)
		maxsz = ra->ra_dsize - pp->p_offset;
	sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	if (bp->b_blkno + pp->p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
	    bp->b_blkno + pp->p_offset + sz > LABELSECTOR &&
#endif
	    (bp->b_flags & B_READ) == 0 && ra->ra_wlabel == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	if (bp->b_blkno < 0 || bp->b_blkno + sz > maxsz) {
		/* if exactly at end of disk, return an EOF */
		if (bp->b_blkno == maxsz) {
			bp->b_resid = bp->b_bcount;
			biodone(bp);
			return;
		}
		/* or truncate if part of it fits */
		sz = maxsz - bp->b_blkno;
		if (sz <= 0) {
			bp->b_error = EINVAL;	/* or hang it up */
			goto bad;
		}
		bp->b_bcount = sz << DEV_BSHIFT;
	}
	udastrat1(bp);
	return;
bad:
	bp->b_flags |= B_ERROR;
	biodone(bp);
}

/*
 * Work routine for udastrategy.
 */
udastrat1(bp)
	register struct buf *bp;
{
	register int unit = udaunit(bp->b_dev);
	register struct uba_ctlr *um;
	register struct buf *dp;
	struct uba_device *ui;
	int s = spl5();

	/*
	 * Append the buffer to the drive queue, and if it is not
	 * already there, the drive to the controller queue.  (However,
	 * if the drive queue is marked to be requeued, we must be
	 * awaiting an on line or get unit status command; in this
	 * case, leave it off the controller queue.)
	 */
	um = (ui = udadinfo[unit])->ui_mi;
	dp = &udautab[unit];
	APPEND(bp, dp, av_forw);
	if (dp->b_active == 0 && (ui->ui_flags & UNIT_REQUEUE) == 0) {
		APPEND(dp, &um->um_tab, b_forw);
		dp->b_active++;
	}

	/*
	 * Start activity on the controller.  Note that unlike other
	 * Unibus drivers, we must always do this, not just when the
	 * controller is not active.
	 */
	udastart(um);
	splx(s);
}

/*
 * Start up whatever transfers we can find.
 * Note that udastart() must be called at spl5().
 */
udastart(um)
	register struct uba_ctlr *um;
{
	register struct uda_softc *sc = &uda_softc[um->um_ctlr];
	register struct buf *bp, *dp;
	register struct mscp *mp;
	struct uba_device *ui;
	struct udadevice *udaddr;
	struct partition *pp;
	int i, sz;

#ifdef lint
	i = 0; i = i;
#endif
	/*
	 * If it is not running, try (again and again...) to initialise
	 * it.  If it is currently initialising just ignore it for now.
	 */
	if (sc->sc_state != ST_RUN) {
		if (sc->sc_state == ST_IDLE && udainit(um->um_ctlr))
			printf("uda%d: still hung\n", um->um_ctlr);
		return;
	}

	/*
	 * If um_cmd is nonzero, this controller is on the Unibus
	 * resource wait queue.  It will not help to try more requests;
	 * instead, when the Unibus unblocks and calls udadgo(), we
	 * will call udastart() again.
	 */
	if (um->um_cmd)
		return;

	sc->sc_flags |= SC_INSTART;
	udaddr = (struct udadevice *) um->um_addr;

loop:
	/*
	 * Service the drive at the head of the queue.  It may not
	 * need anything, in which case it might be shutting down
	 * in udaclose().
	 */
	if ((dp = um->um_tab.b_actf) == NULL)
		goto out;
	if ((bp = dp->b_actf) == NULL) {
		dp->b_active = 0;
		um->um_tab.b_actf = dp->b_forw;
		if (ra_info[dp - udautab].ra_openpart == 0)
			wakeup((caddr_t)dp); /* finish close protocol */
		goto loop;
	}

	if (udaddr->udasa & UDA_ERR) {	/* ctlr fatal error */
		udasaerror(um, 1);
		goto out;
	}

	/*
	 * Get an MSCP packet, then figure out what to do.  If
	 * we cannot get a command packet, the command ring may
	 * be too small:  We should have at least as many command
	 * packets as credits, for best performance.
	 */
	if ((mp = mscp_getcp(&sc->sc_mi, MSCP_DONTWAIT)) == NULL) {
		if (sc->sc_mi.mi_credits > MSCP_MINCREDITS &&
		    (sc->sc_flags & SC_GRIPED) == 0) {
			log(LOG_NOTICE, "uda%d: command ring too small\n",
				um->um_ctlr);
			sc->sc_flags |= SC_GRIPED;/* complain only once */
		}
		goto out;
	}

	/*
	 * Bring the drive on line if it is not already.  Get its status
	 * if we do not already have it.  Otherwise just start the transfer.
	 */
	ui = udadinfo[udaunit(bp->b_dev)];
	if ((ui->ui_flags & UNIT_ONLINE) == 0) {
		mp->mscp_opcode = M_OP_ONLINE;
		goto common;
	}
	if ((ui->ui_flags & UNIT_HAVESTATUS) == 0) {
		mp->mscp_opcode = M_OP_GETUNITST;
common:
if (ui->ui_flags & UNIT_REQUEUE) panic("udastart");
		/*
		 * Take the drive off the controller queue.  When the
		 * command finishes, make sure the drive is requeued.
		 */
		um->um_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		ui->ui_flags |= UNIT_REQUEUE;
		mp->mscp_unit = ui->ui_slave;
		*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
		sc->sc_flags |= SC_STARTPOLL;
#ifdef POLLSTATS
		sc->sc_ncmd++;
#endif
		goto loop;
	}

	pp = &udalabel[ui->ui_unit].d_partitions[udapart(bp->b_dev)];
	mp->mscp_opcode = (bp->b_flags & B_READ) ? M_OP_READ : M_OP_WRITE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_seq.seq_lbn = bp->b_blkno + pp->p_offset;
	sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	mp->mscp_seq.seq_bytecount = bp->b_blkno + sz > pp->p_size ?
		(pp->p_size - bp->b_blkno) >> DEV_BSHIFT : bp->b_bcount;
	/* mscp_cmdref is filled in by mscp_go() */

	/*
	 * Drop the packet pointer into the `command' field so udadgo()
	 * can tell what to start.  If ubago returns 1, we can do another
	 * transfer.  If not, um_cmd will still point at mp, so we will
	 * know that we are waiting for resources.
	 */
	um->um_cmd = (int)mp;
	if (ubago(ui))
		goto loop;

	/*
	 * All done, or blocked in ubago().  If we managed to
	 * issue some commands, start up the beast.
	 */
out:
	if (sc->sc_flags & SC_STARTPOLL) {
#ifdef POLLSTATS
		udastats.cmd[sc->sc_ncmd]++;
		sc->sc_ncmd = 0;
#endif
		i = ((struct udadevice *)um->um_addr)->udaip;
	}
	sc->sc_flags &= ~(SC_INSTART | SC_STARTPOLL);
}

/*
 * Start a transfer.
 *
 * If we are not called from within udastart(), we must have been
 * blocked, so call udastart to do more requests (if any).  If
 * this calls us again immediately we will not recurse, because
 * that time we will be in udastart().  Clever....
 */
udadgo(um)
	register struct uba_ctlr *um;
{
	struct uda_softc *sc = &uda_softc[um->um_ctlr];
	struct mscp *mp = (struct mscp *)um->um_cmd;

	um->um_tab.b_active++;	/* another transfer going */

	/*
	 * Fill in the MSCP packet and move the buffer to the
	 * I/O wait queue.  Mark the controller as no longer on
	 * the resource queue, and remember to initiate polling.
	 */
	mp->mscp_seq.seq_buffer = UBAI_ADDR(um->um_ubinfo) |
		(UBAI_BDP(um->um_ubinfo) << 24);
	mscp_go(&sc->sc_mi, mp, um->um_ubinfo);
	um->um_cmd = 0;	
	um->um_ubinfo = 0;	/* tyke it awye */
	sc->sc_flags |= SC_STARTPOLL;
#ifdef POLLSTATS
	sc->sc_ncmd++;
#endif
	if ((sc->sc_flags & SC_INSTART) == 0)
		udastart(um);
}

udaiodone(mi, bp, info)
	register struct mscp_info *mi;
	struct buf *bp;
	int info;
{
	register struct uba_ctlr *um = udaminfo[mi->mi_ctlr];

	um->um_ubinfo = info;
	ubadone(um);
	biodone(bp);
	if (um->um_bdp && mi->mi_wtab.av_forw == &mi->mi_wtab)
		ubarelse(um->um_ubanum, &um->um_bdp);
	um->um_tab.b_active--;	/* another transfer done */
}

static struct saerr {
	int	code;		/* error code (including UDA_ERR) */
	char	*desc;		/* what it means: Efoo => foo error */
} saerr[] = {
	{ 0100001, "Eunibus packet read" },
	{ 0100002, "Eunibus packet write" },
	{ 0100003, "EUDA ROM and RAM parity" },
	{ 0100004, "EUDA RAM parity" },
	{ 0100005, "EUDA ROM parity" },
	{ 0100006, "Eunibus ring read" },
	{ 0100007, "Eunibus ring write" },
	{ 0100010, " unibus interrupt master failure" },
	{ 0100011, "Ehost access timeout" },
	{ 0100012, " host exceeded command limit" },
	{ 0100013, " unibus bus master failure" },
	{ 0100014, " DM XFC fatal error" },
	{ 0100015, " hardware timeout of instruction loop" },
	{ 0100016, " invalid virtual circuit id" },
	{ 0100017, "Eunibus interrupt write" },
	{ 0104000, "Efatal sequence" },
	{ 0104040, " D proc ALU" },
	{ 0104041, "ED proc control ROM parity" },
	{ 0105102, "ED proc w/no BD#2 or RAM parity" },
	{ 0105105, "ED proc RAM buffer" },
	{ 0105152, "ED proc SDI" },
	{ 0105153, "ED proc write mode wrap serdes" },
	{ 0105154, "ED proc read mode serdes, RSGEN & ECC" },
	{ 0106040, "EU proc ALU" },
	{ 0106041, "EU proc control reg" },
	{ 0106042, " U proc DFAIL/cntl ROM parity/BD #1 test CNT" },
	{ 0106047, " U proc const PROM err w/D proc running SDI test" },
	{ 0106055, " unexpected trap" },
	{ 0106071, "EU proc const PROM" },
	{ 0106072, "EU proc control ROM parity" },
	{ 0106200, "Estep 1 data" },
	{ 0107103, "EU proc RAM parity" },
	{ 0107107, "EU proc RAM buffer" },
	{ 0107115, " test count wrong (BD 12)" },
	{ 0112300, "Estep 2" },
	{ 0122240, "ENPR" },
	{ 0122300, "Estep 3" },
	{ 0142300, "Estep 4" },
	{ 0, " unknown error code" }
};

/*
 * If the error bit was set in the controller status register, gripe,
 * then (optionally) reset the controller and requeue pending transfers.
 */
udasaerror(um, doreset)
	register struct uba_ctlr *um;
	int doreset;
{
	register int code = ((struct udadevice *)um->um_addr)->udasa;
	register struct saerr *e;

	if ((code & UDA_ERR) == 0)
		return;
	for (e = saerr; e->code; e++)
		if (e->code == code)
			break;
	printf("uda%d: controller error, sa=0%o (%s%s)\n",
		um->um_ctlr, code, e->desc + 1,
		*e->desc == 'E' ? " error" : "");
	if (doreset) {
		mscp_requeue(&uda_softc[um->um_ctlr].sc_mi);
		(void) udainit(um->um_ctlr);
	}
}

/*
 * Interrupt routine.  Depending on the state of the controller,
 * continue initialisation, or acknowledge command and response
 * interrupts, and process responses.
 */
udaintr(ctlr)
	int ctlr;
{
	register struct uba_ctlr *um = udaminfo[ctlr];
	register struct uda_softc *sc = &uda_softc[ctlr];
	register struct udadevice *udaddr = (struct udadevice *)um->um_addr;
	register struct uda *ud;
	register struct mscp *mp;
	register int i;

#ifdef QBA
	splx(sc->sc_ipl);	/* Qbus interrupt protocol is odd */
#endif
	sc->sc_wticks = 0;	/* reset interrupt watchdog */

	/*
	 * Combinations during steps 1, 2, and 3: STEPnMASK
	 * corresponds to which bits should be tested;
	 * STEPnGOOD corresponds to the pattern that should
	 * appear after the interrupt from STEPn initialisation.
	 * All steps test the bits in ALLSTEPS.
	 */
#define	ALLSTEPS	(UDA_ERR|UDA_STEP4|UDA_STEP3|UDA_STEP2|UDA_STEP1)

#define	STEP1MASK	(ALLSTEPS | UDA_IE | UDA_NCNRMASK)
#define	STEP1GOOD	(UDA_STEP2 | UDA_IE | (NCMDL2 << 3) | NRSPL2)

#define	STEP2MASK	(ALLSTEPS | UDA_IE | UDA_IVECMASK)
#define	STEP2GOOD	(UDA_STEP3 | UDA_IE | (sc->sc_ivec >> 2))

#define	STEP3MASK	ALLSTEPS
#define	STEP3GOOD	UDA_STEP4

	switch (sc->sc_state) {

	case ST_IDLE:
		/*
		 * Ignore unsolicited interrupts.
		 */
		log(LOG_WARNING, "uda%d: stray intr\n", ctlr);
		return;

	case ST_STEP1:
		/*
		 * Begin step two initialisation.
		 */
		if ((udaddr->udasa & STEP1MASK) != STEP1GOOD) {
			i = 1;
initfailed:
			printf("uda%d: init step %d failed, sa=%b\n",
				ctlr, i, udaddr->udasa, udasr_bits);
			udasaerror(um, 0);
			sc->sc_state = ST_IDLE;
			if (sc->sc_flags & SC_DOWAKE) {
				sc->sc_flags &= ~SC_DOWAKE;
				wakeup((caddr_t)sc);
			}
			return;
		}
		udaddr->udasa = (int)&sc->sc_uda->uda_ca.ca_rspdsc[0] |
			(cpu == VAX_780 || cpu == VAX_8600 ? UDA_PI : 0);
		sc->sc_state = ST_STEP2;
		return;

	case ST_STEP2:
		/*
		 * Begin step 3 initialisation.
		 */
		if ((udaddr->udasa & STEP2MASK) != STEP2GOOD) {
			i = 2;
			goto initfailed;
		}
		udaddr->udasa = ((int)&sc->sc_uda->uda_ca.ca_rspdsc[0]) >> 16;
		sc->sc_state = ST_STEP3;
		return;

	case ST_STEP3:
		/*
		 * Set controller characteristics (finish initialisation).
		 */
		if ((udaddr->udasa & STEP3MASK) != STEP3GOOD) {
			i = 3;
			goto initfailed;
		}
		i = udaddr->udasa & 0xff;
		if (i != sc->sc_micro) {
			sc->sc_micro = i;
			printf("uda%d: version %d model %d\n",
				ctlr, i & 0xf, i >> 4);
		}

		/*
		 * Present the burst size, then remove it.  Why this
		 * should be done this way, I have no idea.
		 *
		 * Note that this assumes udaburst[ctlr] > 0.
		 */
		udaddr->udasa = UDA_GO | (udaburst[ctlr] - 1) << 2;
		udaddr->udasa = UDA_GO;
		printf("uda%d: DMA burst size set to %d\n",
			ctlr, udaburst[ctlr]);

		udainitds(ctlr);	/* initialise data structures */

		/*
		 * Before we can get a command packet, we need some
		 * credits.  Fake some up to keep mscp_getcp() happy,
		 * get a packet, and cancel all credits (the right
		 * number should come back in the response to the
		 * SCC packet).
		 */
		sc->sc_mi.mi_credits = MSCP_MINCREDITS + 1;
		mp = mscp_getcp(&sc->sc_mi, MSCP_DONTWAIT);
		if (mp == NULL)	/* `cannot happen' */
			panic("udaintr");
		sc->sc_mi.mi_credits = 0;
		mp->mscp_opcode = M_OP_SETCTLRC;
		mp->mscp_unit = 0;
		mp->mscp_sccc.sccc_ctlrflags = M_CF_ATTN | M_CF_MISC |
			M_CF_THIS;
		*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
		i = udaddr->udaip;
		sc->sc_state = ST_SETCHAR;
		return;

	case ST_SETCHAR:
	case ST_RUN:
		/*
		 * Handle Set Ctlr Characteristics responses and operational
		 * responses (via mscp_dorsp).
		 */
		break;

	default:
		printf("uda%d: driver bug, state %d\n", ctlr, sc->sc_state);
		panic("udastate");
	}

	if (udaddr->udasa & UDA_ERR) {	/* ctlr fatal error */
		udasaerror(um, 1);
		return;
	}

	ud = &uda[ctlr];

	/*
	 * Handle buffer purge requests.
	 */
	if (ud->uda_ca.ca_bdp) {
		UBAPURGE(um->um_hd->uh_uba, ud->uda_ca.ca_bdp);
		ud->uda_ca.ca_bdp = 0;
		udaddr->udasa = 0;	/* signal purge complete */
	}

	/*
	 * Check for response and command ring transitions.
	 */
	if (ud->uda_ca.ca_rspint) {
		ud->uda_ca.ca_rspint = 0;
		mscp_dorsp(&sc->sc_mi);
	}
	if (ud->uda_ca.ca_cmdint) {
		ud->uda_ca.ca_cmdint = 0;
		MSCP_DOCMD(&sc->sc_mi);
	}
	udastart(um);
}

/*
 * Initialise the various data structures that control the UDA50.
 */
udainitds(ctlr)
	int ctlr;
{
	register struct uda *ud = &uda[ctlr];
	register struct uda *uud = uda_softc[ctlr].sc_uda;
	register struct mscp *mp;
	register int i;

	for (i = 0, mp = ud->uda_rsp; i < NRSP; i++, mp++) {
		ud->uda_ca.ca_rspdsc[i] = MSCP_OWN | MSCP_INT |
			(long)&uud->uda_rsp[i].mscp_cmdref;
		mp->mscp_addr = &ud->uda_ca.ca_rspdsc[i];
		mp->mscp_msglen = MSCP_MSGLEN;
	}
	for (i = 0, mp = ud->uda_cmd; i < NCMD; i++, mp++) {
		ud->uda_ca.ca_cmddsc[i] = MSCP_INT |
			(long)&uud->uda_cmd[i].mscp_cmdref;
		mp->mscp_addr = &ud->uda_ca.ca_cmddsc[i];
		mp->mscp_msglen = MSCP_MSGLEN;
	}
}

/*
 * Handle an error datagram.
 */
udadgram(mi, mp)
	struct mscp_info *mi;
	struct mscp *mp;
{

	mscp_decodeerror(mi->mi_md->md_mname, mi->mi_ctlr, mp);
	/*
	 * SDI status information bytes 10 and 11 are the microprocessor
	 * error code and front panel code respectively.  These vary per
	 * drive type and are printed purely for field service information.
	 */
	if (mp->mscp_format == M_FM_SDI)
		printf("\tsdi uproc error code 0x%x, front panel code 0x%x\n",
			mp->mscp_erd.erd_sdistat[10],
			mp->mscp_erd.erd_sdistat[11]);
}

/*
 * The Set Controller Characteristics command finished.
 * Record the new state of the controller.
 */
udactlrdone(mi, mp)
	register struct mscp_info *mi;
	struct mscp *mp;
{
	register struct uda_softc *sc = &uda_softc[mi->mi_ctlr];

	if ((mp->mscp_status & M_ST_MASK) == M_ST_SUCCESS)
		sc->sc_state = ST_RUN;
	else {
		printf("uda%d: SETCTLRC failed: ",
			mi->mi_ctlr, mp->mscp_status);
		mscp_printevent(mp);
		sc->sc_state = ST_IDLE;
	}
	if (sc->sc_flags & SC_DOWAKE) {
		sc->sc_flags &= ~SC_DOWAKE;
		wakeup((caddr_t)sc);
	}
}

/*
 * Received a response from an as-yet unconfigured drive.  Configure it
 * in, if possible.
 */
udaunconf(mi, mp)
	struct mscp_info *mi;
	register struct mscp *mp;
{

	/*
	 * If it is a slave response, copy it to udaslavereply for
	 * udaslave() to look at.
	 */
	if (mp->mscp_opcode == (M_OP_GETUNITST | M_OP_END) &&
	    (uda_softc[mi->mi_ctlr].sc_flags & SC_INSLAVE) != 0) {
		udaslavereply = *mp;
		return (MSCP_DONE);
	}

	/*
	 * Otherwise, it had better be an available attention response.
	 */
	if (mp->mscp_opcode != M_OP_AVAILATTN)
		return (MSCP_FAILED);

	/* do what autoconf does */
	return (MSCP_FAILED);	/* not yet, arwhite, not yet */
}

/*
 * A drive came on line.  Check its type and size.  Return DONE if
 * we think the drive is truly on line.  In any case, awaken anyone
 * sleeping on the drive on-line-ness.
 */
udaonline(ui, mp)
	register struct uba_device *ui;
	struct mscp *mp;
{
	register struct ra_info *ra = &ra_info[ui->ui_unit];

	wakeup((caddr_t)&ui->ui_flags);
	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS) {
		if (!cold)
			printf("uda%d: ra%d", ui->ui_ctlr, ui->ui_unit);
		printf(": attempt to bring on line failed: ");
		mscp_printevent(mp);
		ra->ra_state = CLOSED;
		return (MSCP_FAILED);
	}

	ra->ra_state = OPENRAW;
	ra->ra_dsize = (daddr_t)mp->mscp_onle.onle_unitsize;
	if (!cold)
		printf("ra%d: uda%d, unit %d, size = %d sectors\n", ui->ui_unit,
		    ui->ui_ctlr, mp->mscp_unit, ra->ra_dsize);
	/* can now compute ncyl */
	ra->ra_geom.rg_ncyl = ra->ra_dsize / ra->ra_geom.rg_ntracks /
		ra->ra_geom.rg_nsectors;
	return (MSCP_DONE);
}

/*
 * We got some (configured) unit's status.  Return DONE if it succeeded.
 */
udagotstatus(ui, mp)
	register struct uba_device *ui;
	register struct mscp *mp;
{

	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS) {
		printf("uda%d: attempt to get status for ra%d failed: ",
			ui->ui_ctlr, ui->ui_unit);
		mscp_printevent(mp);
		return (MSCP_FAILED);
	}
	/* record for (future) bad block forwarding and whatever else */
	uda_rasave(ui->ui_unit, mp, 1);
	return (MSCP_DONE);
}

/*
 * A transfer failed.  We get a chance to fix or restart it.
 * Need to write the bad block forwaring code first....
 */
/*ARGSUSED*/
udaioerror(ui, mp, bp)
	register struct uba_device *ui;
	register struct mscp *mp;
	struct buf *bp;
{

	if (mp->mscp_flags & M_EF_BBLKR) {
		/*
		 * A bad block report.  Eventually we will
		 * restart this transfer, but for now, just
		 * log it and give up.
		 */
		log(LOG_ERR, "ra%d: bad block report: %d%s\n",
			ui->ui_unit, mp->mscp_seq.seq_lbn,
			mp->mscp_flags & M_EF_BBLKU ? " + others" : "");
	} else {
		/*
		 * What the heck IS a `serious exception' anyway?
		 * IT SURE WOULD BE NICE IF DEC SOLD DOCUMENTATION
		 * FOR THEIR OWN CONTROLLERS.
		 */
		if (mp->mscp_flags & M_EF_SEREX)
			log(LOG_ERR, "ra%d: serious exception reported\n",
				ui->ui_unit);
	}
	return (MSCP_FAILED);
}

/*
 * A replace operation finished.
 */
/*ARGSUSED*/
udareplace(ui, mp)
	struct uba_device *ui;
	struct mscp *mp;
{

	panic("udareplace");
}

/*
 * A bad block related operation finished.
 */
/*ARGSUSED*/
udabb(ui, mp, bp)
	struct uba_device *ui;
	struct mscp *mp;
	struct buf *bp;
{

	panic("udabb");
}


/*
 * I/O controls.
 */
udaioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register int unit = udaunit(dev);
	register struct disklabel *lp;
	register struct ra_info *ra = &ra_info[unit];
	int error = 0;

	lp = &udalabel[unit];

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[udapart(dev)];
		break;

	case DIOCSDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			error = setdisklabel(lp, (struct disklabel *)data,
			    (ra->ra_state == OPENRAW) ? 0 : ra->ra_openpart);
		break;

	case DIOCWLABEL:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			ra->ra_wlabel = *(int *)data;
		break;

	case DIOCWDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else if ((error = setdisklabel(lp, (struct disklabel *)data,
		    (ra->ra_state == OPENRAW) ? 0 : ra->ra_openpart)) == 0) {
			int wlab;

			ra->ra_state = OPEN;
			/* simulate opening partition 0 so write succeeds */
			ra->ra_openpart |= (1 << 0);		/* XXX */
			wlab = ra->ra_wlabel;
			ra->ra_wlabel = 1;
			error = writedisklabel(dev, udastrategy, lp);
			ra->ra_openpart = ra->ra_copenpart | ra->ra_bopenpart;
			ra->ra_wlabel = wlab;
		}
		break;

#ifdef notyet
	case UDAIOCREPLACE:
		/*
		 * Initiate bad block replacement for the given LBN.
		 * (Should we allow modifiers?)
		 */
		error = EOPNOTSUPP;
		break;

	case UDAIOCGMICRO:
		/*
		 * Return the microcode revision for the UDA50 running
		 * this drive.
		 */
		*(int *)data = uda_softc[uddinfo[unit]->ui_ctlr].sc_micro;
		break;
#endif

	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

/*
 * A Unibus reset has occurred on UBA uban.  Reinitialise the controller(s)
 * on that Unibus, and requeue outstanding I/O.
 */
udareset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uda_softc *sc;
	register int ctlr;

	for (ctlr = 0, sc = uda_softc; ctlr < NUDA; ctlr++, sc++) {
		if ((um = udaminfo[ctlr]) == NULL || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" uda%d", ctlr);

		/*
		 * Our BDP (if any) is gone; our command (if any) is
		 * flushed; the device is no longer mapped; and the
		 * UDA50 is not yet initialised.
		 */
		if (um->um_bdp) {
			printf("<%d>", UBAI_BDP(um->um_bdp));
			um->um_bdp = 0;
		}
		um->um_ubinfo = 0;
		um->um_cmd = 0;
		sc->sc_flags &= ~SC_MAPPED;
		sc->sc_state = ST_IDLE;

		/* reset queues and requeue pending transfers */
		mscp_requeue(&sc->sc_mi);

		/*
		 * If it fails to initialise we will notice later and
		 * try again (and again...).  Do not call udastart()
		 * here; it will be done after the controller finishes
		 * initialisation.
		 */
		if (udainit(ctlr))
			printf(" (hung)");
	}
}

/*
 * Watchdog timer:  If the controller is active, and no interrupts
 * have occurred for 30 seconds, assume it has gone away.
 */
udawatch()
{
	register int i;
	register struct uba_ctlr *um;
	register struct uda_softc *sc;

	timeout(udawatch, (caddr_t) 0, hz);	/* every second */
	for (i = 0, sc = uda_softc; i < NUDA; i++, sc++) {
		if ((um = udaminfo[i]) == 0 || !um->um_alive)
			continue;
		if (sc->sc_state == ST_IDLE)
			continue;
		if (sc->sc_state == ST_RUN && !um->um_tab.b_active)
			sc->sc_wticks = 0;
		else if (++sc->sc_wticks >= 30) {
			sc->sc_wticks = 0;
			printf("uda%d: lost interrupt\n", i);
			ubareset(um->um_ubanum);
		}
	}
}

/*
 * Do a panic dump.  We set up the controller for one command packet
 * and one response packet, for which we use `struct uda1'.
 */
struct	uda1 {
	struct	uda1ca uda1_ca;	/* communications area */
	struct	mscp uda1_rsp;	/* response packet */
	struct	mscp uda1_cmd;	/* command packet */
} uda1;

#define	DBSIZE	32		/* dump 16K at a time */

udadump(dev)
	dev_t dev;
{
	struct udadevice *udaddr;
	struct uda1 *ud_ubaddr;
	char *start;
	int num, blk, unit, maxsz, blkoff, reg;
	struct partition *pp;
	register struct uba_regs *uba;
	register struct uba_device *ui;
	register struct uda1 *ud;
	register struct pte *io;
	register int i;

	/*
	 * Make sure the device is a reasonable place on which to dump.
	 */
	unit = udaunit(dev);
	if (unit >= NRA)
		return (ENXIO);
#define	phys(cast, addr)	((cast) ((int)addr & 0x7fffffff))
	ui = phys(struct uba_device *, udadinfo[unit]);
	if (ui == NULL || ui->ui_alive == 0)
		return (ENXIO);

	/*
	 * Find and initialise the UBA; get the physical address of the
	 * device registers, and of communications area and command and
	 * response packet.
	 */
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
	ubainit(uba);
	udaddr = (struct udadevice *)ui->ui_physaddr;
	ud = phys(struct uda1 *, &uda1);

	/*
	 * Map the ca+packets into Unibus I/O space so the UDA50 can get
	 * at them.  Use the registers at the end of the Unibus map (since
	 * we will use the registers at the beginning to map the memory
	 * we are dumping).
	 */
	num = btoc(sizeof(struct uda1)) + 1;
	reg = NUBMREG - num;
	io = &uba->uba_map[reg];
	for (i = 0; i < num; i++)
		*(int *)io++ = UBAMR_MRV | (btop(ud) + i);
	ud_ubaddr = (struct uda1 *)(((int)ud & PGOFSET) | (reg << 9));

	/*
	 * Initialise the controller, with one command and one response
	 * packet.
	 */
	udaddr->udaip = 0;
	if (udadumpwait(udaddr, UDA_STEP1))
		return (EFAULT);
	udaddr->udasa = UDA_ERR;
	if (udadumpwait(udaddr, UDA_STEP2))
		return (EFAULT);
	udaddr->udasa = (int)&ud_ubaddr->uda1_ca.ca_rspdsc;
	if (udadumpwait(udaddr, UDA_STEP3))
		return (EFAULT);
	udaddr->udasa = ((int)&ud_ubaddr->uda1_ca.ca_rspdsc) >> 16;
	if (udadumpwait(udaddr, UDA_STEP4))
		return (EFAULT);
	uda_softc[ui->ui_ctlr].sc_micro = udaddr->udasa & 0xff;
	udaddr->udasa = UDA_GO;

	/*
	 * Set up the command and response descriptor, then set the
	 * controller characteristics and bring the drive on line.
	 * Note that all uninitialised locations in uda1_cmd are zero.
	 */
	ud->uda1_ca.ca_rspdsc = (long)&ud_ubaddr->uda1_rsp.mscp_cmdref;
	ud->uda1_ca.ca_cmddsc = (long)&ud_ubaddr->uda1_cmd.mscp_cmdref;
	/* ud->uda1_cmd.mscp_sccc.sccc_ctlrflags = 0; */
	/* ud->uda1_cmd.mscp_sccc.sccc_version = 0; */
	if (udadumpcmd(M_OP_SETCTLRC, ud, ui))
		return (EFAULT);
	ud->uda1_cmd.mscp_unit = ui->ui_slave;
	if (udadumpcmd(M_OP_ONLINE, ud, ui))
		return (EFAULT);

	pp = phys(struct partition *,
	    &udalabel[unit].d_partitions[udapart(dev)]);
	maxsz = pp->p_size;
	blkoff = pp->p_offset;

	/*
	 * Dump all of physical memory, or as much as will fit in the
	 * space provided.
	 */
	start = 0;
	num = maxfree;
	if (dumplo + num >= maxsz)
		num = maxsz - dumplo;
	blkoff += dumplo;

	/*
	 * Write out memory, DBSIZE pages at a time.
	 * N.B.: this code depends on the fact that the sector
	 * size == the page size.
	 */
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		io = uba->uba_map;
		/*
		 * Map in the pages to write, leaving an invalid entry
		 * at the end to guard against wild Unibus transfers.
		 * Then do the write.
		 */
		for (i = 0; i < blk; i++)
			*(int *)io++ = UBAMR_MRV | (btop(start) + i);
		*(int *)io = 0;
		ud->uda1_cmd.mscp_unit = ui->ui_slave;
		ud->uda1_cmd.mscp_seq.seq_lbn = btop(start) + blkoff;
		ud->uda1_cmd.mscp_seq.seq_bytecount = blk << PGSHIFT;
		if (udadumpcmd(M_OP_WRITE, ud, ui))
			return (EIO);
		start += blk << PGSHIFT;
		num -= blk;
	}
	return (0);		/* made it! */
}

/*
 * Wait for some of the bits in `bits' to come on.  If the error bit
 * comes on, or ten seconds pass without response, return true (error).
 */
udadumpwait(udaddr, bits)
	register struct udadevice *udaddr;
	register int bits;
{
	register int timo = todr() + 1000;

	while ((udaddr->udasa & bits) == 0) {
		if (udaddr->udasa & UDA_ERR) {
			printf("udasa=%b\ndump ", udaddr->udasa, udasr_bits);
			return (1);
		}
		if (todr() >= timo) {
			printf("timeout\ndump ");
			return (1);
		}
	}
	return (0);
}

/*
 * Feed a command to the UDA50, wait for its response, and return
 * true iff something went wrong.
 */
udadumpcmd(op, ud, ui)
	int op;
	register struct uda1 *ud;
	struct uba_device *ui;
{
	register struct udadevice *udaddr;
	register int n;
#define mp (&ud->uda1_rsp)

	udaddr = (struct udadevice *)ui->ui_physaddr;
	ud->uda1_cmd.mscp_opcode = op;
	ud->uda1_cmd.mscp_msglen = MSCP_MSGLEN;
	ud->uda1_rsp.mscp_msglen = MSCP_MSGLEN;
	ud->uda1_ca.ca_rspdsc |= MSCP_OWN | MSCP_INT;
	ud->uda1_ca.ca_cmddsc |= MSCP_OWN | MSCP_INT;
	if (udaddr->udasa & UDA_ERR) {
		printf("udasa=%b\ndump ", udaddr->udasa, udasr_bits);
		return (1);
	}
	n = udaddr->udaip;
	n = todr() + 1000;
	for (;;) {
		if (todr() > n) {
			printf("timeout\ndump ");
			return (1);
		}
		if (ud->uda1_ca.ca_cmdint)
			ud->uda1_ca.ca_cmdint = 0;
		if (ud->uda1_ca.ca_rspint == 0)
			continue;
		ud->uda1_ca.ca_rspint = 0;
		if (mp->mscp_opcode == (op | M_OP_END))
			break;
		printf("\n");
		switch (MSCP_MSGTYPE(mp->mscp_msgtc)) {

		case MSCPT_SEQ:
			printf("sequential");
			break;

		case MSCPT_DATAGRAM:
			mscp_decodeerror("uda", ui->ui_ctlr, mp);
			printf("datagram");
			break;

		case MSCPT_CREDITS:
			printf("credits");
			break;

		case MSCPT_MAINTENANCE:
			printf("maintenance");
			break;

		default:
			printf("unknown (type 0x%x)",
				MSCP_MSGTYPE(mp->mscp_msgtc));
			break;
		}
		printf(" ignored\ndump ");
		ud->uda1_ca.ca_rspdsc |= MSCP_OWN | MSCP_INT;
	}
	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS) {
		printf("error: op 0x%x => 0x%x status 0x%x\ndump ", op,
			mp->mscp_opcode, mp->mscp_status);
		return (1);
	}
	return (0);
#undef mp
}

/*
 * Return the size of a partition, if known, or -1 if not.
 */
udasize(dev)
	dev_t dev;
{
	register int unit = udaunit(dev);
	register struct uba_device *ui;

	if (unit >= NRA || (ui = udadinfo[unit]) == NULL ||
	    ui->ui_alive == 0 || (ui->ui_flags & UNIT_ONLINE) == 0 ||
	    ra_info[unit].ra_state != OPEN)
		return (-1);
	return ((int)udalabel[unit].d_partitions[udapart(dev)].p_size);
}

#ifdef COMPAT_42
/*
 * Tables mapping unlabelled drives.
 */
struct size {
	daddr_t nblocks;
	daddr_t blkoff;
} ra60_sizes[8] = {
	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	400176,	0,		/* C=sectors 0 thru 400175 */
	82080,	49324,		/* 4.2 G => D=sectors 49324 thru 131403 */
	268772,	131404,		/* 4.2 H => E=sectors 131404 thru 400175 */
	350852,	49324,		/* F=sectors 49324 thru 400175 */
	157570,	242606,		/* UCB G => G=sectors 242606 thru 400175 */
	193282,	49324,		/* UCB H => H=sectors 49324 thru 242605 */
}, ra70_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15972,		/* B=blk 15972 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	15884,	341220,		/* D=blk 341220 thru 357103 */
	55936,	357192,		/* E=blk 357192 thru 413127 */
	-1,	413457,		/* F=blk 413457 thru end */
	-1,	341220,		/* G=blk 341220 thru end */
	291346,	49731,		/* H=blk 49731 thru 341076 */
}, ra80_sizes[8] = {
	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	242606,	0,		/* C=sectors 0 thru 242605 */
	0,	0,		/* D=unused */
	193282,	49324,		/* UCB H => E=sectors 49324 thru 242605 */
	82080,	49324,		/* 4.2 G => F=sectors 49324 thru 131403 */
	192696,	49910,		/* G=sectors 49910 thru 242605 */
	111202,	131404,		/* 4.2 H => H=sectors 131404 thru 242605 */
}, ra81_sizes[8] ={
/*
 * These are the new standard partition sizes for ra81's.
 * An RA_COMPAT system is compiled with D, E, and F corresponding
 * to the 4.2 partitions for G, H, and F respectively.
 */
#ifndef	UCBRA
	15884,	0,		/* A=sectors 0 thru 15883 */
	66880,	16422,		/* B=sectors 16422 thru 83301 */
	891072,	0,		/* C=sectors 0 thru 891071 */
#ifdef RA_COMPAT
	82080,	49324,		/* 4.2 G => D=sectors 49324 thru 131403 */
	759668,	131404,		/* 4.2 H => E=sectors 131404 thru 891071 */
	478582,	412490,		/* 4.2 F => F=sectors 412490 thru 891071 */
#else
	15884,	375564,		/* D=sectors 375564 thru 391447 */
	307200,	391986,		/* E=sectors 391986 thru 699185 */
	191352,	699720,		/* F=sectors 699720 thru 891071 */
#endif RA_COMPAT
	515508,	375564,		/* G=sectors 375564 thru 891071 */
	291346,	83538,		/* H=sectors 83538 thru 374883 */

/*
 * These partitions correspond to the sizes used by sites at Berkeley,
 * and by those sites that have received copies of the Berkeley driver
 * with deltas 6.2 or greater (11/15/83).
 */
#else UCBRA

	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	891072,	0,		/* C=sectors 0 thru 891071 */
	15884,	242606,		/* D=sectors 242606 thru 258489 */
	307200,	258490,		/* E=sectors 258490 thru 565689 */
	325382,	565690,		/* F=sectors 565690 thru 891071 */
	648466,	242606,		/* G=sectors 242606 thru 891071 */
	193282,	49324,		/* H=sectors 49324 thru 242605 */

#endif UCBRA
}, ra82_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	66880,	16245,		/* B=blk 16245 thru 83124 */
	-1,	0,		/* C=blk 0 thru end */
	15884,	375345,		/* D=blk 375345 thru 391228 */
	307200,	391590,		/* E=blk 391590 thru 698789 */
	-1,	699390,		/* F=blk 699390 thru end */
	-1,	375345,		/* G=blk 375345 thru end */
	291346,	83790,		/* H=blk 83790 thru 375135 */
}, rc25_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	10032,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=blk 340670 thru 356553 */
	0,	0,		/* E=blk 356554 thru 412489 */
	0,	0,		/* F=blk 412490 thru end */
	-1,	25916,		/* G=blk 49324 thru 131403 */
	0,	0,		/* H=blk 131404 thru end */
}, rd52_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	9766,	15884,		/* B=blk 15884 thru 25649 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=unused */
	0,	0,		/* E=unused */
	0,	0,		/* F=unused */
	-1,	25650,		/* G=blk 25650 thru end */
	0,	0,		/* H=unused */
}, rd53_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=unused */
	33440,	0,		/* E=blk 0 thru 33439 */
	-1,	33440,		/* F=blk 33440 thru end */
	-1,	49324,		/* G=blk 49324 thru end */
	-1,	15884,		/* H=blk 15884 thru end */
}, rd54_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	130938,	49324,		/* D=blk 49324 thru 180261 */
	130938,	180262,		/* E=blk 180262 thru 311199 (end) */
	0,	0,		/* F=unused */
	261876,	49324,		/* G=blk 49324 thru 311199 (end) */
	0,	0,		/* H=unused */
}, rx50_sizes[8] = {
	800,	0,		/* A=blk 0 thru 799 */
	0,	0,
	-1,	0,		/* C=blk 0 thru end */
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0,
};

/*
 * Media ID decoding table.
 */
struct	udatypes {
	u_long	ut_id;		/* media drive ID */
	char	*ut_name;	/* drive type name */
	struct	size *ut_sizes;	/* partition tables */
	int	ut_nsectors, ut_ntracks, ut_ncylinders;
} udatypes[] = {
	{ MSCP_MKDRIVE2('R', 'A', 60), "ra60", ra60_sizes, 42, 4, 2382 },
	{ MSCP_MKDRIVE2('R', 'A', 70), "ra70", ra70_sizes, 33, 11, 1507 },
	{ MSCP_MKDRIVE2('R', 'A', 80), "ra80", ra80_sizes, 31, 14, 559 },
	{ MSCP_MKDRIVE2('R', 'A', 81), "ra81", ra81_sizes, 51, 14, 1248 },
	{ MSCP_MKDRIVE2('R', 'A', 82), "ra82", ra82_sizes, 57, 15, 1423 },
	{ MSCP_MKDRIVE2('R', 'C', 25), "rc25-removable",
						rc25_sizes, 42, 4, 302 },
	{ MSCP_MKDRIVE3('R', 'C', 'F', 25), "rc25-fixed",
						rc25_sizes, 42, 4, 302 },
	{ MSCP_MKDRIVE2('R', 'D', 52), "rd52", rd52_sizes, 18, 7, 480 },
	{ MSCP_MKDRIVE2('R', 'D', 53), "rd53", rd53_sizes, 18, 8, 963 },
	{ MSCP_MKDRIVE2('R', 'D', 32), "rd54-from-rd32",
						rd54_sizes, 17, 15, 1220 },
	{ MSCP_MKDRIVE2('R', 'D', 54), "rd54", rd54_sizes, 17, 15, 1220 },
	{ MSCP_MKDRIVE2('R', 'X', 50), "rx50", rx50_sizes, 10, 1, 80 },
	0
};

#define NTYPES (sizeof(udatypes) / sizeof(*udatypes))

udamaptype(unit, lp)
	int unit;
	register struct disklabel *lp;
{
	register struct udatypes *ut;
	register struct size *sz;
	register struct partition *pp;
	register char *p;
	register int i;
	register struct ra_info *ra = &ra_info[unit];

	i = MSCP_MEDIA_DRIVE(ra->ra_mediaid);
	for (ut = udatypes; ut->ut_id; ut++)
		if (ut->ut_id == i &&
		    ut->ut_nsectors == ra->ra_geom.rg_nsectors &&
		    ut->ut_ntracks == ra->ra_geom.rg_ntracks &&
		    ut->ut_ncylinders == ra->ra_geom.rg_ncyl)
			goto found;

	/* not one we know; fake up a label for the whole drive */
	uda_makefakelabel(ra, lp);
	i = ra->ra_mediaid;	/* print the port type too */
	addlog(": no partition table for %c%c %c%c%c%d, size %d;\n\
using (s,t,c)=(%d,%d,%d)",
		MSCP_MID_CHAR(4, i), MSCP_MID_CHAR(3, i),
		MSCP_MID_CHAR(2, i), MSCP_MID_CHAR(1, i),
		MSCP_MID_CHAR(0, i), MSCP_MID_NUM(i), lp->d_secperunit,
		lp->d_nsectors, lp->d_ntracks, lp->d_ncylinders);
	if (!cold)
		addlog("\n");
	return (0);
found:
	p = ut->ut_name;
	for (i = 0; i < sizeof(lp->d_typename) - 1 && *p; i++)
		lp->d_typename[i] = *p++;
	lp->d_typename[i] = 0;
	sz = ut->ut_sizes;
	lp->d_nsectors = ut->ut_nsectors;
	lp->d_ntracks = ut->ut_ntracks;
	lp->d_ncylinders = ut->ut_ncylinders;
	lp->d_npartitions = 8;
	lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
	for (pp = lp->d_partitions; pp < &lp->d_partitions[8]; pp++, sz++) {
		pp->p_offset = sz->blkoff;
		if ((pp->p_size = sz->nblocks) == (u_long)-1)
			pp->p_size = ra->ra_dsize - sz->blkoff;
	}
	return (1);
}
#endif /* COMPAT_42 */

/*
 * Construct a label for a drive from geometry information
 * if we have no better information.
 */
uda_makefakelabel(ra, lp)
	register struct ra_info *ra;
	register struct disklabel *lp;
{
	lp->d_nsectors = ra->ra_geom.rg_nsectors;
	lp->d_ntracks = ra->ra_geom.rg_ntracks;
	lp->d_ncylinders = ra->ra_geom.rg_ncyl;
	lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
	bcopy("ra??", lp->d_typename, sizeof("ra??"));
	lp->d_npartitions = 1;
	lp->d_partitions[0].p_offset = 0;
	lp->d_partitions[0].p_size = lp->d_secperunit;
}
#endif /* NUDA > 0 */
