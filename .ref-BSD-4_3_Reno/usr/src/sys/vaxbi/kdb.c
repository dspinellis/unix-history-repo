/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kdb.c	7.9 (Berkeley) 6/28/90
 */

/*
 * KDB50/MSCP device driver
 */

/*
 * TODO
 *	rethink BI software interface
 *	write bad block forwarding code
 */

#include "kra.h"		/* XXX */

#define	DRIVENAMES	"kra"	/* XXX */

#if NKDB > 0

/*
 * CONFIGURATION OPTIONS.  The next three defines are tunable -- tune away!
 *
 * NRSPL2 and NCMDL2 control the number of response and command
 * packets respectively.  They may be any value from 0 to 7, though
 * setting them higher than 5 is unlikely to be of any value.
 * If you get warnings about your command ring being too small,
 * try increasing the values by one.
 *
 * MAXUNIT controls the maximum slave number (and hence number of drives
 * per controller) we are prepared to handle.
 */
#define	NRSPL2	5		/* log2 number of response packets */
#define NCMDL2	5		/* log2 number of command packets */
#define	MAXUNIT	8		/* maximum allowed unit number */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "map.h"
#include "buf.h"
#include "conf.h"
#include "user.h"
#include "proc.h"
#include "vm.h"
#include "dkstat.h"
#include "cmap.h"
#include "syslog.h"
#include "kernel.h"

#define	NRSP	(1 << NRSPL2)
#define	NCMD	(1 << NCMDL2)

#include "../vax/pte.h"
#include "../vax/cpu.h"
#include "../vax/mscp.h"
#include "../vax/mscpvar.h"
#include "../vax/mtpr.h"

#include "bireg.h"
#include "kdbreg.h"

#include "../vaxuba/ubavar.h"

/*
 * Conversions from kernel virtual to physical and page table addresses.
 * PHYS works only for kernel text and primary (compile time) data addresses.
 */
#define	PHYS(cast, addr) \
	((cast) ((int)(addr) & 0x7fffffff))

/*
 * KDB variables, per controller.
 */
struct kdbinfo {
	/* software info, per KDB */
	struct	kdb_regs *ki_kdb;	/* KDB registers */
	struct	kdb_regs *ki_physkdb;	/* phys address of KDB registers */
	short	ki_state;		/* KDB50 state; see below */
	short	ki_flags;		/* flags; see below */
	int	ki_micro;		/* microcode revision */
	short	ki_vec;			/* scb vector offset */
	short	ki_wticks;		/* watchdog timer ticks */

	/*
	 * KDB PTEs must be contiguous.  Some I/O is done on addresses
	 * for which this is true (PTEs in Sysmap and Usrptmap), but
	 * other transfers may have PTEs that are scattered in physical
	 * space.  Ki_map maps a physically contiguous PTE space used
	 * for these transfers.
	 */
#define KI_MAPSIZ	(NCMD + 2)
	struct	map *ki_map;		/* resource map */
#define KI_PTES		256
	struct	pte ki_pte[KI_PTES];	/* contiguous PTE space */
	long	ki_ptephys;		/* phys address of &ki_pte[0] */

	struct	mscp_info ki_mi;	/* MSCP info (per mscpvar.h) */
	struct	buf ki_tab;		/* controller queue */

	/* stuff read and written by hardware */
	struct	kdbca ki_ca;		/* communications area */
	struct	mscp ki_rsp[NRSP];	/* response packets */
	struct	mscp ki_cmd[NCMD];	/* command packets */
} kdbinfo[NKDB];

#define	ki_ctlr	ki_mi.mi_ctlr

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
#define	KDB_ALIVE	0x01	/* this KDB50 exists */
#define	KDB_GRIPED	0x04	/* griped about cmd ring too small */
#define	KDB_INSLAVE	0x08	/* inside kdbslave() */
#define	KDB_DOWAKE	0x10	/* wakeup when ctlr init done */

struct kdbstats kdbstats;	/* statistics */

/*
 * Device to unit number and partition:
 */
#define	UNITSHIFT	3
#define	UNITMASK	7
#define	kdbunit(dev)	(minor(dev) >> UNITSHIFT)
#define	kdbpart(dev)	(minor(dev) & UNITMASK)

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
/* THESE SHOULD BE SHARED WITH uda.c (but not yet) */
struct size {
	daddr_t nblocks;
	daddr_t blkoff;
} kra81_sizes[8] = {
#ifdef MARYLAND
	67832,	0,		/* A=cyl    0 thru   94 + 2 sectors */
	67828,	67832,		/* B=cyl   95 thru  189 - 2 sectors */
	-1,	0,		/* C=cyl    0 thru 1247 */
	-1,	135660,		/* D=cyl  190 thru 1247 */
	449466,	49324,		/* E xxx */
	64260,	498790,		/* F xxx */
	328022,	563050,		/* G xxx */
	0,	0,
#else
	15884,	0,		/* a */
	33440,	15884,		/* b */
	-1,	0,		/* c */
	-1,	49324,		/* d */
	449466,	49324,		/* e */
	64260,	498790,		/* f */
	328022,	563050,		/* g */
	0,	0,
#endif
}, kra80_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,
	0,	0,
	0,	0,
	82080,	49324,		/* G=blk 49324 thru 131403 */
	-1,	131404,		/* H=blk 131404 thru end */
}, kra60_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	-1,	49324,		/* D=blk 49324 thru end */
	0,	0,
	0,	0,
	82080,	49324,		/* G=blk 49324 thru 131403 */
	-1,	131404,		/* H=blk 131404 thru end */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

/*
 * Drive type index decoding table.  `ut_name' is null iff the
 * type is not known.
 */
struct	kdbtypes {
	char	*ut_name;	/* drive type name */
	struct	size *ut_sizes;	/* partition tables */
} kdbtypes[] = {
	NULL,		NULL,
	"ra80",		kra80_sizes,	/* 1 = ra80 */
	NULL,		NULL,
	NULL,		NULL,
	"ra60",		kra60_sizes,	/* 4 = ra60 */
	"ra81",		kra81_sizes,	/* 5 = ra81 */
};

#define NTYPES 6

/*
 * Definition of the driver for autoconf and generic MSCP code.
 * SOME OF THIS IS BOGUS (must fix config)
 */

#ifdef notdef		/* not when driver is for kra disks */
/*
 * Some of these variables (per-drive stuff) are shared
 * with the UDA50 code (why not, they are the same drives).
 * N.B.: kdbdinfo must not be shared.
 */
#define	kdbutab		udautab		/* shared */
#define	kdbslavereply	udaslavereply	/* shared */
#endif

int	kdbprobe();		/* XXX */
int	kdbslave(), kdbattach();

int	kdbdgram(), kdbctlrdone(), kdbunconf(), kdbiodone();
int	kdbonline(), kdbgotstatus(), kdbioerror();

struct	uba_device *kdbdinfo[NKRA];	/* uba_device indeed! */
struct	buf kdbutab[NKRA];	/* per drive transfer queue */

u_short kdbstd[] = { 0 };	/* XXX */
struct uba_driver kdbdriver =	/* XXX */
 { kdbprobe, kdbslave, kdbattach, 0, kdbstd, DRIVENAMES, kdbdinfo, "kdb" };

struct	mscp_driver kdbmscpdriver =
 { MAXUNIT, NKRA, UNITSHIFT, kdbutab, (struct disklabel *)0, kdbdinfo,
   kdbdgram, kdbctlrdone, kdbunconf, kdbiodone,
   kdbonline, kdbgotstatus, NULL, kdbioerror, NULL,
   "kdb", DRIVENAMES };

/*
 * Miscellaneous private variables.
 */
char	kdbsr_bits[] = KDBSR_BITS;

struct	uba_device *kdbip[NKDB][MAXUNIT];
				/* inverting pointers: ctlr & unit => `Unibus'
				   device pointer */

daddr_t	ra_dsize[NKRA];		/* drive sizes, from on line end packets */

struct	mscp kdbslavereply;	/* get unit status response packet, set
				   for kdbslave by kdbunconf, via kdbintr */

int	kdbwstart, kdbwatch();	/* watchdog timer */
int	wakeup();

/*
 * If kdbprobe is called, return 0 to keep Unibus code from attempting
 * to use this device.	XXX rethink
 */
/* ARGSUSED */
kdbprobe(reg, ctlr)
	caddr_t reg;
	int ctlr;
{

	return (0);
}

/*
 * Configure in a KDB50 controller.
 */
kdbconfig(kdbnum, va, pa, vec)
	int kdbnum;
	struct biiregs *va, *pa;
	int vec;
{
	register struct kdbinfo *ki;
#define mi (&ki->ki_mi)

#ifdef lint
	extern int (*kdbint0[])();

	(*kdbint0[0])(0);	/* this is a config botch */
	kdbintr(0);
#endif

	/*
	 * Set up local KDB status.
	 */
	ki = &kdbinfo[kdbnum];
	ki->ki_kdb = (struct kdb_regs *)va;
	ki->ki_physkdb = (struct kdb_regs *)pa;
	ki->ki_vec = vec;
	ki->ki_map =
	    (struct map *)malloc((u_long)(KI_MAPSIZ * sizeof(struct map)),
	    M_DEVBUF, M_NOWAIT);
	if (ki->ki_map == NULL) {
		printf("kdb%d: cannot get memory for ptes\n", kdbnum);
		return;
	}
	ki->ki_ptephys = PHYS(long, ki->ki_pte); /* kvtophys(ki->ki_pte) */
	ki->ki_flags = KDB_ALIVE;

	/* THE FOLLOWING IS ONLY NEEDED TO CIRCUMVENT A BUG IN rminit */
	bzero((caddr_t)ki->ki_map, KI_MAPSIZ * sizeof(struct map));

	rminit(ki->ki_map, (long)KI_PTES, (long)1, "kdb", KI_MAPSIZ);

	/*
	 * Set up the generic MSCP structures.
	 */
	mi->mi_md = &kdbmscpdriver;
	mi->mi_ctlr = kdbnum;	/* also sets ki->ki_ctlr */
	mi->mi_tab = &ki->ki_tab;
	mi->mi_ip = kdbip[kdbnum];
	mi->mi_cmd.mri_size = NCMD;
	mi->mi_cmd.mri_desc = ki->ki_ca.ca_cmddsc;
	mi->mi_cmd.mri_ring = ki->ki_cmd;
	mi->mi_rsp.mri_size = NRSP;
	mi->mi_rsp.mri_desc = ki->ki_ca.ca_rspdsc;
	mi->mi_rsp.mri_ring = ki->ki_rsp;
	mi->mi_wtab.av_forw = mi->mi_wtab.av_back = &mi->mi_wtab;
#undef mi
}

/*
 * Find a slave.
 * Note that by the time kdbslave is called, the interrupt vector
 * for the KDB50 has been set up (so that kdbunconf() will be called).
 */
kdbslave(ui)
	register struct uba_device *ui;
{
	register struct kdbinfo *ki;
	register struct mscp *mp;
	int next = 0, type, timeout, tries, i;

#ifdef lint
	i = 0; i = i;
#endif
	/*
	 * Make sure the controller is fully initialised, by waiting
	 * for it if necessary.
	 */
	ki = &kdbinfo[ui->ui_ctlr];
	if (ki->ki_state == ST_RUN)
		goto findunit;
	tries = 0;
again:
	if (kdbinit(ki))
		return (0);
	timeout = todr() + 1000;		/* 10 seconds */
	while (todr() < timeout)
		if (ki->ki_state == ST_RUN)	/* made it */
			goto findunit;
	if (++tries < 2)
		goto again;
	printf("kdb%d: controller hung\n", ki->ki_ctlr);
	return (0);

	/*
	 * The controller is all set; go find the unit.  Grab an
	 * MSCP packet and send out a Get Unit Status command, with
	 * the `next unit' modifier if we are looking for a generic
	 * unit.  We set the `in slave' flag so that kdbunconf()
	 * knows to copy the response to `kdbslavereply'.
	 */
findunit:
	kdbslavereply.mscp_opcode = 0;
	ki->ki_flags |= KDB_INSLAVE;
	if ((mp = mscp_getcp(&ki->ki_mi, MSCP_DONTWAIT)) == NULL)
		panic("kdbslave");		/* `cannot happen' */
	mp->mscp_opcode = M_OP_GETUNITST;
	if (ui->ui_slave == '?') {
		mp->mscp_unit = next;
		mp->mscp_modifier = M_GUM_NEXTUNIT;
	} else {
		mp->mscp_unit = ui->ui_slave;
		mp->mscp_modifier = 0;
	}
	*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
	i = ki->ki_kdb->kdb_ip;	/* initiate polling */
	mp = &kdbslavereply;
	timeout = todr() + 1000;
	while (todr() < timeout)
		if (mp->mscp_opcode)
			goto gotit;
	printf("kdb%d: no response to Get Unit Status request\n",
		ki->ki_ctlr);
	ki->ki_flags &= ~KDB_INSLAVE;
	return (0);

gotit:
	ki->ki_flags &= ~KDB_INSLAVE;

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
			printf("kdb%d: unit %d off line:", ki->ki_ctlr,
				mp->mscp_unit);
			mscp_printevent(mp);
			goto try_another;
		}
		break;

	default:
		printf("kdb%d: unable to get unit status:", ki->ki_ctlr);
		mscp_printevent(mp);
		return (0);
	}

	/*
	 * Does this ever happen?  What (if anything) does it mean?
	 */
	if (mp->mscp_unit < next) {
		printf("kdb%d: unit %d, next %d\n",
			ki->ki_ctlr, mp->mscp_unit, next);
		return (0);
	}

	if (mp->mscp_unit >= MAXUNIT) {
		printf("kdb%d: cannot handle unit number %d (max is %d)\n",
			ki->ki_ctlr, mp->mscp_unit, MAXUNIT - 1);
		return (0);
	}

	/*
	 * See if we already handle this drive.
	 * (Only likely if ui->ui_slave=='?'.)
	 */
	if (kdbip[ki->ki_ctlr][mp->mscp_unit] != NULL)
		goto try_another;

	/*
	 * Make sure we know about this kind of drive.
	 * Others say we should treat unknowns as RA81s; I am
	 * not sure this is safe.
	 */
	type = mp->mscp_guse.guse_drivetype;
	if (type >= NTYPES || kdbtypes[type].ut_name == 0) {
		register long id = mp->mscp_guse.guse_mediaid;

		printf("kdb%d: unit %d: media ID `", ki->ki_ctlr,
			mp->mscp_unit);
		printf("%c%c %c%c%c%d",
			MSCP_MID_CHAR(4, id), MSCP_MID_CHAR(3, id),
			MSCP_MID_CHAR(2, id), MSCP_MID_CHAR(1, id),
			MSCP_MID_CHAR(0, id), MSCP_MID_NUM(id));
		printf("' is of unknown type %d; ignored\n", type);
try_another:
		if (ui->ui_slave != '?')
			return (0);
		next = mp->mscp_unit + 1;
		goto findunit;
	}

	/*
	 * Voila!
	 */
	ui->ui_type = type;
	ui->ui_flags = 0;	/* not on line, nor anything else */
	ui->ui_slave = mp->mscp_unit;
	return (1);
}

/*
 * Attach a found slave.  Make sure the watchdog timer is running.
 * If this disk is being profiled, fill in the `wpms' value (used by
 * what?).  Set up the inverting pointer, and attempt to bring the
 * drive on line.
 */
kdbattach(ui)
	register struct uba_device *ui;
{

	if (kdbwstart == 0) {
		timeout(kdbwatch, (caddr_t)0, hz);
		kdbwstart++;
	}
	if (ui->ui_dk >= 0)
		dk_wpms[ui->ui_dk] = (60 * 31 * 256);	/* approx */
	kdbip[ui->ui_ctlr][ui->ui_slave] = ui;
	(void) kdb_bringonline(ui, 1);
	/* should we get its status too? */
}

/*
 * Initialise a KDB50.  Return true iff something goes wrong.
 */
kdbinit(ki)
	register struct kdbinfo *ki;
{
	register struct kdb_regs *ka = ki->ki_kdb;
	int timo;

	/*
	 * While we are thinking about it, reset the next command
	 * and response indicies.
	 */
	ki->ki_mi.mi_cmd.mri_next = 0;
	ki->ki_mi.mi_rsp.mri_next = 0;

	/*
	 * Start up the hardware initialisation sequence.
	 */
#define	STEP0MASK (KDB_ERR | KDB_STEP4 | KDB_STEP3 | KDB_STEP2 | KDB_STEP1)

	ki->ki_state = ST_IDLE;	/* in case init fails */

	bi_reset(&ka->kdb_bi);	/* reset bi node (but not the BI itself) */

	timo = todr() + 1000;
	while ((ka->kdb_sa & STEP0MASK) == 0) {
		if (todr() > timo) {
			printf("kdb%d: timeout during init\n", ki->ki_ctlr);
			return (-1);
		}
	}
	if ((ka->kdb_sa & STEP0MASK) != KDB_STEP1) {
		printf("kdb%d: init failed, sa=%b\n", ki->ki_ctlr,
			ka->kdb_sa, kdbsr_bits);
		return (-1);
	}

	/*
	 * Success!  Record new state, and start step 1 initialisation.
	 * The rest is done in the interrupt handler.
	 */
	ki->ki_state = ST_STEP1;
	ka->kdb_bi.bi_intrdes = 1 << mastercpu;
#ifdef unneeded /* is it? */
	ka->kdb_bi.bi_csr = (ka->kdb_bi.bi_csr&~BICSR_ARB_MASK)|BICSR_ARB_???;
#endif
	ka->kdb_bi.bi_bcicsr |= BCI_STOPEN | BCI_IDENTEN | BCI_UINTEN |
		BCI_INTEN;

/* I THINK THIS IS WRONG */
/* Mach uses 0x601d0, which includes IPL16, but 1d0 is IPL17, nexzvec...? */
	ka->kdb_bi.bi_eintrcsr = BIEIC_IPL15 | ki->ki_vec;	/* ??? */
/* END I THINK WRONG */

	ka->kdb_bi.bi_uintrcsr = ki->ki_vec;
	ka->kdb_sw = KDB_ERR | (NCMDL2 << 11) | (NRSPL2 << 8) | KDB_IE |
		(ki->ki_vec >> 2);
	return (0);
}

/*
 * Open a drive.
 */
/*ARGSUSED*/
kdbopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct uba_device *ui;
	register struct kdbinfo *ki;
	int s;

	/*
	 * Make sure this is a reasonable open request.
	 */
	unit = kdbunit(dev);
	if (unit >= NKRA || (ui = kdbdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);

	/*
	 * Make sure the controller is running, by (re)initialising it if
	 * necessary.
	 */
	ki = &kdbinfo[ui->ui_ctlr];
	s = spl5();
	if (ki->ki_state != ST_RUN) {
		if (ki->ki_state == ST_IDLE && kdbinit(ki)) {
			splx(s);
			return (EIO);
		}
		/*
		 * In case it does not come up, make sure we will be
		 * restarted in 10 seconds.  This corresponds to the
		 * 10 second timeouts in kdbprobe() and kdbslave().
		 */
		ki->ki_flags |= KDB_DOWAKE;
		timeout(wakeup, (caddr_t)&ki->ki_flags, 10 * hz);
		sleep((caddr_t)&ki->ki_flags, PRIBIO);
		if (ki->ki_state != ST_RUN) {
			splx(s);
			printf("kdb%d: controller hung\n", ui->ui_ctlr);
			return (EIO);
		}
		untimeout(wakeup, (caddr_t)&ki->ki_flags);
	}
	if ((ui->ui_flags & UNIT_ONLINE) == 0) {
		/*
		 * Bring the drive on line so we can find out how
		 * big it is.  If it is not spun up, it will not
		 * come on line; this cannot really be considered
		 * an `error condition'.
		 */
		if (kdb_bringonline(ui, 0)) {
			splx(s);
			printf("%s%d: drive will not come on line\n",
				kdbdriver.ud_dname, unit);
			return (EIO);
		}
	}
	splx(s);
	return (0);
}

/*
 * Bring a drive on line.  In case it fails to respond, we set
 * a timeout on it.  The `nosleep' parameter should be set if
 * we are to spin-wait; otherwise this must be called at spl5().
 */
kdb_bringonline(ui, nosleep)
	register struct uba_device *ui;
	int nosleep;
{
	register struct kdbinfo *ki = &kdbinfo[ui->ui_ctlr];
	register struct mscp *mp;
	int i;

	if (nosleep) {
		mp = mscp_getcp(&ki->ki_mi, MSCP_DONTWAIT);
		if (mp == NULL)
			return (-1);
	} else
		mp = mscp_getcp(&ki->ki_mi, MSCP_WAIT);
	mp->mscp_opcode = M_OP_ONLINE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_cmdref = (long)&ui->ui_flags;
	*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
	i = ki->ki_kdb->kdb_ip;

	if (nosleep) {
		i = todr() + 1000;
		while ((ui->ui_flags & UNIT_ONLINE) == 0)
			if (todr() > i)
				return (-1);
	} else {
		timeout(wakeup, (caddr_t)&ui->ui_flags, 10 * hz);
		sleep((caddr_t)&ui->ui_flags, PRIBIO);
		if ((ui->ui_flags & UNIT_ONLINE) == 0)
			return (-1);
		untimeout(wakeup, (caddr_t)&ui->ui_flags);
	}
	return (0);	/* made it */
}

/*
 * Queue a transfer request, and if possible, hand it to the controller.
 *
 * This routine is broken into two so that the internal version
 * kdbstrat1() can be called by the (nonexistent, as yet) bad block
 * revectoring routine.
 */
kdbstrategy(bp)
	register struct buf *bp;
{
	register int unit;
	register struct uba_device *ui;
	register struct size *st;
	daddr_t sz, maxsz;

	/*
	 * Make sure this is a reasonable drive to use.
	 */
	if ((unit = kdbunit(bp->b_dev)) >= NKRA ||
	    (ui = kdbdinfo[unit]) == NULL || ui->ui_alive == 0) {
		bp->b_error = ENXIO;
		bp->b_flags |= B_ERROR;
		biodone(bp);
		return;
	}

	/*
	 * Determine the size of the transfer, and make sure it is
	 * within the boundaries of the drive.
	 */
	sz = (bp->b_bcount + 511) >> 9;
	st = &kdbtypes[ui->ui_type].ut_sizes[kdbpart(bp->b_dev)];
	if ((maxsz = st->nblocks) < 0)
		maxsz = ra_dsize[unit] - st->blkoff;
	if (bp->b_blkno < 0 || bp->b_blkno + sz > maxsz ||
	    st->blkoff >= ra_dsize[unit]) {
		/* if exactly at end of disk, return an EOF */
		if (bp->b_blkno == maxsz)
			bp->b_resid = bp->b_bcount;
		else {
			bp->b_error = EINVAL;
			bp->b_flags |= B_ERROR;
		}
		biodone(bp);
		return;
	}
	kdbstrat1(bp);
}

/*
 * Work routine for kdbstrategy.
 */
kdbstrat1(bp)
	register struct buf *bp;
{
	register int unit = kdbunit(bp->b_dev);
	register struct buf *dp;
	register struct kdbinfo *ki;
	struct uba_device *ui;
	int s;

	/*
	 * Append the buffer to the drive queue, and if it is not
	 * already there, the drive to the controller queue.  (However,
	 * if the drive queue is marked to be requeued, we must be
	 * awaiting an on line or get unit status command; in this
	 * case, leave it off the controller queue.)
	 */
	ui = kdbdinfo[unit];
	ki = &kdbinfo[ui->ui_ctlr];
	dp = &kdbutab[unit];
	s = spl5();
	APPEND(bp, dp, av_forw);
	if (dp->b_active == 0 && (ui->ui_flags & UNIT_REQUEUE) == 0) {
		APPEND(dp, &ki->ki_tab, b_forw);
		dp->b_active++;
	}

	/*
	 * Start activity on the controller.
	 */
	kdbstart(ki);
	splx(s);
}

/*
 * Find the physical address of some contiguous PTEs that map the
 * transfer described in `bp', creating them (by copying) if
 * necessary.  Store the physical base address of the map through
 * mapbase, and the page offset through offset, and any resource
 * information in *info (or 0 if none).
 *
 * If we cannot allocate space, return a nonzero status.
 */
int
kdbmap(ki, bp, mapbase, offset, info)
	struct kdbinfo *ki;
	register struct buf *bp;
	long *mapbase, *offset;
	int *info;
{
	register struct pte *spte, *dpte;
	register struct proc *rp;
	register int i, a, o;
	u_int v;
	int npf;

	o = (int)bp->b_un.b_addr & PGOFSET;

	/* handle contiguous cases */
	if ((bp->b_flags & B_PHYS) == 0) {
		spte = kvtopte(bp->b_un.b_addr);
		kdbstats.ks_sys++;
		*mapbase = PHYS(long, spte);
		*offset = o;
		*info = 0;
		return (0);
	}
	if (bp->b_flags & B_PAGET) {
		spte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
if (spte->pg_v == 0) panic("kdbmap");
		kdbstats.ks_paget++;
		*mapbase = PHYS(long, spte);
		*offset = o;
		*info = 0;
		return (0);
	}

	/* potentially discontiguous or invalid ptes */
	v = btop(bp->b_un.b_addr);
	rp = bp->b_flags & B_DIRTY ? &proc[2] : bp->b_proc;
	if (bp->b_flags & B_UAREA)
		spte = &rp->p_addr[v];
	else
		spte = vtopte(rp, v);
	npf = btoc(bp->b_bcount + o);

#ifdef notdef
	/*
	 * The current implementation of the VM system requires
	 * that all of these be done with a copy.  Even if the
	 * PTEs could be used now, they may be snatched out from
	 * under us later.  It would be nice if we could stop that....
	 */

	/* check for invalid */
	/* CONSIDER CHANGING VM TO VALIDATE PAGES EARLIER */
	for (dpte = spte, i = npf; --i >= 0; dpte++)
		if (dpte->pg_v == 0)
			goto copy1;
	/*
	 * Check for discontiguous physical pte addresses.  It is
	 * not necessary to check each pte, since they come in clumps
	 * of pages.
	 */
	i = howmany(npf + (((int)spte & PGOFSET) / sizeof (*spte)), NPTEPG);
	/* often i==1, and we can avoid work */
	if (--i > 0) {
		dpte = kvtopte(spte);
		a = dpte->pg_pfnum;
		while (--i >= 0)
			if ((++dpte)->pg_pfnum != ++a)
				goto copy2;
	}

	/* made it */
	kdbstats.ks_contig++;
	*mapbase = kvtophys(spte);
	*offset = o;
	*info = 0;
	return (0);

copy1:
	kdbstats.ks_inval++;		/* temp */
copy2:
#endif /* notdef */
	kdbstats.ks_copies++;
	i = npf + 1;
	if ((a = rmalloc(ki->ki_map, (long)i)) == 0) {
		kdbstats.ks_mapwait++;
		return (-1);
	}
	*info = (i << 16) | a;
	a--;
	/* if offset > PGOFSET, btop(offset) indexes mapbase */
	*mapbase = ki->ki_ptephys;
	*offset = (a << PGSHIFT) | o;
	dpte = &ki->ki_pte[a];
	while (--i > 0)
		*(int *)dpte++ = PG_V | *(int *)spte++;
	*(int *)dpte = 0;
	return (0);
}

#define	KDBFREE(ki, info) if (info) \
	rmfree((ki)->ki_map, (long)((info) >> 16), (long)((info) & 0xffff))

/*
 * Start up whatever transfers we can find.
 * Note that kdbstart() must be called at spl5().
 */
kdbstart(ki)
	register struct kdbinfo *ki;
{
	register struct buf *bp, *dp;
	register struct mscp *mp;
	register struct uba_device *ui;
	long mapbase, offset;
	int info, ncmd = 0;

	/*
	 * If it is not running, try (again and again...) to initialise
	 * it.  If it is currently initialising just ignore it for now.
	 */
	if (ki->ki_state != ST_RUN) {
		if (ki->ki_state == ST_IDLE && kdbinit(ki))
			printf("kdb%d: still hung\n", ki->ki_ctlr);
		return;
	}

loop:
	/* if insufficient credit, avoid overhead */
	if (ki->ki_mi.mi_credits <= MSCP_MINCREDITS)
		goto out;

	/*
	 * Service the drive at the head of the queue.  It may not
	 * need anything; eventually this will finish up the close
	 * protocol, but that is yet to be implemented here.
	 */
	if ((dp = ki->ki_tab.b_actf) == NULL)
		goto out;
	if ((bp = dp->b_actf) == NULL) {
		dp->b_active = 0;
		ki->ki_tab.b_actf = dp->b_forw;
		goto loop;
	}

	if (ki->ki_kdb->kdb_sa & KDB_ERR) {	/* ctlr fatal error */
		kdbsaerror(ki);
		goto out;
	}

	 /* find or create maps for this transfer */
	 if (kdbmap(ki, bp, &mapbase, &offset, &info))
		goto out;	/* effectively, resource wait */

	/*
	 * Get an MSCP packet, then figure out what to do.  If
	 * we cannot get a command packet, the command ring may
	 * be too small:  We should have at least as many command
	 * packets as credits, for best performance.
	 */
	if ((mp = mscp_getcp(&ki->ki_mi, MSCP_DONTWAIT)) == NULL) {
		if (ki->ki_mi.mi_credits > MSCP_MINCREDITS &&
		    (ki->ki_flags & KDB_GRIPED) == 0) {
			log(LOG_NOTICE, "kdb%d: command ring too small\n",
				ki->ki_ctlr);
			ki->ki_flags |= KDB_GRIPED;/* complain only once */
		}
		KDBFREE(ki, info);
		goto out;
	}

	/*
	 * Bring the drive on line if it is not already.  Get its status
	 * if we do not already have it.  Otherwise just start the transfer.
	 */
	ui = kdbdinfo[kdbunit(bp->b_dev)];
	if ((ui->ui_flags & UNIT_ONLINE) == 0) {
		mp->mscp_opcode = M_OP_ONLINE;
		goto common;
	}
	if ((ui->ui_flags & UNIT_HAVESTATUS) == 0) {
		mp->mscp_opcode = M_OP_GETUNITST;
common:
if (ui->ui_flags & UNIT_REQUEUE) panic("kdbstart");
		/*
		 * Take the drive off the controller queue.  When the
		 * command finishes, make sure the drive is requeued.
		 * Give up any mapping (not needed now).  This last is
		 * not efficient, but is rare.
		 */
		KDBFREE(ki, info);
		ki->ki_tab.b_actf = dp->b_forw;
		dp->b_active = 0;
		ui->ui_flags |= UNIT_REQUEUE;
		mp->mscp_unit = ui->ui_slave;
		*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
		ncmd++;
		goto loop;
	}

	mp->mscp_opcode = (bp->b_flags & B_READ) ? M_OP_READ : M_OP_WRITE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_seq.seq_lbn = bp->b_blkno +
		kdbtypes[ui->ui_type].ut_sizes[kdbpart(bp->b_dev)].blkoff;
	mp->mscp_seq.seq_bytecount = bp->b_bcount;

	mp->mscp_seq.seq_buffer = offset | KDB_MAP;
	mp->mscp_seq.seq_mapbase = mapbase;

	/* profile the drive */
	if (ui->ui_dk >= 0) {
		dk_busy |= 1 << ui->ui_dk;
		dk_xfer[ui->ui_dk]++;
		dk_wds[ui->ui_dk] += bp->b_bcount >> 6;
	}

	/*
	 * Fill in the rest of the MSCP packet and move the buffer to the
	 * I/O wait queue.
	 */
	mscp_go(&ki->ki_mi, mp, info);
	ncmd++;			/* note the transfer */
	ki->ki_tab.b_active++;	/* another one going */
	goto loop;

out:
	if (ncmd >= KS_MAXC)
		ncmd = KS_MAXC - 1;
	kdbstats.ks_cmd[ncmd]++;
	if (ncmd)		/* start some transfers */
		ncmd = ki->ki_kdb->kdb_ip;
}

/* ARGSUSED */
kdbiodone(mi, bp, info)
	struct mscp_info *mi;
	struct buf *bp;
	int info;
{
	register struct kdbinfo *ki = &kdbinfo[mi->mi_ctlr];

	KDBFREE(ki, info);
	biodone(bp);
	ki->ki_tab.b_active--;	/* another one done */
}

/*
 * The error bit was set in the controller status register.  Gripe,
 * reset the controller, requeue pending transfers.
 */
kdbsaerror(ki)
	register struct kdbinfo *ki;
{

	printf("kdb%d: controller error, sa=%b\n", ki->ki_ctlr,
		ki->ki_kdb->kdb_sa, kdbsr_bits);
	mscp_requeue(&ki->ki_mi);
	(void) kdbinit(ki);
}

/*
 * Interrupt routine.  Depending on the state of the controller,
 * continue initialisation, or acknowledge command and response
 * interrupts, and process responses.
 */
kdbintr(ctlr)
	int ctlr;
{
	register struct kdbinfo *ki = &kdbinfo[ctlr];
	register struct kdb_regs *kdbaddr = ki->ki_kdb;
	register struct mscp *mp;
	register int i;

	ki->ki_wticks = 0;	/* reset interrupt watchdog */

	/*
	 * Combinations during steps 1, 2, and 3: STEPnMASK
	 * corresponds to which bits should be tested;
	 * STEPnGOOD corresponds to the pattern that should
	 * appear after the interrupt from STEPn initialisation.
	 * All steps test the bits in ALLSTEPS.
	 */
#define	ALLSTEPS	(KDB_ERR|KDB_STEP4|KDB_STEP3|KDB_STEP2|KDB_STEP1)

#define	STEP1MASK	(ALLSTEPS | KDB_IE | KDB_NCNRMASK)
#define	STEP1GOOD	(KDB_STEP2 | KDB_IE | (NCMDL2 << 3) | NRSPL2)

#define	STEP2MASK	(ALLSTEPS | KDB_IE | KDB_IVECMASK)
#define	STEP2GOOD	(KDB_STEP3 | KDB_IE | (ki->ki_vec >> 2))

#define	STEP3MASK	ALLSTEPS
#define	STEP3GOOD	KDB_STEP4

	switch (ki->ki_state) {

	case ST_IDLE:
		/*
		 * Ignore unsolicited interrupts.
		 */
		log(LOG_WARNING, "kdb%d: stray intr\n", ctlr);
		return;

	case ST_STEP1:
		/*
		 * Begin step two initialisation.
		 */
		if ((kdbaddr->kdb_sa & STEP1MASK) != STEP1GOOD) {
			i = 1;
initfailed:
			printf("kdb%d: init step %d failed, sa=%b\n",
				ctlr, i, kdbaddr->kdb_sa, kdbsr_bits);
			ki->ki_state = ST_IDLE;
			if (ki->ki_flags & KDB_DOWAKE) {
				ki->ki_flags &= ~KDB_DOWAKE;
				wakeup((caddr_t)&ki->ki_flags);
			}
			return;
		}
		kdbaddr->kdb_sw = PHYS(int, &ki->ki_ca.ca_rspdsc[0]);
		ki->ki_state = ST_STEP2;
		return;

	case ST_STEP2:
		/*
		 * Begin step 3 initialisation.
		 */
		if ((kdbaddr->kdb_sa & STEP2MASK) != STEP2GOOD) {
			i = 2;
			goto initfailed;
		}
		kdbaddr->kdb_sw = PHYS(int, &ki->ki_ca.ca_rspdsc[0]) >> 16;
		ki->ki_state = ST_STEP3;
		return;

	case ST_STEP3:
		/*
		 * Set controller characteristics (finish initialisation).
		 */
		if ((kdbaddr->kdb_sa & STEP3MASK) != STEP3GOOD) {
			i = 3;
			goto initfailed;
		}
		i = kdbaddr->kdb_sa & 0xff;
		if (i != ki->ki_micro) {
			ki->ki_micro = i;
			printf("kdb%d: version %d model %d\n",
				ctlr, i & 0xf, i >> 4);
		}

		kdbaddr->kdb_sw = KDB_GO;

		/* initialise hardware data structures */
		for (i = 0, mp = ki->ki_rsp; i < NRSP; i++, mp++) {
			ki->ki_ca.ca_rspdsc[i] = MSCP_OWN | MSCP_INT |
				PHYS(long, &ki->ki_rsp[i].mscp_cmdref);
			mp->mscp_addr = &ki->ki_ca.ca_rspdsc[i];
			mp->mscp_msglen = MSCP_MSGLEN;
		}
		for (i = 0, mp = ki->ki_cmd; i < NCMD; i++, mp++) {
			ki->ki_ca.ca_cmddsc[i] = MSCP_INT |
				PHYS(long, &ki->ki_cmd[i].mscp_cmdref);
			mp->mscp_addr = &ki->ki_ca.ca_cmddsc[i];
			mp->mscp_msglen = MSCP_MSGLEN;
		}

		/*
		 * Before we can get a command packet, we need some
		 * credits.  Fake some up to keep mscp_getcp() happy,
		 * get a packet, and cancel all credits (the right
		 * number should come back in the response to the
		 * SCC packet).
		 */
		ki->ki_mi.mi_credits = MSCP_MINCREDITS + 1;
		mp = mscp_getcp(&ki->ki_mi, MSCP_DONTWAIT);
		if (mp == NULL)	/* `cannot happen' */
			panic("kdbintr");
		ki->ki_mi.mi_credits = 0;
		mp->mscp_opcode = M_OP_SETCTLRC;
		mp->mscp_unit = 0;
		mp->mscp_sccc.sccc_ctlrflags = M_CF_ATTN | M_CF_MISC |
			M_CF_THIS;
		*mp->mscp_addr |= MSCP_OWN | MSCP_INT;
		i = kdbaddr->kdb_ip;
		ki->ki_state = ST_SETCHAR;
		return;

	case ST_SETCHAR:
	case ST_RUN:
		/*
		 * Handle Set Ctlr Characteristics responses and operational
		 * responses (via mscp_dorsp).
		 */
		break;

	default:
		log(LOG_ERR, "kdb%d: driver bug, state %d\n", ctlr,
			ki->ki_state);
		return;
	}

	if (kdbaddr->kdb_sa & KDB_ERR) {/* ctlr fatal error */
		kdbsaerror(ki);
		return;
	}

	/*
	 * Handle buffer purge requests.
	 * KDB DOES NOT HAVE BDPs
	 */
	if (ki->ki_ca.ca_bdp) {
		printf("kdb%d: purge bdp %d\n", ctlr, ki->ki_ca.ca_bdp);
		panic("kdb purge");
	}

	/*
	 * Check for response and command ring transitions.
	 */
	if (ki->ki_ca.ca_rspint) {
		ki->ki_ca.ca_rspint = 0;
		mscp_dorsp(&ki->ki_mi);
	}
	if (ki->ki_ca.ca_cmdint) {
		ki->ki_ca.ca_cmdint = 0;
		MSCP_DOCMD(&ki->ki_mi);
	}
	if (ki->ki_tab.b_actf != NULL)
		kdbstart(ki);
}

/*
 * Handle an error datagram.  All we do now is decode it.
 */
kdbdgram(mi, mp)
	struct mscp_info *mi;
	struct mscp *mp;
{

	mscp_decodeerror(mi->mi_md->md_mname, mi->mi_ctlr, mp);
}

/*
 * The Set Controller Characteristics command finished.
 * Record the new state of the controller.
 */
kdbctlrdone(mi, mp)
	struct mscp_info *mi;
	struct mscp *mp;
{
	register struct kdbinfo *ki = &kdbinfo[mi->mi_ctlr];

	if ((mp->mscp_status & M_ST_MASK) == M_ST_SUCCESS)
		ki->ki_state = ST_RUN;
	else {
		printf("kdb%d: SETCTLRC failed, status 0x%x\n",
			ki->ki_ctlr, mp->mscp_status);
		ki->ki_state = ST_IDLE;
	}
	if (ki->ki_flags & KDB_DOWAKE) {
		ki->ki_flags &= ~KDB_DOWAKE;
		wakeup((caddr_t)&ki->ki_flags);
	}
}

/*
 * Received a response from an as-yet unconfigured drive.  Configure it
 * in, if possible.
 */
kdbunconf(mi, mp)
	struct mscp_info *mi;
	register struct mscp *mp;
{

	/*
	 * If it is a slave response, copy it to kdbslavereply for
	 * kdbslave() to look at.
	 */
	if (mp->mscp_opcode == (M_OP_GETUNITST | M_OP_END) &&
	    (kdbinfo[mi->mi_ctlr].ki_flags & KDB_INSLAVE) != 0) {
		kdbslavereply = *mp;
		return (MSCP_DONE);
	}

	/*
	 * Otherwise, it had better be an available attention response.
	 */
	if (mp->mscp_opcode != M_OP_AVAILATTN)
		return (MSCP_FAILED);

	/* do what autoconf does */
	return (MSCP_FAILED);	/* not yet */
}

/*
 * A drive came on line.  Check its type and size.  Return DONE if
 * we think the drive is truly on line.  In any case, awaken anyone
 * sleeping on the drive on-line-ness.
 */
kdbonline(ui, mp)
	register struct uba_device *ui;
	struct mscp *mp;
{
	register int type;

	wakeup((caddr_t)&ui->ui_flags);
	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS) {
		printf("kdb%d: attempt to bring %s%d on line failed:",
			ui->ui_ctlr, kdbdriver.ud_dname, ui->ui_unit);
		mscp_printevent(mp);
		return (MSCP_FAILED);
	}

	type = mp->mscp_onle.onle_drivetype;
	if (type >= NTYPES || kdbtypes[type].ut_name == 0) {
		printf("kdb%d: %s%d: unknown type %d\n",
			ui->ui_ctlr, kdbdriver.ud_dname, ui->ui_unit, type);
		return (MSCP_FAILED);
	}
	/*
	 * Note any change of types.  Not sure if we should do
	 * something special about them, or if so, what....
	 */
	if (type != ui->ui_type) {
		printf("%s%d: changed types! was %s\n",
			kdbdriver.ud_dname, ui->ui_unit,
			kdbtypes[ui->ui_type].ut_name);
		ui->ui_type = type;
	}
	ra_dsize[ui->ui_unit] = (daddr_t) mp->mscp_onle.onle_unitsize;
	printf("%s%d: %s, size = %d sectors\n",
		kdbdriver.ud_dname, ui->ui_unit,
		kdbtypes[type].ut_name, ra_dsize[ui->ui_unit]);
	return (MSCP_DONE);
}

/*
 * We got some (configured) unit's status.  Return DONE if it succeeded.
 */
kdbgotstatus(ui, mp)
	register struct uba_device *ui;
	register struct mscp *mp;
{

	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS) {
		printf("kdb%d: attempt to get status for %s%d failed:",
			ui->ui_ctlr, kdbdriver.ud_dname, ui->ui_unit);
		mscp_printevent(mp);
		return (MSCP_FAILED);
	}
	/* need to record later for bad block forwarding - for now, print */
	printf("\
%s%d: unit %d, nspt %d, group %d, ngpc %d, rctsize %d, nrpt %d, nrct %d\n",
		kdbdriver.ud_dname, ui->ui_unit, mp->mscp_unit,
		mp->mscp_guse.guse_nspt, mp->mscp_guse.guse_group,
		mp->mscp_guse.guse_ngpc, mp->mscp_guse.guse_rctsize,
		mp->mscp_guse.guse_nrpt, mp->mscp_guse.guse_nrct);
	return (MSCP_DONE);
}

/*
 * A transfer failed.  We get a chance to fix or restart it.
 * Need to write the bad block forwaring code first....
 */
/*ARGSUSED*/
kdbioerror(ui, mp, bp)
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
		log(LOG_ERR, "%s%d: bad block report: %d%s\n",
			kdbdriver.ud_dname, ui->ui_unit, mp->mscp_seq.seq_lbn,
			mp->mscp_flags & M_EF_BBLKU ? " + others" : "");
	} else {
		/*
		 * What the heck IS a `serious exception' anyway?
		 */
		if (mp->mscp_flags & M_EF_SEREX)
			log(LOG_ERR, "%s%d: serious exception reported\n",
				kdbdriver.ud_dname, ui->ui_unit);
	}
	return (MSCP_FAILED);
}


#ifdef notyet
/*
 * I/O controls.  Not yet!
 */
kdbioctl(dev, cmd, flag, data)
	dev_t dev;
	int cmd, flag;
	caddr_t data;
{
	int error = 0;
	register int unit = kdbunit(dev);

	if (unit >= NKRA || uddinfo[unit] == NULL)
		return (ENXIO);

	switch (cmd) {

	case KDBIOCREPLACE:
		/*
		 * Initiate bad block replacement for the given LBN.
		 * (Should we allow modifiers?)
		 */
		error = EOPNOTSUPP;
		break;

	case KDBIOCGMICRO:
		/*
		 * Return the microcode revision for the KDB50 running
		 * this drive.
		 */
		*(int *)data = kdbinfo[kdbdinfo[unit]->ui_ctlr].ki_micro;
		break;

	case KDBIOCGSIZE:
		/*
		 * Return the size (in 512 byte blocks) of this
		 * disk drive.
		 */
		*(daddr_t *)data = ra_dsize[unit];
		break;

	default:
		error = EINVAL;
		break;
	}
	return (error);
}
#endif

#ifdef notyet
/*
 * Reset a KDB50 (self test and all).
 * What if it fails?
 */
kdbreset(ki)
	register struct kdbinfo *ki;
{

	printf("reset kdb%d", ki->ki_ctlr);
	bi_selftest(&ki->ki_kdb.kdb_bi);
	ki->ki_state = ST_IDLE;
	rminit(ki->ki_map, (long)KI_PTES, (long)1, "kdb", KI_MAPSIZ);
	mscp_requeue(&ki->ki_mi);
	if (kdbinit(ctlr))
		printf(" (hung)");
	printf("\n");
}
#endif

/*
 * Watchdog timer:  If the controller is active, and no interrupts
 * have occurred for 30 seconds, assume it has gone away.
 */
kdbwatch()
{
	register struct kdbinfo *ki;
	register int i;

	timeout(kdbwatch, (caddr_t)0, hz);	/* every second */
	for (i = 0, ki = kdbinfo; i < NKDB; i++, ki++) {
		if ((ki->ki_flags & KDB_ALIVE) == 0)
			continue;
		if (ki->ki_state == ST_IDLE)
			continue;
		if (ki->ki_state == ST_RUN && !ki->ki_tab.b_active)
			ki->ki_wticks = 0;
		else if (++ki->ki_wticks >= 30) {
			ki->ki_wticks = 0;
			printf("kdb%d: lost interrupt\n", i);
			/* kdbreset(ki); */
			panic("kdb lost interrupt");
		}
	}
}

/*
 * Do a panic dump.
 */
#define	DBSIZE	32		/* dump 16K at a time */

struct kdbdumpspace {
	struct	kdb1ca kd_ca;
	struct	mscp kd_rsp;
	struct	mscp kd_cmd;
} kdbdumpspace;

kdbdump(dev)
	dev_t dev;
{
	register struct kdbdumpspace *kd;
	register struct kdb_regs *k;
	register int i;
	struct uba_device *ui;
	char *start;
	int num, blk, unit, maxsz, blkoff;

	/*
	 * Make sure the device is a reasonable place on which to dump.
	 */
	unit = kdbunit(dev);
	if (unit >= NKRA)
		return (ENXIO);
	ui = PHYS(struct uba_device *, kdbdinfo[unit]);
	if (ui == NULL || ui->ui_alive == 0)
		return (ENXIO);

	/*
	 * Find and initialise the KDB; get the physical address of the
	 * device registers, and of communications area and command and
	 * response packet.
	 */
	k = PHYS(struct kdbinfo *, &kdbinfo[ui->ui_ctlr])->ki_physkdb;
	kd = PHYS(struct kdbdumpspace *, &kdbdumpspace);

	/*
	 * Initialise the controller, with one command and one response
	 * packet.
	 */
	bi_reset(&k->kdb_bi);
	if (kdbdumpwait(k, KDB_STEP1))
		return (EFAULT);
	k->kdb_sw = KDB_ERR;
	if (kdbdumpwait(k, KDB_STEP2))
		return (EFAULT);
	k->kdb_sw = (int)&kd->kd_ca.ca_rspdsc;
	if (kdbdumpwait(k, KDB_STEP3))
		return (EFAULT);
	k->kdb_sw = ((int)&kd->kd_ca.ca_rspdsc) >> 16;
	if (kdbdumpwait(k, KDB_STEP4))
		return (EFAULT);
	k->kdb_sw = KDB_GO;

	/*
	 * Set up the command and response descriptor, then set the
	 * controller characteristics and bring the drive on line.
	 * Note that all uninitialised locations in kd_cmd are zero.
	 */
	kd->kd_ca.ca_rspdsc = (long)&kd->kd_rsp.mscp_cmdref;
	kd->kd_ca.ca_cmddsc = (long)&kd->kd_cmd.mscp_cmdref;
	/* kd->kd_cmd.mscp_sccc.sccc_ctlrflags = 0; */
	/* kd->kd_cmd.mscp_sccc.sccc_version = 0; */
	if (kdbdumpcmd(M_OP_SETCTLRC, k, kd, ui->ui_ctlr))
		return (EFAULT);
	kd->kd_cmd.mscp_unit = ui->ui_slave;
	if (kdbdumpcmd(M_OP_ONLINE, k, kd, ui->ui_ctlr))
		return (EFAULT);

	/*
	 * Pick up the drive type from the on line end packet;
	 * convert that to a dump area size and a disk offset.
	 * Note that the assembler uses pc-relative addressing
	 * to get at kdbtypes[], no need for PHYS().
	 */
	i = kd->kd_rsp.mscp_onle.onle_drivetype;
	if (i >= NTYPES || kdbtypes[i].ut_name == 0) {
		printf("disk type %d unknown\ndump ");
		return (EINVAL);
	}
	printf("on %s ", kdbtypes[i].ut_name);

	maxsz = kdbtypes[i].ut_sizes[kdbpart(dev)].nblocks;
	blkoff = kdbtypes[i].ut_sizes[kdbpart(dev)].blkoff;

	/*
	 * Dump all of physical memory, or as much as will fit in the
	 * space provided.
	 */
	start = 0;
	num = maxfree;
	if (dumplo < 0)
		return (EINVAL);
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
		kd->kd_cmd.mscp_unit = ui->ui_slave;
		kd->kd_cmd.mscp_seq.seq_lbn = btop(start) + blkoff;
		kd->kd_cmd.mscp_seq.seq_bytecount = blk << PGSHIFT;
		kd->kd_cmd.mscp_seq.seq_buffer = (long)start | KDB_PHYS;
		if (kdbdumpcmd(M_OP_WRITE, k, kd, ui->ui_ctlr))
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
kdbdumpwait(k, bits)
	register struct kdb_regs *k;
	register int bits;
{
	register int timo = todr() + 1000;

	while ((k->kdb_sa & bits) == 0) {
		if (k->kdb_sa & KDB_ERR) {
			printf("kdb_sa=%b\ndump ", k->kdb_sa, kdbsr_bits);
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
 * Feed a command to the KDB50, wait for its response, and return
 * true iff something went wrong.
 */
kdbdumpcmd(op, k, kd, ctlr)
	int op;
	register struct kdb_regs *k;
	register struct kdbdumpspace *kd;
	int ctlr;
{
	register int n;
#define mp (&kd->kd_rsp)

	kd->kd_cmd.mscp_opcode = op;
	kd->kd_cmd.mscp_msglen = MSCP_MSGLEN;
	kd->kd_rsp.mscp_msglen = MSCP_MSGLEN;
	kd->kd_ca.ca_rspdsc |= MSCP_OWN | MSCP_INT;
	kd->kd_ca.ca_cmddsc |= MSCP_OWN | MSCP_INT;
	if (k->kdb_sa & KDB_ERR) {
		printf("kdb_sa=%b\ndump ", k->kdb_sa, kdbsr_bits);
		return (1);
	}
	n = k->kdb_ip;
	n = todr() + 1000;
	for (;;) {
		if (todr() > n) {
			printf("timeout\ndump ");
			return (1);
		}
		if (kd->kd_ca.ca_cmdint)
			kd->kd_ca.ca_cmdint = 0;
		if (kd->kd_ca.ca_rspint == 0)
			continue;
		kd->kd_ca.ca_rspint = 0;
		if (mp->mscp_opcode == (op | M_OP_END))
			break;
		printf("\n");
		switch (MSCP_MSGTYPE(mp->mscp_msgtc)) {

		case MSCPT_SEQ:
			printf("sequential");
			break;

		case MSCPT_DATAGRAM:
			mscp_decodeerror("kdb", ctlr, mp);
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
		kd->kd_ca.ca_rspdsc |= MSCP_OWN | MSCP_INT;
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
kdbsize(dev)
	dev_t dev;
{
	register int unit = kdbunit(dev);
	register struct uba_device *ui;
	register struct size *st;

	if (unit >= NKRA || (ui = kdbdinfo[unit]) == NULL || ui->ui_alive == 0)
		return (-1);
	st = &kdbtypes[ui->ui_type].ut_sizes[kdbpart(dev)];
	if (st->nblocks == -1) {
		int s = spl5();

		/*
		 * We need to have the drive on line to find the size
		 * of this particular partition.
		 * IS IT OKAY TO GO TO SLEEP IN THIS ROUTINE?
		 * (If not, better not page on one of these...)
		 */
		if ((ui->ui_flags & UNIT_ONLINE) == 0) {
			if (kdb_bringonline(ui, 0)) {
				splx(s);
				return (-1);
			}
		}
		splx(s);
		if (st->blkoff > ra_dsize[unit])
			return (-1);
		return (ra_dsize[unit] - st->blkoff);
	}
	return (st->nblocks);
}

#endif NKDB > 0
