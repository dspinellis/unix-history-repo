/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: autoconf.c 1.25 89/10/07$
 *
 *	@(#)autoconf.c	7.2 (Berkeley) 5/25/90
 */

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "buf.h"
#include "dkstat.h"
#include "vm.h"
#include "conf.h"
#include "dmap.h"
#include "reboot.h"

#include "pte.h"
#include "cpu.h"
#include "isr.h"
#include "../hpdev/device.h"
#include "../hpdev/grfioctl.h"
#include "../hpdev/grfvar.h"

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold;		    /* if 1, still working on cold-start */
int	dkn;		    /* number of iostat dk numbers assigned so far */
int	cpuspeed = MHZ_8;   /* relative cpu speed */
struct	isr isrqueue[NISR];
struct	hp_hw sc_table[MAX_CTLR];

extern	int internalhpib;

#ifdef DEBUG
int	acdebug = 0;
#endif

/*
 * Determine mass storage and memory configuration for a machine.
 */
configure()
{
	register struct hp_hw *hw;
	int found;

	/*
	 * XXX: these should be consolidated into some kind of table
	 */
	hilinit();
	isrinit();
	dmainit();

	/*
	 * Look over each hardware device actually found and attempt
	 * to match it with an ioconf.c table entry.
	 */
	for (hw = sc_table; hw->hw_type; hw++) {
		if (hw->hw_type & CONTROLLER)
			found = find_controller(hw);
		else
			found = find_device(hw);
#ifdef DEBUG
		if (!found) {
			int sc = addrtosc((u_int)hw->hw_addr);

			printf("unconfigured %s ", hw->hw_name);
			if (sc < 256)
				printf("at sc%d\n", sc);
			else
				printf("csr at %x\n", sc);
		}
#endif
	}

#include "cd.h"
#if NCD > 0
	/*
	 * Now deal with concatenated disks
	 */
	find_cdevices();
#endif

#if GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	swapconf();
	cold = 0;
}

#define dr_type(d, s)	\
	(strcmp((d)->d_name, (s)) == 0)

#define same_hw_ctlr(hw, hc) \
	((hw)->hw_type == HPIB && dr_type((hc)->hp_driver, "hpib") || \
	 (hw)->hw_type == SCSI && dr_type((hc)->hp_driver, "scsi"))

find_controller(hw)
	register struct hp_hw *hw;
{
	register struct hp_ctlr *hc;
	struct hp_ctlr *match_c;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_controller: hw: %s at sc%d (%x), type %x...",
		       hw->hw_name, hw->hw_sc, hw->hw_addr, hw->hw_type);
#endif
	sc = hw->hw_sc;
	match_c = NULL;
	for (hc = hp_cinit; hc->hp_driver; hc++) {
		if (hc->hp_alive)
			continue;
		/*
		 * Make sure we are looking at the right
		 * controller type.
		 */
		if (!same_hw_ctlr(hw, hc))
			continue;
		/*
		 * Exact match; all done
		 */
		if ((int)hc->hp_addr == sc) {
			match_c = hc;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if ((int)hc->hp_addr == WILD_CARD_CTLR && match_c == NULL)
			match_c = hc;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_c)
			printf("found %s%d\n",
			       match_c->hp_driver->d_name,
			       match_c->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece of hardware,
	 * just ignore it.
	 */
	if (match_c == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize and configure all attached
	 * slaves.  Note, we can still fail if HW won't initialize.
	 */
	hc = match_c;
	oaddr = hc->hp_addr;
	hc->hp_addr = hw->hw_addr;
	if ((*hc->hp_driver->d_init)(hc)) {
		hc->hp_alive = 1;
		printf("%s%d", hc->hp_driver->d_name, hc->hp_unit);
		sc = addrtosc((u_int)hc->hp_addr);
		if (sc < 256)
			printf(" at sc%d,", sc);
		else
			printf(" csr 0x%x,", sc);
		printf(" ipl %d", hc->hp_ipl);
		if (hc->hp_flags)
			printf(" flags 0x%x", hc->hp_flags);
		printf("\n");
		find_slaves(hc);
	} else
		hc->hp_addr = oaddr;
	return(1);
}

find_device(hw)
	register struct hp_hw *hw;
{
	register struct hp_device *hd;
	struct hp_device *match_d;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_device: hw: %s at sc%d (%x), type %x...",
		       hw->hw_name, hw->hw_sc, hw->hw_addr, hw->hw_type);
#endif
	match_d = NULL;
	for (hd = hp_dinit; hd->hp_driver; hd++) {
		if (hd->hp_alive)
			continue;
		/* Must not be a slave */
		if (hd->hp_cdriver)
			continue;
		/*
		 * XXX: A graphics device that was found as part of the
		 * console init will have the hp_addr field already set
		 * (i.e. no longer the select code).  Gotta perform a
		 * slightly different check for an exact match.
		 */
		if (hw->hw_type == BITMAP && hd->hp_addr >= (caddr_t)IOBASE) {
			/* must be an exact match */
			if (hd->hp_addr == hw->hw_addr) {
				match_d = hd;
				break;
			}
			continue;
		}
		sc = (int) hd->hp_addr;
		/*
		 * Exact match; all done.
		 */
		if (sc > 0 && sc == hw->hw_sc) {
			match_d = hd;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if (sc == 0 && same_hw_device(hw, hd) && match_d == NULL)
			match_d = hd;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_d)
			printf("found %s%d\n",
			       match_d->hp_driver->d_name,
			       match_d->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece
	 * of hardware, just ignore it.
	 */
	if (match_d == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize.
	 * Note, we can still fail if HW won't initialize.
	 */
	hd = match_d;
	oaddr = hd->hp_addr;
	hd->hp_addr = hw->hw_addr;
	if ((*hd->hp_driver->d_init)(hd)) {
		hd->hp_alive = 1;
		printf("%s%d", hd->hp_driver->d_name, hd->hp_unit);
		sc = addrtosc((u_int)hd->hp_addr);
		if (sc < 32)
			printf(" at sc%d", sc);
		else
			printf(" csr 0x%x", sc);
		if (hd->hp_ipl)
			printf(", ipl %d", hd->hp_ipl);
		if (hd->hp_flags)
			printf(", flags 0x%x", hd->hp_flags);
		printf("\n");
	} else
		hd->hp_addr = oaddr;
	return(1);
}

find_slaves(hc)
	struct hp_ctlr *hc;
{
	/*
	 * The SCSI bus is structured very much like the HP-IB 
	 * except that the host adaptor is slave 7 so we only want
	 * to look at the first 6 slaves.
	 */
	if (dr_type(hc->hp_driver, "hpib"))
		find_busslaves(hc, MAXSLAVES);
	else if (dr_type(hc->hp_driver, "scsi"))
		find_busslaves(hc, MAXSLAVES-1);
}

/*
 * Search each BUS controller found for slaves attached to it.
 * The bad news is that we don't know how to uniquely identify all slaves
 * (e.g. PPI devices on HP-IB).  The good news is that we can at least
 * differentiate those from slaves we can identify.  At worst (a totally
 * wildcarded entry) this will cause us to locate such a slave at the first
 * unused position instead of where it really is.  To save grief, non-
 * identifing devices should always be fully qualified.
 */
find_busslaves(hc, maxslaves)
	register struct hp_ctlr *hc;
	int maxslaves;
{
	register int s;
	register struct hp_device *hd;
	struct hp_device *match_s;
	int new_s, new_c, old_s, old_c;
	int rescan;
	
#ifdef DEBUG
	if (acdebug)
		printf("find_busslaves: for %s%d\n",
		       hc->hp_driver->d_name, hc->hp_unit);
#endif
	for (s = 0; s < maxslaves; s++) {
		rescan = 1;
		match_s = NULL;
		for (hd = hp_dinit; hd->hp_driver; hd++) {
			/*
			 * Rule out the easy ones:
			 * 1. slave already assigned or not a slave
			 * 2. not of the proper type
			 * 3. controller specified but not this one
			 * 4. slave specified but not this one
			 */
			if (hd->hp_alive || hd->hp_cdriver == NULL)
				continue;
			if (!dr_type(hc->hp_driver, hd->hp_cdriver->d_name))
				continue;
			if (hd->hp_ctlr >= 0 && hd->hp_ctlr != hc->hp_unit)
				continue;
			if (hd->hp_slave >= 0 && hd->hp_slave != s)
				continue;
			/*
			 * Case 0: first possible match.
			 * Remember it and keep looking for better.
			 */
			if (match_s == NULL) {
				match_s = hd;
				new_c = hc->hp_unit;
				new_s = s;
				continue;
			}
			/*
			 * Case 1: exact match.
			 * All done.  Note that we do not attempt any other
			 * matches if this one fails.  This allows us to
			 * "reserve" locations for dynamic addition of
			 * disk/tape drives by fully qualifing the location.
			 */
			if (hd->hp_slave == s && hd->hp_ctlr == hc->hp_unit) {
				match_s = hd;
				rescan = 0;
				break;
			}
			/*
			 * Case 2: right controller, wildcarded slave.
			 * Remember first and keep looking for an exact match.
			 */
			if (hd->hp_ctlr == hc->hp_unit &&
			    match_s->hp_ctlr < 0) {
				match_s = hd;
				new_s = s;
				continue;
			}
			/*
			 * Case 3: right slave, wildcarded controller.
			 * Remember and keep looking for a better match.
			 */
			if (hd->hp_slave == s &&
			    match_s->hp_ctlr < 0 && match_s->hp_slave < 0) {
				match_s = hd;
				new_c = hc->hp_unit;
				continue;
			}
			/*
			 * OW: we had a totally wildcarded spec.
			 * If we got this far, we have found a possible
			 * match already (match_s != NULL) so there is no
			 * reason to remember this one.
			 */
			continue;
		}
		/*
		 * Found a match.  We need to set hp_ctlr/hp_slave properly
		 * for the init routines but we also need to remember all
		 * the old values in case this doesn't pan out.
		 */
		if (match_s) {
			hd = match_s;
			old_c = hd->hp_ctlr;
			old_s = hd->hp_slave;
			if (hd->hp_ctlr < 0)
				hd->hp_ctlr = new_c;
			if (hd->hp_slave < 0)
				hd->hp_slave = new_s;
#ifdef DEBUG
			if (acdebug)
				printf("looking for %s%d at slave %d...",
				       hd->hp_driver->d_name,
				       hd->hp_unit, hd->hp_slave);
#endif

			if ((*hd->hp_driver->d_init)(hd)) {
#ifdef DEBUG
				if (acdebug)
					printf("found\n");
#endif
				printf("%s%d at %s%d, slave %d",
				       hd->hp_driver->d_name, hd->hp_unit,
				       hc->hp_driver->d_name, hd->hp_ctlr,
				       hd->hp_slave);
				if (hd->hp_flags)
					printf(" flags 0x%x", hd->hp_flags);
				printf("\n");
				hd->hp_alive = 1;
				if (hd->hp_dk && dkn < DK_NDRIVE)
					hd->hp_dk = dkn++;
				else
					hd->hp_dk = -1;
				rescan = 1;
			} else {
#ifdef DEBUG
				if (acdebug)
					printf("not found\n");
#endif
				hd->hp_ctlr = old_c;
				hd->hp_slave = old_s;
			}
			/*
			 * XXX: This should be handled better.
			 * Re-scan a slave.  There are two reasons to do this.
			 * 1. It is possible to have both a tape and disk
			 *    (e.g. 7946) or two disks (e.g. 9122) at the
			 *    same slave address.  Here we need to rescan
			 *    looking only at entries with a different
			 *    physical unit number (hp_flags).
			 * 2. It is possible that an init failed because the
			 *    slave was there but of the wrong type.  In this
			 *    case it may still be possible to match the slave
			 *    to another ioconf entry of a different type.
			 *    Here we need to rescan looking only at entries
			 *    of different types.
			 * In both cases we avoid looking at undesirable
			 * ioconf entries of the same type by setting their
			 * alive fields to -1.
			 */
			if (rescan) {
				for (hd = hp_dinit; hd->hp_driver; hd++) {
					if (hd->hp_alive)
						continue;
					if (match_s->hp_alive == 1) {	/* 1 */
						if (hd->hp_flags == match_s->hp_flags)
							hd->hp_alive = -1;
					} else {			/* 2 */
						if (hd->hp_driver == match_s->hp_driver)
							hd->hp_alive = -1;
					}
				}
				s--;
				continue;
			}
		}
		/*
		 * Reset bogon alive fields prior to attempting next slave
		 */
		for (hd = hp_dinit; hd->hp_driver; hd++)
			if (hd->hp_alive == -1)
				hd->hp_alive = 0;
	}
}

sctoaddr(addr)
	register int addr;
{
	if (addr == 7 && internalhpib)
		addr = internalhpib;
	else if (addr < 32)
		addr = IOV(EXTIOBASE + (addr * IOCARDSIZE));
	else
		addr = IOV(addr);
	return(addr);
}

addrtosc(addr)
	register u_int addr;
{
#if defined(HP360) || defined(HP370)
	extern char grfregs[];

	if (addr == (u_int)grfregs)
		addr = 132;
	else
#endif
	if (addr == internalhpib)
		addr = 7;
	else if (addr >= IOV(IOBASE)) {
		addr = UNIOV(addr);
		if (addr >= EXTIOBASE)
			addr = (addr - EXTIOBASE) / IOCARDSIZE;
	}
	return((int)addr);
}

same_hw_device(hw, hd)
	struct hp_hw *hw;
	struct hp_device *hd;
{
	int found = 0;

	switch (hw->hw_type) {
	case HPIB:
	case RD:
	case PPI:
	case CT:
		found = dr_type(hd->hp_driver, "hpib");
		break;
	case BITMAP:
		found = dr_type(hd->hp_driver, "grf");
		break;
	case NET:
		found = dr_type(hd->hp_driver, "le");
		break;
	case COMMDCA:
		found = dr_type(hd->hp_driver, "dca");
		break;
	case COMMDCL:
		found = dr_type(hd->hp_driver, "dcl");
		break;
	case COMMDCM:
		found = dr_type(hd->hp_driver, "dcm");
		break;
	case SCSI:
		found = dr_type(hd->hp_driver, "scsi");
		break;
	case FPA:    /* Unsupported so far */
	case VME:
	case FLINK:
	case MISC:
		break;
	}
	return(found);
}

find_devs()
{
	short sc;
	u_char *id_reg;
	register int addr;
	register struct hp_hw *hw;

	hw = sc_table;
	for (sc = -1; sc < 32; sc++) {
#if defined(HP360) || defined(HP370)
		/*
		 * XXX: special check for bit-mapped display
		 * at SC132 in DIO II space on the 340.
		 */
		if (sc == -1 && machineid == HP_340) {
			extern struct pte Grfmap[];
			extern char grfregs[];

			physaccess(Grfmap, (caddr_t)DIOIIBASE,
				   DIOIICSIZE, PG_RW|PG_CI);
			addr = (int) grfregs;
			/*
			 * Nothing there or not a display,
			 * try the usual internal display address.
			 */
			if (badaddr((caddr_t)addr) ||
			    (((u_char *)addr)[1] & 0xff) != 57)
				addr = IOV(GRFIADDR);
		} else
#endif
		/*
		 * Probe all select codes + internal display addr
		 */
		if (sc == -1)
			addr = IOV(GRFIADDR);
		else
			addr = sctoaddr(sc);
		if (badaddr((caddr_t)addr))
			continue;

		id_reg = (u_char *) addr;
		hw->hw_id = id_reg[1] & 0xff;
		hw->hw_sc = sc;
		hw->hw_addr = (char *) addr;
		/*
		 * Internal HP-IB on some machines (345/375) doesn't return
		 * consistant id info so we use the info gleaned from the
		 * boot ROMs SYSFLAG.
		 */
		if (sc == 7 && internalhpib) {
			hw->hw_name = "98624A";
			hw->hw_type = HPIB;
			hw++;
			continue;
		}
		/*
		 * XXX: the following could be in a big static table
		 */
		switch (hw->hw_id) {
		/* Null device? */
		case 0:
			break;
		case 2:
		case 128+2:
			hw->hw_name = "98626A";
			hw->hw_type = COMMDCA;
			break;
		case 3:
			hw->hw_name = "98622A";
			hw->hw_type = MISC;
			break;
		case 4:
			hw->hw_name = "98623A";
			hw->hw_type = MISC;
			break;
		case 5:
		case 128+5:
			hw->hw_name = "98642A";
			hw->hw_type = COMMDCM;
			break;
		case 6:
			hw->hw_name = "Parallel Port";
			hw->hw_type = PPORT;
			break;
		case 7:
		case 39:
		case 71:
		case 103:
			hw->hw_name = "98265A";
			hw->hw_type = SCSI;
			break;
		case 8:
			hw->hw_name = "98625B";
			hw->hw_type = HPIB;
			break;
		case 9:
			hw->hw_name = "98287A";
			hw->hw_type = KEYBOARD;
			break;
		case 10:
			hw->hw_name = "98635A";
			hw->hw_type = FPA;
			break;
		case 11:
			hw->hw_name = "Timer";
			hw->hw_type = MISC;
			break;
		case 18:
			hw->hw_name = "98640A";
			hw->hw_type = MISC;
			break;
		case 21:
			hw->hw_name = "98643A";
			hw->hw_type = NET;
			break;
		case 22:
			hw->hw_name = "98659A";
			hw->hw_type = MISC;
			break;
		case 25:
			hw->hw_name = "237";
			hw->hw_type = BITMAP;
			break;
		case 26:
			hw->hw_name = "Quad";
			hw->hw_type = MISC;
			break;
		case 27:
			hw->hw_name = "98253A";
			hw->hw_type = MISC;
			break;
		case 28:
			hw->hw_name = "98627A";
			hw->hw_type = BITMAP;
			break;
		case 29:
			hw->hw_name = "98633A";
			hw->hw_type = BITMAP;
			break;
		case 30:
			hw->hw_name = "98259A";
			hw->hw_type = MISC;
			break;
		case 31:
			hw->hw_name = "8741";
			hw->hw_type = MISC;
			break;
		case 49:
			hw->hw_name = "98577A";
			hw->hw_type = VME;
			sc++;
			break;
		case 52:
		case 180:
			hw->hw_name = "98628A";
			hw->hw_type = COMMDCL;
			break;
		case 57:
			hw->hw_type = BITMAP;
			hw->hw_id2 = id_reg[0x15];
			switch (hw->hw_id2) {
			case 1:
				hw->hw_name = "98700 ";
				break;
			case 2:
				hw->hw_name = "TOPCAT";
				break;
			case 4:
				hw->hw_name = "98720 ";
				sc++;
				break;
			case 5:
			case 6:
			case 7:
			case 9:
				hw->hw_name = "CATSEYE";
				break;
			case 8:
				hw->hw_name = "98730 ";
				sc++;
				break;
			default:
				hw->hw_name = "987xx ";
				break;
			}
			break;
		case 66:
		case 128+66:
			hw->hw_name = "98644A";
			hw->hw_type = COMMDCA;
			break;
		case 128:
			hw->hw_name = "98624A";
			hw->hw_type = HPIB;
			break;
		default:
			hw->hw_name = "DEFAULT";
			hw->hw_type = MISC;
			break;
		}
		hw++;
	}
}

#if NCD > 0
#include "../hpdev/cdvar.h"

find_cdevices()
{
	register struct cddevice *cd;

	for (cd = cddevice; cd->cd_unit >= 0; cd++) {
		/*
		 * XXX
		 * Assign disk index first so that init routine
		 * can use it (saves having the driver drag around
		 * the cddevice pointer just to set up the dk_*
		 * info in the open routine).
		 */
		if (dkn < DK_NDRIVE)
			cd->cd_dk = dkn++;
		else
			cd->cd_dk = -1;
		if (cdinit(cd))
			printf("cd%d configured\n", cd->cd_unit);
		else if (cd->cd_dk >= 0) {
			cd->cd_dk = -1;
			dkn--;
		}
	}
}
#endif

isrinit()
{
	register int i;

	for (i = 0; i < NISR; i++)
		isrqueue[i].isr_forw = isrqueue[i].isr_back = &isrqueue[i];
}

void
isrlink(isr)
	register struct isr *isr;
{
	int i = ISRIPL(isr->isr_ipl);

	if (i < 0 || i >= NISR) {
		printf("bad IPL %d\n", i);
		panic("configure");
	}
	insque(isr, isrqueue[i].isr_back);
}

/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}

#define	DOSWAP			/* Change swdevt, argdev, and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	0,0,		/* 0 = ct */
	0,0,		/* 1 = xx */
	'r','d',	/* 2 = rd */
	0,0,		/* 3 = sw */
	's','d',	/* 4 = rd */
};

#define	PARTITIONMASK	0x7
#define	PARTITIONSHIFT	3

/*
 * Attempt to find the device from which we were booted.
 * If we can do so, and not instructed not to do so,
 * change rootdev to correspond to the load device.
 */
setroot()
{
	register struct hp_ctlr *hc;
	register struct hp_device *hd;
	int  majdev, mindev, unit, part, adaptor;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != (u_long)B_DEVMAGIC)
		return;
	majdev = (bootdev >> B_TYPESHIFT) & B_TYPEMASK;
	if (majdev > sizeof(devname) / sizeof(devname[0]))
		return;
	adaptor = (bootdev >> B_ADAPTORSHIFT) & B_ADAPTORMASK;
	part = (bootdev >> B_PARTITIONSHIFT) & B_PARTITIONMASK;
	unit = (bootdev >> B_UNITSHIFT) & B_UNITMASK;
	/*
	 * First, find the controller type which support this device.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_driver->d_name[0] == devname[majdev][0] &&
		    hd->hp_driver->d_name[1] == devname[majdev][1])
			break;
	if (hd->hp_driver == 0)
		return;
	/*
	 * Next, find the controller of that type corresponding to
	 * the adaptor number.
	 */
	for (hc = hp_cinit; hc->hp_driver; hc++)
		if (hc->hp_alive && hc->hp_unit == adaptor &&
		    hc->hp_driver == hd->hp_cdriver)
			break;
	if (hc->hp_driver == 0)
		return;
	/*
	 * Finally, find the device in question attached to that controller.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_alive && hd->hp_slave == unit &&
		    hd->hp_cdriver == hc->hp_driver &&
		    hd->hp_ctlr == hc->hp_unit)
			break;
	if (hd->hp_driver == 0)
		return;
	mindev = hd->hp_unit;
	/*
	 * Form a new rootdev
	 */
	mindev = (mindev << PARTITIONSHIFT) + part;
	orootdev = rootdev;
	rootdev = makedev(majdev, mindev);
	/*
	 * If the original rootdev is the same as the one
	 * just calculated, don't need to adjust the swap configuration.
	 */
	if (rootdev == orootdev)
		return;

	printf("Changing root device to %c%c%d%c\n",
		devname[majdev][0], devname[majdev][1],
		mindev >> PARTITIONSHIFT, part + 'a');

#ifdef DOSWAP
	mindev &= ~PARTITIONMASK;
	for (swp = swdevt; swp->sw_dev; swp++) {
		if (majdev == major(swp->sw_dev) &&
		    mindev == (minor(swp->sw_dev) & ~PARTITIONMASK)) {
			temp = swdevt[0].sw_dev;
			swdevt[0].sw_dev = swp->sw_dev;
			swp->sw_dev = temp;
			break;
		}
	}
	if (swp->sw_dev == 0)
		return;

	/*
	 * If argdev and dumpdev were the same as the old primary swap
	 * device, move them to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
	if (temp == argdev)
		argdev = swdevt[0].sw_dev;
#endif
}

strcmp(s1, s2)
	register char *s1, *s2;
{
	while (*s1 == *s2++)
		if (*s1++=='\0')
			return (0);
	return (*s1 - *--s2);
}
