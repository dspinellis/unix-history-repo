/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department, Ralph Campbell, Sony Corp. and Kazumasa
 * Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: autoconf.c 1.31 91/01/21$
 *
 *	@(#)autoconf.c	7.4 (Berkeley) %G%
 */

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */

#include "hb.h"

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/dkstat.h>
#include <sys/conf.h>
#include <sys/dmap.h>
#include <sys/reboot.h>

#include <news3400/news3400/machid.h>
#include <machine/adrsmap.h>
#include <machine/cpu.h>

#if NHB > 0
#include <news3400/hbdev/hbvar.h>
#endif

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold = 1;	/* if 1, still working on cold-start */
int	dkn;		/* number of iostat dk numbers assigned so far */
int	cpuspeed = 6;	/* approx # instr per usec. */

struct idrom	idrom;

/*
 * Determine mass storage and memory configuration for a machine.
 * Get cpu type, and then switch out to machine specific procedures
 * which will probe adaptors to see what is out there.
 */
configure()
{
	register struct scsi_device *dp;
	register struct driver *drp;

	readidrom((u_char *)&idrom);
	printf("SONY NET WORK STATION, Model %s, ", idrom.id_model);
	printf("Machine ID #%d\n", idrom.id_serial);

	/* print what type of CPU and FPU we have */
	switch (cpu.cpu.cp_imp) {
	    case MIPS_R2000:
		printf("cpu0 (MIPS R2000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	    case MIPS_R3000:
		printf("cpu0 (MIPS R3000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	    case MIPS_R4000:
		printf("cpu0 (MIPS R4000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	    default:
		printf("cpu0 (implementation %d revision %d.%d)\n",
			cpu.cpu.cp_imp, cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
	}

	switch (fpu.cpu.cp_imp) {
	    case MIPS_R2010:
		printf("fpu0 (MIPS R2010 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	    case MIPS_R3010:
		printf("fpu0 (MIPS R3010 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	    case MIPS_R4010:
		printf("fpu0 (MIPS R4000 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	    default:
		printf("fpu0 (implementation %d revision %d.%d)\n",
			fpu.cpu.cp_imp, fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
	}

	printf("data cache size %dK inst cache size %dK\n",
		machDataCacheSize >> 10, machInstCacheSize >> 10);

	init_hb_intr();

	probeio();

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

/*
 * Probe the main IO bus(es).
 * The percpu structure gives us a handle on the addresses and/or types.
 */
probeio()
{
#if NHB > 0
	hbfind();
#endif
}

#if NHB > 0
int	scsidev_setup_time = 10;

/*
 * Find devices on a Hyper-bus.
 * Fills in the tables, with help from a per-driver
 * slave initialization routine.
 */
hbfind()
{
	register struct hb_device *hi;
	register struct hb_ctlr *hm;
	register int intr, i;
	register caddr_t reg;
	register struct hb_hd *hhp;
	register struct hb_driver *hdp;
	int scsi_inq_done = 0;
	int scsi_skip = 0;
	caddr_t calloc();

	/*
	 * Initialize the SCSI.
	 */
#if defined(RB_NOINITSCSI)
	if ((boothowto & RB_NOINITSCSI) == 0) {
#else
	if (1) {
#endif
		printf("Initializing SCSI");
		scop_init(0);
		for (i = 0; i < scsidev_setup_time; i++) {
			printf(".");
#ifndef notdef
			DELAY( 200000);	/* 0.2 sec? */
#else
			DELAY(1000000);	/* 1 sec. */
#endif
		}
		printf(" done\n");
	}

	hhp = &hb_hd;

	/*
	 * Check each Hyper_bus mass storage controller.
	 * For each one which is potentially on Hyper_bus,
	 * see if it is really there, and if it is record it and
	 * then go looking for slaves.
	 */
	for (hm = hminit; hdp = hm->hm_driver; hm++) {
		intr = (int)hm->hm_intr;
		reg = hm->hm_addr;
		if (intr < 17) {
			if ((scsi_inq_done & (1 << intr)) == 0) {
				scsi_inq_done |= (1 << intr);
				if (psdprobe(hm) < 0) {
					scsi_skip |= (1 << intr);
					continue;
				}
			} else if (scsi_skip & (1 << intr)) {
				continue;
			}
		}
		i = (*hdp->hd_probe)(hm);
		if (i == 0)
			continue;
		scsi_skip |= (1 << intr);
		if (intr >= 0 && intr <= 13) {
			/*
			 * SCSI device !!
			 */
			/* hm->hm_driver is re-writed by driver probe routine */
			hm->hm_scnum = intr / 7;
			hm->hm_intr = intr % 7;
		}
		else {
			hm->hm_scnum = -1;
		}
		printf("%s%d at hb addr %x intr %d\n",
			hdp->hd_mname, hm->hm_ctlr, hm->hm_addr, intr);
		hm->hm_alive = 1;
		hm->hm_hd = &hb_hd;
		hdp->hd_minfo[hm->hm_ctlr] = hm;
		for (hi = hdinit; hi->hi_driver; hi++) {
			if (hi->hi_driver != hdp || hi->hi_alive ||
			    hi->hi_ctlr != hm->hm_ctlr && hi->hi_ctlr != '?')
				continue;
			if (intr >= 0 && intr <= 13) {
				hi->hi_ctlr = hm->hm_ctlr;
			}
			if ((*hdp->hd_slave)(hi, reg, intr)) {
				hi->hi_alive = 1;
				hi->hi_ctlr = hm->hm_ctlr;
				hi->hi_hd = &hb_hd;
				hi->hi_addr = (caddr_t)0;
				if (hi->hi_dk && dkn < DK_NDRIVE)
					hi->hi_dk = dkn++;
				else
					hi->hi_dk = -1;
				hi->hi_mi = hm;
				/* hi_type comes from driver */
				hdp->hd_dinfo[hi->hi_unit] = hi;
				printf("%s%d at %s%d slave %d",
				    hdp->hd_dname, hi->hi_unit,
				    hdp->hd_mname, hm->hm_ctlr, hi->hi_slave);
				if (hi->hi_intr < 16 && (hi->hi_intr & 7) != 7)
					printf(" (bus=%d, chan=%d, lun=%d)",
						hi->hi_intr / 8,
						hi->hi_intr & 7,
						hi->hi_slave);
				printf("\n");
				(*hdp->hd_attach)(hi);
			}
		}
	}

	/*
	 * Now look for non-mass storage peripherals.
	 */
	for (hi = hdinit; hdp = hi->hi_driver; hi++) {
		if (hi->hi_alive || hi->hi_slave != -1)
			continue;
		intr = (int)hi->hi_intr;
		i = (*hdp->hd_probe)(hi);
		if (i == 0)
			continue;
		printf("%s%d at hb addr %x intr %d",
		    hi->hi_driver->hd_dname, hi->hi_unit, hi->hi_addr, hi->hi_intr);
		if (hi->hi_intr < 16 && (hi->hi_intr & 7) != 7)
			printf(" (bus=%d, chan=%d, lun=%d)",
				hi->hi_intr / 8,
				hi->hi_intr & 7,
				hi->hi_slave);
		printf("\n");
		hi->hi_hd = &hb_hd;
		hi->hi_alive = 1;

		if(hm->hm_intr >= 0 && hm->hm_intr <= 13) {
			/* This is SCSI device !! */
			hm->hm_scnum = (int)hm->hm_intr / 7;
			hm->hm_intr = hm->hm_intr % 7;
		}
		else {
			hm->hm_scnum = -1 ;
		}

		hi->hi_addr = (caddr_t)0;
		hi->hi_dk = -1;
		/* hi_type comes from driver */
		hdp->hd_dinfo[hi->hi_unit] = hi;
		(*hdp->hd_attach)(hi);
	}
}
#endif /* NHB > 0 */

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

#define	DOSWAP			/* Change swdevt and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	's', 'd',	/*  0 = sd */
	'f', 'd',	/*  1 = fd */
	'f', 'h',	/*  2 = fh */
	0, 0,		/*  3 = not use */
	0, 0,		/*  4 = not use */
	'r', 'd',	/*  5 = rd */
	0, 0,		/*  6 = not use */
	0, 0,		/*  7 = not use */
	0, 0,		/*  8 = not use */
	0, 0,		/*  9 = not use */
	'o', 'd',	/* 10 = od */
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
	register struct hb_device *ip;
	int  majdev, mindev, unit, part, controller;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != B_DEVMAGIC)
		return;
	majdev = B_TYPE(bootdev);
	if (majdev >= sizeof(devname) / sizeof(devname[0]))
		return;
	controller = B_CONTROLLER(bootdev);
	part = B_PARTITION(bootdev);
	unit = B_UNIT(bootdev);

	for (ip = hdinit; ip->hi_driver; ip++) {
		if (ip->hi_alive && ip->hi_slave == unit &&
		   ip->hi_ctlr == controller &&
		   ip->hi_driver->hd_dname[0] == devname[majdev][0] &&
		   ip->hi_driver->hd_dname[1] == devname[majdev][1])
			break;
	}

	if (ip->hi_driver == 0)
		return;
	mindev = ip->hi_unit;

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
	 * If dumpdev was the same as the old primary swap
	 * device, move it to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
#endif
}

readidrom(rom)
	register u_char *rom;
{
	register u_char *p = (u_char *)IDROM;
	register int i;

	for (i = 0; i < sizeof (struct idrom); i++, p += 2)
		*rom++ = ((*p & 0x0f) << 4) + (*(p + 1) & 0x0f);
	return (0);
}
