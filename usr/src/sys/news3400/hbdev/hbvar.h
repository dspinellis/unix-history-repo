/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: hbvar.h,v 4.300 91/06/09 06:21:52 root Rel41 $ SONY
 *
 *	@(#)hbvar.h	7.1 (Berkeley) %G%
 */

/*
 * Copyright (c) 1987,1988 by SONY Corporation.
 */

/*
 *	hbvar.h.c	ver 0.1		
 *					Form	Fri Dec 11 16:07:41 JST 1987
 *					Modify	Thu Feb  2 08:57:52 JST 1989
 *
 */

#ifndef __HBVAR__
#define __HBVAR__ 1

/*
 * This file contains definitions related to the kernel structures
 * for dealing with the HYPER-BUS interface.
 *
 * Each HYPER-BUS has a hb_hd structure.
 * Each HYPER-BUS controller which is not a device has a hb_ctlr structure.
 * Each HYPER-BUS device has a hb_device structure.
 */

#ifndef LOCORE
/*
 * Per-HYPER-BUS structure.
 *
 * This structure holds the interrupt vector for the HYPER-BUS,
 * and its address in physical and virtual space.  At boot time
 * we determine the devices attached to the HYPER-BUS and their
 * interrupt vectors, filling in vh_vec.  We free the map
 * register resources of the HYPER-BUS into the structures
 * defined here.
 *
 * During normal operation, resources are allocated and returned
 * to the structures here.  We watch the number of passive releases
 * on each HYPER-BUS, and if the number is excessive may reset the HYPER-BUS.
 * 
 * When HYPER-BUS resources are needed and not available,
 * then device drivers may have to wait to get to the bus and are
 * queued here.  It is also possible for processes to block in
 * the HYPER-BUS driver in resource wait (mrwant); these
 * wait states are also recorded here.
 */
struct	hb_hd {
	caddr_t hh_phys; /* Not used and init now. */
};

/*
 *	Per-controller structure.
 *
 * If a controller has devices attached, then there are
 * cross-referenced hb_drive structures.
 * This structure is the one which is queued in HYPER-BUS resource wait,
 * and saves the information about HYPER-BUS resources which are used.
 * The queue of devices waiting to transfer is also attached here.
 */
struct hb_ctlr {
	struct hb_driver *hm_driver;
	short hm_ctlr;			/* controller index in driver */
	short hm_alive;			/* controller exists */
	caddr_t hm_addr;		/* address of command area */
	int hm_intr;			/* interrupt number */
	short hm_scnum ;		/* the scsi it is on */
	struct hb_hd *hm_hd;
	int hm_hbinfo;			/* save map registers, etc */
	struct buf hm_tab;		/* queue of devices for this controller */
};

/*
 *	Per ``device'' structure.
 *
 * If a controller has many drives attached, then there will
 * be several hb_device structures associated with a single hb_ctlr
 * structure.
 */
struct hb_device {
	struct hb_driver *hi_driver;
	short hi_unit;		/* unit number on the system */
	short hi_ctlr;		/* mass ctlr number; -1 if none */
	short hi_slave;		/* slave on controller */
	caddr_t hi_addr;	/* address of command area */
	int hi_intr;		/* interrupt number */
	short hi_dk;		/* if init 1 set to number for iostat */
	int hi_flags;		/* parameter from system specification */
	short hi_alive;		/* device exists */
	short hi_type;		/* driver specific type information */
/*	caddr_t hi_physaddr;	/* phys addr, for standalone (dump) code */
/* this is the forward link in a list of devices on a controller */
	struct hb_device *hi_forw;
/* if the device is connected to a controller, this is the controller */
	struct hb_ctlr *hi_mi;
	struct hb_hd *hi_hd;
};

/*
 *	Per-driver structure.
 *
 * Each HYPER-BUS driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct hb_driver {
	int (*hd_probe)();		/* see if a driver is really there */
	int (*hd_slave)();		/* see if a slave is there */
	int (*hd_attach)();		/* setup driver for a slave */
	int (*hd_dgo)();		/* start transfer */
	int (*hd_intr)();		/* interrupt handler */
	char *hd_dname;			/* name of a device */
	struct hb_device **hd_dinfo;	/* backpointers to hdinit structs */
	char *hd_mname;			/* name of a controller */
	struct hb_ctlr **hd_minfo;	/* backpointers to hminit structs */
	short hd_xclu;			/* want exclusive use */
};

#ifdef KERNEL

/*
 * HYPER-BUS related kernel variables
 */
struct	hb_hd hb_hd;

/*
 * Hbinit and hdinit initialize the mass storage controller and
 * device tables specifying possible devices.
 */
extern	struct	hb_ctlr hminit[];
extern	struct	hb_device hdinit[];

#endif /* KERNEL */

#endif /* !LOCORE */

#endif /* !__HBVAR__ */
