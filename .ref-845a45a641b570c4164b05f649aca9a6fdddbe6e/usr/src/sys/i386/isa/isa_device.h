/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)isa_device.h	7.1 (Berkeley) %G%
 */

/*
 * ISA Bus Autoconfiguration
 */

/*
 * Per device structure.
 */
struct isa_device {
	struct	isa_driver *id_driver;
	short	id_iobase;	/* base i/o address */
	short	id_irq;		/* interrupt request */
	short	id_drq;		/* DMA request */
	caddr_t id_maddr;	/* physical i/o memory address on bus (if any)*/
	int	id_msize;	/* size of i/o memory */
	int	(*id_intr)();	/* interrupt interface routine */
	int	id_unit;	/* unit number */
	int	id_scsiid;	/* scsi id if needed */
	int	id_alive;	/* device is present */
};

/*
 * Per-driver structure.
 *
 * Each device driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct isa_driver {
	int	(*probe)();		/* test whether device is present */
	int	(*attach)();		/* setup driver for a device */
	char	*name;			/* device name */
};

extern struct isa_device isa_devtab_bio[], isa_devtab_tty[], isa_devtab_net[],
		isa_devtab_null[];
