/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)vbavar.h	7.1 (Berkeley) %G%
 */

/*
 * This file contains definitions related to the kernel structures
 * for dealing with the VERSAbus adapters.
 *
 * Each VERSAbus has a vba_hd structure.
 * Each VERSAbus controller which is not a device has a vba_ctlr structure.
 * Each VERSAbus device has a vba_device structure.
 */

#ifndef LOCORE
/*
 * Per-vba structure.
 */
struct	vba_hd {
	int	vh_lastiv;		/* last interrupt vector assigned */
};

/*
 * Per-controller structure.
 * (E.g. one for each disk and tape controller, and other things
 * which use and release buffered data paths.)
 *
 * If a controller has devices attached, then there are
 * cross-referenced vba_drive structures.
 * This structure is the one which is queued in VERSAbus resource wait,
 * and saves the information about VERSAbus resources which are used.
 * The queue of devices waiting to transfer is also attached here.
 */
struct vba_ctlr {
	struct	vba_driver *um_driver;
	short	um_ctlr;	/* controller index in driver */
	short	um_vbanum;	/* the vba it is on */
	short	um_alive;	/* controller exists */
	int	(**um_intr)();	/* interrupt handler(s) */
	caddr_t	um_addr;	/* address of device in i/o space */
	struct	vba_hd *um_hd;
/* the driver saves the prototype command here for use in its go routine */
	int	um_cmd;		/* communication to dgo() */
	int	um_vbinfo;	/* save VERSAbus registers, etc */
	struct	buf um_tab;	/* queue of devices for this controller */
};

/*
 * Per ``device'' structure.
 * (A controller has devices or uses and releases buffered data paths).
 * (Everything else is a ``device''.)
 *
 * If a controller has many drives attached, then there will
 * be several vba_device structures associated with a single vba_ctlr
 * structure.
 *
 * This structure contains all the information necessary to run
 * a VERSAbus device.  It also contains information
 * for slaves of VERSAbus controllers as to which device on the slave
 * this is.  A flags field here can also be given in the system specification
 * and is used to tell which tty lines are hard wired or other device
 * specific parameters.
 */
struct vba_device {
	struct	vba_driver *ui_driver;
	short	ui_unit;	/* unit number on the system */
	short	ui_ctlr;	/* mass ctlr number; -1 if none */
	short	ui_vbanum;	/* the vba it is on */
	short	ui_slave;	/* slave on controller */
	int	(**ui_intr)();	/* interrupt handler(s) */
	caddr_t	ui_addr;	/* address of device in i/o space */
	short	ui_dk;		/* if init 1 set to number for iostat */
	long	ui_flags;	/* parameter from system specification */
	short	ui_alive;	/* device exists */
	short	ui_type;	/* driver specific type information */
	caddr_t	ui_physaddr;	/* phys addr, for standalone (dump) code */
/* this is the forward link in a list of devices on a controller */
	struct	vba_device *ui_forw;
/* if the device is connected to a controller, this is the controller */
	struct	vba_ctlr *ui_mi;
	struct	vba_hd *ui_hd;
};
#endif

/*
 * Per-driver structure.
 *
 * Each VERSAbus driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct vba_driver {
	int	(*ud_probe)();		/* see if a driver is really there */
	int	(*ud_slave)();		/* see if a slave is there */
	int	(*ud_attach)();		/* setup driver for a slave */
	int	(*ud_dgo)();		/* fill csr/ba to start transfer */
	long	*ud_addr;		/* device csr addresses */
	char	*ud_dname;		/* name of a device */
	struct	vba_device **ud_dinfo;	/* backpointers to vbdinit structs */
	char	*ud_mname;		/* name of a controller */
	struct	vba_ctlr **ud_minfo;	/* backpointers to vbminit structs */
};

/*
 * Common state for Versabus driver I/O resources,
 * including memory for intermediate buffer and page map,
 * allocated by vbainit.
 */
struct vb_buf {
	/* these fields set up once by vbainit */
	int	vb_flags;		/* device parameters */
	struct	pte *vb_map;		/* private page entries */
	caddr_t	vb_utl;			/* virtual addresses mapped by vb_map */
	caddr_t	vb_rawbuf;		/* intermediate buffer */
	u_long	vb_physbuf;		/* phys addr of intermediate buffer */
	u_long	vb_bufsize;		/* intermediate buffer size */
	u_long	vb_maxphys;		/* physical address limit */
	/* remaining fields apply to current transfer: */
	int	vb_copy;		/* copy to/from intermediate buffer */
	int	vb_iskernel;		/* is to/from kernel address space */
};

/*
 * flags to vbainit
 */
#define	VB_32BIT	0x00		/* device uses 32-bit addressing */
#define	VB_24BIT	0x01		/* device uses 24-bit addressing */
#define	VB_20BIT	0x02		/* device uses 20-bit addressing */
#define	VB_SCATTER	0x04		/* device does scatter-gather */

/*
 * hardware addressing limits
 */
#define	VB_MAXADDR20	0x000fffff	/* highest addr for 20-bit */
#define	VB_MAXADDR24	0x007fffff	/* highest addr for 23/24-bit */
#define	VB_MAXADDR32	0x3effffff	/* highest addr for 32-bit */

/*
 * Statistics on vba operations.
 */
struct vbastat {
	u_long	k_raw;		/* to/from contiguous kernel DMA buffer */
	u_long	u_raw;		/* to/from contiguous user DMA buffer */
	u_long	k_copy;		/* copied to/from kernel */
	u_long	u_copy;		/* copied to/from user */
	u_long	k_sg;		/* scatter-gather to/from kernel */
	u_long	u_sg;		/* scatter-gather to/from user */
};

#ifndef LOCORE
#ifdef KERNEL
/*
 * VBA related kernel variables
 */
int	numvba;					/* number of uba's */
struct	vba_hd vba_hd[];
struct	vbastat vbastat;

/*
 * Vbminit and vbdinit initialize the mass storage controller and
 * device tables specifying possible devices.
 */
extern	struct	vba_ctlr vbminit[];
extern	struct	vba_device vbdinit[];

/*
 * VERSAbus device address space is mapped by VMEMmap
 * into virtual address vmem[][].
 */
extern	struct pte VMEMmap[];	/* vba device addr pte's */
extern	caddr_t vmem;		/* vba device addr space */
u_long	vbasetup();
#endif KERNEL
#endif !LOCORE
