/*	vbavar.h	1.1	85/07/21	*/

/*
 * This file contains definitions related to the kernel structures
 * for dealing with the Versabus adapters.
 *
 * Each Versabus controller which is not a device has a vba_ctlr structure.
 * Each Versabus device has a vba_device structure.
 */


#ifndef LOCORE
/*
 * Per-controller structure.
 * (E.g. one for each disk and tape controller, and other things
 * which use and release buffered data paths.)
 *
 * If a controller has devices attached, then there are
 * cross-referenced vba_drive structures.
 * This structure is the one which is queued in Versabus resource wait,
 * and saves the information about Versabus resources which are used.
 * The queue of devices waiting to transfer is also attached here.
 */
struct vba_ctlr {
	struct	vba_driver *um_driver;
	short	um_ctlr;	/* controller index in driver */
	short	um_vbanum;	/* the vba it is on */
	short	um_alive;	/* controller exists */
	int	(**um_intr)();	/* interrupt handler(s) */
	caddr_t	um_addr;	/* address of device in i/o space */
/* the driver saves the prototype command here for use in its go routine */
	int	um_cmd;		/* communication to dgo() */
	int	um_vbinfo;	/* save Versabus registers, etc */
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
 * a Versabus device.  It also contains information
 * for slaves of Versabus controllers as to which device on the slave
 * this is.  A flags field here can also be given in the system specification
 * and is used to tell which vcx lines are hard wired or other device
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
	short	ui_flags;	/* parameter from system specification */
	short	ui_alive;	/* device exists */
	short	ui_type;	/* driver specific type information */
	caddr_t	ui_physaddr;	/* phys addr, for standalone (dump) code */
/* this is the forward link in a list of devices on a controller */
	struct	vba_device *ui_forw;
/* if the device is connected to a controller, this is the controller */
	struct	vba_ctlr *ui_mi;
};
#endif

/*
 * Per-driver structure.
 *
 * Each Versabus driver defines entries for a set of routines
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
	short	ud_xclu;		/* want exclusive use of bdp's */
};

/*
 * Flags to VBA map/bdp allocation routines
 */
#define	VBA_NEEDBDP	1		/* transfer needs a bdp */
#define	VBA_CANTWAIT	2		/* don't block me */
#define	VBA_NEED16	3		/* need 16 bit addresses only */

#define	numvba  1				/* number of vba's */
#ifndef LOCORE
#ifdef KERNEL
/*
 * VBA related kernel variables
 */

/*
 * Vbminit and vbdinit initialize the mass storage controller and
 * device tables specifying possible devices.
 */
extern	struct	vba_ctlr vbminit[];
extern	struct	vba_device vbdinit[];

/*
 * Versabus device address space is mapped by VMEMmap
 * into virtual address umem[][].
 */
extern	struct pte VMEMmap[];	/* vba device addr pte's */
extern	char vmem[];		/* vba device addr space */

#endif KERNEL
#endif !LOCORE
