/*	mbavar.h	4.14	81/03/09	*/

/*
 * This file contains definitions related to the kernel structures
 * for dealing with the massbus adapters.
 *
 * Each mba has a mba_hd structure.
 * Each massbus device has a mba_device structure.
 * Each massbus slave has a mba_slave structure.
 *
 * At boot time we prowl the structures and fill in the pointers
 * for devices which we find.
 */

/*
 * Per-mba structure.
 *
 * The initialization routine uses the information in the mbdinit table
 * to initialize the what is attached to each massbus slot information.
 * It counts the number of devices on each mba (to see if bothering to
 * search/seek is appropriate).
 *
 * During normal operation, the devices attached to the mba which wish
 * to transfer are queued on the mh_act? links.
 */
struct	mba_hd {
	short	mh_active;		/* set if mba is active */
	short	mh_ndrive;		/* number of devices, to avoid seeks */
	struct	mba_regs *mh_mba;	/* virt addr of mba */
	struct	mba_regs *mh_physmba;	/* phys addr of mba */
	struct	mba_device *mh_mbip[8];	/* what is attached to each dev */
	struct	mba_device *mh_actf;	/* head of queue to transfer */
	struct	mba_device *mh_actl;	/* tail of queue to transfer */
};

/*
 * Per-device structure
 * (one for each RM/RP disk, and one for each tape formatter).
 *
 * This structure is used by the device driver as its argument
 * to the massbus driver, and by the massbus driver to locate
 * the device driver for a particular massbus slot.
 *
 * The device drivers hang ready buffers on this structure,
 * and the massbus driver will start i/o on the first such buffer
 * when appropriate.
 */
struct	mba_device {
	struct	mba_driver *mi_driver;
	short	mi_unit;	/* unit number to the system */
	short	mi_mbanum;	/* the mba it is on */
	short	mi_drive;	/* controller on mba */
	short	mi_dk;		/* driver number for iostat */
	short	mi_alive;	/* device exists */
	short	mi_type;	/* driver specific unit type */
	struct	buf mi_tab;	/* head of queue for this device */
	struct	mba_device *mi_forw;
/* we could compute these every time, but hereby save time */
	struct	mba_regs *mi_mba;
	struct	mba_drv *mi_drv;
	struct	mba_hd *mi_hd;
};

/*
 * Tape formatter slaves are specified by
 * the following information which is used
 * at boot time to initialize the tape driver
 * internal tables.
 */
struct	mba_slave {
	struct	mba_driver *ms_driver;
	short	ms_ctlr;		/* which of several formatters */
	short	ms_unit;		/* which unit to system */
	short	ms_slave;		/* which slave to formatter */
	short	ms_alive;
};

/*
 * Per device-type structure.
 *
 * Each massbus driver defines entries for a set of routines used
 * by the massbus driver, as well as an array of types which are
 * acceptable to it.
 */
struct mba_driver {
	int	(*md_attach)();		/* attach a device */
	int	(*md_slave)();		/* attach a slave */
	int	(*md_ustart)();		/* unit start routine */
	int	(*md_start)();		/* setup a data transfer */
	int	(*md_dtint)();		/* data transfer complete */
	int	(*md_ndint)();		/* non-data transfer interrupt */
	short	*md_type;		/* array of drive type codes */
	char	*md_dname, *md_sname;	/* device, slave names */
	struct	mba_device **md_info;	/* backpointers to mbinit structs */
};

/*
 * Possible return values from unit start routines.
 */
#define	MBU_NEXT	0		/* skip to next operation */
#define	MBU_BUSY	1		/* dual port busy; wait for intr */
#define	MBU_STARTED	2		/* non-data transfer started */
#define	MBU_DODATA	3		/* data transfer ready; start mba */

/*
 * Possible return values from data transfer interrupt handling routines
 */
#define	MBD_DONE	0		/* data transfer complete */
#define	MBD_RETRY	1		/* error occurred, please retry */
#define	MBD_RESTARTED	2		/* driver restarted i/o itself */

/*
 * Possible return values from non-data-transfer interrupt handling routines
 */
#define	MBN_DONE	0		/* non-data transfer complete */
#define	MBN_RETRY	1		/* failed; retry the operation */
#define	MBN_SKIP	2		/* don't do anything */

/*
 * Clear attention status for specified device.
 */
#define	mbclrattn(mi)	((mi)->mi_mba->mba_drv[0].mbd_as = 1 << (mi)->mi_drive)

/*
 * Kernel definitions related to mba.
 */
#ifdef KERNEL
#if NMBA > 0
struct	mba_hd mba_hd[NMBA];
extern	Xmba0int(), Xmba1int(), Xmba2int(), Xmba3int();

extern	struct	mba_device mbdinit[];
extern	struct	mba_slave mbsinit[];
int	nummba;
#endif
#endif
