/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ubavar.h	7.6 (Berkeley) %G%
 */

/*
 * This file contains definitions related to the kernel structures
 * for dealing with the unibus adapters.
 *
 * Each uba has a uba_hd structure.
 * Each unibus controller which is not a device has a uba_ctlr structure.
 * Each unibus device has a uba_device structure.
 */

#ifndef LOCORE
/*
 * Per-uba structure.
 *
 * This structure holds the interrupt vector for the uba,
 * and its address in physical and virtual space.  At boot time
 * we determine the devices attached to the uba's and their
 * interrupt vectors, filling in uh_vec.  We free the map
 * register and bdp resources of the uba into the structures
 * defined here.
 *
 * During normal operation, resources are allocated and returned
 * to the structures here.  We watch the number of passive releases
 * on each uba, and if the number is excessive may reset the uba.
 * 
 * When uba resources are needed and not available, or if a device
 * which can tolerate no other uba activity (rk07) gets on the bus,
 * then device drivers may have to wait to get to the bus and are
 * queued here.  It is also possible for processes to block in
 * the unibus driver in resource wait (mrwant, bdpwant); these
 * wait states are also recorded here.
 */
struct	uba_hd {
	int	uh_type;		/* type of adaptor */
	struct	uba_regs *uh_uba;	/* virt addr of uba adaptor regs */
	struct	uba_regs *uh_physuba;	/* phys addr of uba adaptor regs */
	struct	pte *uh_mr;		/* start of page map */
	int	uh_memsize;		/* size of uba memory, pages */
	caddr_t	uh_mem;			/* start of uba memory address space */
	caddr_t	uh_iopage;		/* start of uba io page */
	int	(**uh_vec)();		/* interrupt vector */
	struct	uba_device *uh_actf;	/* head of queue to transfer */
	struct	uba_device *uh_actl;	/* tail of queue to transfer */
	short	uh_mrwant;		/* someone is waiting for map reg */
	short	uh_bdpwant;		/* someone awaits bdp's */
	int	uh_bdpfree;		/* free bdp's */
	int	uh_hangcnt;		/* number of ticks hung */
	int	uh_zvcnt;		/* number of recent 0 vectors */
	long	uh_zvtime;		/* time over which zvcnt accumulated */
	int	uh_zvtotal;		/* total number of 0 vectors */
	int	uh_errcnt;		/* number of errors */
	int	uh_lastiv;		/* last free interrupt vector */
	short	uh_users;		/* transient bdp use count */
	short	uh_xclu;		/* an rk07 is using this uba! */
	int	uh_lastmem;		/* limit of any unibus memory */
#define	UAMSIZ	100
	struct	map *uh_map;		/* register free map */
};

/* given a pointer to uba_regs, find DWBUA registers */
/* this should be replaced with a union in uba_hd */
#define	BUA(uba)	((struct dwbua_regs *)(uba))

/*
 * Per-controller structure.
 * (E.g. one for each disk and tape controller, and other things
 * which use and release buffered data paths.)
 *
 * If a controller has devices attached, then there are
 * cross-referenced uba_drive structures.
 * This structure is the one which is queued in unibus resource wait,
 * and saves the information about unibus resources which are used.
 * The queue of devices waiting to transfer is also attached here.
 */
struct uba_ctlr {
	struct	uba_driver *um_driver;
	short	um_ctlr;	/* controller index in driver */
	short	um_ubanum;	/* the uba it is on */
	short	um_alive;	/* controller exists */
	int	(**um_intr)();	/* interrupt handler(s) */
	caddr_t	um_addr;	/* address of device in i/o space */
	struct	uba_hd *um_hd;
/* the driver saves the prototype command here for use in its go routine */
	int	um_cmd;		/* communication to dgo() */
	int	um_ubinfo;	/* save unibus registers, etc */
	int	um_bdp;		/* for controllers that hang on to bdp's */
	struct	buf um_tab;	/* queue of devices for this controller */
};

/*
 * Per ``device'' structure.
 * (A controller has devices or uses and releases buffered data paths).
 * (Everything else is a ``device''.)
 *
 * If a controller has many drives attached, then there will
 * be several uba_device structures associated with a single uba_ctlr
 * structure.
 *
 * This structure contains all the information necessary to run
 * a unibus device such as a dz or a dh.  It also contains information
 * for slaves of unibus controllers as to which device on the slave
 * this is.  A flags field here can also be given in the system specification
 * and is used to tell which dz lines are hard wired or other device
 * specific parameters.
 */
struct uba_device {
	struct	uba_driver *ui_driver;
	short	ui_unit;	/* unit number on the system */
	short	ui_ctlr;	/* mass ctlr number; -1 if none */
	short	ui_ubanum;	/* the uba it is on */
	short	ui_slave;	/* slave on controller */
	int	(**ui_intr)();	/* interrupt handler(s) */
	caddr_t	ui_addr;	/* address of device in i/o space */
	short	ui_dk;		/* if init 1 set to number for iostat */
	int	ui_flags;	/* parameter from system specification */
	short	ui_alive;	/* device exists */
	short	ui_type;	/* driver specific type information */
	caddr_t	ui_physaddr;	/* phys addr, for standalone (dump) code */
/* this is the forward link in a list of devices on a controller */
	struct	uba_device *ui_forw;
/* if the device is connected to a controller, this is the controller */
	struct	uba_ctlr *ui_mi;
	struct	uba_hd *ui_hd;
};

/*
 * Per-driver structure.
 *
 * Each unibus driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct uba_driver {
	int	(*ud_probe)();		/* see if a driver is really there */
	int	(*ud_slave)();		/* see if a slave is there */
	int	(*ud_attach)();		/* setup driver for a slave */
	int	(*ud_dgo)();		/* fill csr/ba to start transfer */
	u_short	*ud_addr;		/* device csr addresses */
	char	*ud_dname;		/* name of a device */
	struct	uba_device **ud_dinfo;	/* backpointers to ubdinit structs */
	char	*ud_mname;		/* name of a controller */
	struct	uba_ctlr **ud_minfo;	/* backpointers to ubminit structs */
	short	ud_xclu;		/* want exclusive use of bdp's */
	short	ud_keepbdp;		/* hang on to bdp's once allocated */
	int	(*ud_ubamem)();		/* see if dedicated memory is present */
};
#endif

/*
 * Flags to UBA map/bdp allocation routines
 */
#define	UBA_NEEDBDP	0x01		/* transfer needs a bdp */
#define	UBA_CANTWAIT	0x02		/* don't block me */
#define	UBA_NEED16	0x04		/* need 16 bit addresses only */
#define	UBA_HAVEBDP	0x08		/* use bdp specified in high bits */

/*
 * Macros to bust return word from map allocation routines.
 * SHOULD USE STRUCTURE TO STORE UBA RESOURCE ALLOCATION:
 */
#ifdef notyet
struct ubinfo {
	long	ub_addr;	/* unibus address: mr + boff */
	int	ub_nmr;		/* number of registers, 0 if empty */
	int	ub_bdp;		/* bdp number, 0 if none */
};
#define	UBAI_MR(i)	(((i) >> 9) & 0x7ff)	/* starting map register */
#define	UBAI_BOFF(i)	((i)&0x1ff)		/* page offset */
#else
#define	UBAI_BDP(i)	((int)(((unsigned)(i)) >> 28))
#define	BDPMASK		0xf0000000
#define	UBAI_NMR(i)	((int)((i) >> 20) & 0xff)	/* max 255 (=127.5K) */
#define	UBA_MAXNMR	255
#define	UBAI_MR(i)	((int)((i) >> 9) & 0x7ff)	/* max 2047 */
#define	UBA_MAXMR	2047
#define	UBAI_BOFF(i)	((int)((i) & 0x1ff))
#define	UBAI_ADDR(i)	((int)((i) & 0xfffff))	/* uba addr (boff+mr) */
#define	UBAI_INFO(off, mr, nmr, bdp) \
	(((bdp) << 28) | ((nmr) << 20) | ((mr) << 9) | (off))
#endif

#ifndef LOCORE
#ifdef KERNEL
#define	ubago(ui)	ubaqueue(ui, 0)

/*
 * UBA related kernel variables
 */
int	numuba;					/* number of uba's */
struct	uba_hd uba_hd[];

/*
 * Ubminit and ubdinit initialize the mass storage controller and
 * device tables specifying possible devices.
 */
extern	struct	uba_ctlr ubminit[];
extern	struct	uba_device ubdinit[];

/*
 * UNIBUS device address space is mapped by UMEMmap
 * into virtual address umem[][].
 * The IO page is mapped to the last 8K of each.
 * This should be enlarged for the Q22 bus.
 */
extern	struct pte UMEMmap[][512];	/* uba device addr pte's */
extern	char umem[][512*NBPG];		/* uba device addr space */

/*
 * Since some VAXen vector their unibus interrupts
 * just adjacent to the system control block, we must
 * allocate space there when running on ``any'' cpu.  This space is
 * used for the vectors for all ubas.
 */
extern	int (*UNIvec[][128])();			/* unibus vec for ubas */
extern	int (*eUNIvec)();			/* end of unibus vec */

#if defined(VAX780) || defined(VAX8600)
/*
 * On DW780's, we must set the scb vectors for the nexus of the
 * UNIbus adaptors to vector to locore unibus adaptor interrupt dispatchers
 * which make 780's look like the other VAXen.
 */
extern	Xua0int(), Xua1int(), Xua2int(), Xua3int();
#endif VAX780
#endif KERNEL
#endif !LOCORE
