/*
 * Copyright (c) 1982, 1990, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)device.h	7.2 (Berkeley) %G%
 */

struct driver {
	int	(*d_init)();
	char	*d_name;
	int	(*d_start)();
	int	(*d_go)();
	int	(*d_intr)();
	int	(*d_done)();
};

struct hp_ctlr {
	struct driver	*hp_driver;
	int		hp_unit;
	int		hp_alive;
	char		*hp_addr;
	int		hp_flags;
	int		hp_ipl;
};

struct hp_device {
	struct driver	*hp_driver;
	struct driver	*hp_cdriver;
	int		hp_unit;
	int		hp_ctlr;
	int		hp_slave;
	char		*hp_addr;
	int		hp_dk;
	int		hp_flags;
	int		hp_alive;
	int		hp_ipl;
};

struct	devqueue {
	struct	devqueue *dq_forw;
	struct	devqueue *dq_back;
	int	dq_ctlr;
	int	dq_unit;
	int	dq_slave;
	struct	driver *dq_driver;
};

#define	MAXCTLRS	16	/* Size of HW table (arbitrary) */
#define	MAXSLAVES	8	/* Slaves per controller (HPIB/SCSI limit) */

struct hp_hw {
	caddr_t	hw_pa;		/* physical address of control space */
	int	hw_size;	/* size of control space */
	caddr_t	hw_kva;		/* kernel virtual address of control space */
	short	hw_id;		/* HW returned id */
	short	hw_secid;	/* secondary HW id (displays) */
	short	hw_type;	/* type (defined below) */
	short	hw_sc;		/* select code (if applicable) */
};

/* bus types */
#define	B_MASK		0xE000
#define	B_DIO		0x2000
#define B_DIOII		0x4000
#define B_VME		0x6000
/* controller types */
#define	C_MASK		0x8F
#define C_FLAG		0x80
#define	C_HPIB		0x81
#define C_SCSI		0x82
#define C_VME		0x83
/* device types (controllers with no slaves) */
#define D_MASK		0x8F
#define	D_BITMAP	0x01
#define	D_LAN		0x02
#define	D_FPA		0x03
#define	D_KEYBOARD	0x04
#define	D_COMMDCA	0x05
#define	D_COMMDCM	0x06
#define	D_COMMDCL	0x07
#define	D_PPORT		0x08
#define	D_SIO		0x09
#define	D_BMC		0x0A
#define	D_MISC		0x7F

#define HW_ISCTLR(hw)	((hw)->hw_type & C_FLAG)
#define HW_ISDIOII(hw)	((hw)->hw_type & B_DIOII)
#define HW_ISHPIB(hw)	(((hw)->hw_type & C_MASK) == C_HPIB)
#define HW_ISSCSI(hw)	(((hw)->hw_type & C_MASK) == C_SCSI)
#define HW_ISDEV(hw,d)	(((hw)->hw_type & D_MASK) == (d))

#ifdef KERNEL
extern struct hp_hw sc_table[];
extern struct hp_ctlr hp_cinit[];
extern struct hp_device hp_dinit[];
extern caddr_t sctova(), sctopa(), iomap();
#endif
