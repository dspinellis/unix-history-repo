/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)device.h	7.2 (Berkeley) 5/25/90
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

struct hp_hw {
	char	*hw_addr;	/* physical address of registers */
	short	hw_sc;		/* select code (if applicable) */
	short	hw_type;	/* type (defined below) */
	short	hw_id;		/* HW returned id */
	short	hw_id2;		/* secondary HW id (displays) */
	char	*hw_name;	/* HP product name */
};

#define	MAX_CTLR	16	/* Totally arbitrary */
#define	MAXSLAVES	8	/* Currently the HPIB limit */

#define	WILD_CARD_CTLR	0

/* A controller is a card which can have one or more slaves attached */
#define	CONTROLLER	0x10
#define	HPIB		0x16
#define	SCSI		0x17
#define	VME		0x18
#define	FLINK		0x19

/* Slaves are devices which attach to controllers, e.g. disks, tapes */
#define	RD		0x2a
#define	PPI		0x2b
#define	CT		0x2c

/* These are not controllers, but may have their own HPIB address */
#define	BITMAP		1
#define	NET		2
#define	FPA		4
#define	MISC		5
#define	KEYBOARD	6
#define	COMMDCA		7
#define	COMMDCM		8
#define	COMMDCL		9
#define	PPORT		10

#ifdef KERNEL
extern struct hp_ctlr	hp_cinit[];
extern struct hp_device	hp_dinit[];
extern struct hp_hw	sc_table[];
#endif
