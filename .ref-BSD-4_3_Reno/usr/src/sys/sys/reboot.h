/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
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
 *	@(#)reboot.h	7.6 (Berkeley) 6/28/90
 */

/*
 * Arguments to reboot system call.
 * These are passed to boot program in r11,
 * and on to init.
 */
#define	RB_AUTOBOOT	0	/* flags for system auto-booting itself */

#define	RB_ASKNAME	0x01	/* ask for file name to reboot from */
#define	RB_SINGLE	0x02	/* reboot to single user only */
#define	RB_NOSYNC	0x04	/* dont sync before reboot */
#define	RB_HALT		0x08	/* don't reboot, just halt */
#define	RB_INITNAME	0x10	/* name given for /etc/init (unused) */
#define	RB_DFLTROOT	0x20	/* use compiled-in rootdev */
#define	RB_KDB		0x40	/* give control to kernel debugger */
#define	RB_RDONLY	0x80	/* mount root fs read-only */
#define	RB_DUMP		0x100	/* dump kernel memory before reboot */

/*
 * Constants for converting boot-style device number to type,
 * adaptor (uba, mba, etc), unit number and partition number.
 * Type (== major device number) is in the low byte
 * for backward compatibility.  Except for that of the "magic
 * number", each mask applies to the shifted value.
 * Format:
 *	 (4) (4) (4) (4)  (8)     (8)
 *	--------------------------------
 *	|MA | AD| CT| UN| PART  | TYPE |
 *	--------------------------------
 */
#define	B_ADAPTORSHIFT		24
#define	B_ADAPTORMASK		0x0f
#define	B_ADAPTOR(val)		(((val) >> B_ADAPTORSHIFT) & B_ADAPTORMASK)
#define B_CONTROLLERSHIFT	20
#define B_CONTROLLERMASK	0xf
#define	B_CONTROLLER(val)	(((val)>>B_CONTROLLERSHIFT) & B_CONTROLLERMASK)
#define B_UNITSHIFT		16
#define B_UNITMASK		0xf
#define	B_UNIT(val)		(((val) >> B_UNITSHIFT) & B_UNITMASK)
#define B_PARTITIONSHIFT	8
#define B_PARTITIONMASK		0xff
#define	B_PARTITION(val)	(((val) >> B_PARTITIONSHIFT) & B_PARTITIONMASK)
#define	B_TYPESHIFT		0
#define	B_TYPEMASK		0xff
#define	B_TYPE(val)		(((val) >> B_TYPESHIFT) & B_TYPEMASK)

#define	B_MAGICMASK	((u_long)0xf0000000)
#define	B_DEVMAGIC	((u_long)0xa0000000)

#define MAKEBOOTDEV(type, adaptor, controller, unit, partition) \
	(((type) << B_TYPESHIFT) | ((adaptor) << B_ADAPTORSHIFT) | \
	((controller) << B_CONTROLLERSHIFT) | ((unit) << B_UNITSHIFT) | \
	((partition) << B_PARTITIONSHIFT) | B_DEVMAGIC)
