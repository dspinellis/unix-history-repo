/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)reboot.h	7.1 (Berkeley) %G%
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
#define	RB_INITNAME	0x10	/* name given for /etc/init */
#define	RB_DFLTROOT	0x20	/* use compiled-in rootdev */

#define	RB_PANIC	0	/* reboot due to panic */
#define	RB_BOOT		1	/* reboot due to boot() */

/*
 * Constants for converting boot-style device number to type,
 * adaptor (uba, mba, etc), unit number and partition number.
 * Type (== major device number) is in the low byte
 * for backward compatibility.  Except for that of the "magic
 * number", each mask applies to the shifted value.
 */
#define	B_ADAPTORSHIFT	24
#define	B_ADAPTORMASK	0x0f
#define B_UNITSHIFT	16
#define B_UNITMASK	0xff
#define B_PARTITIONSHIFT 8
#define B_PARTITIONMASK	0xff
#define	B_TYPESHIFT	0
#define	B_TYPEMASK	0xff
#define	B_MAGICMASK	0xf0000000
#define	B_DEVMAGIC	0xa0000000
