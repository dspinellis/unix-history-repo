/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)reboot.h	6.2 (Berkeley) %G%
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

#define	RB_PANIC	0	/* reboot due to panic */
#define	RB_BOOT		1	/* reboot due to boot() */
