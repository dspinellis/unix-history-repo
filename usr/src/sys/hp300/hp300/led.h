/*
 * Copyright (c) 1992 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: led.h 1.2 92/08/27$
 *
 *	@(#)led.h	7.1 (Berkeley) %G%
 */

#define	LED_ADDR	0x1FFFF		/* a ROM address--strange but true */

#define	LED_LANXMT	0x80		/* for LAN transmit activity */
#define	LED_LANRCV	0x40		/* for LAN receive activity */
#define	LED_DISK	0x20		/* for disk activity */
#define	LED_PULSE	0x10		/* heartbeat */

#ifdef KERNEL
extern	char *ledaddr;
extern	int inledcontrol;
#endif
