/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbd.h	7.3 (Berkeley) %G%
 *
 * from: $Header: kbd.h,v 1.5 92/11/26 01:15:33 torek Exp $ (LBL)
 */

/*
 * Keyboard `registers'.  (This should be called kbd_reg.h but we need to
 * be compatible.)
 */

/*
 * Control codes sent from type 2, 3, and 4 keyboards.
 *
 * Note that KBD_RESET is followed by a keyboard ID, while KBD_IDLE is not.
 * KBD_IDLE does not take the place of any `up' transitions (it merely occurs
 * after them).
 */
#define	KBD_RESET	0xff		/* keyboard `reset' response */
#define	KBD_IDLE	0x7f		/* keyboard `all keys are up' code */

/* Keyboard IDs */
#define	KB_SUN2		2		/* type 2 keyboard */
#define	KB_SUN3		3		/* type 3 keyboard */
#define	KB_SUN4		4		/* type 4 keyboard */

/* Key codes are in 0x00..0x7e; KBD_UP is set if the key goes up */
#define	KBD_KEYMASK	0x7f		/* keyboard key mask */
#define	KBD_UP		0x80		/* keyboard `up' transition */

/* Keyboard codes needed to recognize the L1-A sequence */
#define	KBD_L1		1		/* keyboard code for `L1' key */
#define	KBD_A		77		/* keyboard code for `A' key */

/* Control codes sent to the various keyboards */
#define	KBD_CMD_RESET	1		/* reset keyboard */
#define	KBD_CMD_BELL	2		/* turn bell on */
#define	KBD_CMD_NOBELL	3		/* turn bell off */
#define	KBD_CMD_CLICK	10		/* turn keyclick on */
#define	KBD_CMD_NOCLICK	11		/* turn keyclick off */
