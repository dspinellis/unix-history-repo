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
 *	@(#)auxreg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: auxreg.h,v 1.8 92/11/26 03:04:45 torek Exp $ (LBL)
 */

/*
 * Sun-4c Auxiliary I/O register.  This register talks to the floppy
 * (if it exists) and the front-panel LED.
 */

#define	AUXIO_MB1	0xf0		/* must be set on write */
#define	AUXIO_FHD	0x20		/* floppy: high density (unreliable?)*/
#define	AUXIO_FDC	0x10		/* floppy: diskette was changed */
#define	AUXIO_FDS	0x08		/* floppy: drive select */
#define	AUXIO_FTC	0x04		/* floppy: drives Terminal Count pin */
#define	AUXIO_FEJ	0x02		/* floppy: eject disk */
#define	AUXIO_LED	0x01		/* front panel LED */

/*
 * We use a fixed virtual address for the register because we use it for
 * timing short sections of code (via external hardware attached to the LED).
 */
#define	AUXIO_REG	((volatile u_char *)(AUXREG_VA + 3))

#define LED_ON		*AUXIO_REG = AUXIO_MB1|AUXIO_FEJ|AUXIO_LED
#define LED_OFF		*AUXIO_REG = AUXIO_MB1|AUXIO_FEJ
#define LED_FLIP	*AUXIO_REG = (*AUXIO_REG | AUXIO_MB1) ^ AUXIO_LED

#define	AUXIO_BITS	"\20\6FHD\5FDC\4FDS\3FTC\2FEJ\1LED"
