/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)preset.h	7.1 (Berkeley) %G%
 */

/*
 * preset.h -- preset infomation
 *   by A.Fujita, Dec-12-1992
 */

/*
 * DIP SW-1
 */

#define	PS_AUTOBOOT	0x01				/* 1: PROM auto-boot */
#define	PS_BMC_CONS	0x02				/* 2: use bitmap display as console */
#define	PS_BW_DISP	0x04				/* 3: use B&W display (unused) */
							/* 4: HD write verify (???) */
#define	PS_COFF		0x10				/* 5: boot COFF format kernel */
#define	PS_PLAIN	0x20				/* 6: number of bitmap plain (unused) */
							/* NOTE: it has 4 plain, if turn it on, else 1 */
							/* 7: boot device (unused) */
							/* NOTE: it should be net-booting, if turn it on */
							/* 8: load diagnostic program (unused) */

