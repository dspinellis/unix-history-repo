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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)printer.h	5.3 (Berkeley) %G%
 *
 * from: $Header: printer.h,v 1.3 92/12/02 03:52:03 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Printer Devices.
 */
#define	CMD_FORMAT		0x04	/* (set) format */
#define	CMD_PRINT		0x0a	/* print */
#define	CMD_SLEW_PRINT		0x0b	/* slew and print */
#define	CMD_FLUSH_BUFFER	0x10	/* flush buffer */
#define	CMD_RBD			0x14	/* recover buffered data */
#define	CMD_MODE_SELECT		0x15	/* mode select */
#define	CMD_RESERVE_UNIT	0x16	/* reserve unit */
#define	CMD_RELEASE_UNIT	0x17	/* release unit */
#define	CMD_MODE_SENSE		0x1a	/* mode sense */
#define	CMD_STOP_PRINT		0x1b	/* stop print */
