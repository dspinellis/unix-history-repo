/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)printer.h	5.1 (Berkeley) %G%
 *
 * from: $Header: printer.h,v 1.2 92/05/15 11:24:00 torek Exp $ (LBL)
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
