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
 *	@(#)processor.h	5.2 (Berkeley) %G%
 *
 * from: $Header: processor.h,v 1.2 92/05/15 11:24:01 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Processor Devices.
 */
#define	CMD_RECEIVE		0x08	/* receive */
#define	CMD_SEND		0x0a	/* send */

/*
 * Structure of a RECEIVE or SEND command (i.e., the cdb).
 */
struct scsi_cdb_rs {
	u_char	cdb_cmd,		/* 0x8 or 0xa */
		cdb_lun:3,		/* logical unit number */
		cdb_xxx:5,		/* reserved */
		cdb_lenh,		/* buffer or data length (MSB) */
		cdb_lenm,		/* buffer or data length */
		cdb_lenl,		/* buffer or data length (LSB) */
		cdb_ctrl;		/* control byte */
};
