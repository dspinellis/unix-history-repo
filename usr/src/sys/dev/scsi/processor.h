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
 *	@(#)processor.h	5.3 (Berkeley) %G%
 *
 * from: $Header: processor.h,v 1.3 92/12/02 03:52:27 torek Exp $ (LBL)
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
		cdb_lun_xxx,		/* logical unit number + reserved */
		cdb_lenh,		/* buffer or data length (MSB) */
		cdb_lenm,		/* buffer or data length */
		cdb_lenl,		/* buffer or data length (LSB) */
		cdb_ctrl;		/* control byte */
};
