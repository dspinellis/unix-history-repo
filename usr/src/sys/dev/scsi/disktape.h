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
 *	@(#)disktape.h	5.2 (Berkeley) %G%
 *
 * from: $Header: disktape.h,v 1.2 92/05/15 11:24:00 torek Exp $ (LBL)
 */

/*
 * Commands common to disk and tape devices, but not other SCSI devices.
 */

/*
 * Structure of a MODE SENSE command (i.e., the cdb).
 */
struct scsi_cdb_modesense {
	u_char	cdb_cmd,	/* command */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:5,	/* reserved */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_len,	/* allocation length */
		cdb_ctrl;	/* control byte */
};

/*
 * Structure of returned mode sense data.
 */
struct scsi_modesense {
	u_char	ms_len,		/* total sense data length */
		ms_mt,		/* medium type */
		ms_wbs,		/* write protect, buffered mode, & speed */
		ms_bdl;		/* block descriptor length */
	struct scsi_ms_bdesc {
		u_char	dc,	/* density code */
			nbh,	/* number of blocks (MSB) */
			nbm,	/* number of blocks */
			nbl,	/* number of blocks (LSB) */
			xxx,	/* reserved */
			blh,	/* block length (MSB) */
			blm,	/* block length */
			bll;	/* block length (LSB) */
	} ms_bd[1];
	/* followed by Vendor Unique bytes */
};

/*
 * Structure of a PREVENT/ALLOW MEDIUM REMOVAL command.
 */
struct scsi_cdb_pamr {
	u_char	cdb_cmd,	/* 0x1e */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:5,	/* reserved */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_prevent,	/* 1=prevent, 0=allow */
		cdb_ctrl;
};
