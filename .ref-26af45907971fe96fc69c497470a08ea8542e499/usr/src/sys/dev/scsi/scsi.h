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
 *	@(#)scsi.h	5.3 (Berkeley) %G%
 *
 * from: $Header: scsi.h,v 1.5 93/02/01 19:19:15 torek Exp $ (LBL)
 */

/*
 * Machine independent SCSI defintions.
 */

/*
 * Mostly-generic command descriptor blocks (6 and 10 bytes).
 * Note that no SCSI command uses the 12 byte variety, hence it
 * is not defined here.
 */
struct scsi_cdb6 {
	u_char	cdb_cmd,	/* command code */
		cdb_lun_lbah,	/* logical unit number, & lba (MSB) */
		cdb_lbam,	/* logical block address */
		cdb_lbal,	/* logical block address (LSB) */
		cdb_len,	/* transfer length */
		cdb_ctrl;	/* control byte */
};

struct scsi_cdb10 {
	u_char	cdb_cmd,	/* command code */
		cdb_lun_rel,	/* logical unit number, rsvd, & reladr flag */
		cdb_lbah,	/* logical block address (MSB) */
		cdb_lbahm,	/* logical block address (high middle byte) */
		cdb_lbalm,	/* logical block address (low middle byte) */
		cdb_lbal,	/* logical block address (LSB) */
		cdb_xxx,	/* reserved */
		cdb_lenh,	/* transfer length (MSB) */
		cdb_lenl,	/* transfer length (LSB) */
		cdb_ctrl;	/* control byte */
};

/*
 * SCSI `generic' cdb.
 * The length of the cdb is implicit in the first byte (see scsivar.h).
 * This is 16 bytes, rather than 10 or 12, just to help out alignment.
 */
struct scsi_cdb {
	u_char	cdb_bytes[16];	/* up to 16 bytes of command */
};
#define CDB6(cdb)	((struct scsi_cdb6 *)&(cdb)->cdb_bytes[0])
#define CDB10(cdb)	((struct scsi_cdb10 *)&(cdb)->cdb_bytes[0])

/*
 * SCSI command (cdb_cmd/cdb_bytes[0]) byte definitions.  Only those
 * that are common across all devices are actually defined here.
 * (The SCSI standard defines six groups of devices: direct access,
 * sequential access, printer, processor, WORM direct access, and
 * ROM direct access.  DADs and SADs are basically `disk' and `tape';
 * printers and processors are obvious; and WORMs and ROMs are really
 * just disks that are missing one or two operations.  A few commands
 * are required of all devices; these are defined here, and the others
 * are defined in separate headers.)
 *
 * Letter flags in parentheses in the comment column indicate:
 *	M = mandatory (command is implemented on all SCSI devices)
 *	E = extended (command implemented if SCSI device does extended SCSI)
 *	O = optional
 *	R = reserved to future SCSI standard
 *	V = vendor unique
 *	* = depends on device type
 *
 * Note that SCSI commands are broken into 8 `groups' given by bits 7..5.
 * Group 0 is 6-byte commands, 1 is 10-byte commands, and 5 is 12-byte
 * commands (of which none exist); groups 6 and 7 are vendor unique.
 * Commands are normally just treated as a simple 8-bit number,
 * but the size of the associated cdb is implied by the group.
 */
	/* group 0 */
#define	CMD_TEST_UNIT_READY	0x00	/* (O) test unit ready */
/*				0x01	   (*) */
/*				0x02	   (V) */
#define	CMD_REQUEST_SENSE	0x03	/* (M) request sense */
/*				0x04..0x05 (*) */
/*				0x06	   (V) */
/*				0x07..0x08 (*) */
/*				0x09	   (V) */
/*				0x0a..0x0b (*) */
/*				0x0c..0x0e (V) */
/*				0x0f..0x11 (*) */
#define	CMD_INQUIRY		0x12	/* (E) inquiry */
/*				0x13..0x17 (*) */
#define	CMD_COPY		0x18	/* (O) copy */
/*				0x19..0x1b (*) */
#define	CMD_RECEIVE_DIAG	0x1c	/* (O) receive diagnostic results */
#define	CMD_SEND_DIAG		0x1d	/* (O) send diagnostic */
/*				0x1e	   (*) */
/*				0x1f	   (R) */
	/* group 1 */
/*				0x20..0x24 (V) */
/*				0x25	   (*) */
/*				0x26..0x27 (V) */
/*				0x28	   (*) */
/*				0x29	   (V) */
/*				0x2a..0x2b (*) */
/*				0x2c..0x2d (V) */
/*				0x2e..0x33 (*) */
/*				0x34..0x37 (R) */
#define	CMD_COMPARE		0x38	/* (O) compare */
#define	CMD_COMPARE_VERIFY	0x39	/* (O) compare and verify */
/*				0x3a..0x3f (R) */
	/* group 2 (40-5f) reserved */
	/* group 3 (60-7f) reserved */
	/* group 4 (80-9f) reserved */
	/* group 5 (a0-bf) reserved */
	/* group 6 (c0-df) vendor unique */
	/* group 7 (e0-ff) vendor unique */

/*
 * SCSI control byte.
 * Bits 7 and 6 are vendor unique; bits 5, 4, 3, and 2 are reserved.
 * Bit 1 may be 1 only if bit 0 is 1; if so, it tells the target to
 * send a LINKED COMMAND COMPLETE (WITH FLAG) message.  If not, but
 * bit 0 is set, this tells the target to send a LINKED COMMAND COMPLETE
 * message.
 */
#define	CTRL_VU_MASK		0xc0	/* vendor unique */
#define	CTRL_RSVD		0x3c	/* reserved, must be zero */
#define	CTRL_LCCF		0x02	/* send LCCF if sending LCC */
#define	CTRL_LINK		0x01	/* linked command */

/*
 * Generic sense: regular and extended.
 * A sense operation returned an extended sense iff the error class
 * is 7.  The format is vendor unique unless the error code is 0.
 * The regular and extended formats are completely different; we
 * define macros to obtain values from them.
 */
struct scsi_sense {
	u_char	sn_vcc;		/* valid bit, error class, & error code */
	u_char	sn_var[7];	/* bytes 1-3, or 1-7; variant formats */
	u_char	sn_addl[32-8];	/* additional sense data, if extended */
};

#define	SENSE_ECLASS(sn)	(((sn)->sn_vcc >> 4) & 7)
#define	SENSE_ECODE(sn)		((sn)->sn_vcc & 0xf)
#define	SENSE_ISXSENSE(sn)	(SENSE_ECLASS(sn) == 7)

/* for non-extended sense (`r'egular or `r'estricted sense) */
#define	RSENSE_LVALID(sn)	((sn)->sn_vcc & 0x80)
#define	RSENSE_VU(sn)		((sn)->sn_var[0] >> 5)
#define	RSENSE_LBA(sn) \
  ((((sn)->sn_var[0] & 0x1f) << 16) | ((sn)->sn_var[1] << 8) | (sn)->sn_var[2])

/* for extended sense */
#define	XSENSE_ISSTD(sn)	(SENSE_ECODE(sn) == 0)
			/* if not standard, cannot interpret it at all */
#define	XSENSE_IVALID(sn)	((sn)->sn_vcc & 0x80)
#define	XSENSE_SEG(sn)		((sn)->sn_var[0])
#define	XSENSE_FM(sn)		((sn)->sn_var[1] & 0x80) /* filemark */
#define	XSENSE_EOM(sn)		((sn)->sn_var[1] & 0x40) /* end of media */
#define	XSENSE_ILI(sn)		((sn)->sn_var[1] & 0x20) /* incor length ind */
#define	XSENSE_KEY(sn)		((sn)->sn_var[1] & 0x0f) /* sense key */
#define	XSENSE_INFO(sn) \
	(((sn)->sn_var[2] << 24) | ((sn)->sn_var[3] << 16) | \
	 ((sn)->sn_var[4] << 8) | (sn)->sn_var[5])
#define	XSENSE_ADDL(sn)		((sn)->sn_var[6])	/* add'l sense len */

/*
 * SCSI INQUIRY data: general, and ANSI versions 1 and 2
 * (including common command set).
 */
struct scsi_inquiry {
	u_char	si_type;	/* peripheral device type (below) */
	u_char	si_qual;	/* qualifier (see below) */
	u_char	si_version;	/* version (see below) */
	u_char	si_v2info;	/* scsi version 2 stuff (see below) */
	u_char	si_len;		/* additional length */
	u_char	si_more[252-5];	/* actually si_len bytes */
};
struct scsi_inq_ansi {
	u_char	si_type;	/* peripheral qualifier and device type */
	u_char	si_qual;	/* RMB and device type qualifier */
	u_char	si_version;	/* ISO, ECMA and ANSI-approved versions */
	u_char	si_v2info;	/* ? */
	u_char	si_len;		/* addition length */
	char	si_xxx1[2];	/* reserved */
	char	si_flags;	/* (see below) */
	char	si_vendor[8];	/* vendor (blank padded) */
	char	si_product[16];	/* product (blank padded) */
	char	si_rev[4];	/* revision (blank padded) */

	/* scsi version 2 stuff follows */
	char	si_vend1[20];	/* vendor specific */
	char	si_xxx2[40];	/* reserved */
	char	si_vend2[252-96]; /* vendor specific parameters */
};

/* peripheral device types */
#define TYPE_QUAL_MASK		0xe0	/* peripheral qualifer mask */
#define TYPE_TYPE_MASK		0x1f	/* peripheral device type mask */
#define TYPE_QUAL_NORM		0x00	/* device is normal */
#define TYPE_QUAL_NOTCONN	0x20	/* not connected */
#define TYPE_QUAL_XXX		0x40	/* reserved */
#define TYPE_QUAL_NOLUN		0x60	/* logical unit not supported */
#define TYPE_QUAL_VT4		0x80	/* vendor specific type 4 */
#define TYPE_QUAL_VT5		0xa0	/* vendor specific type 5 */
#define TYPE_QUAL_VT6		0xc0	/* vendor specific type 6 */
#define TYPE_QUAL_VT7		0xe0	/* vendor specific type 7 */

#define	TYPE_DAD		0x00	/* direct access device (disk) */
#define	TYPE_SAD		0x01	/* sequential access device (tape) */
#define	TYPE_PRINTER		0x02	/* printer */
#define	TYPE_PROCESSOR		0x03	/* processor */
#define	TYPE_WORM		0x04	/* WORM disk */
#define	TYPE_ROM		0x05	/* CD-ROM disk */
#define	TYPE_SCANNER		0x06	/* scanner */
#define	TYPE_MO			0x07	/* magneto-optical */
#define	TYPE_JUKEBOX		0x08	/* medium changer */
#define	TYPE_LAN		0x09	/* communications device */
#define	TYPE_NP			0x1f	/* unknown or no device */

/* qualifiers */
#define	QUAL_RMB		0x80	/* removable medium bit */
#define	QUAL_MASK		0x7f	/* mask for `user' bits */

/* version (shifts and masks for subfields) */
#define	VER_ISO_SHIFT		6	/* ISO version: top 2 bits */
#define	VER_ISO_MASK		3
#define	VER_ECMA_SHIFT		3	/* ECMA version: middle 3 bits */
#define	VER_ECMA_MASK		7
#define	VER_ANSI_SHIFT		0	/* ANSI version: bottom 3 bits */
#define	VER_ANSI_MASK		7

/* v2 info */
#define V2INFO_AENC		0x80	/* device can accept AEN data */
#define V2INFO_TRMIOP		0x40	/* supports TERMINATE I/O PROC msg */
#define V2INFO_XXX		0x30	/* reserved */
#define V2INFO_RDF_MASK		0x0f	/* response data format mask */
#define V2INFO_RDF_SCSI1	0x00	/* SCSI-1 standard INQUIRY data */
#define V2INFO_RDF_CCS		0x01	/* common command set INQUIRY data */
#define V2INFO_RDF_SCSI2	0x02	/* SCSI-2 standard INQUIRY data */

/* flags */
#define V2FLAG_RELADR		0x80	/* supports relative addressing */
#define V2FLAG_WBUS32		0x40	/* supports 32 bit data xfer */
#define V2FLAG_WBUS16		0x20	/* supports 32 bit data xfer */
#define V2FLAG_SYNC		0x10	/* supports synchronous data xfer */
#define V2FLAG_LINKED		0x08	/* supports linked commands */
#define V2FLAG_XXX		0x04	/* reserved */
#define V2FLAG_CMDQUE		0x02	/* supports tagged command queueing */
#define V2FLAG_SOFTRESET	0x01	/* RST causes soft reset */

/*
 * SCSI message codes bytes.  The `command complete' code is required;
 * all others are optional.  `Identify' is a flag bit, not a code (thus
 * codes are actually at most 7 bits).
 */
#define	MSG_IDENTIFY		0x80	/* flag => this is an identify msg */
#define	MSG_IDENTIFY_DR		0x40	/* IDENTIFY: flag => discon/resel ok */
#define	MSG_IDENTIFY_RSVD	0x38	/* IDENTIFY: these bits are reserved */
#define	MSG_IDENTIFY_LUN	0x07	/* IDENTIFY: these bits give LUN */

#define	MSG_CMD_COMPLETE	0x00	/* command complete */
#define	MSG_EXT_MESSAGE		0x01	/* extended message */
#define	MSG_SAVE_DATA_PTR	0x02	/* save data pointer */
#define	MSG_RESTORE_PTR		0x03	/* restore pointers */
#define	MSG_DISCONNECT		0x04	/* disconnect */
#define	MSG_INIT_DETECT_ERROR	0x05	/* initiator detected error */
#define	MSG_ABORT		0x06	/* abort */
#define	MSG_REJECT		0x07	/* message reject */
#define	MSG_NOOP		0x08	/* no operation */
#define	MSG_PARITY_ERROR	0x09	/* message parity error */
#define	MSG_LCC			0x0a	/* linked command complete */
#define	MSG_LCCF		0x0b	/* linked command complete (w/ flag) */
#define	MSG_BUS_DEVICE_RESET	0x0c	/* bus device reset */
#define	MSG_ABORT_TAG		0x0d	/* abort tagged msg */
#define	MSG_CLEAR_QUEUE		0x0e	/* clear queue */
#define	MSG_INITIATE_RECOVERY	0x0f	/* initiate recovery */
#define	MSG_RELEASE_RECOVERY	0x10	/* release recovery */
#define	MSG_TERMINATE_PROCESS	0x11	/* ? */
#define	MSG_SIMPLE_Q_TAG	0x20	/* ? */
#define	MSG_HEAD_Q_TAG		0x21	/* ? */
#define	MSG_ORDERED_Q_TAG	0x22	/* ? */
#define	MSG_IGNORE_WIDE_RESID	0x23	/* ? */

/*
 * SCSI extended message format.
 */
struct scsi_xmsg {
	u_char	xm_xmsg,	/* value 1, i.e., SMSG_EXT_MESSAGE */
		xm_len,		/* length of this extended message */
		xm_code,	/* actual code */
		xm_args[253];	/* actualy xm_len-1 bytes */
};

/*
 * SCSI extended message codes.
 */
#define	XMSG_MDP		0x00	/* modify data pointer */
#define	XMSG_SDTR		0x01	/* synchronous data transfer request */
#define	XMSG_XID		0x02	/* extended identify */

/*
 * SCSI status byte values.  Bits 6, 5, and 0 are Vendor Unique.
 */
#define	STS_EXT			0x80	/* flag => extended status valid */
#define	STS_MASK		0x1e	/* mask for non-VU bits */
#define	STS_VU			0x61	/* mask for Vendor Unique bits */

#define	STS_GOOD		0x00	/* success, command done */
#define	STS_CHECKCOND		0x02	/* check condition (do a REQ SENSE) */
#define	STS_CONDMET		0x04	/* condition met (search succeeded) */
			/*	0x06	   reserved */
#define	STS_BUSY		0x08	/* busy */
			/*	0x0a	   reserved */
			/*	0x0c	   reserved */
			/*	0x0e	   reserved */
#define	STS_INTERMED		0x10	/* succeeded, doing linked cmd */
			/*	0x12	   reserved */
#define	STS_INTERMED_CONDMET	0x14	/* condition met, doing linked cmd */
			/*	0x16	   reserved */
#define	STS_RESERV_CONFLICT	0x18	/* reservation conflict */
			/*	0x1a	   reserved */
			/*	0x1c	   reserved */
			/*	0x1e	   reserved */
