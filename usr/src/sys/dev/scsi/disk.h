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
 *	@(#)disk.h	5.2 (Berkeley) %G%
 *
 * from: $Header: disk.h,v 1.2 92/05/15 11:23:58 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Direct Access Devices (disks).
 * This includes WORMs and CD-ROMs (although a few commands, such as
 * format or write, are nonsensical on some).
 *
 * Commands defined in the common header (scsi.h) appear here as comments.
 */

	/* group 0 */
/*	CMD_TEST_UNIT_READY	0x00	   test unit ready */
#define	CMD_REZERO		0x01	/* rezero unit */
/*	CMD_REQUEST_SENSE	0x03	/* request sense */
#define	CMD_FORMAT_UNIT		0x04	/* format unit (disk) */
#define	CMD_REASSIGN_BLOCKS	0x07	/* reassign blocks (disk, WORM) */
#define	CMD_READ6		0x08	/* read (6 byte cdb) */
#define	CMD_WRITE6		0x0a	/* write (6 byte cdb) */
#define	CMD_SEEK6		0x0b	/* seek (6 byte cdb) */
/*	CMD_INQUIRY		0x12	   inquiry */
#define	CMD_MODE_SELECT		0x15	/* mode select */
#define	CMD_RESERVE		0x16	/* reserve */
#define	CMD_RELEASE		0x17	/* release */
/*	CMD_COPY		0x18	   copy */
#define	CMD_MODE_SENSE		0x1a	/* mode sense */
#define	CMD_SSU			0x1b	/* start/stop unit */
/*	CMD_RECEIVE_DIAG	0x1c	   receive diagnostic results */
/*	CMD_SEND_DIAG		0x1d	   send diagnostic */
#define	CMD_PAMR		0x1e	/* prevent/allow medium removal */

	/* group 1 */
#define	CMD_READ_CAPACITY	0x25	/* read capacity */
#define	CMD_READ10		0x28	/* read (10 byte cdb) */
#define	CMD_WRITE10		0x2a	/* write (10 byte cdb) */
#define	CMD_SEEK10		0x2b	/* write (10 byte cdb) */
#define	CMD_WRITE_VERIFY	0x2e	/* write and verify */
#define	CMD_VERIFY		0x2f	/* verify */
#define	CMD_SEARCH_H		0x30	/* search data high */
#define	CMD_SEARCH_E		0x31	/* search data equal */
#define	CMD_SEARCH_L		0x32	/* search data low */
#define	CMD_SET_LIMITS		0x33	/* set limits */
/*	CMD_COMPARE		0x39	   compare */
#define	CMD_COPY_VERIFY		0x3a	/* copy and verify */

/* this one is in van's but not in my 17B documents */
#define	CMD_READ_DEFECT_DATA	0x37	/* read defect data */ /* ??? */

/*
 * Structure of a FORMAT UNIT command (i.e., the cdb):
 *	byte 0: opcode<8>
 *	byte 1: lun<3> format_data<1> complete_list<1> defect_list_format<3>
 *	byte 2: vendor unique
 *	byte 3: interleave (MSB)
 *	byte 4: interleave (LSB)
 *	byte 5: control
 */
struct scsi_cdb_fu {
	u_char	cdb_cmd;		/* SCSI_CMD_FU */
	u_char	cdb_lun_etc;		/* lun+FD+CL+DLF */
	u_char	cdb_vu;			/* vendor unique */
	u_char	cdb_ilvh;		/* interleave (MSB) */
	u_char	cdb_ilvl;		/* interleave (LSB) */
	u_char	cdb_ctrl;		/* control byte */
};

/*
 * If format data are supplied, they give either additional (cl=0) or
 * new (cl=1) defect list in one of the following formats.
 * Formats 1, 2, and 3 are the same as 0; formats 6 and 7 are
 * vendor unique and reserved, respectively.  (The `backwards'
 * in `backwards compatible'...)
 */
#define	SCSI_DLF_BLOCK	0		/* dlf = blocks */
#define	SCSI_DLF_BFI	4		/* dlf = bytes from index */
#define	SCSI_DLF_PS	5		/* dlf = physical sectors */

/*
 * Defect list header, block format (`defect block address').
 *
 * N.B.: this structure is also used for the Reassign Blocks command;
 * there the `defect block address' becomes a `defect logical block address'.
 */
struct scsi_dlf_dba {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_dba_desc {
		u_char	dbah,		/* defect block address (MSB) */
			dbahm,		/* defect block address */
			dbalm,		/* defect block address */
			dbal;		/* defect block address (LSB) */
	} dlf_dba[1];			/* actually longer */
};

/*
 * Defect list header, Bytes From Index format.
 */
struct scsi_dlf_bfi {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_bfi_desc {
		u_char	cylh,		/* cylinder number of defect (MSB) */
			cylm,		/* cylinder number of defect */
			cyll,		/* cylinder number of defect (LSB) */
			head,		/* head number of defect */
			bfih,		/* defect bytes from index (MSB) */
			bfihm,		/* defect bytes from index */
			bfilm,		/* defect bytes from index */
			bfil;		/* defect bytes from index (LSB) */
	} dlf_bfi[1];			/* actually longer */
};

/*
 * Defect list header, Physical Sector format.
 */
struct scsi_dlf_ps {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_ps_desc {
		u_char	cylh,		/* cylinder number of defect (MSB) */
			cylm,		/* cylinder number of defect */
			cyll,		/* cylinder number of defect (LSB) */
			head,		/* head number of defect */
			dsnh,		/* defect sector number (MSB) */
			dsnhm,		/* defect sector number */
			dsnlm,		/* defect sector number */
			dsnl;		/* defect sector number (LSB) */
	} dlf_ps[1];			/* actually longer */
};

/*
 * Structure of data passed via a MODE SELECT command.
 */
struct scsi_ms {
	u_char	ms_xxx0,		/* reserved */
		ms_mt,			/* medium type */
		ms_xxx1,		/* reserved */
		ms_bdl;			/* block descriptor length */
	struct scsi_ms_b_desc {
		u_char	dc,		/* density code */
			nbh,		/* number of blocks (MSB) */
			nbm,		/* number of blocks */
			nbl,		/* number of blocks (LSB) */
			xxx,		/* reserved */
			blh,		/* block length (MSB) */
			blm,		/* block length */
			bll;		/* block length (LSB) */
	} ms_bd[1];			/* actually longer */
	/* followed by vendor unique bytes */
};

/* values for the Medium Type field - disks */
#define	SCSI_CMD_MS_MT_DEFAULT	0x00	/* whatever is current */
#define	SCSI_CMD_MS_MT_SS	0x01	/* single sided, unspecified medium */
#define	SCSI_CMD_MS_MT_DS	0x02	/* double sided, unspecified medium */
#define	SCSI_CMD_MS_MT_8SSSD	0x05	/* 8" floppy, SSSD (X3.73-1980) */
#define	SCSI_CMD_MS_MT_8DSSD	0x06	/* 8" floppy, DSSD (X3B8-140) */
#define	SCSI_CMD_MS_MT_8SSDD	0x09	/* 8" floppy, SSDD (X3B8/78-139) */
#define	SCSI_CMD_MS_MT_8DSDD	0x0a	/* 8" floppy, DSDD (X3.121-1984) */
#define	SCSI_CMD_MS_MT_5SSSD	0x0d	/* 5.25" floppy, SSSD (X3.82-1980) */
#define	SCSI_CMD_MS_MT_5DSDD	0x12	/* 5.25" floppy, DSDD (X3.125-1984) */
#define	SCSI_CMD_MS_MT_5DSDD96	0x16	/* 5.25", DSDD, 96tpi (X3.126-198X) */
#define	SCSI_CMD_MS_MT_5DSQD	0x1a	/* 5.25", DSQD, 96tpi (DIS 8630) */
#define	SCSI_CMD_MS_MT_3DS	0x1e	/* 3.5", double sided (X3.137-198X) */

/* values for the Medium Type field - tapes */
#define	SCSI_CMD_MS_MT_QIC_12T	0x40	/* 0.25", 12 tracks */
#define	SCSI_CMD_MS_MT_QIC_24T	0x44	/* 0.25", 24 tracks */

/*
 * Bits in cdb_lenl for a READ CAPACITY command,
 * and structure returned as data.
 *
 * If PMI is off, the lba in the cdb must be 0.
 */
#define	SCSI_CMD_RC_PMI		0x01	/* Partial Medium Indicator */

struct scsi_rc {
	u_char	rc_lbah;		/* logical block address (MSB) */
	u_char	rc_lbahm;		/* logical block address */
	u_char	rc_lbalm;		/* logical block address */
	u_char	rc_lbal;		/* logical block address (LSB) */
	u_char	rc_blh;			/* block length (MSB) */
	u_char	rc_blhm;		/* block length */
	u_char	rc_bllm;		/* block length */
	u_char	rc_bll;			/* block length (LSB) */
};
