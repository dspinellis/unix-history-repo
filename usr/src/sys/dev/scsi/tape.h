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
 *	@(#)tape.h	5.2 (Berkeley) %G%
 *
 * from: $Header: tape.h,v 1.2 92/05/15 11:24:06 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Sequential Access Devices (tapes).
 */
#define	CMD_REWIND		0x01	/* rewind */
#define	CMD_READ_BLOCK_LIMITS	0x05	/* read block limits */
#define	CMD_READ		0x08	/* read */
#define	CMD_WRITE		0x0a	/* write */
#define	CMD_TRACK_SELECT	0x0b	/* track select */
#define	CMD_READ_REVERSE	0x0f	/* read reverse */
#define	CMD_WRITE_FILEMARK	0x10	/* write file marks */
#define	CMD_SPACE		0x11	/* space */
#define	CMD_VERIFY		0x13	/* verify */
#define	CMD_RBD			0x14	/* recover buffered data */
#define	CMD_MODE_SELECT		0x15	/* mode select */
#define	CMD_RESERVE_UNIT	0x16	/* reserve unit */
#define	CMD_RELEASE_UNIT	0x17	/* release unit */
/*	CMD_COPY		0x18	   copy (common to all scsi devs) */
#define	CMD_ERASE		0x19	/* erase */
#define	CMD_MODE_SENSE		0x1a	/* mode sense */
#define	CMD_LOAD_UNLOAD		0x1b	/* load/unload */
#define	CMD_PAMR		0x1e	/* prevent/allow medium removal */

/*
 * Structure of READ, WRITE, READ REVERSE, RECOVER BUFFERED DATA
 * commands (i.e., the cdb).
 * Also used for VERIFY commands.
 */
struct scsi_cdb_rw {
	u_char	cdb_cmd,	/* 0x08 or 0x0a or 0x0f or 0x13 or 0x14 */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx:3,	/* reserved */
		cdb_bytecmp:1,	/* byte-by-byte comparison (VERIFY only) */
		cdb_fixed:1,	/* fixed length blocks */
		cdb_lenh,	/* transfer length (MSB) */
		cdb_lenm,	/* transfer length */
		cdb_lenl,	/* transfer length (LSB) */
		cdb_ctrl;	/* control byte */
};

/*
 * Structure of a TRACK SELECT command.
 */
struct scsi_cdb_ts {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:5,	/* reserved */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_track,	/* track value */
		cdb_ctrl;	/* control byte */
};

/*
 * Structure of a WRITE FILEMARKS command.
 */
struct scsi_cdb_wfm {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:5,	/* reserved */
		cdb_nfh,	/* number of filemarks (MSB) */
		cdb_nfm,	/* number of filemarks */
		cdb_nfl,	/* number of filemarks (LSB) */
		cdb_ctrl;	/* control byte */
};

/*
 * Structure of a SPACE command.
 */
struct scsi_cdb_space {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:3,	/* reserved */
		cdb_code:2,	/* code (see below) */
		cdb_counth,	/* count (MSB) */
		cdb_countm,	/* count */
		cdb_countl,	/* count (LSB) */
		cdb_ctrl;	/* control byte */
};
#define	SCSI_CMD_SPACE_BLOCKS	0	/* skip blocks */
#define	SCSI_CMD_SPACE_FMS	1	/* skip file marks */
#define	SCSI_CMD_SPACE_SFMS	2	/* skip sequential file marks */
#define	SCSI_CMD_SPACE_PEOD	3	/* skip to physical end of data */

/*
 * Mode Select parameters (data).
 */
struct scsi_msel {
	u_short	msel_xxx0;	/* reserved */
	u_char	msel_xxx1:1,	/* reserved */
		msel_bm:3,	/* buffered mode */
		msel_speed:4,	/* speed */
		msel_bdl;	/* block descriptor length */
	struct scsi_msel_bdesc {
		u_char	dc,	/* density code */
			nbh,	/* number of blocks (MSB) */
			nbm,	/* number of blocks */
			nbl,	/* number of blocks (LSB) */
			xxx,	/* reserved */
			blh,	/* block length (MSB) */
			blm,	/* block length */
			bll;	/* block length (LSB) */
	} msel_bd[1];		/* actually longer */
	/* followed by Vendor Unique bytes */
};

/* buffered mode and speed */
#define	SCSI_MSEL_BM_UNBUFFERED	0	/* unbuffered writes */
#define	SCSI_MSEL_BM_BUFFERED	1	/* buffered writes allowed */
#define	SCSI_MSEL_SPEED_DEFAULT	0	/* use device default speed */
#define	SCSI_MSEL_SPEED_LOW	1	/* use lowest speed */
#define	SCSI_MSEL_SPEED_HIGH	15	/* use highest speed */

/* density codes */
#define	SCSI_MSEL_DC_DEFAULT	0	/* use device default density */
#define	SCSI_MSEL_DC_9T_800BPI	1	/* 9 track, 800 bpi */
#define	SCSI_MSEL_DC_9T_1600BPI	2	/* 9 track, 1600 bpi */
#define	SCSI_MSEL_DC_9T_6250BPI	3	/* 9 track, 6250 bpi */
#define	SCSI_MSEL_DC_QIC_XX1	4	/* QIC-11? 4 or 9 track, 8000 bpi */
#define	SCSI_MSEL_DC_QIC_XX2	5	/* QIC-11? 4 or 9 track, 8000 bpi */
#define	SCSI_MSEL_DC_9T_3200BPI	6	/* 9 track, 3200 bpi */
#define	SCSI_MSEL_DC_QIC_XX3	7	/* QIC, 4 track, 6400 bpi */
#define	SCSI_MSEL_DC_CS_XX4	8	/* cassette 4 track, 8000 bpi 8/
#define	SCSI_MSEL_DC_HIC_XX5	9	/* half inch cartridge, 18 track */
#define	SCSI_MSEL_DC_HIC_XX6	10	/* HIC, 22 track, 6667 bpi */
#define	SCSI_MSEL_DC_QIC_XX7	11	/* QIC, 4 track, 1600 bpi */
#define	SCSI_MSEL_DC_HIC_XX8	12	/* HIC, 24 track, 12690 bpi */
#define	SCSI_MSEL_DC_HIC_XX9	13	/* HIC, 24 track, 25380 bpi */

/*
 * Structure of an ERASE command.
 */
struct scsi_cdb_erase {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:4,	/* reserved */
		cdb_long:1,	/* long erase */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_xxx3,	/* reserved */
		cdb_ctrl;	/* control byte */
};

/*
 * Structure of a LOAD/UNLOAD command.
 */
struct scsi_cdb_lu {
	u_char	cdb_cmd,	/* 0x1b */
		cdb_lun:3,	/* logical unit number */
		cdb_xxx0:4,	/* reserved */
		cdb_immed:1,	/* return status immediately */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_xxx3:6,	/* reserved */
		cdb_reten:1,	/* retension tape */
		cdb_load:1,	/* load (else unload) */
		cdb_ctrl;	/* control byte */
};
