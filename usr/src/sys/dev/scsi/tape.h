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
 *	@(#)tape.h	5.3 (Berkeley) %G%
 *
 * from: $Header: tape.h,v 1.3 92/12/02 03:53:14 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Sequential Access Devices (tapes).
 *
 * Commands defined in common headers (scsi.h or disktape.h) are omitted.
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
#define	CMD_RESERVE_UNIT	0x16	/* reserve unit */
#define	CMD_RELEASE_UNIT	0x17	/* release unit */
/*	CMD_COPY		0x18	   copy (common to all scsi devs) */
#define	CMD_ERASE		0x19	/* erase */
#define	CMD_LOAD_UNLOAD		0x1b	/* load/unload */
#define	CMD_PAMR		0x1e	/* prevent/allow medium removal */

/*
 * Structure of READ, WRITE, READ REVERSE, RECOVER BUFFERED DATA
 * commands (i.e., the cdb).
 * Also used for VERIFY commands.
 */
struct scsi_cdb_rw {
	u_char	cdb_cmd,	/* 0x08 or 0x0a or 0x0f or 0x13 or 0x14 */
		cdb_lun_bf,	/* LUN + reserved + bytecmp + fixed */
		cdb_lenh,	/* transfer length (MSB) */
		cdb_lenm,	/* transfer length */
		cdb_lenl,	/* transfer length (LSB) */
		cdb_ctrl;	/* control byte */
};
#define	SCSI_RW_BYTECMP		0x02	/* byte compare flag if verify */
#define	SCSI_RW_FIXEDBLK	0x01	/* fixed block size for read/write */

/*
 * Structure of a TRACK SELECT command.
 */
struct scsi_cdb_ts {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun_xxx,	/* logical unit number + reserved */
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
		cdb_lun_xxx,	/* logical unit number + reserved */
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
		cdb_lun_code,	/* LUN + reserved + 2-bit code */
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
 * Structure of an ERASE command.
 */
struct scsi_cdb_erase {
	u_char	cdb_cmd,	/* 0x0b */
		cdb_lun_long,	/* LUN + reserved + long-erase flag */
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
		cdb_lun_immed,	/* LUN + reserved + immediate flag */
		cdb_xxx1,	/* reserved */
		cdb_xxx2,	/* reserved */
		cdb_rl,		/* reserved + retension flag + load flag */
		cdb_ctrl;	/* control byte */
};
#define	SCSI_LU_RL_RETEN	0x02	/* retension */
#define	SCSI_LU_RL_LOAD		0x01	/* load */
#define	SCSI_LU_RL_UNLOAD	0x00	/* unload (pseudo flag) */
