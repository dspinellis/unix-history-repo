/*
 * Copyright (c) 1990, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)scsireg.h	8.1 (Berkeley) 6/10/93
 */

/*
 * MB89352 SCSI Protocol Controller Hardware Description.
 */

struct scsidevice {
	u_char	scsi_bdid, p0, p1, p2;		/* 000 */
	u_char	scsi_sctl, p3, p4, p5;		/* 004 */
#define			SCTL_DISABLE	0x80
#define			SCTL_CTRLRST	0x40
#define			SCTL_DIAG	0x20
#define			SCTL_ABRT_ENAB	0x10
#define			SCTL_PARITY_ENAB 0x08
#define			SCTL_SEL_ENAB	0x04
#define			SCTL_RESEL_ENAB	0x02
#define			SCTL_INTR_ENAB	0x01
	u_char	scsi_scmd, p6, p7, p8;		/* 008 */
#define			SCMD_RST	0x10
#define			SCMD_ICPT_XFR	0x08
#define			SCMD_PROG_XFR	0x04
#define			SCMD_PAD	0x01	/* if initiator */
#define			SCMD_PERR_STOP	0x01	/* if target */
			/* command codes */
#define			SCMD_BUS_REL	0x00
#define			SCMD_SELECT	0x20
#define			SCMD_RST_ATN	0x40
#define			SCMD_SET_ATN	0x60
#define			SCMD_XFR	0x80
#define			SCMD_XFR_PAUSE	0xa0
#define			SCMD_RST_ACK	0xc0
#define			SCMD_SET_ACK	0xe0
	u_char	scsi_tmod, p9, p10, p11;	/* 00C */
#define			TMOD_SYNC	0x80
	u_char	scsi_ints, p12, p13, p14;	/* 010 */
#define			INTS_SEL	0x80
#define			INTS_RESEL	0x40
#define			INTS_DISCON	0x20
#define			INTS_CMD_DONE	0x10
#define			INTS_SRV_REQ	0x08
#define			INTS_TIMEOUT	0x04
#define			INTS_HARD_ERR	0x02
#define			INTS_RST	0x01
	u_char	scsi_psns, p15, p16, p17;	/* 014 */
#define			PSNS_REQ	0x80
#define			PSNS_ACK	0x40
#define			PSNS_ATN	0x20
#define			PSNS_SEL	0x10
#define			PSNS_BSY	0x08
#define		scsi_sdgc scsi_psns
#define			SDGC_XFER_ENAB	0x20
	u_char	scsi_ssts, p18, p19, p20;	/* 018 */
#define			SSTS_INITIATOR	0x80
#define			SSTS_TARGET	0x40
#define			SSTS_BUSY	0x20
#define			SSTS_XFR	0x10
#define			SSTS_ACTIVE	(SSTS_INITIATOR|SSTS_XFR)
#define			SSTS_RST	0x08
#define			SSTS_TCZERO	0x04
#define			SSTS_DREG_FULL	0x02
#define			SSTS_DREG_EMPTY	0x01
	u_char	scsi_serr, p21, p22, p23;	/* 01C */
#define			SERR_SCSI_PAR	0x80
#define			SERR_SPC_PAR	0x40
#define			SERR_XFER_OUT	0x20
#define			SERR_TC_PAR	0x08
#define			SERR_PHASE_ERR	0x04
#define			SERR_SHORT_XFR	0x02
#define			SERR_OFFSET	0x01
	u_char	scsi_pctl, p24, p25, p26;	/* 020 */
#define			PCTL_BFINT_ENAB	0x80
	u_char	scsi_mbc,  p27, p28, p29;	/* 024 */
	u_char	scsi_dreg, p30, p31, p32;	/* 028 */
	u_char	scsi_temp, p33, p34, p35;	/* 02C */
	u_char	scsi_tch,  p36, p37, p38;	/* 030 */
	u_char	scsi_tcm,  p39, p40, p41;	/* 034 */
	u_char	scsi_tcl,  p42, p43, p44;	/* 038 */
	u_char	scsi_exbf, p45, p46, p47;	/* 03C */
};

/* psns/pctl phase lines as bits */
#define	PHASE_MSG	0x04
#define	PHASE_CD	0x02		/* =1 if 'command' */
#define	PHASE_IO	0x01		/* =1 if data inbound */
/* Phase lines as values */
#define	PHASE		0x07		/* mask for psns/pctl phase */
#define	DATA_OUT_PHASE	0x00
#define	DATA_IN_PHASE	0x01
#define	CMD_PHASE	0x02
#define	STATUS_PHASE	0x03
#define	BUS_FREE_PHASE	0x04
#define	ARB_SEL_PHASE	0x05	/* Fuji chip combines arbitration with sel. */
#define	MESG_OUT_PHASE	0x06
#define	MESG_IN_PHASE	0x07

/* SCSI Messages */

#define	MSG_CMD_COMPLETE	0x00
#define MSG_EXT_MESSAGE		0x01
#define	MSG_SAVE_DATA_PTR	0x02
#define	MSG_RESTORE_PTR		0x03
#define	MSG_DISCONNECT		0x04
#define	MSG_INIT_DETECT_ERROR	0x05
#define	MSG_ABORT		0x06
#define	MSG_REJECT		0x07
#define	MSG_NOOP		0x08
#define	MSG_PARITY_ERROR	0x09
#define	MSG_BUS_DEVICE_RESET	0x0C
#define	MSG_IDENTIFY		0x80
#define	MSG_IDENTIFY_DR		0xc0	/* (disconnect/reconnect allowed) */
#define	MSG_SYNC_REQ 		0x01

/* SCSI Commands */

#define CMD_TEST_UNIT_READY	0x00
#define CMD_REQUEST_SENSE	0x03
#define	CMD_INQUIRY		0x12
#define CMD_SEND_DIAGNOSTIC	0x1D

#define CMD_REWIND		0x01
#define CMD_FORMAT_UNIT		0x04
#define CMD_READ_BLOCK_LIMITS	0x05
#define CMD_REASSIGN_BLOCKS	0x07
#define CMD_READ		0x08
#define CMD_WRITE		0x0A
#define CMD_WRITE_FILEMARK	0x10
#define CMD_SPACE		0x11
#define CMD_MODE_SELECT		0x15
#define CMD_RELEASE_UNIT	0x17
#define CMD_ERASE		0x19
#define CMD_MODE_SENSE		0x1A
#define CMD_LOADUNLOAD		0x1B
#define CMD_RECEIVE_DIAG	0x1C
#define CMD_SEND_DIAG		0x1D
#define CMD_P_A_MEDIA_REMOVAL	0x1E
#define CMD_READ_CAPACITY	0x25
#define CMD_READ_EXT		0x28
#define CMD_WRITE_EXT		0x2A
#define CMD_READ_DEFECT_DATA	0x37
#define		SD_MANUFAC_DEFECTS	0x14000000
#define		SD_GROWN_DEFECTS	0x0c000000
#define CMD_READ_BUFFER		0x3B
#define CMD_WRITE_BUFFER	0x3C
#define CMD_READ_FULL		0xF0
#define CMD_MEDIA_TEST		0xF1
#define CMD_ACCESS_LOG		0xF2
#define CMD_WRITE_FULL		0xFC
#define CMD_MANAGE_PRIMARY	0xFD
#define CMD_EXECUTE_DATA	0xFE

/* SCSI status bits */

#define	STS_CHECKCOND	0x02	/* Check Condition (ie., read sense) */
#define	STS_CONDMET	0x04	/* Condition Met (ie., search worked) */
#define	STS_BUSY	0x08
#define	STS_INTERMED	0x10	/* Intermediate status sent */
#define	STS_EXT		0x80	/* Extended status valid */

/* command descriptor blocks */

struct scsi_cdb6 {
	u_char	cmd;		/* command code */
	u_char	lun:  3,	/* logical unit on ctlr */
		lbah: 5;	/* msb of read/write logical block addr */
	u_char	lbam;		/* middle byte of l.b.a. */
	u_char	lbal;		/* lsb of l.b.a. */
	u_char	len;		/* transfer length */
	u_char	xtra;
};

struct scsi_cdb10 {
	u_char	cmd;		/* command code */
	u_char	lun: 3,		/* logical unit on ctlr */
		   : 4,
		rel: 1;		/* l.b.a. is relative addr if =1 */
	u_char	lbah;		/* msb of read/write logical block addr */
	u_char	lbahm;		/* high middle byte of l.b.a. */
	u_char	lbalm;		/* low middle byte of l.b.a. */
	u_char	lbal;		/* lsb of l.b.a. */
	u_char	reserved;
	u_char	lenh;		/* msb transfer length */
	u_char	lenl;		/* lsb transfer length */
	u_char	xtra;
};

/* basic sense data */

struct scsi_sense {
	u_char	valid: 1,	/* l.b.a. is valid */
		class: 3,
		code:  4;
	u_char	vu:    4,	/* vendor unique */
		lbah:  4;
	u_char	lbam;
	u_char	lbal;
};

struct scsi_xsense {
	u_char	valid: 1,	/* l.b.a. is valid */
		class: 3,
		code:  4;
	u_char	segment;
	u_char	filemark: 1,
		eom:      1,
		ili:      1,	/* illegal length indicator */
		rsvd:	  1,
		key:	  4;
	u_char	info1;
	u_char	info2;
	u_char	info3;
	u_char	info4;
	u_char	len;		/* additional sense length */
};

/* inquiry data */
struct scsi_inquiry {
	u_char	type;
	u_char	qual;
	u_char	version;
	u_char	rsvd;
	u_char	len;
	char	class[3];
	char	vendor_id[8];
	char	product_id[16];
	char	rev[4];
};

struct scsi_format_parms {		/* physical BFI format */
	u_short	reserved;
	u_short	list_len;
	struct defect {
		unsigned cyl  : 24;
		unsigned head : 8;
		long	bytes_from_index;
	} defect[127];
} format_parms;

struct scsi_reassign_parms {
	u_short	reserved;
	u_short	list_len;	/* length in bytes of defects only */
	struct new_defect {
		unsigned lba;	/* logical block address */
	} new_defect[2];
} reassign_parms;

struct scsi_modesel_hdr {
	u_char	rsvd1;
	u_char	media_type;
	u_char 	rsvd2;
	u_char	block_desc_len;
	u_int	density		: 8;
	u_int	number_blocks	:24;
	u_int	rsvd3		: 8;
	u_int	block_length	:24;
}; 

struct scsi_modesense_hdr {
	u_char	len;
	u_char	media_type;
	u_char 	wp    : 1;
	u_char 	rsvd1 : 7;
	u_char	block_desc_len;
	u_int	density		: 8;
	u_int	number_blocks	:24;
	u_int	rsvd2		: 8;
	u_int	block_length	:24;
}; 

/*
 * Mode Select / Mode sense "pages"
 */

/*
 * Page One - Error Recovery Parameters 
 */
struct scsi_err_recovery {
	u_char	page_savable	: 1;	/* save parameters */
	u_char	reserved	: 1;
	u_char	page_code	: 6;	/* = 0x01 */
	u_char	page_length;		/* = 6 */
	u_char	awre		: 1;	/* auto write realloc enabled */
	u_char	arre		: 1;	/* auto read realloc enabled */
	u_char	tb		: 1;	/* transfer block */
	u_char 	rc		: 1;	/* read continuous */
	u_char	eec		: 1;	/* enable early correction */
	u_char	per		: 1;	/* post error */
	u_char	dte		: 1;	/* disable transfer on error */
	u_char	dcr		: 1;	/* disable correction */
	u_char	retry_count;
	u_char	correction_span;
	u_char	head_offset_count;
	u_char	strobe_offset_count;
	u_char	recovery_time_limit;
};

/*
 * Page Two - Disconnect / Reconnect Control Parameters
 */
struct scsi_disco_reco {
	u_char	page_savable	: 1;	/* save parameters */
	u_char	rsvd		: 1;
	u_char	page_code	: 6;	/* = 0x02 */
	u_char	page_length;		/* = 10 */
	u_char	buffer_full_ratio;	/* write, how full before reconnect? */
	u_char	buffer_empty_ratio;	/* read, how full before reconnect? */

	u_short	bus_inactivity_limit;	/* how much bus time for busy */
	u_short	disconnect_time_limit;	/* min to remain disconnected */
	u_short	connect_time_limit;	/* min to remain connected */
	u_short	reserved_1;
};

/*
 * Page Three - Direct Access Device Format Parameters
 */
struct scsi_format {
	u_char	page_savable	: 1;	/* save parameters */
	u_char	rsvd		: 1;
	u_char	page_code	: 6;	/* = 0x03 */
	u_char	page_length;		/* = 22 */
	u_short	tracks_per_zone;	/*  Handling of Defects Fields */
	u_short	alt_sect_zone;
	u_short alt_tracks_zone;
	u_short	alt_tracks_vol;
	u_short	sect_track;		/* Track Format Field */
	u_short data_sect;		/* Sector Format Fields */
	u_short	interleave;
	u_short	track_skew_factor;
	u_short	cyl_skew_factor;
	u_char	ssec		: 1;	/* Drive Type Field */
	u_char	hsec		: 1;
	u_char	rmb		: 1;
	u_char	surf		: 1;
	u_char	ins		: 1;
	u_char	reserved_1	: 3;
	u_char	reserved_2;
	u_char	reserved_3;
	u_char	reserved_4;
};

/*
 * Page Four - Rigid Disk Drive Geometry Parameters 
 */
struct scsi_geometry {
	u_char	page_savable	: 1;	/* save parameters */
	u_char	rsvd		: 1;
	u_char	page_code	: 6;	/* = 0x04 */
	u_char	page_length;		/* = 18 */
	u_char	cyl_ub;			/* number of cylinders */
	u_char	cyl_mb;
	u_char	cyl_lb;
	u_char	heads;			/* number of heads */
	u_char	precomp_cyl_ub;		/* cylinder to start precomp */
	u_char	precomp_cyl_mb;
	u_char	precomp_cyl_lb;
	u_char	current_cyl_ub;		/* cyl to start reduced current */
	u_char	current_cyl_mb;
	u_char	current_cyl_lb;
	u_short	step_rate;		/* drive step rate */
	u_char	landing_cyl_ub;		/* landing zone cylinder */
	u_char	landing_cyl_mb;
	u_char	landing_cyl_lb;
	u_char	reserved_1;
	u_char	reserved_2;
	u_char	reserved_3;
};

/*
 * Page 0x38 - Cache Control Parameters
 */
struct scsi_cache {
	u_char	page_savable	: 1;	/* save parameters */
	u_char	rsvd		: 1;
	u_char	page_code	: 6;	/* = 0x38 */
	u_char	page_length;		/* = 14 */
	u_char rsvd_1	: 1;
	u_char wie	: 1; 		/* write index enable */
	u_char rsvd_2	: 1;
	u_char ce	: 1; 		/* cache enable */
	u_char table_size : 4;
	u_char	prefetch_threshold;
	u_char	maximum_threshold;
	u_char	maximumprefetch_multiplier;
	u_char	minimum_threshold;
	u_char	minimum_prefetch_multiplier;
	u_char	reserved[8];
};

/*
 * Driver ioctl's for various scsi operations.
 */
#ifndef _IOCTL_
#include <sys/ioctl.h>
#endif

/*
 * Control for SCSI "format" mode.
 *
 * "Format" mode allows a privileged process to issue direct SCSI
 * commands to a drive (it is intended primarily to allow on-line
 * formatting).  SDIOCSFORMAT with a non-zero arg will put the drive
 * into format mode; a zero arg will take it out.  When in format
 * mode, only the process that issued the SDIOCFORMAT can read or
 * write the drive.
 *
 * In format mode, process is expected to
 *	- do SDIOCSCSICOMMAND to supply cdb for next SCSI op
 *	- do read or write as appropriate for cdb
 *	- if i/o error, optionally do SDIOCSENSE to get completion
 *	  status and sense data from last scsi operation.
 */

struct scsi_fmt_cdb {
	int len;		/* cdb length (in bytes) */
	u_char cdb[28];		/* cdb to use on next read/write */
};

struct scsi_fmt_sense {
	u_int status;		/* completion status of last op */
	u_char sense[28];	/* sense data (if any) from last op */
};

#define	SDIOCSFORMAT		_IOW('S', 0x1, int)
#define	SDIOCGFORMAT		_IOR('S', 0x2, int)
#define	SDIOCSCSICOMMAND	_IOW('S', 0x3, struct scsi_fmt_cdb)
#define	SDIOCSENSE		_IOR('S', 0x4, struct scsi_fmt_sense)
