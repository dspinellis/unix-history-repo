/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Harris Corp.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)hdreg.h	7.4 (Berkeley) 6/28/90
 */

#ifndef COMPAT_42
#define	COMPAT_42
#endif

#define	HDC_READ	0
#define	HDC_WRITE	1

#define	HDC_MAXBUS	2		/* max# buses */
#define	HDC_MAXCTLR	21		/* max# hdc controllers per bus */
#define	HDC_MAXDRIVE	4		/* max# drives per hdc controller */
#define	HDC_MAXMCBS	32		/* max# mcb's the hdc can handle */
#define	HDC_MAXCHAIN	64		/* max# of data chains */
#define	HDC_MAXBC	64*1024		/* max# byte count per data chain */
#define	HDC_MAXFLAWS	8000		/* max# flaws per hdc disk */

#define	HDC_SPB		2		/* sectors per block for hdc's */
#define	HDC_VDATA_SIZE	16		/* vendor data size (long words) */

#define	HDC_REG(x)	(hd->reg->x)	/* set an HDC register */
					/* number of blocks per dump record */
#define	HDC_DUMPSIZE	(HDC_MAXBC/DEV_BSIZE*HDC_MAXCHAIN)

/*
 * These are the 4 hdc i/o register addresses.  Writing to "master_mcb"
 * tells the hdc controller where the master mcb is and initiates hdc
 * operation. The hdc then reads the master mcb and all new mcb's in the
 * active mcb queue.  Writing to "module_id" causes the hdc to return the
 * hdc's module id word in the location specified by the address written
 * into the register.  "soft_reset" causes orderly shutdown of HDC; it's
 * unclear from the manual what "hard_reset" does, but it should never be
 * used as use while the HDC is active may cause format errors.
 */
struct registers {
	u_long	master_mcb,		/* set the master mcb address */
		module_id,		/* returns hdc's module id (hdc_mid) */
		soft_reset,		/* shut down the hdc */
		hard_reset;		/* send a system reset to the hdc */
};

/*
 * Definition for the module id returned by the hdc when "module_id"
 * is written to.  The format is defined by the hdc microcode.
 */
#define	HID_HDC		0x01		/* hvme_id for HDC */
#define	HDC_MID		HID_HDC		/* module id code for hdc's */
struct module_id {
	u_char	module_id,		/* module id; hdc's return HDC_MID */
		reserved,
		code_rev,		/* micro-code rev#; FF= not loaded */
		fit;			/* FIT test result; FF= no error */
};

/*
 * This structure defines the mcb's.  A portion of this structure is used
 * only by the software.  The other portion is set up by software and sent
 * to the hdc firmware to perform an operation; the order of this part of
 * the mcb is determined by the controller firmware.
 *
 * "context" is the software context word.  The hdc firmware copies the
 * contents of this word to the master mcb whenever the mcb has been
 * completed.  The virtual address of the mcb is usually saved here.
 *
 * "forw_phaddr" forms a linked list of mcbs.  The addresses are physical
 * since they are used by the hdc firmware.
 *
 * Bits in device control word #1 define the hdc command and control the
 * operation of the hdc.  Bits in device control word #2 define the disk
 * sector address for the operation defined in control word #1.
 */
#define	LWC_DATA_CHAIN	0x80000000	/* mask for data chain bit in wcount */
struct mcb {
	u_long	forw_phaddr;		/* phys address of next mcb */
	u_int	priority  :  8,		/* device control word #1 */
		interrupt :  1,		/*        "               */
		drive     :  7,		/*        "               */
		command   : 16,		/*        "   (see HCMD_) */
		cyl       : 13,		/* device control word #2 */
		head      :  9,		/*        "               */
		sector    : 10;		/*        "               */
	u_long	r1, r2,
		context;		/* software context word */
	struct chain {
		long	wcount,		/* word count */
			memadr;		/* transfer address */
	} chain[HDC_MAXCHAIN];		/* data chain */
};
					/* defines for the "command"s */
#define	HCMD_STATUS	0x40		/* command: read drive status */
#define	HCMD_READ	0x60		/* command: read data */
#define	HCMD_VENDOR	0x6a		/* command: read vendor data */
#define	HCMD_VERIFY	0x6d		/* command: verify a track */
#define	HCMD_WRITE	0x70		/* command: write data */
#define	HCMD_FORMAT	0x7e		/* command: format a track */
#define	HCMD_CERTIFY	0x7f		/* command: certify a track */
#define	HCMD_WCS	0xd0		/* command: write control store */

/*
 * This structure defines the master mcb - one per hdc controller.
 * The order of this structure is determined by the controller firmware.
 * "R" and "W" indicate read-only and write-only.
 *
 * Bits in the module control long word, "mcl", control the invocation of
 * operations on the hdc.
 *
 * The hdc operates in queued mode or immediate mode.  In queued mode, it
 * grabs new mcb's, prioritizes them, and adds them to its queue; it knows
 * if we've added any mcb's by checking forw_phaddr to see if any are
 * linked off of there.
 *
 * Bits in the master mcb's status word, "mcs", indicate the status
 * of the last-processed mcb.  The MCS_ definitions define these bits.
 * This word is set to zero when the mcb queue is passed to the hdc
 * controller; the hdc controller then sets bits in this word.
 * We cannot modify the mcb queue until the hdc has completed an mcb
 * (the hdc sets the MCS_Q_DONE bit).
 *
 * The "context" word is copied from the context word of the completed
 * mcb.  It is currently the virtual pointer to the completed mcb.
 */
					/* definition of master mcb "mcl" */
#define	MCL_QUEUED	0x00000010	/* start queued execution of mcb's */
#define	MCL_IMMEDIATE	0x00000001	/* start immediate xqt of an mcb */
					/* definition of master mcb "mcs" */
#define	MCS_DONE	0x00000080	/* an mcb is done; status is valid */
#define	MCS_FATALERROR	0x00000002	/* a fatal error occurred */
#define	MCS_SOFTERROR	0x00000001	/* a recoverable error occurred */

struct master_mcb {
	u_long	mcw,			/* W  module control word (MCL_) */
		interrupt,		/* W  interrupt acknowledge word */
		forw_phaddr,		/* W  physical address of first mcb */
		r1, r2,
		mcs,			/* R  status for last completed mcb */
		cmcb_phaddr,		/* W  physical addr of completed mcb */
		context,		/* W  software context word */
#define	HDC_XSTAT_SIZE	128		/* size of extended status (lwords) */
		xstatus[HDC_XSTAT_SIZE];/* R  xstatus of last mcb */
};

/*
 * This structure defines the information returned by the hdc controller for
 * a "read drive status" (HCMD_STATUS) command.  The format of this structure
 * is determined by the hdc firmware.  r[1-11] are reserved for future use.
 */
					/* defines for drive_stat drs word */
#define	DRS_FAULT	0x00000080	/* drive is reporting a fault */
#define	DRS_RESERVED	0x00000040	/* drive is reserved by other port */
#define	DRS_WRITE_PROT	0x00000020	/* drive is write protected */
#define	DRS_ON_CYLINDER	0x00000002	/* drive heads are not moving now */
#define	DRS_ONLINE	0x00000001	/* drive is available for operation */

struct status {
	u_long	drs,			/* drive status (see DRS_) */
		r1, r2, r3;
	u_short	max_cyl,		/* max logical cylinder address */
		max_head,		/* max logical head address */
		r4,
		max_sector,		/* max logical sector address */
		def_cyl,		/* definition track cylinder address */
		def_cyl_count,		/* definition track cylinder count */
		diag_cyl,		/* diagnostic track cylinder address */
		diag_cyl_count,		/* diagnostic track cylinder count */
		max_phys_cyl,		/* max physical cylinder address */
		max_phys_head,		/* max physical head address */
		r5,
		max_phys_sector,	/* max physical sector address */
		r6,
		id,			/* drive id (drive model) */
		r7,
		bytes_per_sec,		/* bytes/sector -vendorflaw conversn */
		r8,
		rpm;			/* disk revolutions per minute */
	u_long	r9, r10, r11;
};

#ifdef COMPAT_42
#define	GB_ID		"geometry"
#define	GB_ID_LEN 	sizeof(GB_ID)-1
#define	GB_MAXPART	8
#define	GB_VERSION	1

#define	HDC_DEFPART	GB_MAXPART-1	/* partition# of def and diag cyls */
#define	BPS		512		/* bytes per sector */

/*
 * Geometry Block:
 *
 * The geometry block defines partition offsets and information about the
 * flaw maps on the flaw map track.  It resides on the first sector of the
 * flaw map track.  This structure is also used by vddc disk controllers.
 * In this case, the block resides at sector 0 of the disk.
 *
 * The geometry_sector structure defines the sector containing the geometry
 * block.  This sector is checksumed independent of the geometry information.
 * The fields in these structured which should never be moved are the id and
 * version fields in the geometry_block structure and the checksum field in
 * the geometry_sector structure.  This will provide for easy extensions in
 * the future.
 */

#define	DRIVE_TYPE	flaw_offset	/* For VDDC Geometry Blocks Only */

typedef struct {
	char	id[GB_ID_LEN];		/* identifies the geometry block */
	long	version,		/* geometry block version number */
		flaw_offset,		/* flaw map byte offset in partition7 */
		flaw_size,		/* harris flaw map size in bytes */
		flaw_checksum,		/* sum of bytes in harris flaw map */
		unused[3];		/* --- available for use */
	struct par_tab {
		long	start,		/* starting 1K block number */
			length;		/* partition size in 1K blocks */
	} partition[GB_MAXPART];	/* partition definitions */
} geometry_block;

typedef struct {
	geometry_block	geometry_block;	/* disk geometry */
	char	filler[BPS - sizeof(geometry_block) - sizeof(long)];
	long	checksum;		/* sector checksum */
} geometry_sector;

/*
 * GB_CHECKSUM:
 *
 * This macro computes the checksum for the geometry sector and returns the
 * value.  Input to this macro is a pointer to the geometry_sector.  Pretty
 * useless, should at least have done an XOR.
 */
#define GB_CHECKSUM(_gs_ptr, _checksum) { \
	register u_char *_ptr; \
	register u_long _i, _xsum; \
	_xsum = 0; \
	_ptr = (u_char *)(_gs_ptr); \
	for (_i = 0; _i < (sizeof(geometry_sector) - sizeof(long)); _i++) \
		_xsum += * _ptr++; \
	_checksum = _xsum; \
}
#endif /* COMPAT_42 */
