/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)scsi.h	8.1 (Berkeley) 6/10/93
 *
 * scsi.h --
 *
 *	Common declarations for SCSI command formaters. This file only covers
 *	definitions pertaining to the SCSI common command set that are
 *	common to all SCSI device types (ie disk, tapes, WORM, printers, etc).
 *	Some of the references from the proceedings of the
 *	1984 Mini/Micro Northeast conference might help in understanding SCSI. 
 *
 * from: $Header: /sprite/src/kernel/dev/RCS/scsi.h,
 *	v 9.1 90/02/13 23:11:24 jhh Exp $ SPRITE (Berkeley)
 */

#ifndef _SCSI_H
#define _SCSI_H

/*
 * "Standard" SCSI Commands.
 * SCSI commands are divided into 8 groups as follows:
 *	Group0	(0x00 - 0x1f).	Basic commands. 6 bytes long
 *	Group1	(0x20 - 0x3f).	Extended command. 10 bytes.
 *	Group2	(0x40 - 0x5f).	Reserved.
 *	Group2	(0x60 - 0x7f).	Reserved.
 *	Group2	(0x80 - 0x9f).	Reserved.
 *	Group2	(0xa0 - 0xbf).	Reserved.
 *	Group6	(0xc0 - 0xdf).	Vendor Unique
 *	Group7	(0xe0 - 0xff).	Vendor Unique
 */

/*
 * Scsi Group0 commands all are 6 bytes and have a format according to 
 * struct ScsiGroup0Cmd.
 */
#define SCSI_TEST_UNIT_READY	0x00
#define SCSI_REZERO_UNIT	0x01
#define SCSI_REWIND		0x01
#define SCSI_REQUEST_SENSE	0x03
#define	SCSI_FORMAT_UNIT	0x04
#define	SCSI_READ_BLOCK_LIMITS	0x05
#define SCSI_REASSIGN_BLOCKS	0x07
#define SCSI_READ		0x08
#define SCSI_WRITE		0x0a
#define SCSI_SEEK		0x0b
#define	SCSI_TRACK_SELECT	0x0b
#define	SCSI_READ_REVERSE	0x0f
#define SCSI_WRITE_EOF		0x10
#define SCSI_SPACE		0x11
#define SCSI_INQUIRY		0x12
#define SCSI_VERIFY		0x13
#define	SCSI_READ_BUFFER	0x14
#define SCSI_MODE_SELECT	0x15
#define	SCSI_RESERVE_UNIT	0x16
#define	SCSI_RELEASE_UNIT	0x17
#define SCSI_COPY		0x18
#define SCSI_ERASE_TAPE		0x19
#define SCSI_MODE_SENSE		0x1a
#define SCSI_START_STOP		0x1b
#define	SCSI_LOAD_UNLOAD	0x1b
#define	SCSI_RECV_DIAG_RESULTS	0x1c
#define SCSI_SEND_DIAGNOSTIC	0x1d
#define SCSI_PREVENT_ALLOW	0x1e

/*
 * Group1 commands are all 10 bytes and have a format according to
 * struct ScsiGroup1Cmd.
 */
#define SCSI_READ_CAPACITY	0x25	
#define	SCSI_READ_EXT		0x28
#define	SCSI_WRITE_EXT		0x2a
#define	SCSI_SEEK_EXT		0x2b
#define	SCSI_WRITE_VERIFY	0x2e
#define	SCSI_VERIFY_EXT		0x2f
#define	SCSI_SEARCH_HIGH	0x30
#define SCSI_SEARCH_EQUAL	0x31
#define	SCSI_SEARCH_LOW		0x32
#define	SCSI_SET_LIMITS		0x33
#define	SCSI_COMPARE		0x39
#define	SCSI_COPY_VERIFY	0x3a

/*
 * Control byte flags for Group0 and Group1 commands.
 *
 * SCSI_CTRL_LINK - This is used to prevent a bus free phase between commands.
 *	If the command terminates successfully, a SCSI_LINKED_CMD_COMPLETE
 *	message is returned instead of the normal SCSI_COMMAND_COMPLETE message. *	The last command in a chain should not have this bit set
 *	(and consequently gets a normal SCSI_COMMAND_COMPLETE message).
 * SCSI_CTRL_LINK_FLAG - This bit should only set when SCSI_CTRL_LINK is set and
 *	causes a SCSI_LINKED_FLAGED_CMD_COMPLETE to be returned instead of
 *	a SCSI_LINKED_CMD_COMPLETE.
 */
#define SCSI_CTRL_LINK		0x01	/* Link commands (no bus free phase) */
#define SCSI_CTRL_LINK_INTR	0x02	/* Interrupt after linked command */

/*
 * The standard group0 6-byte SCSI control block.  Note that the 
 * fields between highAddr and blockCount inclusive are command dependent.
 * The definitions Addr and BlockCount cover most of the commands we will
 * use.
 */
typedef struct ScsiGroup0Cmd {
	u_char	command;		/* command code, defined below.  The
					 * upper three bits of this are zero
					 * to indicate the control block is
					 * only 6 bytes long */
#if BYTE_ORDER == BIG_ENDIAN
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
	u_char	highAddr	:5;	/* High bits of address */
#else
	u_char	highAddr	:5;	/* High bits of address */
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
#endif
	u_char	midAddr;		/* Middle bits of address */
	u_char	lowAddr;		/* Low bits of address */
	u_char	blockCount;		/* Blocks to transfer */
	u_char	control;		/* See flags for common bits */
} ScsiGroup0Cmd;

/*
 * Format of a SCSI_START_STOP command. This is a group 0 command, but
 * the command contents are different.
 */
typedef struct ScsiStartStopCmd {
#if BYTE_ORDER == BIG_ENDIAN
	u_char	command;		/* command code, defined below.  The
					 * upper three bits of this are zero
					 * to indicate the control block is
					 * only 6 bytes long */
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
	u_char	pad1		:4;	/* Reserved */
	u_char	immed		:1;	/* Immediate status bit */
	u_char	pad2;			/* Reserved */
	u_char	pad3;			/* Reserved */
	u_char	pad4		:6;	/* Reserved */
	u_char	loadEject	:1;	/* Load or eject medium */
	u_char	start		:1;	/* Start or stop medium */
	u_char	control;		/* See flags for common bits */
#else
	u_char	command;		/* command code, defined below.  The
					 * upper three bits of this are zero
					 * to indicate the control block is
					 * only 6 bytes long */
	u_char	immed		:1;	/* Immediate status bit */
	u_char	pad1		:4;	/* Reserved */
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
	u_char	pad2;			/* Reserved */
	u_char	pad3;			/* Reserved */
	u_char	start		:1;	/* Start or stop medium */
	u_char	loadEject	:1;	/* Load or eject medium */
	u_char	pad4		:6;	/* Reserved */
	u_char	control;		/* See flags for common bits */
#endif
} ScsiStartStopCmd;

/*
 * The standard group1 10-byte SCSI control block.  Note that the 
 * fields between highAddr and blockCount inclusive are command dependent.
 * The definitions Addr and BlockCount cover most of the commands we will
 * use.
 */
typedef struct ScsiGroup1Cmd {
	u_char	command;		/* command code, defined below.  The
					 * upper three bits of this are zero
					 * to indicate the control block is
					 * only 6 bytes long */
#if BYTE_ORDER == BIG_ENDIAN
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
	u_char	pad1		:5;	/* Reserved */
#else
	u_char	pad1		:5;	/* Reserved */
	u_char	unitNumber	:3;	/* Logical Unit (LUN) to which to
					 * pass the command.  The device
					 * has already been selected using
					 * the "targetID" bit. */
#endif
	u_char	highAddr;		/* High bits of address */
	u_char	midHighAddr;		/* Middle high bits of address */
	u_char	midLowAddr;		/* Middle low bits of address */
	u_char	lowAddr;		/* Low bits of address */
	u_char	pad2;			/* Reserved */
	u_char	highBlockCount;	/* High bits of blocks to transfer */
	u_char	lowBlockCount;	/* Low bits of blocks to transfer */
	u_char	control;		/* See flags for common bits */
} ScsiGroup1Cmd;

/*
 * SCSI status completion information.
 * This is returned by the device when a command completes. 
 */
#define	SCSI_STATUS_CHECKCOND	0x02	/* Check Condition (ie., read sense) */
#define	SCSI_STATUS_CONDMET	0x04	/* Condition Met (ie., search worked) */
#define	SCSI_STATUS_BUSY	0x08
#define	SCSI_STATUS_INTERMED	0x10	/* Intermediate status sent */
#define	SCSI_STATUS_EXT		0x80	/* Extended status valid */

/*
 * Sense information provided after some errors.  This is divided into
 * two kinds, classes 0-6, and class 7.  This is 30 bytes big to allow
 * for the drive specific sense bytes that follow the standard 4 byte header.
 *
 * For extended sense, this buffer may be cast into another type.  Also
 * The actual size of the sense data returned is used to detect what
 * kind of tape drive is out there.  Kludgy, but true.
 */
typedef struct ScsiClass0Sense {
#if BYTE_ORDER == BIG_ENDIAN
	u_char	valid		:1;	/* Sense data is valid */
	u_char	error		:7;	/* 3 bits class and 4 bits code */
#else
	u_char	error		:7;	/* 3 bits class and 4 bits code */
	u_char	valid		:1;	/* Sense data is valid */
#endif
	u_char	highAddr;		/* High byte of block address */
	u_char	midAddr;		/* Middle byte of block address */
	u_char	lowAddr;		/* Low byte of block address */
	u_char	sense[26];		/* Target specific sense data */
} ScsiClass0Sense;

/*
 * Definitions for errors in the sense data.  The error field is specified
 * as a 3 bit class and 4 bit code, but it is easier to treat it as a
 * single 7 bit field.
 */
#define SCSI_NO_SENSE_DATA		0x00
#define SCSI_NOT_READY			0x04
#define SCSI_NOT_LOADED			0x09
#define SCSI_INSUF_CAPACITY		0x0a
#define SCSI_HARD_DATA_ERROR		0x11
#define SCSI_WRITE_PROTECT		0x17
#define SCSI_CORRECTABLE_ERROR		0x18
#define SCSI_FILE_MARK			0x1c
#define SCSI_INVALID_COMMAND		0x20
#define SCSI_UNIT_ATTENTION		0x30
#define SCSI_END_OF_MEDIA		0x34

/*
 * The standard "extended" sense data returned by SCSI devices.  This
 * has an error field of 0x70, for a "class 7" error.
 */
typedef struct ScsiClass7Sense {
#if BYTE_ORDER == BIG_ENDIAN
	u_char	valid		:1;	/* Sense data is valid */
	u_char	error7		:7;	/* == 0x70 */
	u_char	pad1;			/* Also "segment number" for copy */
	u_char	fileMark	:1;	/* File mark on device */
	u_char	endOfMedia	:1;	/* End of media reached */
	u_char	badBlockLen	:1;	/* Block length mis-match (Exabyte) */
	u_char	pad2		:1;
	u_char	key		:4;	/* Sense keys defined below */
	u_char	info1;			/* Information byte 1 */
	u_char	info2;			/* Information byte 2 */
	u_char	info3;			/* Information byte 3 */
	u_char	info4;			/* Information byte 4 */
	u_char	length;			/* Number of additional info bytes */
#else
	u_char	error7		:7;	/* == 0x70 */
	u_char	valid		:1;	/* Sense data is valid */
	u_char	pad1;			/* Also "segment number" for copy */
	u_char	key		:4;	/* Sense keys defined below */
	u_char	pad2		:1;
	u_char	badBlockLen	:1;	/* Block length mis-match (Exabyte) */
	u_char	endOfMedia	:1;	/* End of media reached */
	u_char	fileMark	:1;	/* File mark on device */
	u_char	info1;			/* Information byte 1 */
	u_char	info2;			/* Information byte 2 */
	u_char	info3;			/* Information byte 3 */
	u_char	info4;			/* Information byte 4 */
	u_char	length;			/* Number of additional info bytes */
#endif
} ScsiClass7Sense;			/* 8 Bytes */

/*
 * Key values for standardized sense class 7. 
 */
#define SCSI_CLASS7_NO_SENSE		0
#define SCSI_CLASS7_RECOVERABLE		1
#define SCSI_CLASS7_NOT_READY		2
#define SCSI_CLASS7_MEDIA_ERROR		3
#define SCSI_CLASS7_HARDWARE_ERROR	4
#define SCSI_CLASS7_ILLEGAL_REQUEST	5

/*
 * These seem to have different meanings to different vendors....
 */
#define SCSI_CLASS7_MEDIA_CHANGE	6
#define SCSI_CLASS7_UNIT_ATTN		6

#define SCSI_CLASS7_WRITE_PROTECT	7
#define SCSI_CLASS7_BLANK_CHECK		8
#define SCSI_CLASS7_VENDOR		9
#define SCSI_CLASS7_POWER_UP_FAILURE	10
#define SCSI_CLASS7_ABORT		11
#define SCSI_CLASS7_EQUAL		12
#define SCSI_CLASS7_OVERFLOW		13
#define SCSI_CLASS7_RESERVED_14		14
#define SCSI_CLASS7_RESERVED_15		15

/*
 * Data return by the SCSI inquiry command. 
 */
typedef struct ScsiInquiryData {
#if BYTE_ORDER == BIG_ENDIAN
	u_char	type;		/* Peripheral Device type. See below. */
	u_char	rmb:1;		/* Removable Medium bit. */
	u_char	qualifier:7;	/* Device type qualifier. */
	u_char	version;	/* Version info. */
	u_char	reserved:4;	/* reserved. */
	u_char	format:4;	/* Response format. */
	u_char	length;		/* length of data returned. */
	u_char	reserved2[2];	/* Reserved */
	u_char	flags;		/* SCSI II flags (see below) */
	u_char	vendorID[8];	/* Vendor ID (ASCII) */
	u_char	productID[16];	/* Product ID (ASCII) */
	u_char	revLevel[4];	/* Revision level (ASCII) */
	u_char	revData[8];	/* Revision data (ASCII) */
#else
	u_char	type;		/* Peripheral Device type. See below. */
	u_char	qualifier:7;	/* Device type qualifier. */
	u_char	rmb:1;		/* Removable Medium bit. */
	u_char	version;	/* Version info. */
	u_char	format:4;	/* Response format. */
	u_char	reserved:4;	/* reserved. */
	u_char	length;		/* length of data returned. */
	u_char	reserved2[2];	/* Reserved */
	u_char	flags;		/* SCSI II flags (see below) */
	u_char	vendorID[8];	/* Vendor ID (ASCII) */
	u_char	productID[16];	/* Product ID (ASCII) */
	u_char	revLevel[4];	/* Revision level (ASCII) */
	u_char	revData[8];	/* Revision data (ASCII) */
#endif
} ScsiInquiryData;

/*
 * The SCSI Peripheral type ID codes as return by the SCSI_INQUIRY command.
 *
 * SCSI_DISK_TYPE - Direct Access Device.
 * SCSI_TAPE_TYPE - Sequential Access Device.
 * SCSI_PRINTER_TYPE - Printer Device.
 * SCSI_HOST_TYPE - Processor Device.
 * SCSI_WORM_TYPE - Write-Once Read-Multiple Device.
 * SCSI_ROM_TYPE - Read-Only Direct Access Device.
 * SCSI_SCANNER_TYPE - Scanner device.
 * SCSI_OPTICAL_MEM_TYPE - Optical memory device.
 * SCSI_MEDIUM_CHANGER_TYPE - Medium changer device.
 * SCSI_COMMUNICATIONS_TYPE - Communications device.
 * SCSI_NODEVICE_TYPE - Logical Unit not present or implemented.
 *
 * Note that codes 0xa-0x7e are reserved and 0x80-0xff are vendor unique.
 */
#define	SCSI_DISK_TYPE			0
#define	SCSI_TAPE_TYPE			1
#define	SCSI_PRINTER_TYPE		2
#define	SCSI_HOST_TYPE			3
#define	SCSI_WORM_TYPE			4
#define	SCSI_ROM_TYPE			5
#define	SCSI_SCANNER_TYPE		6
#define	SCSI_OPTICAL_MEM_TYPE		7
#define	SCSI_MEDIUM_CHANGER_TYPE	8
#define	SCSI_COMMUNICATIONS_TYPE	9
#define	SCSI_NODEVICE_TYPE		0x7f

/*
 * The SCSI I & II inquiry flags.
 *
 * SCSI_REL_ADR - Relative addressing supported.
 * SCSI_WIDE_32 - 32 bit wide SCSI bus transfers supported.
 * SCSI_WIDE_16 - 16 bit wide SCSI bus transfers supported.
 * SCSI_SYNC - Synchronous data transfers supported.
 * SCSI_LINKED - Linked commands supported.
 * SCSI_CMD_QUEUE - Tagged command queuing supported.
 * SCSI_SOFT_RESET - Soft RESET alternative suported.
 */
#define SCSI_REL_ADR		0x80
#define SCSI_WIDE_32		0x40
#define SCSI_WIDE_16		0x20
#define SCSI_SYNC		0x10
#define SCSI_LINKED		0x08
#define SCSI_CMD_QUEUE		0x02
#define SCSI_SOFT_RESET		0x01

/*
 * Standard header for SCSI_MODE_SENSE and SCSI_MODE_SELECT commands for tapes.
 */
typedef struct ScsiTapeModeSelectHdr {
	u_char	len;		/* length */
	u_char	media;		/* media type */
#if BYTE_ORDER == BIG_ENDIAN
	u_char	writeprot:1;	/* Write protected media */
	u_char	bufferedMode:3;	/* Type of buffer to be done. */
	u_char	speed:4;	/* Drive speed. */
#else
	u_char	speed:4;	/* Drive speed. */
	u_char	bufferedMode:3;	/* Type of buffer to be done. */
	u_char	writeprot:1;	/* Write protected media */
#endif
	u_char	length;		/* Block descriptor length. */
	u_char	density;	/* tape density code */
	u_char	blocks_2;	/* number of blocks (MSB) */
	u_char	blocks_1;	/* number of blocks */
	u_char	blocks_0;	/* number of blocks (LSB) */
	u_char	reserved;	/* */
	u_char	block_size2;	/* Tape block size (MSB) */
	u_char	block_size1;	/* Tape block size */
	u_char	block_size0;	/* Tape block size (LSB) */
	u_char	vendor[6];	/* vendor specific data */
} ScsiTapeModeSelectHdr;

/*
 * Definitions of SCSI messages.
 *
 * SCSI_COMMAND_COMPLETE - After a command has completed, successfully
 *	or not, this is returned to the host from the target.
 *
 * SCSI_EXTENDED_MSG - Indicates that a multi-byte message is being sent.
 *
 * The following messages are used with connect/disconnect:
 * SCSI_SAVE_DATA_POINTER - Sent from target to host to request saving
 *	of current DMA address and count.  Indicates a pending dis-connect.
 * SCSI_RESTORE_POINTER - Sent from the target to the host to request
 *	restoring pointers saved before a disconnect
 * SCSI_DISCONNECT - Sent from the target to the host to disconnect.
 * SCSI_ABORT - Sent from the host to the target to abort current request.
 * SCSI_MESSAGE_REJECT -  Indicates receipt, by either host or target, of
 *	an unimplemented message.
 * SCSI_NO_OP - Sent from host to target if it has no real message to send.
 * SCSI_MESSAGE_PARITY_ERROR - Sent from host to target on message parity error
 * SCSI_BUS_RESET - Sent from host to target to reset all current I/O
 *
 * SCSI_IDENTIFY - The low order two bits of this message type indicate
 *	the Logical Unit of the Target which is requesting a reconnect.
 * SCSI_DIS_REC_IDENTIFY - Sent from the host to a target to indicate
 *	is supports connect/dis-connect
 *	
 */
#define SCSI_COMMAND_COMPLETE		0x00
#define SCSI_EXTENDED_MSG		0x01
#define SCSI_SAVE_DATA_POINTER		0x02
#define SCSI_RESTORE_POINTERS		0x03
#define SCSI_DISCONNECT			0x04
#define SCSI_ABORT			0x06
#define SCSI_MESSAGE_REJECT		0x07
#define SCSI_NO_OP			0x08
#define SCSI_MESSAGE_PARITY_ERROR	0x09
#define SCSI_LINKED_CMD_COMPLETE	0x0A
#define SCSI_LINKED_FLAGED_CMD_COMPLETE	0x0B
#define SCSI_BUS_RESET			0x0C

#define SCSI_IDENTIFY			0x80
#define SCSI_DIS_REC_IDENTIFY		0xc0

/*
 * Extended message types (2nd byte of SCSI_EXTENDED_MSG).
 */
#define SCSI_MODIFY_DATA_PTR		0x00
#define SCSI_SYNCHRONOUS_XFER		0x01
#define SCSI_EXTENDED_IDENTIFY		0x02 /* only in SCSI I */
#define SCSI_WIDE_XFER			0x03

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
	int	len;		/* cdb length (in bytes) */
	u_char	cdb[28];	/* cdb to use on next read/write */
};

struct scsi_fmt_sense {
	u_int	status;		/* completion status of last op */
	u_char	sense[32];	/* sense data (if any) from last op */
};

#define	SDIOCSFORMAT		_IOW('S', 0x1, int)
#define	SDIOCGFORMAT		_IOR('S', 0x2, int)
#define	SDIOCSCSICOMMAND	_IOW('S', 0x3, struct scsi_fmt_cdb)
#define	SDIOCSENSE		_IOR('S', 0x4, struct scsi_fmt_sense)

#ifdef KERNEL
/*
 * Routines.
 */
extern void scsiGroup0Cmd();
extern void scsiGroup1Cmd();
#endif /* KERNEL */

#endif /* _SCSI_H */
