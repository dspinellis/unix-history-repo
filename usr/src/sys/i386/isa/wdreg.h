/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)wdreg.h	5.1 (Berkeley) %G%
 */


/*
 * Disk Controller register definitions.
 */
#define	wd_data		0x0		/* data register (R/W - 16 bits) */
#define wd_error	0x1		/* error register (R) */
#define	wd_precomp	wd_error	/* write precompensation (W) */
#define	wd_seccnt	0x2		/* sector count (R/W) */
#define	wd_sector	0x3		/* first sector number (R/W) */
#define	wd_cyl_lo	0x4		/* cylinder address, low byte (R/W) */
#define	wd_cyl_hi	0x5		/* cylinder address, high byte (R/W)*/
#define	wd_sdh		0x6		/* sector size/drive/head (R/W)*/
#define	wd_command	0x7		/* command register (W)	 */
#define	wd_status wd_command		/* immediate status (R)	 */

#define	wd_altsts	0x206	 /*alternate fixed disk status(via 1015) (R)*/
#define	wd_ctlr		0x206	 /*fixed disk controller control(via 1015) (W)*/
#define	wd_digin	0x207	 /* disk controller input(via 1015) (R)*/

/*
 * Status Bits.
 */
#define	WDCS_BUSY	0x80		/* Controller busy bit. */
#define	WDCS_READY	0x40		/* Selected drive is ready */
#define	WDCS_WRTFLT	0x20		/* Write fault */
#define	WDCS_SEEKCMPLT	0x10		/* Seek complete */
#define	WDCS_DRQ	0x08		/* Data request bit. */
#define	WDCS_ECCCOR	0x04		/* ECC correction made in data */
#define	WDCS_INDEX	0x02		/* Index pulse from selected drive */
#define	WDCS_ERR	0x01		/* Error detect bit. */

#define WDCS_BITS	"\020\010busy\006rdy\006wrtflt\005seekdone\004drq\003ecc_cor\002index\001err"

#define WDERR_BITS	"\020\010badblk\007uncorr\006id_crc\005no_id\003abort\002tr000\001no_dam"

/*
 * Commands for Disk Controller.
 */
#define	WDCC_READ	0x20		/* disk read code */
#define	WDCC_WRITE	0x30		/* disk write code */
#define	WDCC_RESTORE	0x10		/* disk restore code -- resets cntlr */
#define	WDCC_FORMAT	0x50		/* disk format code */

#define	WD_STEP		0		/* winchester- default 35us step */

#define	WDSD_IBM	0xa0		/* forced to 512 byte sector, ecc */

