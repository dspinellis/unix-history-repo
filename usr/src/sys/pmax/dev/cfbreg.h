/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cfbreg.h	7.1 (Berkeley) %G%
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

/*
 * HISTORY
 * $Log:	bt459.h,v $
 * Revision 2.4  91/02/05  17:39:43  mrt
 * 	Added author notices
 * 	[91/02/04  11:11:57  mrt]
 * 
 * 	Changed to use new Mach copyright
 * 	[91/02/02  12:09:39  mrt]
 * 
 * Revision 2.3  90/12/05  23:30:26  af
 * 	Cursor color register are supported, contrary to specs.
 * 	[90/12/03  23:07:22  af]
 * 
 * Revision 2.1.1.1  90/11/01  03:36:40  af
 * 	Created, from Brooktree specs:
 * 	"Product Databook 1989"
 * 	"Bt459 135 MHz Monolithic CMOS 256x64 Color Palette RAMDAC"
 * 	Brooktree Corp. San Diego, CA
 * 	LA59001 Rev. J
 * 	[90/09/03            af]
 */

/*
 *	File: bt459.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	9/90
 *
 *	Defines for the bt459 Cursor/RAMDAC chip
 */

typedef struct {
	unsigned char	addr_lo;
	char						pad0[3];
	unsigned char	addr_hi;
	char						pad1[3];
	unsigned char	addr_reg;
	char						pad2[3];
	unsigned char	addr_cmap;
	char						pad3[3];
} bt459_regmap_t;

/*
 * Additional registers addressed indirectly
 */

				/* 0000-00ff Color Map entries */
				/* 0100-010f Overlay color regs, unsupp */
#define	BT459_REG_CCOLOR_1	0x0181	/* Cursor color regs */
#define	BT459_REG_CCOLOR_2	0x0182
#define	BT459_REG_CCOLOR_3	0x0183
#define	BT459_REG_ID		0x0200	/* read-only, gives "4a" */
#define	BT459_REG_CMD0		0x0201
#define	BT459_REG_CMD1		0x0202
#define	BT459_REG_CMD2		0x0203
#define	BT459_REG_PRM		0x0204
				/* 0205 reserved */
#define	BT459_REG_PBM		0x0206
				/* 0207 reserved */
#define	BT459_REG_ORM		0x0208
#define	BT459_REG_OBM		0x0209
#define	BT459_REG_ILV		0x020a
#define	BT459_REG_TEST		0x020b
#define	BT459_REG_RSIG		0x020c
#define	BT459_REG_GSIG		0x020d
#define	BT459_REG_BSIG		0x020e
				/* 020f-02ff reserved */
#define	BT459_REG_CCR		0x0300
#define	BT459_REG_CXLO		0x0301
#define	BT459_REG_CXHI		0x0302
#define	BT459_REG_CYLO		0x0303
#define	BT459_REG_CYHI		0x0304
#define	BT459_REG_WXLO		0x0305
#define	BT459_REG_WXHI		0x0306
#define	BT459_REG_WYLO		0x0307
#define	BT459_REG_WYHI		0x0308
#define	BT459_REG_WWLO		0x0309
#define	BT459_REG_WWHI		0x030a
#define	BT459_REG_WHLO		0x030b
#define	BT459_REG_WHHI		0x030c
				/* 030d-03ff reserved */
#define BT459_REG_CRAM_BASE	0x0400
#define BT459_REG_CRAM_END	0x07ff
