/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfbreg.h	7.1 (Berkeley) %G%
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
 * This configuration uses two twin Bt431s and a single Bt455 colour map.
 * (Yep, that's a colour map on a single bitplane mono display)
 */
#define	SET_VALUE(x)	(((x)<<8)|((x)&0xff))
#define	GET_VALUE(x)	((x)&0xff)

typedef struct {
	volatile u_short	addr_lo;
	short			pad0;
	volatile u_short	addr_hi;
	short			pad1;
	volatile u_short	addr_cmap;
	short			pad2;
	volatile u_short	addr_reg;
	short			pad3;
} bt431_regmap_t;

/*
 * Generic register access
 */
/* when using autoincrement */
#define	BT431_WRITE_REG_AUTOI(regs, val) { \
		(regs)->addr_reg = SET_VALUE(val); \
		MachEmptyWriteBuffer(); \
	}

#define	BT431_READ_REG_AUTOI(regs) \
		GET_VALUE(((regs)->addr_reg))

#define	BT431_WRITE_CMAP_AUTOI(regs, val) { \
		(regs)->addr_cmap = (val); \
		MachEmptyWriteBuffer(); \
	}

#define	BT431_READ_CMAP_AUTOI(regs) \
		((regs)->addr_cmap)

typedef struct {
	volatile u_char	addr_cmap;
	char		pad0[3];
	volatile u_char	addr_cmap_data;
	char		pad1[3];
	volatile u_char	addr_clr;
	char		pad2[3];
	volatile u_char	addr_ovly;
	char		pad3[3];
} bt455_regmap_t;


/*
 * Generic register access
 */
#define BT455_SELECT_ENTRY(regs, regno) { \
		(regs)->addr_cmap = (regno)&0x0f; \
		MachEmptyWriteBuffer(); \
	}

/*
 * Additional registers addressed indirectly
 */
#define	BT431_REG_CMD		0x0000
#define	BT431_REG_CXLO		0x0001
#define	BT431_REG_CXHI		0x0002
#define	BT431_REG_CYLO		0x0003
#define	BT431_REG_CYHI		0x0004
#define	BT431_REG_WXLO		0x0005
#define	BT431_REG_WXHI		0x0006
#define	BT431_REG_WYLO		0x0007
#define	BT431_REG_WYHI		0x0008
#define	BT431_REG_WWLO		0x0009
#define	BT431_REG_WWHI		0x000a
#define	BT431_REG_WHLO		0x000b
#define	BT431_REG_WHHI		0x000c

#define BT431_REG_CRAM_BASE	0x0000
#define BT431_REG_CRAM_END	0x01ff

/*
 * Command register
 */

#define BT431_CMD_CURS_ENABLE	0x40
#define BT431_CMD_XHAIR_ENABLE	0x20
#define BT431_CMD_OR_CURSORS	0x10
#define BT431_CMD_AND_CURSORS	0x00
#define BT431_CMD_1_1_MUX	0x00
#define BT431_CMD_4_1_MUX	0x04
#define BT431_CMD_5_1_MUX	0x08
#define BT431_CMD_xxx_MUX	0x0c
#define BT431_CMD_THICK_1	0x00
#define BT431_CMD_THICK_3	0x01
#define BT431_CMD_THICK_5	0x02
#define BT431_CMD_THICK_7	0x03
