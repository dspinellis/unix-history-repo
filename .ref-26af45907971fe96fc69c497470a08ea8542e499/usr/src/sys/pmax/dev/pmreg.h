/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmreg.h	7.1 (Berkeley) %G%
 */

/*
 * Defines for the graphics hardware.
 */

/*
 * Structure to descripte the programmable cursor chip (DC503) from DEC.
 */
typedef volatile struct PCCRegs {
	u_short	cmdr;	/* cursor command register */
	short	pad1;
	u_short	xpos;	/* cursor X position */
	short	pad2;
	u_short	ypos;	/* cursor Y position */
	short	pad3;
	u_short	xmin1;	/* region 1 left edge */
	short	pad4;
	u_short	xmax1;	/* region 1 right edge */
	short	pad5;
	u_short	ymin1;	/* region 1 top edge */
	short	pad6;
	u_short	ymax1;	/* region 1 bottom edge */
	short	pad7[9];
	u_short	xmin2;	/* region 2 left edge */
	short	pad8;
	u_short	xmax2;	/* region 2 right edge */
	short	pad9;
	u_short	ymin2;	/* region 2 top edge */
	short	pad10;
	u_short	ymax2;	/* region 2 bottom edge */
	short	pad11;
	u_short	memory;	/* cursor sprite pattern load */
} PCCRegs;

	/* define bits in pcc_cmdr */
#define PCC_TEST	0x8000
#define PCC_HSHI	0x4000
#define PCC_VBHI	0x2000
#define PCC_LODSA	0x1000
#define PCC_FORG2	0x0800
#define PCC_ENRG2	0x0400
#define PCC_FORG1	0x0200
#define PCC_ENRG1	0x0100
#define PCC_XHWID	0x0080
#define PCC_XHCL1	0x0040
#define PCC_XHCLP	0x0020
#define PCC_XHAIR	0x0010
#define PCC_FOPB	0x0008
#define PCC_ENPB	0x0004
#define PCC_FOPA	0x0002
#define PCC_ENPA	0x0001

	/* offset for cursor X & Y locations */
#define PCC_X_OFFSET	212
#define PCC_Y_OFFSET	34

typedef volatile struct VDACRegs {
	u_char	mapWA;		/* address register (color map write) */
	char	pad1[3];
	u_char	map;		/* color map */
	char	pad2[3];
	u_char	mask;		/* pixel read mask */
	char	pad3[3];
	u_char	mapRA;		/* address register (color map read) */
	char	pad4[3];
	u_char	overWA;		/* address register (overlay map write) */
	char	pad5[3];
	u_char	over;		/* overlay map */
	char	pad6[7];
	u_char	overRA;		/* address register (overlay map read) */
} VDACRegs;
