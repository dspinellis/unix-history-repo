/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cgsixreg.h	8.1 (Berkeley) %G%
 *
 * from: $Header: cgsixreg.h,v 1.1 93/10/12 15:29:24 torek Exp $
 */

/*
 * CG6 display registers.  (Note, I got tired of writing `cgsix' about
 * halfway through and changed everything to cg6, but I probably missed
 * some.  Unfortunately, the way config works, we need to spell out `six'
 * in some places anyway.)
 *
 * The cg6 is a complicated beastie.  We have been unable to extract any
 * documentation and most of the following are guesses based on a limited
 * amount of reverse engineering.
 *
 * A cg6 is composed of numerous groups of control registers, all with TLAs:
 *	FBC - frame buffer control?
 *	FHC - fbc hardware configuration?
 *	DHC - ???
 *	TEC - transform engine control?
 *	THC - TEC Hardware Configuration (this is where our action goes)
 *	ROM - a 64Kbyte ROM with who knows what in it.
 *	colormap - see below
 *	frame buffer memory (video RAM)
 *	possible other stuff
 *
 * Like the cg3, the cg6 uses a Brooktree Video DAC (see btreg.h).
 */

/*
 * The layout of the THC.
 */
struct cg6_thc {
	u_int	thc_xxx0[512];	/* ??? */
	u_int	thc_hsync1;	/* horizontal sync timing */
	u_int	thc_hsync2;	/* more hsync timing */
	u_int	thc_hsync3;	/* yet more hsync timing */
	u_int	thc_vsync1;	/* vertical sync timing */
	u_int	thc_vsync2;	/* only two of these */
	u_int	thc_refresh;	/* refresh counter */
	u_int	thc_misc;	/* miscellaneous control & status */
	u_int	thc_xxx1[56];	/* ??? */
	u_int	thc_cursxy;	/* cursor x,y position (16 bits each) */
	u_int	thc_cursmask[32];	/* cursor mask bits */
	u_int	thc_cursbits[32];	/* what to show where mask enabled */
};

/* bits in thc_misc */
#define	THC_MISC_XXX0		0xfff00000	/* unused */
#define	THC_MISC_REVMASK	0x000f0000	/* cg6 revision? */
#define	THC_MISC_REVSHIFT	16
#define	THC_MISC_XXX1		0x0000e000	/* unused */
#define	THC_MISC_RESET		0x00001000	/* ??? */
#define	THC_MISC_XXX2		0x00000800	/* unused */
#define	THC_MISC_VIDEN		0x00000400	/* video enable */
#define	THC_MISC_SYNC		0x00000200	/* not sure what ... */
#define	THC_MISC_VSYNC		0x00000100	/* ... these really are */
#define	THC_MISC_SYNCEN		0x00000080	/* sync enable */
#define	THC_MISC_CURSRES	0x00000040	/* cursor resolution */
#define	THC_MISC_INTEN		0x00000020	/* v.retrace intr enable */
#define	THC_MISC_INTR		0x00000010	/* intr pending / ack bit */
#define	THC_MISC_XXX		0x0000000f	/* ??? */

/* cursor x / y position value for `off' */
#define	THC_CURSOFF	(65536-32)	/* i.e., USHRT_MAX+1-32 */

/*
 * This structure exists only to compute the layout of the CG6
 * hardware.  Each of the individual substructures lives on a
 * separate `page' (where a `page' is at least 4K), and many are
 * very far apart.  We avoid large offsets (which make for lousy
 * code) by using pointers to the individual interesting pieces,
 * and map them in independently (to avoid using up PTEs unnecessarily).
 */
struct cg6_layout {
	/* ROM at 0 */
	union {
		long un_id;		/* ID = ?? */
		char un_rom[65536];	/* 64K rom */
		char un_pad[0x200000];
	} cg6_rom_un;

	/* Brooktree DAC at 0x200000 */
	union {
		struct bt_regs un_btregs;
		char un_pad[0x040000];
	} cg6_bt_un;

	/* DHC, whatever that is, at 0x240000 */
	union {
		char un_pad[0x40000];
	} cg6_dhc_un;

	/* ALT, whatever that is, at 0x280000 */
	union {
		char un_pad[0x80000];
	} cg6_alt_un;

	/* FHC, whatever that is, at 0x300000 */
	union {
		char un_pad[0x1000];
	} cg6_fhc_un;

	/* THC at 0x301000 */
	union {
		struct cg6_thc un_thc;
		char un_pad[0x400000 - 0x1000];
	} cg6_thc_un;

	/* FBC at 0x700000 */
	union {
		char un_pad[0x1000];
	} cg6_fbc_un;

	/* TEC at 0x701000 */
	union {
		char un_pad[0x100000 - 0x1000];
	} cg6_tec_un;

	/* Video RAM at 0x800000 */
	char	cg6_ram[1024 * 1024];	/* approx.? */
};
