/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
#if	RCSID && IN_MAIN
static char *trcsid="$XConsortium: topcat.h,v 1.4 88/09/06 15:22:02 jim Exp $";
#endif	RCSID

/*
 *  topcat.h	- access to topcat chip
 *
 *  This module is part of the low-level graphics primitives 
 *  for the Hewlett-Packard 9000 Series 300.
 *
 *  Hewlett-Packard Instrument Systems Lab - Loveland, Colorado
 *
 *  (c) Copyright 1986 Hewlett-Packard Company.  All rights are
 *  reserved.  Copying or other reproduction of the program except
 *  for archival purposes is prohibited without prior written
 *  consent of Hewlett-Packard Company.
 *
 * $Log: topcat.h,v $
 * Revision 1.1  1992/09/30  03:20:46  root
 * Initial revision
 *
 * Revision 1.2  89/02/15  19:54:23  stoller
 * hpbsd change: Add id_second field to topcat struct so we can determine
 * if we are running on a Catseye or Topcat.
 * gcc change: waitbusy loop was being optimized away.
 * 
 * Revision 1.1  89/01/16  15:15:36  stoller
 * Initial revision
 * 
 * Revision 1.4  88/09/06  15:22:02  jim
 * XConsortium
 * 
 * Revision 1.3  88/08/17  13:40:20  jim
 * new version from hp
 * 
 * Revision 1.10  88/01/27  15:58:17  15:58:17  harry (Harry Phinney)
 * added extern decleration of tcPolyOptText()
 * 
 * Revision 1.9  88/01/12  16:40:38  16:40:38  harry (Harry Phinney)
 * Change for optimized variable width fonts
 * 
 * Revision 1.8  87/12/21  17:57:21  17:57:21  harry (Harry Phinney)
 * Merging with MIT sources
 * 
 * 
 * Revision 1.1  87/12/16  13:35:00  hp
 * Initial revision
 * 
 * Revision 1.1  87/12/14  14:48:02  hp
 * Initial revision
 * 
 * Revision 1.7  87/12/02 17:14:08 GMT  harry
 * Added extern declarations for the text and rectangle fill functions
 * 
 * Revision 1.6  87/11/29  12:58:54  12:58:54  harry (Harry Phinney)
 * Parameterized waitbusy for gp_hardware & surrounded both params w/ parens
 * 
 * Revision 1.5  87/11/04  14:30:12  14:30:12  andreas (Jim Andreas)
 * graphics.h is doubly included (by topcat.h and topcat.c)
 * removed include of graphics.h
 * 
 * Revision 1.4  87/10/26  14:55:38  14:55:38  bennett
 * HP frame buffer reorg;
 * 
 * Revision 1.3  87/10/20  11:56:53  11:56:53  leichner (Robert C. Leichner)
 * Made topcatMover an extern procedure.
 * 
 * Revision 1.2  87/10/08  15:24:13  15:24:13  bennett
 * Define some new macros to reach into the bowels of the screen and pull
 * out some hardware specific into;
 * 
 * Revision 1.1  87/09/22  18:04:56  18:04:56  leichner (Robert C. Leichner)
 * Initial revision
 * 
 * Revision 3.4  87/09/14  18:11:06  18:11:06  leichner (Robert C. Leichner)
 * Use new definition for screen->devPrivate and no longer use screens
 * array.
 * 
 * Revision 3.3  87/09/03  14:30:31  14:30:31  leichner (Robert C. Leichner)
 * Moved several defines into .h file. Also added extern declarations
 * from topcat.c. Externs are used by topcatgc.c.
 * 
 * Revision 3.2  87/08/31  12:20:00  12:20:00  leichner (Robert C. Leichner)
 * Assorted fixes to allow multiple screens. 
 * Also eliminated extra parameter in topcatUninstallColormap.
 * 
 * Revision 3.1  87/08/28  17:07:32  17:07:32  leichner (Robert C. Leichner)
 * consolidated device dependant info in a single structure.
 * 
 * Revision 3.0  87/08/12  16:15:08  16:15:08  bennett
 * beta 3.0
 * 
 * Revision 1.1  87/07/02  10:10:25  bennett
 * Initial revision
 * 
 * Revision 1.1  87/06/01  10:16:32  10:16:32  palevich (John H. Palevich)
 * Initial revision
 * 
 * Revision 1.2  86/08/20  13:06:40  13:06:40  dan ()
 * Checkpoint before output speed optimization.
 * 
 * Revision 1.1  86/07/22  16:37:55  16:37:55  dan ()
 * Initial revision
 * 
 * Revision 1.2  86/07/17  07:33:37  07:33:37  dan ()
 * Fix the replacement rule locations.
 * .,
 * 
 * Revision 1.1  86/06/04  12:34:01  12:34:01  dan ()
 * Initial revision
 * 
 * Revision 1.4  86/04/03  10:21:13  tw
 * Checkpoint before revising to use bufalloc.
 * 
 * Revision 1.3  86/04/01  15:45:02  tw
 * Finally got colormap hardware registers mapped correctly.
 * 
 * Revision 1.2  86/03/04  17:26:32  tw
 * Changed replacement rules to be u_chars instead of u_shorts.
 * 
 * Revision 1.1  86/02/27  16:05:14  tw
 * Initial revision
 * 
 * 
 */
 
#include <sys/types.h>

/*
 * list of topcat external procedures
 */

extern RegionPtr tcCopyPlane();

extern void tcFillBoxSoild();
extern void tcPolyFillRect();

extern void tcWholeGlyph();
extern void tcImageVarGlyph();
extern void tcImageGlyphBlt();

extern void tcPutImage();
extern void tcSolidFS();
extern void tcPaintWindow();

extern Bool topcatCreateGC();

extern void tcImageOptTEText8();
extern void tcImageOptText8();
extern int tcPolyOptText8();
extern void tcPolyFillStippledRect();

extern void topcatZeroLine();
extern void topcatZeroDash();

extern void topcatInstallColormap();
extern void topcatUninstallColormap();
extern int topcatListInstalledColormaps();
extern void topcatStoreColors();
extern void topcatResolvePseudoColor();

#include "../hpDisplays.h"

/*
 * Map of the topcat chip in memory ...
 */

typedef VOLATILE struct {
    u_short id_reset;		/* id and reset register 	*/ /* 0x01 */
    u_char filler2;
    u_char interrupts;		/* interrupts			*/ /* 0x03 */
    u_char filler2a;
    u_char t_memwide;		/* top half frame buf width	*/ /* 0x05 */
    u_char filler2b;
    u_char b_memwide;		/* bottom half frame buf width	*/ /* 0x07 */
    u_char filler2c;
    u_char t_memhigh;		/* top half frame buf height 	*/ /* 0x09 */
    u_char filler2d;
    u_char b_memhigh;		/* bot half frame buf height	*/ /* 0x0b */
    u_char filler2e;
    u_char t_dispwide;		/* top half display width	*/ /* 0x0d */
    u_char filler2f;
    u_char b_dispwide;		/* bot half display width	*/ /* 0x0f */
    u_char filler2g;
    u_char t_disphigh;		/* top half display height	*/ /* 0x11 */
    u_char filler2h;
    u_char b_disphigh;		/* bot half display height	*/ /* 0x13 */
    u_char filler2i;
    u_char id_second;		/* secondary id 5=LCC 6=HRC 	*/ /* 0x15 */
    u_char filler2j;            /* 7=HRM, 9=319X		*/
    u_char bits;		/* 0 square pixels, 1 double hi */ /* 0x17 */
#if 1
    u_char filler3[67];
#else
    /* catseye stuff */
    u_char filler2k;
    u_char byte_pixel;          /* byte/pixel at powerup        *//* 0x19 */
    u_char filler2l;
    u_char id_crtc;             /* CRTC ID IRIS                 *//* 0x1B */
    u_char filler2m;
    u_char rom_rev;             /* rom revision                 *//* 0x1D */
    u_char filler3[57];
    u_char t_cmapaddr;          /* color map address (MSB)      *//* 0x57 */
    u_char filler2n;
    u_char b_cmapaddr;          /* color map address (LSB)      *//* 0x59 */
    u_char filler2o;
#endif
    u_char num_planes;		/* number of color planes	*/ /* 0x5b */
    u_char id_font[16356];	/* display id, font, ...	*/

/* plane status registers (read only) at 0x0040*/

    u_char vert_blank;		/* vertical blanking		*/ /* 0x4040 */
    u_char filler01[3];
    u_char move_active;		/* window move active		*/
    u_char filler02[3];
    u_char vert_request;	/* vert retrace intr request	*/
    u_char filler03[3];
    u_char move_request;	/* window move intr request	*/   
    u_char filler04[51];

/* plane control registers (read/write) at 0x0080 */

    u_char nblank;		/* video output enable		*/ /* 0x4080 */
    u_char filler4[3];
    u_char enable_sync;		/* sync enable			*/
    u_char filler5[3];
    u_char write_enable;	/* enable writes		*/
    u_char filler6[3];
    u_char read_enable;		/* enable reads			*/
    u_char filler7[3];
    u_char frame_buf_write_enable; /* frame buffer write enable	*/
    u_char filler8[3];
    u_char vert_enable;		/* vertical retrace int enable	*/
    u_char filler9[3];
    u_char move_enable;		/* window move interrupt enable */
    u_char filler10[3];
    u_char  start_move;		/* start window move		*/
    u_char filler11[3];
    u_char blink;		/* blink control		*/
    u_char filler12[11];
    u_char cursor_on;		/* turn cursor on		*/
    u_char filler13[61];

/* raster op control registers (read/write, word wide) at 0x00ea */

    u_char    pixel_write_replacement_rule;			  /* 0x40ea */
    u_char fillerA[4];
    u_char    window_move_replacement_rule;	/* this is 0x40ef */
    u_char fillerB[1];
    u_short source_x;
    u_short fillerC;
    u_short source_y;
    u_short fillerD;
    u_short dest_x;
    u_short fillerE;
    u_short dest_y;
    u_short fillerF;
    u_short window_width;
    u_short fillerG;
    u_short window_height;
    u_short fillerH;
    u_short cursor_x;
    u_short fillerI;
    u_short cursor_y;
    u_short fillerJ;
    u_short cursor_length;
    u_char filler99[7919];

/* color map status (color systems only) at 0x2002 */

    u_char  colormap_status;	/* bit 2 set if RAM is busy	*/ /* 0x6003 */
    u_char  filler999[172];

/* color map registers (color systems only) at 0x20B2 */	   /* 0x60b2 */

    u_short filler1A;
    u_short red_data;
    u_short green_data;
    u_short blue_data;
    u_short color_index;
    u_short plane_mask;
    u_char  filler1E[52];
    u_short colormap_writestrobe;
} TOPCAT;

/* additional info for topcat control. */
#define IDREG	(0x1)
#define INTREG	(0x3)
#define RAI0	(0x23)
#define RAI1	(0x27)
#define RAI2	(0x2b)
#define RAI3	(0x2f)

#define TC_VIDEO_ON  0xff
#define TC_VIDEO_OFF 0x00

/* private field for topcat display */
typedef struct {
    TOPCAT  *topcatDev;        /* pointer to device hardware */
    ColormapPtr InstalledMap;  /* pointer to installed colormap */
    void (*UpdateColormap)();  /* cmap update differs for topcat & catseye */
} topcatPriv;
typedef topcatPriv *topcatPrivPtr;

#define waitbusy(pMask, gp_hardware) \
		while ((pMask) & (gp_hardware)->move_active) \
			{ int i; for (i=0;i<20;i++); }

#define getTcHardware(pScreen) \
    (((topcatPrivPtr) getPrivScreenPtr(pScreen)->pHardwareScreen)->topcatDev)

#ifndef	GXclear

/* defines for replacement rules -- same as for X window system */

#define	GXclear		0x0		/* 0 			*/
#define GXand		0x1		/* src AND dst 		*/
#define GXandReverse	0x2		/* src AND NOT dst	*/
#define GXcopy		0x3		/* src 			*/
#define GXandInverted	0x4		/* NOT src AND dst	*/
#define GXnoop		0x5		/* dst			*/
#define GXxor		0x6		/* src XOR dst		*/
#define GXor		0x7		/* src OR dst		*/
#define GXnor		0x8		/* NOT src AND NOT dst	*/
#define GXequiv		0x9		/* NOT src XOR dst	*/
#define GXinvert	0xa		/* NOT dst		*/
#define GXorReverse	0xb		/* src OR NOT dst	*/
#define GXcopyInverted	0xc		/* NOT src		*/
#define GXorInverted	0xd		/* NOT src OR dst	*/
#define GXnand		0xe		/* NOT src OR NOT dst	*/
#define GXset		0xf		/* 1			*/

#endif	/* GXclear */

#ifdef	TEST_DEFINES

#include <stdio.h>

TOPCAT t;

main ()
{
    int T;

    T = (int) &t;

    printf ("          \tGot\tWanted\n");
    printf ("topcat at:\t0x%x\t0x%x\n", T);
    printf ("vert_blank at:\t0x%x\t0x%x\n", &t.vert_blank,  0x4040 + T);
    printf ("nblank at:\t0x%x\t0x%x\n", &t.nblank,  0x4080 + T);
    printf ("cursor_on at:\t0x%x\t0x%x\n", &t.cursor_on,  0x40ac + T);
    printf ("pixel_wrr at:\t0x%x\t0x%x\n", &t.pixel_write_replacement_rule,  0x40ea + T);
    printf ("cursor_len at:\t0x%x\t0x%x\n", &t.cursor_length,  0x4112 + T);
    printf ("cmap_status at:\t0x%x\t0x%x\n", &t.colormap_status,  0x6003 + T);
    printf ("red_data at:\t0x%x\t0x%x\n", &t.red_data,  0x60b2 + T);
    printf ("green_data at:\t0x%x\t0x%x\n", &t.green_data,  0x60b4 + T);
    printf ("blue_data at:\t0x%x\t0x%x\n", &t.blue_data,  0x60b6 + T);
    printf ("write_str at:\t0x%x\t0x%x\n", &t.colormap_writestrobe,  0x60f0 + T);
    fflush (stdout);
    exit (0);
}

#endif	/* TEST_DEFINES */
