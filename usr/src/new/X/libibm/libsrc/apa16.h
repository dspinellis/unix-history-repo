/* $Header: apa16.h,v 10.1 86/11/19 10:45:01 jg Exp $ */
/* apa16.h - Definitions and macros required to access APA16
 *
 *	Defines required to support APA16
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#define SCREEN_BASE (IO_ADDR | 0x4D80000)	/* frame buffer address */
#define XDEV_ID	  XDEV_IBMAPA16		/* device id for info structure */

#define X_SCREEN_WIDTH 	1024		/* number of bits/line for apa16 */
#define X_SCREEN_HEIGHT	768		/* number of lines on the screen */

#define REAL_SCREEN_WIDTH   X_SCREEN_WIDTH	/* actual screen width */
#define REAL_SCREEN_HEIGHT  X_SCREEN_HEIGHT	/* actual screen height */

#define CURSOR_TYPE	HARD_CURSOR	/* apa16 uses hardware cursor */

#define DISPLAY_INIT()	apa16_init()	/* display initialization routine */

#define SCREEN_DEVICE   "/dev/apa16"	/* device number */
#define MOUSE_DEVICE	"/dev/msapa16"	/* device name of mouse for display */

/*
 * base I/O address
 */

#define IO_ADDR 0xF0000000

/*
 * Mode register.
 */

#define MR (IO_ADDR | 0x0D10)
#define MR_DEFAULT      0x8090

/*
 * Control/Status register.
 */

#define CSR (IO_ADDR | 0x0D12)
#define BACKGRND_BIT	0x0400
#define BLACK_ON_WHITE	(*(u_short *)(CSR) | BACKGRND_BIT)
#define WHITE_ON_BLACK	(*(u_short *)(CSR) & ~BACKGRND_BIT)
#define TOGGLE_BACKGRND (*(u_short *)(CSR) ^ BACKGRND_BIT)

/*
 * Video data output.
 */

#define VD_OUT (IO_ADDR | 0x0D1A)

/*
 * Enable video data output
 */

#define ENABLE_VD_OUT()      *(u_short *)VD_OUT = 0

/*
 * Reset adaptor register.
 */

#define RA_R    (IO_ADDR | 0x0D20)
#define RESET_APA16()        *(u_short *)RA_R = 0;
