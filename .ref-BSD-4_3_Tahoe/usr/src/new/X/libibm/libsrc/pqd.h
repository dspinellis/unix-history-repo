/* $Header: pqd.h,v 10.1 86/11/19 10:46:07 jg Exp $ */
/* pqd.h - Definition and macros required to access PQD
 *
 *	Defines required to support PQD
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

#define SCREEN_BASE	0xF40C0000	/* base address of frame buffer */
#define XDEV_ID		XDEV_IBMPQD	/* device id for info structure */

#define REAL_SCREEN_WIDTH 	1024	/* number of bits/line for pqd */
#define REAL_SCREEN_HEIGHT	1024	/* Number of lines on pqd */

#define X_SCREEN_WIDTH 	1024		/* visible bits/line for pqd */
#define X_SCREEN_HEIGHT	800		/* visible lines on the screen */

#define SCREEN_DEVICE   "/dev/pqd"	/* device name */
#define MOUSE_DEVICE	"/dev/mspqd"	/* device name of mouse for display */

#define CURSOR_TYPE	HARD_CURSOR	/* pqd uses hardware cursor */

#define DISPLAY_INIT()	pqd_init()	/* display initialization routine */

#define PQD_IOADDR 0xF0000320		/* base address for controller regs */

#define PQD_WM1		(PQD_IOADDR | 0x00000006)
#define PQD_WM0		(PQD_IOADDR | 0x00000008)
#define PQD_PCR		(PQD_IOADDR | 0x0000000A)
#define PQD_VCR		(PQD_IOADDR | 0x0000000C)
#define PQD_WCR		(PQD_IOADDR | 0x0000000E)

/*
 * macros for controlling access to plane bitmaps
 */

#define SELECT_PLANE(n)	*(unsigned short *)PQD_PCR = plane_no[n]

#define SET_PLANE(RW)	*(unsigned short *)PQD_PCR = RW

#define RW_PLANE0	0x1000
#define RW_PLANE1	0x2100
#define RW_PLANE2	0x4200
#define RW_PLANE3	0x8300

static unsigned short plane_no[] = {
	RW_PLANE0,
	RW_PLANE1,
	RW_PLANE2,
	RW_PLANE3
};

