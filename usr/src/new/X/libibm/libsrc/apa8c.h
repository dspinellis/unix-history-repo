/* $Header: apa8c.h,v 10.1 86/11/19 10:45:18 jg Exp $ */
/* apa8c.h - Definitions and macros required to access APA8C
 *
 *	Defines required to support APA8C
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

#define SCREEN_BASE (0xF4D20000)	/* base address for screen bitmap */
#define XDEV_ID	  XDEV_IBMAPA8C		/* device id for info structure */

#define REAL_SCREEN_WIDTH 	1024	/* number of bits/line for apa8c */
#define REAL_SCREEN_HEIGHT	512	/* Number of lines on apa8c */

#define X_SCREEN_WIDTH 	720	/* number of bits/line for apa8c display */
#define X_SCREEN_HEIGHT	512	/* visible lines on the screen */

#define SCREEN_DEVICE   "/dev/apa8c"	/* device name */
#define MOUSE_DEVICE	"/dev/msapa8c"	/* device name of mouse for display */

#define CURSOR_TYPE	SOFT_CURSOR	/* apa8c uses software cursor */

#define DISPLAY_INIT()	apa8c_init()	/* display initialization routine */

/*
 * Control register (short)
 */

#define APA8C_FC 0xF0000150

/*
 * Data mask registers (2 each 8 bits)
 */

#define APA8C_DM 0xF0000152

/*
 * Color Plane Select register.
 */

#define CPS_REG 0xF0000154

/*
 * Foreground/ Background register.
 */

#define FGBG_REG 0xF0000156

/*
 * Video Look-up Table register.
 */

#define VLT_REG 0xF0000158

/*
 * bits 0-2  rotate count
 */

#define DCR_ROTMASK 0x0007

/*
 * bits 3-5  logic unit function control
 */
 
#define DCR_FCMASK	0x0038
#define DCR_FCOR	0x0028
#define DCR_FCNOR	0x0038
#define DCR_FCPA	0x0010
#define DCR_FCPB	0x0000
#define DCR_FCPNA	0x0030
#define DCR_FCPNB	0x0020

#define DEFAULT_FUNC	DCR_FCPNB
#define HIDDEN_FUNC     DCR_FCPA
#define RDWR_FUNC	DCR_FCPA

/*
 * 6-7 reserved
 */

/*
 * 8-9 memory mode
 */

#define DCR_MEMMODE	0x0300
#define DCR_SWR		0x0000
#define DCR_ISWR	0x0100
#define DCR_ADWR	0x0200
#define DCR_AURW	0x0300

/*
 * 10 inc/dec of the address counter
 */

#define DCR_DEC		0x0400
#define DCR_INC		~0x0400

/*
 * 11 x/y stepping
 */

#define DCR_X		0x0800
#define DCR_Y		~0x0800

/*
 * 12  1 means block transfer
 */

#define DCR_BT		0x1000

/*
 * 13  1 means interrupt enable
 */

#define DCR_IEN		0x2000

/*
 * 14  1 means Sync enable
 */

#define DCR_SEN		0x4000

/*
 * 15  1 means Video enable
 */

#define DCR_VEN		0x8000

/*
 * Data Mask Register masks (typically are the
 * inverse of each other)
 */

#define DM_A		0xFF00
#define DM_B		0x00FF

#define CLEAR_DM	0x0000
#define DEFAULT_DM	DM_B
#define HIDDEN_DM       DM_A
#define RDWR_DM		DM_A

/*
 * Values for the writemask.
 */

#define NOMASK 0x0000
#define MASKBYTE2 0x00FF
#define MASKBYTE1 0xFF00

/*
 * Values for the color plane select register.
 */

#define DEFAULT_CPS 0x001F;

/*
 * Values for the foreground/background register.
 */

#define DEFAULT_FGBG 0x00F0;

/*
 * Color definitions (not all colors are defined)
 */

#define DARK_RED	0x0100
#define DARK_GREEN	0x0400
#define DARK_BLUE	0x1000
#define RED		0x0200
#define GREEN		0x0800
#define BLUE		0x2000
#define LIGHT_RED	0x0300
#define LIGHT_GREEN	0x0C00
#define LIGHT_BLUE	0x3000
#define BLACK		0x0000
#define WHITE		0x3F00

/*
 * Defines to work with the Video Look-up table.
 * foreground = 1 bits     background = 0 bits
 * (Note: In "X" zero bits are black and one bits are white)
 */

#define VLT_SIZE 16
#define FG_COLOR WHITE
#define BG_COLOR DARK_GREEN

/*
 * Bitmap to BUS address offsets.  All address offsets in the bitmap area
 * must be multiplied by 2 before being used as BUS offsets.
 */

#define BM_TO_BUS(n)	((n) << 1)

/*
 * The physical bitmap addresses on the apa8C adaptor card
 * go from 0000 to 0xFFFF. (the size of a short or word, 16 bits addresses
 * these spots.
 */

#define MAXOFFSET 0xFFFF

/*
 * This is defined to be the last scan line in the hidden area.
 * It is needed because the side affect of setting the writemask registers
 * is the mask is also written to the bitmap.
 */

#define WRMASK_SCREEN_BASE \
	(*(u_short *)(SCREEN_BASE + BM_TO_BUS(MAXOFFSET - 1)))

#define SET_APA8C_FC(control_info)	{			\
	*(u_short *) APA8C_FC = (u_short)(control_info);	\
}

#define SET_APA8C_WRITEMASK(mask) {					\
	SET_APA8C_FC(DCR_SEN | DCR_VEN | DCR_ISWR | DCR_X | DCR_FCOR);	\
	WRMASK_SCREEN_BASE = (u_short) mask;				\
}

#define SET_APA8C_DATAMASK(mask) {		\
	*(u_short *)APA8C_DM = (u_short) mask;	\
}
