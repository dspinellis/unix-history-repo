/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)qdreg.h	7.1 (Berkeley) 5/9/91
 */

/************************************************************************
 *									*
 *			Copyright (c) 1985, 1986 by			*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 *   This software is furnished under a license and may be used and	*
 *   copied  only  in accordance with the terms of such license and	*
 *   with the  inclusion  of  the  above  copyright  notice.   This	*
 *   software  or  any	other copies thereof may not be provided or	*
 *   otherwise made available to any other person.  No title to and	*
 *   ownership of the software is hereby transferred.			*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or	reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/* Dragon ADDER reg map */
/* ADDER register bit definitions */
/* Y_SCROLL_CONSTANT */

#define SCROLL_ERASE		0x2000
#define ADDER_SCROLL_DOWN	0x1000

/* ADDER status and interrupt enable registers [1], [2], [3] */

#define DISABLE 		0x0000
#define PAUSE_COMPLETE		0x0001
#define FRAME_SYNC		0x0002
#define INIT_COMPLETE		0x0004
#define RASTEROP_COMPLETE	0x0008

#define ADDRESS_COMPLETE	0x0010
#define RX_READY		0x0020
#define TX_READY		0x0040
#define ID_SCROLL_READY 	0x0080

#define TOP_CLIP		0x0100
#define BOTTOM_CLIP		0x0200
#define LEFT_CLIP		0x0400
#define RIGHT_CLIP		0x0800
#define NO_CLIP 		0x1000
#define VSYNC			0x2000

/* ADDER command register [8], [10] */

#define OCR_zero		0x0000
#define Z_BLOCK0		0x0000
#define OCRA			0x0000
#define OCRB			0x0004
#define RASTEROP		0x02c0
#define PBT			0x03c0
#define BTPZ			0x0bb0
#define PTBZ			0x07a0
#define DTE			0x0400
#define S1E			0x0800
#define S2E			0x1000
#define VIPER_Z_LOAD		0x01A0
#define ID_LOAD 		0x0100
#define CANCEL			0x0000
#define LF_R1			0x0000
#define LF_R2			0x0010
#define LF_R3			0x0020
#define LF_R4			0x0030

/* ADDER rasterop mode register [9] */

#define NORMAL			0x0000
#define LINEAR_PATTERN		0x0002
#define X_FILL			0x0003
#define Y_FILL			0x0007
#define BASELINE		0x0008
#define HOLE_ENABLE		0x0010
#define SRC_1_INDEX_ENABLE	0x0020
#define DST_INDEX_ENABLE	0x0040
#define DST_WRITE_ENABLE	0x0080

/* ADDER source 2 size register */

#define NO_TILE 		0x0080

/* External registers base addresses */

#define CS_UPDATE_MASK		0x0060
#define CS_SCROLL_MASK		0x0040

/* VIPER registers */

#define RESOLUTION_MODE 	0x0080
#define MEMORY_BUS_WIDTH	0x0081
#define PLANE_ADDRESS		0x0083
#define LU_FUNCTION_R1		0x0084
#define LU_FUNCTION_R2		0x0085
#define LU_FUNCTION_R3		0x0086
#define LU_FUNCTION_R4		0x0087
#define MASK_1			0x0088
#define MASK_2			0x0089
#define SOURCE			0x008a
#define SOURCE_Z		0x0000
#define BACKGROUND_COLOR	0x008e
#define BACKGROUND_COLOR_Z	0x000C
#define FOREGROUND_COLOR	0x008f
#define FOREGROUND_COLOR_Z	0x0004
#define SRC1_OCR_A		0x0090
#define SRC2_OCR_A		0x0091
#define DST_OCR_A		0x0092
#define SRC1_OCR_B		0x0094
#define SRC2_OCR_B		0x0095
#define DST_OCR_B		0x0096

/* VIPER scroll registers */

#define SCROLL_CONSTANT 	0x0082
#define SCROLL_FILL		0x008b
#define SCROLL_FILL_Z		0x0008
#define LEFT_SCROLL_MASK	0x008c
#define RIGHT_SCROLL_MASK	0x008d

/* VIPER register bit definitions */

#define EXT_NONE		0x0000
#define EXT_SOURCE		0x0001
#define EXT_M1_M2		0x0002
#define INT_NONE		0x0000
#define INT_SOURCE		0x0004
#define INT_M1_M2		0x0008
#define ID			0x0010
#define NO_ID			0x0000
#define WAIT			0x0020
#define NO_WAIT 		0x0000
#define BAR_SHIFT_DELAY 	WAIT
#define NO_BAR_SHIFT_DELAY	NO_WAIT


/* VIPER logical function unit codes */

#define LF_ZEROS		0x0000
#define LF_D_XOR_S		0x0006
#define LF_SOURCE		0x000A
#define LF_D_OR_S		0x000E
#define LF_ONES 		0x000F
#define INV_M1_M2		0x0030
#define FULL_SRC_RESOLUTION	0X00C0 /* makes second pass like first pass */

/* VIPER scroll register [2] */

#define SCROLL_DISABLE		0x0040
#define SCROLL_ENABLE		0x0020
#define VIPER_LEFT		0x0000
#define VIPER_RIGHT		0x0010
#define VIPER_UP		0x0040
#define VIPER_DOWN		0x0000

/* Adder scroll register */

#define ADDER_UP		0x0000
#define ADDER_DOWN		0x1000

/* Misc scroll definitions */

#define UP		0
#define DOWN		1
#define LEFT		2
#define RIGHT		3
#define NODIR		4
#define SCROLL_VMAX	31
#define SCROLL_HMAX	15
#define NEW		2
#define OLD		1
#define BUSY		1
#define DRAG		1
#define SCROLL		0

/* miscellaneous defines */

#define ALL_PLANES	0xffffffff
#define UNITY		0x1fff		 /* Adder scale factor */
#define MAX_SCREEN_X	1024
#define MAX_SCREEN_Y	864
#define FONT_HEIGHT	32

	struct adder {

	    /* adder control registers */

	    u_short register_address;	/* ADDER reg pntr for use by DGA */
	    u_short request_enable;	/* DMA request enables */
	    u_short interrupt_enable;	/* interrupt enables */
	    u_short status;		/* ADDER status bits */
	    u_short reserved1;		/* test function only */
	    u_short spare1;		/* spare address (what else?) */

	    u_short reserved2;		/* test function only */
	    u_short id_data;		/* data path to I/D bus */
	    u_short command;		/* ADDER chip command register */
	    u_short rasterop_mode;	/* sets rasterop execution modes */
	    u_short cmd;		/* duplicate path to above cmd reg */
	    u_short reserved3;		/* test function only */

	    /* scroll registers */

	    u_short ID_scroll_data;	/* I/D bus scroll data */
	    u_short ID_scroll_command;	/* I/D bus scroll command */
	    u_short scroll_x_min;	/* X scroll min - left boundary */
	    u_short scroll_x_max;	/* X scroll max - right boundary */
	    u_short scroll_y_min;	/* Y scroll min - upper boundary */
	    u_short scroll_y_max;	/* Y scroll max - lower boundary */
	    u_short pause;		/* Y coord to set stat when scanned */
	    u_short y_offset_pending;	/* vertical scroll control */
	    u_short y_scroll_constant;

	    /* update control registers */

	    u_short x_index_pending;	/* x pending index */
	    u_short y_index_pending;	/* y pending index */
	    u_short x_index_new;	/* new x index */
	    u_short y_index_new;		/* new y index */
	    u_short x_index_old;		/* old x index */
	    u_short y_index_old;		/* old y index */
	    u_short x_clip_min; 	/* left clipping boundary */
	    u_short x_clip_max; 	/* right clipping boundary */
	    u_short y_clip_min; 	/* upper clipping boundary */
	    u_short y_clip_max; 	/* lower clipping boundary */
	    u_short spare2;		/* spare address (another!) */

	    /* rasterop control registers */

	    u_short source_1_dx;	/* source #1 x vector */
	    u_short source_1_dy;	/* source #1 y vector*/
	    u_short source_1_x; 	/* source #1 x origin */
	    u_short source_1_y; 	/* source #1 y origin */
	    u_short destination_x;	/* destination x origin */
	    u_short destination_y;	/* destination y origin */
	    u_short fast_dest_dx;	/* destination x fast vector */
	    u_short fast_dest_dy;	/* destination y fast vector */
	    u_short slow_dest_dx;	/* destination x slow vector */
	    u_short slow_dest_dy;	/* destination y slow vector */
	    u_short fast_scale; 	/* scale factor for fast vector */
	    u_short slow_scale; 	/* scale factor for slow vector */
	    u_short source_2_x; 	/* source #2 x origin */
	    u_short source_2_y; 	/* source #2 y origin */
	    u_short source_2_size;	/* source #2 height & width */
	    u_short error_1;		/* error regs (?) */
	    u_short error_2;

	    /* screen format control registers */

	    u_short y_scan_count_0;	/* y scan counts for vert timing */
	    u_short y_scan_count_1;
	    u_short y_scan_count_2;
	    u_short y_scan_count_3;
	    u_short x_scan_conf;	/* x scan configuration */
	    u_short x_limit;
	    u_short y_limit;
	    u_short x_scan_count_0;	/* x scan count for horiz timing */
	    u_short x_scan_count_1;
	    u_short x_scan_count_2;
	    u_short x_scan_count_3;
	    u_short x_scan_count_4;
	    u_short x_scan_count_5;
	    u_short x_scan_count_6;
	    u_short sync_phase_adj;	/* sync phase (horiz sync count) */
	};

/*---------------------
* DUART definitions */

	/* command definitions */

#define EN_RCV		0x01
#define DIS_RCV 	0x02
#define EN_XMT		0x04
#define DIS_XMT 	0x08
#define RESET_M 	0x10
#define RESET_RCV	0x20
#define RESET_XMT	0x30
#define RESET_ERR	0x40
#define RESET_BD	0x50
#define START_BREAK	0x60
#define STOP_BREAK	0x70

	/* interupt bit definitions */

#define EI_XMT_A	0x01
#define EI_RCV_A	0x02
#define EI_XMT_B	0x10
#define EI_RCV_B	0x20

#define XMT_RDY_A	0x01
#define RCV_RDY_A	0x02
#define XMT_RDY_B	0x10
#define RCV_RDY_B	0x20

	/* status register bit defintions */

#define RCV_RDY 	0x01
#define FIFO_FULL	0x02
#define XMT_RDY 	0x04
#define XMT_EMT 	0x08
#define OVER_ERR	0x10
#define ERR_PARITY	0x20
#define FRAME_ERR	0x40
#define RCVD_BREAK	0x80


	struct duart {

	    /* channel A - LK201 */

	    short modeA;		/* ch.A mode reg (read/write) */
	    short statusA;		/* ch.A status reg (read) */
#define clkselA statusA 		/* ch.A clock slect reg (write) */
	    short cmdA; 		/* ch.A command reg (write) */
	    short dataA;		/* rcv/xmt data ch.A (read/write) */
	    short inchng;		/* input change state reg (read) */
#define auxctl inchng			/* auxiliary control reg (write) */
	    short istatus;		/* interrupt status reg (read) */
#define imask istatus			/* interrupt mask reg (write) */
	    short CThi; 		/* counter/timer hi byte (read) */
#define CTRhi CThi			/* counter/timer hi reg (write) */
	    short CTlo; 		/* counter/timer lo byte (read) */
#define CTRlo CTlo			/* counter/timer lo reg (write) */

	    /* channel B - pointing device */

	    short modeB;		/* ch.B mode reg (read/write) */
	    short statusB;		/* ch.B status reg (read) */
#define clkselB statusB 		/* ch.B clock select reg (write) */
	    short cmdB; 		/* ch.B command reg (write) */
	    short dataB;		/* ch.B rcv/xmt data (read/write) */
	    short rsrvd;
	    short inport;		/* input port (read) */
#define outconf inport			/* output port config reg (write) */
	    short strctr;		/* start counter command (read) */
#define setbits setctr			/* output bits set command (write) */
	    short stpctr;		/* stop counter command (read) */
#define resetbits stpctr		/* output bits reset cmd (write) */

};
