#ifndef lint
static char *rcsid_bitblt_apa16_c = "$Header: bitblt_apa16.c,v 10.1 86/11/19 10:50:57 jg Exp $";
#endif	lint
/*
 * This file contains routines to do hardware bit block transfers (bitblt's)
 *  on the APA-16.
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 *
 * Written by Daniel Stone, Brown University/IRIS, April 23, 1986.
 */

#if (APA16 && USE_APA16_HDWR)

#include "bitblt_int.h"
#include "bitblt_apa16.h"

/*
 * This array indicates whether or not the coorisponding combination rule
 * can be done in hardware.  A non-zero indicates the hardware can do it.
 */
#define NO_FUNC		0

unsigned short execute_cmd[32] = {
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_CLEAR),	    /* DstClear 0 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_DSTandSRC),/* SrcAnd   1 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_NotDSTandSRC),/*SrcAndNotDst2*/
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_COPYSRC),  /* SrcCopy 3 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_DSTandNotSRC),/* NotSrcAnd 4 */
	NO_FUNC,	/* DstCopy         5 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_DSTxorSRC),   /* SrcXor 6 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_DSTorSRC),    /* SrcOr 7 */
	NO_FUNC,	/* NotSrcAndNotDst 8 */
	NO_FUNC,	/* NotSrcXor       9 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_NotDST),       /*NotDstCopy 10*/
	NO_FUNC,	/* SrcOrNotDst	   11*/
	NO_FUNC,	/* NotSrcCopy      12 */
	NO_FUNC,	/* NotSrcOr        13 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_RECTCP,LF_NotDSTorNotSRC),
							/* NotSrcOrNotDst 14*/
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_SET),   /* DstSet          15 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_CLEAR),/* TileDstClear     16 */
	NO_FUNC,	/* TileAnd          17 */
	NO_FUNC,	/* TileAndNotDst    18 */
	NO_FUNC,	/* TileCopy         19 */
	NO_FUNC,	/* NotTileAnd       20 */
	NO_FUNC,	/* TileDstCopy      21 */
	NO_FUNC,	/* TileXor          22 */
	NO_FUNC,	/* TileOr           23 */
	NO_FUNC,	/* NotTileAndNotDst 24 */
	NO_FUNC,	/* NotTileXor       25 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_NotDST),/*TileNotDstCopy   26 */
	NO_FUNC,	/* TileOrNotDst     27 */
	NO_FUNC,	/* NotTileCopy      28 */
	NO_FUNC,	/* NotTileOr        29 */
	NO_FUNC,	/* NotTileOrNotDst  30 */
	BUILD_EXCMD(DECR_QUE_COUNT,ROT_WRDST,LF_SET),/* TileDstSet       31 */
};

/*
 * Set up hardware to do screen to screen copy then do it.
 * Returns 1 if the hardware couldn't handle it (should never happen) and 0
 * if the blt was successful.
 */
apa16_StoS(sv)
register Blt_sysdata *sv;	/* System variables */
{
	register unsigned short *Qptr = (unsigned short *)LAST_QUE_APA16BASE;
	register tmp;	/* just a garbage register */
	int c;

	/*
	 * Check the destination and source bitmap pointers to the screen.
	 * If they are different than the start address of the screen then
	 * offset each rectangle.
	 */
	if (sv->dst.data > (unsigned short *)APA16BASE) {
		/*
		 * Take apart the address.  Note the address is in bytes
		 * corner_x is in bits so it must be multiplied by 8.
		 */
		sv->dst.rect.corner_x += (((long)sv->dst.data & 0x7F) << 3);
		sv->dst.rect.corner_y += (((long)sv->dst.data & 0x7FF80) >> 7);
	}
 
	if (sv->src.data > (unsigned short *)APA16BASE) {
		/*
		 * Take apart the address.  Note the address is in bytes
		 * corner_x is in bits so it must be multiplied by 8.
		 */
		sv->src.rect.corner_x += (((long)sv->src.data & 0x7F) << 3);
		sv->src.rect.corner_y += (((long)sv->src.data & 0x7FF80) >> 7);
	}

	/*
	 * Wait for the previous command to finish.
	 */
	WAIT_QUE(c,sv->rule);

	/*
	 * Load the the instructions that tell the queue command
	 * processor to load:
	 *      - The X and Y destination registers with the bottom
	 *	  right corner of the screen area where the
	 * 	  destination rectangle resides.
	 *
	 *      - The X and Y source registers with the bottom right
	 *        corner of the source screen area.
	 *
	 *      - The destination width and height registers with the
	 *	  width and height of the rectangle to be moved.
	 *
	 * Then load an execute command instruction with one of the copy source
	 * logic functions and a Raster Operation Type (ROT) of copy screen
	 * to screen.
	 */
	*Qptr-- = BUILD_REGLOAD(XDST_REG,sv->dst.rect.corner_x+1);
	*Qptr-- = BUILD_REGLOAD(RECT_YDST_REG,sv->dst.rect.corner_y+1);
	*Qptr-- = BUILD_REGLOAD(XSRC_REG,sv->src.rect.corner_x+1);
	*Qptr-- = BUILD_REGLOAD(YSRC_REG,sv->src.rect.corner_y+1);
	*Qptr-- = BUILD_REGLOAD(WIDTH_REG,(sv->width & 1023));
	*Qptr-- = BUILD_REGLOAD(RECT_HEIGHT_REG,(sv->height & 1023));
	*Qptr = execute_cmd[sv->rule];

	*QUE_PTR_R = SPTR_TO_QPTR(LAST_QUE_APA16BASE);

	INCR_QUE_COUNT(tmp);
	return(0);
}

#ifdef notdef
/*
 * Set up hardware to do memory to screen copy then do it.
 * Returns 1 if the hardware couldn't handle it (should never happen) and
 * 0 if the blt was successful.
 */
apa16_MtoS(sv)
register Blt_sysdata *sv;	/* System variables */
{
	register unsigned short *Qptr = (unsigned short *)LAST_QUE_APA16BASE;
	register tmp;	/* just a garbage register */
	int c;

	/*
	 * Wait for the previous command to finish.
	 */
	WAIT_QUE(c,sv->rule);

	/*
	 * Load the the instructions that tell the queue command
	 * processor to load:
	 *	- The high,middle and low bytes of the source address into
	 *	  the APA-16 hardware registers.
	 *
	 *      - The starting bit of the starting source word.
	 *
	 *      - The number of shorts needed to increment the destination
	 *	  pointer to the next scanline.
	 *
	 *	- The X and Y destination corner points plus 1 because
	 *	  it uses these points exclusively.
	 *
	 *      - The destination width and height registers with the
	 *	  width and height of the rectangle to be moved.
	 *
	 * Then load an execute command instruction with one of the copy source
	 * logic functions and a Raster Operation Type (ROT) of memory to
	 * screen (TDMA).
	 */
	*Qptr-- = BUILD_REGLOAD(REG_HIGHBYTE,HIGH_BYTE(sv->src.data));
	*Qptr-- = BUILD_REGLOAD(REG_LOWBYTE,MID_BYTE(sv->src.data));
	*Qptr-- = BUILD_REGLOAD(REG_Y_SRC,LOW_BYTE(sv->src.data));

	*Qptr-- = BUILD_REGLOAD(REG_GEN_B,MOD_BPW(sv->src.rect.origin_x));
	*Qptr-- = BUILD_REGLOAD(REG_GEN_A,sv->src.nextline);

	*Qptr-- = BUILD_REGLOAD(REG_X_DST,sv->dst.rect.corner_x+1);
	*Qptr-- = BUILD_REGLOAD(REG_Y_DST,sv->dst.rect.corner_y+1);

	*Qptr-- = BUILD_REGLOAD(REG_X_SRC,(sv->width & 1023));
	*Qptr-- = BUILD_REGLOAD(REG_Y_LOOP,(sv->height & 1023));

	/*
	 * This HACK works, but only because ROT_TDDMA has all the bits
	 * on anyway.  To do this right one would do:
	 * 	(execute_cmd[sv->rule] & 0x03f0) | 0x03f0
	 */
	*Qptr = (execute_cmd[sv->rule] | ROT_TDDMA);

	/*
	 * Set up the queue pointer register and execute the command.
	 */
	*QUE_PTR_R = SPTR_TO_QPTR(LAST_QUE_APA16BASE);

	/*
	 * I think I have to enable DMA first.
	 */
	ENABLE_DMA_PROC(tmp);
	INCR_QUE_COUNT(tmp);

	return(0);
}
#endif notdef

/*
 * Depending on the tile rule, copy the tile to the hidden area of the
 * screen and then set up a string of queue commands to move the tile to the
 * screen.
 */
apa16_copyTile(sv)
register Blt_sysdata *sv;	/* System variables */
{
	register unsigned short *Qptr = (unsigned short *)LAST_QUE_APA16BASE;
	register tmp;	/* just a garbage register */
	int c;

	/*
	 * Wait for the previous command to finish.
	 */
	WAIT_QUE(c,sv->rule);

	/*
	 * Check and see if this tile combination rule is a destination only
	 * rule like DstClear, TileDstClear, NotDstCopy, TileNotDstCopy,
	 * DstSet, TileDstSet.
	 */
	if (IS_ROT_WRDST(execute_cmd[sv->rule])) {
		*Qptr-- = BUILD_REGLOAD(XDST_REG,sv->dst.rect.corner_x+1);
		*Qptr-- = BUILD_REGLOAD(YDST_REG,sv->dst.rect.corner_y+1);
		*Qptr-- = BUILD_REGLOAD(WIDTH_REG,(sv->width & 1023));
		*Qptr-- = BUILD_REGLOAD(HEIGHT_REG,(sv->height & 1023));
		*Qptr = execute_cmd[sv->rule];

		/*
		 * Set up the queue pointer register and execute the command.
		 */
		*QUE_PTR_R = SPTR_TO_QPTR(LAST_QUE_APA16BASE);

		INCR_QUE_COUNT(tmp);
	}
	else {
		/*
		 * Must copy the tile to the hidden area of the screen
		 * and then setup a series of que commands to copy it to
		 * the screen.
		 */
		return(1);
	}

	return(0);
}
#endif APA16 && USE_APA16_HDWR
