#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	StoreCursors - This subroutine stores all the cursors used
 *	by the X Window System window manager (xwm).
 *
 *	File:		StoreCursors.c
 */

#include "xwm.h"

#ifndef lint
static char *rcsid_StoreCursors_c = "$Header: StoreCursors.c,v 10.3 86/02/01 16:10:10 tony Rel $";
#endif

/*
 * Include the cursor definition files.
 */
#include "../cursors/arrow_cross.cursor"
#include "../cursors/arrow_cross_mask.cursor"
#include "../cursors/ll_angle.cursor"
#include "../cursors/ll_angle_mask.cursor"
#include "../cursors/ul_angle.cursor"
#include "../cursors/ul_angle_mask.cursor"
#include "../cursors/lr_angle.cursor"
#include "../cursors/lr_angle_mask.cursor"
#include "../cursors/ur_angle.cursor"
#include "../cursors/ur_angle_mask.cursor"
#include "../cursors/top_tee.cursor"
#include "../cursors/top_tee_mask.cursor"
#include "../cursors/left_tee.cursor"
#include "../cursors/left_tee_mask.cursor"
#include "../cursors/bottom_tee.cursor"
#include "../cursors/bottom_tee_mask.cursor"
#include "../cursors/right_tee.cursor"
#include "../cursors/right_tee_mask.cursor"
#include "../cursors/dot.cursor"
#include "../cursors/dot_mask.cursor"
#include "../cursors/circle.cursor"
#include "../cursors/circle_mask.cursor"
#include "../cursors/xterm.cursor"
#include "../cursors/xterm_mask.cursor"
#include "../cursors/icon.cursor"
#include "../cursors/icon_mask.cursor"

/*
 * Store all the cursors into global variables.
 */
StoreCursors()
{
    /*
     * Main xwm cursor and movement cursor.
     */
    ArrowCrossCursor = XCreateCursor(
    	arrow_cross_width, arrow_cross_height, 
    	arrow_cross_bits, arrow_cross_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );
    if (ArrowCrossCursor == FAILURE) {
	Error("StoreCursors -> Unable to store ArrowCrossCursor.");
    }

    /*
     * Upper left angle cursor used to resize windows.
     */
    ULAngleCursor = XCreateCursor(
    	ul_angle_width, ul_angle_height, 
    	ul_angle_bits, ul_angle_mask_bits, 
	2, 2,
	BlackPixel, WhitePixel,
	CursorFunc
    );
    if (ULAngleCursor == FAILURE) {
	Error("StoreCursors -> Unable to store ULAngleCursor.");
    }

    /*
     * Lower left angle cursor used to resize windows.
     */
    LLAngleCursor = XCreateCursor(
    	ll_angle_width, ll_angle_height, 
    	ll_angle_bits, ll_angle_mask_bits, 
	2, 15,
	BlackPixel, WhitePixel,
	CursorFunc
    );
    if (LLAngleCursor == FAILURE) {
	Error("StoreCursors -> Unable to store LLAngleCursor.");
    }

    /*
     * Lower right angle cursor used to resize windows.
     */
    LRAngleCursor = XCreateCursor(
    	lr_angle_width, lr_angle_height, 
    	lr_angle_bits, lr_angle_mask_bits, 
	15, 15,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (LRAngleCursor == FAILURE) {
	Error("StoreCursors -> Unable to store LRAngleCursor.");
    }

    /*
     * Upper right angle cursor used to resize windows.
     */
    URAngleCursor = XCreateCursor(
    	ur_angle_width, ur_angle_height, 
    	ur_angle_bits, ur_angle_mask_bits, 
	15, 2,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (URAngleCursor == FAILURE) {
	Error("StoreCursors -> Unable to store URAngleCursor.");
    }

    /*
     * Top tee cursor used to resize windows.
     */
    TopTeeCursor = XCreateCursor(
    	top_tee_width, top_tee_height, 
    	top_tee_bits, top_tee_mask_bits, 
	8, 2,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (TopTeeCursor == FAILURE) {
	Error("StoreCursors -> Unable to store TopTeeCursor.");
    }

    /*
     * Left tee cursor used to resize windows.
     */
    LeftTeeCursor = XCreateCursor(
    	left_tee_width, left_tee_height, 
    	left_tee_bits, left_tee_mask_bits, 
	2, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (LeftTeeCursor == FAILURE) {
	Error("StoreCursors -> Unable to store LeftTeeCursor.");
    }

    /*
     * Bottom tee cursor used to resize windows.
     */
    BottomTeeCursor = XCreateCursor(
    	bottom_tee_width, bottom_tee_height, 
    	bottom_tee_bits, bottom_tee_mask_bits, 
	8, 15,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (BottomTeeCursor == FAILURE) {
	Error("StoreCursors -> Unable to store BottomTeeCursor.");
    }

    /*
     * Right tee cursor used to resize windows.
     */
    RightTeeCursor = XCreateCursor(
    	right_tee_width, right_tee_height, 
    	right_tee_bits, right_tee_mask_bits, 
	15, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (RightTeeCursor == FAILURE) {
	Error("StoreCursors -> Unable to store RightTeeCursor.");
    }

    /*
     * Dot cursor used to lower windows.
     */
    DotCursor = XCreateCursor(
    	dot_width, dot_height, 
    	dot_bits, dot_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (DotCursor == FAILURE) {
	Error("StoreCursors -> Unable to store DotCursor.");
    }

    /*
     * Circle cursor used to raise windows.
     */
    CircleCursor = XCreateCursor(
    	circle_width, circle_height, 
    	circle_bits, circle_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (CircleCursor == FAILURE) {
	Error("StoreCursors -> Unable to store CircleCursor.");
    }

    /*
     * Text cursor used in icons.
     */
    TextCursor = XCreateCursor(
    	xterm_width, xterm_height, 
    	xterm_bits, xterm_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	CursorFunc
    );	
    if (TextCursor == FAILURE) {
	Error("StoreCursors -> Unable to store TextCursor.");
    }

    /*
     * Icon cursor used to iconify windows.
     */
    IconCursor = XCreateCursor(
    	icon_width, icon_height, 
    	icon_bits, icon_mask_bits, 
	8, 8,
	ITextForground, ITextBackground,
	IconCursorFunc
    );	
    if (IconCursor == FAILURE) {
	Error("StoreCursors -> Unable to store IconCursor.");
    }
}
