#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * globals.c - X Window System window manager global data.
 *
 *	Modified:	ADF	29-May-85	Converted to X version 8.0
 */
/* $Header: globals.c,v 10.3 86/02/01 16:10:30 tony Rel $ */

#include "xwm.h"

Window Pop;			/* Pop up dimension display window. */
Font IFont;			/* Icon output font. */
FontInfo IFontInfo;		/* Icon output font information. */
Font PFont;			/* Pop up output font. */
FontInfo PFontInfo;		/* Pop up output font information. */
Pixmap GrayPixmap;		/* Gray pixmap. */
Pixmap IBorder;			/* Icon window border pixmap. */
Pixmap IBackground;		/* Icon window background pixmap. */
Pixmap PBorder;			/* Pop-Up Window border pixmap. */
Pixmap PBackground;		/* Pop-up Window background pixmap. */
Cursor ArrowCrossCursor;	/* Arrow cross cursor. */
Cursor ULAngleCursor;		/* Upper left angle cursor. */
Cursor LLAngleCursor;		/* Lower left angle cursor. */
Cursor LRAngleCursor;		/* Lower right angle cursor. */
Cursor URAngleCursor;		/* Upper right angle cursor. */
Cursor TopTeeCursor;		/* Top tee cursor. */
Cursor LeftTeeCursor;		/* Left tee cursor. */
Cursor BottomTeeCursor;		/* Bottom tee cursor. */
Cursor RightTeeCursor;		/* Right tee cursor. */
Cursor DotCursor;		/* Dot cursor. */
Cursor CircleCursor;		/* Circle Cursor. */
Cursor TextCursor;		/* Text cursor used in icon windows. */
Cursor IconCursor;		/* Icon Cursor. */
int ScreenWidth;		/* Display screen width. */
int ScreenHeight;		/* Display screen height. */
int CursorFunc;			/* Mouse cursor function. */
int IconCursorFunc;		/* Icon Mouse Cursor function. */
int ITextForground;		/* Icon window text forground color. */
int ITextBackground;		/* Icon window text background color. */
int IBorderWidth;		/* Icon window border width. */
int IPadding;			/* Icon window padding. */
int PTextForground;		/* Pop-up window text forground color. */
int PTextBackground;		/* Pop-up window text background color. */
int PWidth;			/* Pop-up window width. */
int PHeight;			/* Pop-up window height. */
int PBorderWidth;		/* Pop-up window border width. */
int PPadding;			/* Pop-up window padding. */
int ButtonMask;			/* Global mouse button event mask. */
int Delta;			/* Mouse movement slop. */

Bool Debug;			/* Global debug flag. */
Bool Grid;			/* Should the m/r box contain a 9 seg. grid. */
Bool Zap;			/* Should the the zap effect be used. */

char PText[7] = INIT_PTEXT;	/* Pop-up window dummy text. */
int PTextSize = sizeof(PText);	/* Pop-up window dummy text size. */
