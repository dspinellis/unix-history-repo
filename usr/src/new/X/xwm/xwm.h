#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * xwm - X Window System window manager header file.
 *
 *	File:		xwm.h
 */

/* $Header: xwm.h,v 10.3 86/02/01 16:10:49 tony Rel $ */

#include <errno.h>
#include <stdio.h>
#include <X/Xlib.h>

typedef enum _bool {FALSE, TRUE} Bool;

#define min(x, y)	((x) <= (y) ? (x) : (y))
#define max(x, y)	((x) >= (y) ? (x) : (y))

#define DEF_BUTTON_MASK		MetaMask
#define DEF_KEY_MASK		MetaMask
#define DEF_DELTA		5
#define DEF_I_FONT		"6x10"
#define DEF_P_FONT		"6x10"
#define DEF_FUNC		GXcopy
#define DEF_ICON_BORDER_WIDTH 	2
#define DEF_ICON_PADDING	4
#define DEF_POP_BORDER_WIDTH 	2
#define DEF_POP_PADDING		4

#define INIT_PTEXT		{'0', '0', '0', 'x', '0', '0', '0'}

#define CURSOR_WIDTH		16
#define CURSOR_HEIGHT		16

#define MAX_ZAP_VECTORS		16
#define MAX_BOX_VECTORS		26

#define DRAW_HEIGHT		1
#define DRAW_WIDTH		1
#define DRAW_VALUE		~0
#define DRAW_FUNC		GXxor
#define DRAW_PLANES		1

#define FAILURE			0

/*
 * External variable definitions.
 */
extern int errno;

extern Window Pop;		/* Pop up dimension display window. */
extern Font IFont;		/* Icon output font. */
extern FontInfo IFontInfo;	/* Icon output font information. */
extern Font PFont;		/* Pop up output font. */
extern FontInfo PFontInfo;	/* Pop up output font information. */
extern Pixmap GrayPixmap;	/* Gray pixmap. */
extern Pixmap IBorder;		/* Icon window border pixmap. */
extern Pixmap IBackground;	/* Icon window background pixmap. */
extern Pixmap PBorder;		/* Pop-Up Window border pixmap. */
extern Pixmap PBackground;	/* Pop-up Window background pixmap. */
extern Cursor ArrowCrossCursor; /* Arrow cross cursor. */
extern Cursor ULAngleCursor;	/* Upper left angle cursor. */
extern Cursor LLAngleCursor;	/* Lower left angle cursor. */
extern Cursor LRAngleCursor;	/* Lower right angle cursor. */
extern Cursor URAngleCursor;	/* Upper right angle cursor. */
extern Cursor TopTeeCursor;	/* Top tee cursor. */
extern Cursor LeftTeeCursor;	/* Left tee cursor. */
extern Cursor BottomTeeCursor;	/* Bottom tee cursor. */
extern Cursor RightTeeCursor;	/* Right tee cursor. */
extern Cursor DotCursor;	/* Dot cursor. */
extern Cursor CircleCursor;	/* Circle Cursor. */
extern Cursor TextCursor;	/* Text cursor used in icon windows. */
extern Cursor IconCursor;	/* Icon Cursor. */
extern int ScreenWidth;		/* Display screen width. */
extern int ScreenHeight;	/* Display screen height. */
extern int CursorFunc;		/* Mouse cursor function. */
extern int IconCursorFunc;	/* Icon Mouse Cursor function. */
extern int ITextForground;	/* Icon window text forground color. */
extern int ITextBackground;	/* Icon window text background color. */
extern int IBorderWidth;	/* Icon window border width. */
extern int IPadding;		/* Icon window padding. */
extern int PTextForground;	/* Pop-up window text forground color. */
extern int PTextBackground;	/* Pop-up window text background color. */
extern int PWidth;		/* Pop-up window width (including borders). */
extern int PHeight;		/* Pop-up window height (including borders). */
extern int PBorderWidth;	/* Pop-up window border width. */
extern int PPadding;		/* Pop-up window padding. */
extern int ButtonMask;		/* Global mouse button event mask. */
extern int Delta;		/* Mouse movement slop. */

extern Bool Debug;		/* Global debug flag. */
extern Bool Grid;		/* Should the m/r box contain a 9 seg. grid. */
extern Bool Zap;		/* Should the the zap effect be used. */


extern char PText[];		/* Pop-up window dummy text. */
extern int PTextSize;		/* Pop-up window dummy text size. */

#ifdef PROFIL
int ptrap();
#endif

/*
 * External routine typing.
 */
extern Bool Change();
extern Bool GetButton();
extern int LowerIconify();
extern int Move();
extern int StoreCursors();
extern int StoreBox();
extern int StoreGridBox();
extern int StoreZap();
extern int Error();
extern int XError();
