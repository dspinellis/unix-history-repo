#ifndef lint
static char *rcsid_globals_c = "$Header: globals.c,v 10.4 86/11/19 16:25:04 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)globals.c	3.8	1/24/86";
#endif
/*
 * globals.c - window manager global data
 *
 */

#include "uwm.h"

Window Pop;			/* Pop up dimension display window. */
Window Frozen;			/* Contains window id of "gridded" window. */
Font IFont;			/* Icon text font. */
Font PFont;			/* Pop-up text font. */
Font MFont;			/* Menu text font. */
FontInfo IFontInfo;		/* Icon text font information. */
FontInfo PFontInfo;		/* Pop-up text font information. */
FontInfo MFontInfo;		/* Menu text font information. */
Pixmap GrayPixmap;		/* Gray pixmap. */
Pixmap IBorder;			/* Icon window border pixmap. */
Pixmap IBackground;		/* Icon window background pixmap. */
Pixmap PBorder;			/* Pop-Up Window border pixmap. */
Pixmap PBackground;		/* Pop-up Window background pixmap. */
Pixmap MBorder;			/* Menu Window border pixmap. */
Pixmap MBackground;		/* Menu Window background pixmap. */
Cursor ArrowCrossCursor;	/* Arrow cross cursor. */
Cursor TextCursor;		/* Text cursor used in icon windows. */
Cursor IconCursor;		/* Icon Cursor. */
Cursor MenuCursor;		/* Menu cursor. */
Cursor MenuMaskCursor;		/* Menu mask cursor. */
Cursor LeftButtonCursor;	/* Left button main cursor. */
Cursor MiddleButtonCursor;	/* Middle button main cursor. */
Cursor RightButtonCursor;	/* Right button main cursor. */
int ScreenWidth;		/* Display screen width. */
int ScreenHeight;		/* Display screen height. */
int CursorFunc;			/* Mouse cursor function. */
int IconCursorFunc;		/* Icon Mouse Cursor function. */
int ITextForground;		/* Icon window text forground color. */
int ITextBackground;		/* Icon window text background color. */
int IBorderWidth;		/* Icon window border width. */
int PTextForground;		/* Pop-up window text forground color. */
int PTextBackground;		/* Pop-up window text background color. */
int MTextForground;		/* Menu window text forground color. */
int MTextBackground;		/* Menu window text background color. */
int PWidth;			/* Pop-up window width. */
int PHeight;			/* Pop-up window height. */
int PBorderWidth;		/* Pop-up window border width. */
int PPadding;			/* Pop-up window padding. */
int MBorderWidth;		/* Menu window border width. */
int MPadding;			/* Menu window padding. */
int ButtonMask;			/* Global mouse button event mask. */
int Delay;			/* Delay between clicks of a double click. */
int Delta;			/* Mouse movement slop. */
int HIconPad;			/* Icon horizontal padding. */
int VIconPad;			/* Icon vertical padding. */
int HMenuPad;			/* Menu horizontal padding. */
int VMenuPad;			/* Menu vertical padding. */
int MaxColors;			/* Maximum number of colors to use. */
int Pushval;			/* Number of pixels to push window by. */
int Volume;			/* Audible alarm volume. */
int status;			/* Routine return status. */
int Maxfd;			/* Maximum file descriptors for select(2). */
MenuLink *Menus;		/* Linked list of menus. */
Binding *Blist;			/* Button/key binding list. */

Bool Autoselect;		/* Warp mouse to default menu selection? */
Bool Freeze;			/* Freeze server during move/resize? */
Bool Grid;			/* Should the m/r box contain a 9 seg. grid. */
Bool NWindow;           	 /* Normalize windows? */
Bool NIcon;             	 /* Normalize icons? */
Bool Push;			/* Relative=TRUE, Absolute=FALSE. */
Bool Reverse;			/* Reverse video? */
Bool Zap;			/* Should the the zap effect be used. */

char PText[7] = INIT_PTEXT;	/* Pop-up window dummy text. */
int PTextSize = sizeof(PText);	/* Pop-up window dummy text size. */

int Lineno = 1;			/* Line count for parser. */
Bool Startup_File_Error = FALSE;/* Startup file error flag. */
char Startup_File[NAME_LEN] = "";/* Startup file name. */
char IFontName[NAME_LEN];	/* Icon font name. */
char PFontName[NAME_LEN];	/* Pop-up font name. */
char MFontName[NAME_LEN];	/* Menu font name. */
char **Argv;			/* Pointer to command line parameters. */
char **Environ;			/* Pointer to environment. */

/*
 * Keyword lookup table for parser.
 */
Keyword KeywordTable[] = {
    { "autoselect",	IsBoolTrue,	&Autoselect,0,0,0 },
    { "delay",		IsNumeric,	0,&Delay,0,0 },
    { "delta",		IsNumeric,	0,&Delta,0,0 },
    { "freeze",		IsBoolTrue,	&Freeze,0,0,0 },
    { "iconfont",	IsString,	0,0,IFontName,0 },
    { "f.beep",		IsFunction,	0,0,0,Beep },
    { "f.circledown",	IsFunction,	0,0,0,CircleDown },
    { "f.circleup",	IsFunction,	0,0,0,CircleUp },
    { "f.continue",	IsFunction,	0,0,0,Continue },
    { "f.focus",	IsFunction,	0,0,0,Focus },
    { "f.iconify",	IsFunction,	0,0,0,Iconify },
    { "f.lower",	IsFunction,	0,0,0,Lower },
    { "f.menu",		IsMenuMap,	0,0,0,Menu },
    { "f.move",		IsDownFunction,	0,0,0,Move },
    { "f.moveopaque",	IsDownFunction,	0,0,0,MoveOpaque },
    { "f.newiconify",	IsDownFunction,	0,0,0,NewIconify },
    { "f.pause",	IsFunction,	0,0,0,Pause },
    { "f.pushdown",	IsFunction,	0,0,0,PushDown },
    { "f.pushleft",	IsFunction,	0,0,0,PushLeft },
    { "f.pushright",	IsFunction,	0,0,0,PushRight },
    { "f.pushup",	IsFunction,	0,0,0,PushUp },
    { "f.raise",	IsFunction,	0,0,0,Raise },
    { "f.refresh",	IsFunction,	0,0,0,Refresh },
    { "f.resize",	IsDownFunction,	0,0,0,Resize },
    { "f.restart",	IsQuitFunction,	0,0,0,Restart },
    { "grid",		IsBoolTrue,	&Grid,0,0,0 },
    { "hiconpad",	IsNumeric,	0,&HIconPad,0,0 },
    { "hmenupad",	IsNumeric,	0,&HMenuPad,0,0 },
    { "maxcolors",	IsNumeric,	0,&MaxColors,0,0 },
    { "menu",		IsMenu,		0,0,0,0 },
    { "menufont",	IsString,	0,0,MFontName,0 },
    { "noautoselect",	IsBoolFalse,	&Autoselect,0,0,0 },
    { "nofreeze",	IsBoolFalse,	&Freeze,0,0,0 },
    { "nogrid",		IsBoolFalse,	&Grid,0,0,0 },
    { "nonormali",	IsBoolFalse,	&NIcon,0,0,0 },
    { "nonormalw",	IsBoolFalse,	&NWindow,0,0,0 },
    { "noreverse",	IsBoolFalse,	&Reverse,0,0,0 },
    { "normali",	IsBoolTrue,	&NIcon,0,0,0 },
    { "normalw",	IsBoolTrue,	&NWindow,0,0,0 },
    { "nozap",		IsBoolFalse,	&Zap,0,0,0 },
    { "push",		IsNumeric,	0,&Pushval,0,0 },
    { "pushabsolute",	IsBoolFalse,	&Push,0,0,0 },
    { "pushrelative",	IsBoolTrue,	&Push,0,0,0 },
    { "resetbindings",	IsParser,	0,0,0,ResetBindings },
    { "resetmenus",	IsParser,	0,0,0,ResetMenus },
    { "resetvariables",	IsParser,	0,0,0,ResetVariables },
    { "resizefont",	IsString,	0,0,PFontName,0 },
    { "reverse",	IsBoolTrue,	&Reverse,0,0,0 },
    { "viconpad",	IsNumeric,	0,&VIconPad,0,0 },
    { "vmenupad",	IsNumeric,	0,&VMenuPad,0,0 },
    { "volume",		IsNumeric,	0,&Volume,0,0 },
    { "zap",		IsBoolTrue,	&Zap,0,0,0 },
    { NULL,		NULL,		NULL,NULL,NULL,NULL }
};

/*
 * Key expression table for parser.
 */
KeyExpr KeyExprTbl[] = {
    { "ctrl", ControlMask },
    { "c", ControlMask },
    { "lock", ShiftLockMask },
    { "l", ShiftLockMask },
    { "meta", MetaMask },
    { "m", MetaMask },
    { "shift", ShiftMask },
    { "s", ShiftMask },
    { NULL, NULL }
};

/*
 * Context expression table for parser.
 */
ContExpr ContExprTbl[] = {
    { "icon", ICON },
    { "i", ICON },
    { "root", ROOT },
    { "r", ROOT },
    { "window", WINDOW },
    { "w", WINDOW },
    { NULL, NULL }
};

/*
 * Button expression table for parser.
 */
ButtonModifier ButtModTbl[] = {
    { "left", LeftMask },
    { "leftbutton", LeftMask },
    { "l", LeftMask },
    { "middle", MiddleMask },
    { "middlebutton", MiddleMask },
    { "m", MiddleMask },
    { "right", RightMask },
    { "rightbutton", RightMask },
    { "r", RightMask },
    /*
     * The following are for double-clicking the mouse.  It's a future!
     */
    /***
    { "double", DoubleClick },
    { "doubleclick", DoubleClick },
    { "dclick", DoubleClick },
    ***/
    { "move", DeltaMotion },
    { "motion", DeltaMotion },
    { "delta", DeltaMotion },
    { "down", ButtonDown },
    { "d", ButtonDown },
    { "up", ButtonUp },
    { "u", ButtonUp },
    { NULL, NULL }
};
