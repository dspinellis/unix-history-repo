/* $Header: uwm.h,v 10.4 86/11/19 16:21:45 jg Rel $ */
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

#include <errno.h>
#include <stdio.h>
#include <strings.h>
#include <X/Xlib.h>

#define MIN(x, y)	((x) <= (y) ? (x) : (y))
#define MAX(x, y)	((x) >= (y) ? (x) : (y))

typedef enum _bool {FALSE, TRUE} Bool;

#define DEF_DELTA		1
#define DEF_FONT		"6x10"
#define DEF_FUNC		GXcopy
#define DEF_ICON_BORDER_WIDTH 	2
#define DEF_ICON_PADDING	4
#define DEF_POP_BORDER_WIDTH 	2
#define DEF_POP_PADDING		4
#define DEF_MENU_BORDER_WIDTH 	2
#define DEF_MENU_PADDING	4
#define DEF_VOLUME		4

#define INIT_PTEXT		{'0', '0', '0', 'x', '0', '0', '0'}
#define TEMPFILE		"/tmp/uwm.XXXXXX"
#define SYSFILE			"/usr/new/lib/X/uwm/system.uwmrc"

#define CURSOR_WIDTH		16
#define CURSOR_HEIGHT		16

#define MAX_ZAP_VECTORS		16
#define MAX_BOX_VECTORS		26

#define DRAW_HEIGHT		1
#define DRAW_WIDTH		1
#define DRAW_VALUE		0xfd
#define DRAW_FUNC		GXxor
#define DRAW_PLANES		1

#define ROOT			0x1
#define WINDOW			0x2
#define ICON			0x4

#define FAILURE			0
#define NAME_LEN		256	/* Maximum length of filenames. */
#define KeyMask(x)	(x & (ControlMask|MetaMask|ShiftMask|ShiftLockMask))
#define EVENTMASK		(ButtonPressed | ButtonReleased)
#define ButtonValue(x)		(x & (LeftMask|MiddleMask|RightMask) >> 9)

#define DrawBox() XDraw(RootWindow,box,num_vectors,DRAW_HEIGHT,DRAW_WIDTH,DRAW_VALUE,DRAW_FUNC,AllPlanes)
#define DrawZap() XDraw(RootWindow,zap,num_vectors,DRAW_HEIGHT,DRAW_WIDTH,DRAW_VALUE,DRAW_FUNC,AllPlanes)

/*
 * Keyword table entry.
 */
typedef struct _keyword {
    char *name;
    int type;
    Bool *bptr;
    int *nptr;
    char *sptr;
    Bool (*fptr)();
} Keyword;

/*
 * Keyword table type entry.
 */
#define IsString	1
#define IsBoolTrue	2
#define IsBoolFalse	3
#define IsFunction	4
#define IsMenuMap	5
#define IsMenu		6
#define IsDownFunction	7
#define IsParser	8
#define IsNumeric	9
#define IsQuitFunction	10

/*
 * Button/key binding type.
 */
typedef struct _binding {
    struct _binding *next;
    int context;
    short mask;
    short button;
    Bool (*func)();
    char *menuname;
    struct _menuinfo *menu;
} Binding;

/*
 * Key expression type.
 */
typedef struct _keyexpr {
    char *name;
    short mask;
} KeyExpr;

/*
 * Context expression type.
 */
typedef struct _contexpr {
    char *name;
    int mask;
} ContExpr;

/*
 * Button modifier type.
 */
typedef struct _buttonmodifier {
    char *name;
    short mask;
} ButtonModifier;

/*
 * Button modifier mask definitions.
 * Note: The upper byte definitions are found in <X/X.h>.
 */
#define DoubleClick	0x1
#define DeltaMotion	0x2
#define ButtonUp	0x4
#define ButtonDown	0x8
#define ButtonMods	0xf

/*
 * MenuInfo data type.
 */
typedef struct _menuinfo {
    char *name;			/* Name of this menu. */
    Window w;			/* Menu window. */
    int iheight;		/* Height of an item. */
    int width;			/* Width of this menu. */
    int height;			/* Height of this menu. */
    Pixmap image;		/* Saved image of the menu. */
    char *foreground;		/* Name of foreground color. */
    char *background;		/* Name of background color. */
    char *fghighlight;		/* Name of hightlight foreground color. */
    char *bghighlight;		/* Name of highlight background color. */
    Color fg;			/* Foreground color definition. */
    Color bg;			/* Background color definition. */
    Color hlfg;			/* Foreground highlight color definition. */
    Color hlbg;			/* Background highlight color definition. */
    struct _menuline *line;	/* Linked list of menu items. */
} MenuInfo;

/*
 * Menu Line data type.
 */
typedef struct _menuline {
    struct _menuline *next;	/* Pointer to next line. */
    char *name;			/* Name of this line. */
    int type;			/* IsShellCommand, IsText, IsTextNL... */
    Window w;			/* Subwindow for this line. */
    char *text;			/* Text string to be acted upon. */
    Bool (*func)();		/* Window manager function to be invoked. */
    struct _menuinfo *menu;	/* Menu to be invoked. */
    char *foreground;		/* Name of foreground color. */
    char *background;		/* Name of background color. */
    Color fg;			/* Foreground color definition. */
    Color bg;			/* Background color definition. */
} MenuLine;

/*
 * MenuLine->type definitions.
 */
#define IsShellCommand		1
#define IsText			2
#define IsTextNL		3
#define IsUwmFunction		4
#define IsMenuFunction		5
#define IsImmFunction		6     /* Immediate (context-less) function. */

/*
 * Menu Link data type.  Used by the parser when creating a linked list
 * of menus.
 */
typedef struct _menulink {
    struct _menulink *next;	/* Pointer to next MenuLink. */
    struct _menuinfo *menu;	/* Pointer to the menu in this link. */
} MenuLink;

/*
 * External variable definitions.
 */
extern int errno;
extern Window Pop;		/* Pop-up dimension display window. */
extern Window Frozen;		/* Contains window id of "gridded" window. */
extern Font IFont;		/* Icon text font. */
extern Font PFont;		/* Pop-up text font. */
extern Font MFont;		/* Menu text font. */
extern FontInfo IFontInfo;	/* Icon text font information. */
extern FontInfo PFontInfo;	/* Pop-up text font information. */
extern FontInfo MFontInfo;	/* Menu text font information. */
extern Pixmap GrayPixmap;	/* Gray pixmap. */
extern Pixmap IBorder;		/* Icon window border pixmap. */
extern Pixmap IBackground;	/* Icon window background pixmap. */
extern Pixmap PBorder;		/* Pop-Up Window border pixmap. */
extern Pixmap PBackground;	/* Pop-up Window background pixmap. */
extern Pixmap MBorder;		/* Menu Window border pixmap. */
extern Pixmap MBackground;	/* Menu Window background pixmap. */
extern Cursor ArrowCrossCursor; /* Arrow cross cursor. */
extern Cursor TextCursor;	/* Text cursor used in icon windows. */
extern Cursor IconCursor;	/* Icon Cursor. */
extern Cursor MenuCursor;	/* Menu cursor. */
extern Cursor MenuMaskCursor;	/* Menu mask cursor. */
extern Cursor LeftButtonCursor;	/* Left button main cursor. */
extern Cursor MiddleButtonCursor;/* Middle button main cursor. */
extern Cursor RightButtonCursor;/* Right button main cursor. */
extern int ScreenWidth;		/* Display screen width. */
extern int ScreenHeight;	/* Display screen height. */
extern int CursorFunc;		/* Mouse cursor function. */
extern int IconCursorFunc;	/* Icon Mouse Cursor function. */
extern int ITextForground;	/* Icon window text forground color. */
extern int ITextBackground;	/* Icon window text background color. */
extern int IBorderWidth;	/* Icon window border width. */
extern int PTextForground;	/* Pop-up window text forground color. */
extern int PTextBackground;	/* Pop-up window text background color. */
extern int PWidth;		/* Pop-up window width (including borders). */
extern int PHeight;		/* Pop-up window height (including borders). */
extern int PBorderWidth;	/* Pop-up window border width. */
extern int MTextForground;	/* Menu window text forground color. */
extern int MTextBackground;	/* Menu window text background color. */
extern int MBorderWidth;	/* Menu window border width. */
extern int PPadding;		/* Pop-up window padding. */
extern int ButtonMask;		/* Global mouse button event mask. */
extern int Delay;		/* Delay between clicks of double click. */
extern int Delta;		/* Mouse movement slop. */
extern int HIconPad;		/* Icon horizontal padding. */
extern int VIconPad;		/* Icon vertical padding. */
extern int HMenuPad;		/* Menu horizontal padding. */
extern int VMenuPad;		/* Menu vertical padding. */
extern int MaxColors;		/* Maximum number of colors to use. */
extern int Pushval;		/* Number of pixels to push window by. */
extern int Volume;		/* Audible alarm volume. */
extern int status;		/* Routine return status. */
extern int Maxfd;		/* Maximum file descriptors for select(2). */
extern MenuLink *Menus;		/* Linked list of menus. */

extern Bool Autoselect;		/* Warp mouse to default menu selection? */
extern Bool Freeze;		/* Freeze server during move/resize? */
extern Bool Grid;		/* Should the m/r box contain a 9 seg. grid. */
extern Bool NWindow;		/* Normalize windows? */
extern Bool NIcon;		/* Normalize icons? */
extern Bool Push;		/* Relative=TRUE, Absolute=FALSE. */
extern Bool Reverse;		/* Reverse video? */
extern Bool Zap;		/* Should the the zap effect be used. */

extern char PText[];		/* Pop-up window dummy text. */
extern int PTextSize;		/* Pop-up window dummy text size. */

extern int Lineno;		/* Line count for parser. */
extern Bool Startup_File_Error;	/* Startup file error flag. */
extern char Startup_File[];	/* Startup file name. */
extern char IFontName[];	/* Icon font name. */
extern char PFontName[];	/* Pop-up font name. */
extern char MFontName[];	/* Menu font name. */
extern char **Argv;		/* Pointer to command line parameters. */
extern char **Environ;		/* Pointer to environment. */

extern char *DefaultBindings[];	/* Default bindings string array. */
extern Keyword KeywordTable[];	/* Keyword lookup table. */
extern Binding *Blist;		/* Button/key bindings list. */
extern KeyExpr KeyExprTbl[];	/* Key expression table. */
extern ContExpr ContExprTbl[];	/* Context expression table. */
extern ButtonModifier ButtModTbl[];/* Button modifier table. */

#ifdef PROFIL
int ptrap();
#endif

/*
 * External routine typing.
 */
extern Bool Beep();
extern Bool CircleDown();
extern Bool CircleUp();
extern Bool Continue();
extern Bool Focus();
extern Bool GetButton();
extern Bool Iconify();
extern Bool Lower();
extern Bool Menu();
extern Bool Move();
extern Bool MoveOpaque();
extern Bool NewIconify();
extern Bool Pause();
extern Bool PushDown();
extern Bool PushLeft();
extern Bool PushRight();
extern Bool PushUp();
extern Bool Raise();
extern Bool Refresh();
extern Bool ResetBindings();
extern Bool ResetMenus();
extern Bool ResetVariables();
extern Bool Resize();
extern Bool Restart();
extern int StoreCursors();
extern int StoreBox();
extern int StoreGridBox();
extern int StoreZap();
extern int Error();
extern int XError();
extern int CreateMenus();

extern char *stash();
