#include <X/mit-copyright.h>

/* Copyright 1985 Massachusetts Institute of Technology */

/*
 * xsetroot.c 	MIT Project Athena, X Window system root window 
 *		parameter setting utility.  This program will set 
 *		various parameters of the X root window.
 *
 *  Author:	Tony Della Fera, DEC
 *		28-Nov-84
 */
#ifndef lint
static char *rcsid_xsetroot_c = "$Header: xsetroot.c,v 10.10 86/11/25 13:37:47 jg Rel $";
#endif

#include <X/Xlib.h>
#include <stdio.h>
#include <strings.h>
#define NULL 0

#define MAX(a, b) (a) > (b) ? (a) : (b)
#define MIN(a, b) (a) < (b) ? (a) : (b)
#define ABS(a) (a) < 0 ? -(a) : (a)

#define DEF_MSE_COLOR		BlackPixel
#define DEF_SOLID_COLOR		WhitePixel
#define DEF_FG_COLOR		BlackPixel
#define DEF_BG_COLOR		WhitePixel
#define DEF_ROOT_NAME		" X Root Window "

#define BITMAP_SIZE		16
#define BITMAP_HOT		8

#define FAILURE			0

typedef enum bool {FALSE, TRUE} bool;

static unsigned short solid_line = 0xffff;

static short gray_bits[BITMAP_SIZE] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555
};

main(argc, argv)
    int argc;
    char **argv;
{
    register int i;
    register int x;
    register int y;
    register unsigned short pattern_line = 0;
    int mse = DEF_MSE_COLOR;
    int solid = DEF_SOLID_COLOR;
    int fg = DEF_FG_COLOR;
    int bg = DEF_BG_COLOR;
    int width;
    int height;
    int x_hot;
    int y_hot;
    int xmod;
    int ymod;
    char *def_val;
    char *strind;
    char *root_name;
    char *cursor_fname;
    char *mask_fname;
    char *bitmap_fname;
    char *mse_name = NULL;
    char *solid_name = NULL;
    char *fg_name = NULL;
    char *bg_name = NULL;
    char display[40];
    bool def_sw = FALSE;
    bool name_sw = FALSE;
    bool cursor_sw = FALSE;
    bool solid_sw = FALSE;
    bool invert_sw = FALSE;
    bool gray_sw = FALSE;
    bool bitmap_sw = FALSE;
    bool mod_sw = FALSE;

    short *cursor_bits;
    short *mask_bits;
    short *bkgnd_bits;

    Color color_def;
    Cursor cursor;
    Bitmap bkgnd_bitmap;
    Pixmap bkgnd_pixmap;

    display[0] = '\0';
    
    /*
     * Get X defaults.
     */
    def_val = XGetDefault(argv[0], "Foreground");
    fg_name = def_val;

    def_val = XGetDefault(argv[0], "Background");
    bg_name = def_val;

    def_val = XGetDefault(argv[0], "Mouse");
    mse_name = def_val;

    /*
     * Parse argument list.
     */
    for (i = 1; i < argc; i++) {
        strind = index(argv[i], ':');
        if(strind != NULL) {
            (void) strncpy(display, argv[i], sizeof(display));
	    if (argc == 2) def_sw = TRUE;
            continue;
        }
        strind = index (argv[i], '-');
        if (strind == NULL) Syntax(argv[0]);
        if (strncmp(argv[i], "-help", 5) == 0) {
            Syntax(argv[0]);
        }
        if (strncmp(argv[i], "-def", 4) == 0) {
	    def_sw = TRUE;
	    continue;
        }
        if (strncmp(argv[i], "-fg", 3) == 0) {
            if (++i >= argc) Syntax(argv[0]);
	    fg_name = argv[i];
	    continue;
	}
        if (strncmp(argv[i], "-bg", 3) == 0) {
            if (++i >= argc) Syntax(argv[0]);
	    bg_name = argv[i];
	    continue;
	}
        if (strncmp(argv[i], "-invert", 7) == 0) {
	    invert_sw = TRUE;
	    continue;
	}
        if (strncmp(argv[i], "-name", 5) == 0) {
	    if (++i >= argc) Syntax(argv[0]);
            root_name = argv[i];
	    name_sw = TRUE;
            continue;
        }
	if (strncmp(argv[i], "-cursor", 7) == 0) {
            if (++i >= argc) Syntax(argv[0]);
	    cursor_fname = argv[i];
            if (++i >= argc) Syntax(argv[0]);
	    mask_fname = argv[i];
	    cursor_sw = TRUE;
	    continue;
	}
	if (strncmp(argv[i], "-solid", 6) == 0) {
	    if (
		gray_sw == TRUE ||
		bitmap_sw == TRUE ||
		mod_sw == TRUE
	    ) Syntax(argv[0]);
            if (++i >= argc) Syntax(argv[0]);
	    solid_name = argv[i];
	    solid_sw = TRUE;
	    continue;
	}
	if (
	    (strncmp(argv[i], "-gray", 5) == 0) ||
	    (strncmp(argv[i], "-grey", 5) == 0)
	){
	    if (
		solid_sw == TRUE ||
		bitmap_sw == TRUE ||
		mod_sw == TRUE
	    ) Syntax(argv[0]);
	    gray_sw = TRUE;
	    continue;
	}
	if (strncmp(argv[i], "-bitmap", 7) == 0) {
	    if (
		gray_sw == TRUE ||
		solid_sw == TRUE ||
		mod_sw == TRUE
	    ) Syntax(argv[0]);
            if (++i >= argc) Syntax(argv[0]);
	    bitmap_fname = argv[i];
	    bitmap_sw = TRUE;
	    continue;
	}
        if (strncmp(argv[i], "-mod", 5) == 0) {
	    if (
		gray_sw == TRUE ||
		bitmap_sw == TRUE ||
		solid_sw == TRUE
	    ) Syntax(argv[0]);
            if (++i >= argc) Syntax(argv[0]);
            xmod = atoi(argv[i]);
            if (++i >= argc) Syntax(argv[0]);
            ymod = atoi(argv[i]);
	    mod_sw = TRUE;
            continue;
        }
        Syntax(argv[0]);
    }
    
    /*
     * If there are no arguments then restore defaults.
     */
    if (argc == 1) def_sw = TRUE;

    /*
     * Open the display.
     */
    if (XOpenDisplay(display) == NULL) {
	fprintf(stderr, "%s: Can't open display '%s'\n",
		argv[0], XDisplayName(display));
	exit(1);
    }

    /*
     * Parse color definintions.
     */
    if ((DisplayCells() > 2) && (solid_name != NULL)) {
	if (
	    XParseColor(solid_name, &color_def) &&
	    XGetHardwareColor(&color_def)
	) solid = color_def.pixel;
    }
    else if (solid_name && strcmp(solid_name, "black") == 0) solid = BlackPixel;
    else if (solid_name && strcmp(solid_name, "white") == 0) solid = WhitePixel;

    if ((DisplayCells() > 2) && (fg_name != NULL)) {
	if (
	    XParseColor(fg_name, &color_def) &&
	    XGetHardwareColor(&color_def)
	) fg = color_def.pixel;
    }
    else if (solid_name && strcmp(fg_name, "black") == 0) fg = BlackPixel;
    else if (solid_name && strcmp(fg_name, "white") == 0) fg = WhitePixel;

    if ((DisplayCells() > 2) && (bg_name != NULL)) {
	if (
	    XParseColor(bg_name, &color_def) &&
	    XGetHardwareColor(&color_def)
	) bg = color_def.pixel;
    }
    else if (bg_name && strcmp(bg_name, "black") == 0) bg = BlackPixel;
    else if (bg_name && strcmp(bg_name, "white") == 0) bg = WhitePixel;

    if ((DisplayCells() > 2) && (mse_name != NULL)) {
	if (
	    XParseColor(mse_name, &color_def) &&
	    XGetHardwareColor(&color_def)
	) mse = color_def.pixel;
    }
    else if (mse_name && strcmp(mse_name, "black") == 0) mse = BlackPixel;
    else if (mse_name && strcmp(mse_name, "white") == 0) mse = WhitePixel;

    /*
     * Set the root window name if a new root name supplied.
     */
    if (name_sw == TRUE) XStoreName(RootWindow, root_name);

    /*
     * Set cursor if a cursor is supplied.
     */
    if (cursor_sw == TRUE) {
	int status;
	/*
	 * Open and read the mask file.
	 */
	status = XReadBitmapFile(
	    mask_fname, 
	    &width, &height, &mask_bits,
	    NULL, NULL
	);
	if (status == 0) Error ("Unable to open mask file");
	else if (status < 0) Error ("Unable to parse mask file");
	else if ((width != BITMAP_SIZE) || (height != BITMAP_SIZE))
	    Error("Invaild mask Bitmap size");
	/*
	 * Open and read the cursor file.
	 */
	status = XReadBitmapFile(
	    cursor_fname,
	    &width, &height, &cursor_bits,
	    &x_hot, &y_hot
	);
	if (status == 0) Error ("Unable to open cursor file");
	else if (status < 0) Error("Unable to parse cursor file");
	else if ((width != BITMAP_SIZE) || (height != BITMAP_SIZE))
	    Error("Invaild cursor Bitmap size");
	

	/*
	 * If there is no hot spot defined  or if the one defined is
	 * invalid, place the hot spot at BITMAP_HOT.
	 */
	if (
	    (x_hot >= BITMAP_SIZE) || (x_hot < 0)
	){
	    x_hot = BITMAP_HOT;
	}
	if (
	    (y_hot >= BITMAP_SIZE) || (y_hot < 0)
	){
	    y_hot = BITMAP_HOT;
	}
	
	/*
	 * Create the cursor.
	 */
	cursor = XCreateCursor(
	    width, height,
	    cursor_bits, mask_bits,
	    x_hot, y_hot,
	    mse, bg,
	    GXcopy
	);
	if (cursor == FAILURE) Error("Unable to store cursor");

	/*
	 * Define the root window's cursor to be the new
	 * cursor.
	 */
	XDefineCursor(RootWindow, cursor);
    }

    /*
     * Set background to a solid color if requested.
     */
    if (solid_sw == TRUE) {
	/*
	 * Create the tile Pixmap.
	 */
	bkgnd_pixmap = XMakeTile(solid);
	if (bkgnd_pixmap == FAILURE) Error("Unable to store solid Pixmap");
	/*
	 * Change the window.
	 */
	XChangeBackground(RootWindow, bkgnd_pixmap);
	Done();
    }
	
    /*
     * Set background to a gray pattern if requested.
     */
    if (gray_sw == TRUE) {
	/*
	 * Create the Bitmap.
	 */
	bkgnd_bitmap = XStoreBitmap(BITMAP_SIZE, BITMAP_SIZE, gray_bits);
	if (bkgnd_bitmap == FAILURE) Error("Unable to store gray Bitmap");
	/*
	 * Create the tile Pixmap.
	 */
	if (invert_sw == TRUE) {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, bg, fg);
	}
	else {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, fg, bg);
	}
	if (bkgnd_pixmap == FAILURE) Error("Unable to store gray Pixmap");
	/*
	 * Change the window.
	 */
	XChangeBackground(RootWindow, bkgnd_pixmap);
	Done();
    }
	
    /*
     * Set background to a bitmap pattern if requested.
     */
    if (bitmap_sw == TRUE) {
	int status;
	/*
	 * Open and read the bitmap file.
	 */
	status = XReadBitmapFile(
	    bitmap_fname,
	    &width, &height, &bkgnd_bits,
	    &x_hot, &y_hot
	);
	if (status == 0) Error ("Unable to open Bitmap file");
	else if (status < 0) Error("Unable to parse Bitmap file");
	else if ((width != BITMAP_SIZE) || (height != BITMAP_SIZE))
	    Error("Invaild Bitmap size");
	bkgnd_bitmap = XStoreBitmap (width, height, bkgnd_bits);
	if (bkgnd_bitmap == FAILURE)
	    Error("Unable to store Bitmap");
	
	/*
	 * Create the tile pixmap.
	 */
	if (invert_sw == TRUE) {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, bg, fg);
	}
	else {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, fg, bg);
	}
	if (bkgnd_pixmap == FAILURE) Error("Unable to store Pixmap");
	/*
	 * Change the window.
	 */
	XChangeBackground(RootWindow, bkgnd_pixmap);
	Done();
    }

    /*
     * Set background to a modula pattern if requested.
     */
    if (mod_sw == TRUE) {
	bkgnd_bits = (short *) malloc (BITMAP_SIZE*sizeof(short));
	/*
	 * Compute modula bits.
	 */
	for (x = 0; x < BITMAP_SIZE; x++) {
	    pattern_line <<= 1;
	    if (x % xmod == 0) pattern_line |= 0x0001;
	}
	for (y = 0; y < BITMAP_SIZE; y++) {
	    if (y % ymod == 0) {
		bkgnd_bits[y] = solid_line;
	    }
	    else {
		bkgnd_bits[y] = pattern_line;
	    }
	}
	/*
	 * Create and store the pattern pixmap.
	 */
	bkgnd_bitmap = XStoreBitmap(BITMAP_SIZE, BITMAP_SIZE, bkgnd_bits);
	if (bkgnd_bitmap == FAILURE) Error("Error storing bitmap");
	if (invert_sw == TRUE) {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, bg, fg);
	}
	else {
	    bkgnd_pixmap = XMakePixmap(bkgnd_bitmap, fg, bg);
	}
	if (bkgnd_pixmap == FAILURE) Error("Error storing pixmap");
	XChangeBackground(RootWindow, bkgnd_pixmap);
	Done();
    }

    /*
     * If we got here then check to see if the default switch is on
     * OR if there were no arguments.
     */
    if (def_sw == TRUE) {
	if (name_sw == FALSE) XStoreName(RootWindow, DEF_ROOT_NAME);
	if (cursor_sw == FALSE) XUndefineCursor(RootWindow);
	XChangeBackground(RootWindow, (Pixmap)0);
	Done();
    }
    else XFlush();
    exit(0);
}



/*
 * Clear the root window, flush all output and exit.
 */
Done()
{
    XClear(RootWindow);
    XFlush();
    exit(0);
}



/*
 * Report an internal error.
 */
Error(description)
    char *description;
{
    printf ("\nxsetroot: %s.\n\n", description);
    exit(1);
}



/*
 * Report the syntax for calling xsetroot.
 */
Syntax(call)
    char *call;
{
    printf("\nUsage: %s [-help] [-def] [-fg <color>] [-bg <color>] [-invert]\n", call);
    printf("         [-name <string>] [-cursor <cursor file> <mask file>]\n");
    printf("         [-solid <color>] [-gray, -grey] [-bitmap <file>]\n");
    printf("         [-mod <x> <y>] [[<host>]:<vs>]\n");
    printf("\nNOTE: *** Use only one of [-solid] [-gray, -grey] [-bitmap] [-mod] ***\n\n");
    exit(1);
}

/* End of xsetroot.c */
