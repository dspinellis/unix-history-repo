#include <X/mit-copyright.h>

/* $Header: XMenuCreate.c,v 10.12 86/07/11 16:59:46 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuCreate -	Creates an X window system menu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 23, 1986
 *
 */

#include "XMenuInternal.h"

#include "../bitmaps/dimple1.bitmap"
#include "../bitmaps/dimple3.bitmap"
#include "../bitmaps/gray1.bitmap"
#include "../bitmaps/gray3.bitmap"
#include "../bitmaps/cross_weave.bitmap"

#include "../cursors/left_ptr.cursor"
#include "../cursors/left_ptr_mask.cursor"
#include "../cursors/right_ptr.cursor"
#include "../cursors/right_ptr_mask.cursor"
#include "../cursors/center_ptr.cursor"
#include "../cursors/center_ptr_mask.cursor"

#define DEF_FREEZE		0
#define DEF_REVERSE		0
#define DEF_MENU_STYLE		RIGHT
#define DEF_MENU_MODE		INVERT
#define DEF_INACT_PNUM		3
#define MAX_INACT_PNUM		4

#define DEF_P_STYLE		CENTER
#define DEF_P_EVENTS		(EnterWindow | ExposeWindow)
#define DEF_P_FNT_NAME		"8x13"
#define DEF_P_SPREAD		0.5
#define DEF_P_BDR_WIDTH		2

#define DEF_S_STYLE		LEFT
#define DEF_S_EVENTS		(EnterWindow | LeaveWindow)
#define DEF_S_FNT_NAME		"6x10"
#define DEF_S_SPREAD		0.10
#define DEF_S_BDR_WIDTH		1

#define XASSOC_TABLE_SIZE	64

#define TILE_BUF_SIZE		5

int atoi();
double atof();

XMenu *
XMenuCreate(parent, def_env)
    Window parent;		/* Window ID of the menu's parent window. */
    register char *def_env;	/* X Defaults program environment name. */
{
    register int i;		/* Loop counter. */
    register int j;		/* Loop counter. */
    register char *def_val;	/* X Default value temp variable. */

    register XMenu *menu;	/* Pointer to the new menu. */
    XMStyle menu_style;		/* Menu display style. */
    XMMode menu_mode;		/* Menu display mode. */
    XMPane *pane;		/* Pane list header. */
    XAssocTable *assoc_tab;	/* XAssocTable pointer. */
    Cursor mouse_cursor;	/* Mouse cursor. */
    int freeze;			/* Freeze server mode. */
    int reverse;		/* Reverse video mode. */
    int tile_count;		/* Number of tiles created by XMakeTiles. */

    XMStyle p_style;		/* Pane display style. */
    char *p_fnt_name;		/* Flag font name. */
    FontInfo *p_fnt_info;	/* Flag font information. */
    int p_fnt_pad;		/* Flag font padding in pixels. */
    double p_spread;		/* Pane spread in flag height fractions. */
    int p_bdr_width;		/* Pane border width. */
    int flag_height;		/* Flag window height. */
    int p_height;		/* Pane window height. */
    int p_x_off;		/* Pane X offset. */
    int p_y_off;		/* Pane Y offset. */

    XMStyle s_style;		/* Selection display style. */
    char *s_fnt_name;		/* Selection font name. */
    FontInfo *s_fnt_info;	/* Selection font information. */
    int s_fnt_pad;		/* Selection font padding in pixels. */
    double s_spread;		/* Select spread in line height fractions. */
    int s_bdr_width;		/* Highlight border width. */
    int s_height;		/* Selection window height. */
    int s_x_off;		/* Selection window X offset. */
    int s_y_off;		/* Selection window Y offset. */

    Color color_def;		/* Color definition holder. */
    int p_bdr_color;		/* Color of border pixmap. */
    int s_bdr_color;		/* Color of highlight pixmap. */
    int p_frg_color;		/* Color of pane foreground pixmap. */
    int s_frg_color;		/* Color of selection foreground pixmap. */
    int bkgnd_color;		/* Color of background pixmap. */
    int mouse_color;		/* Color of mouse cursor. */

    Bitmap inact_bitmap;	/* Inactive background pattern bitmap. */
    int inact_pnum;		/* Inactive background pattern number. */

    Pixmap p_bdr_pixmap;	/* Pane border pixmap. */
    Pixmap s_bdr_pixmap;	/* Selection border pixmap. */
    Pixmap p_frg_pixmap;	/* Pane forground pixmap. */
    Pixmap s_frg_pixmap;	/* Selection forground pixmap. */
    Pixmap bkgnd_pixmap;	/* Menu background pixmap. */
    Pixmap inact_pixmap;	/* Menu inactive pixmap. */

    TileFrame tile_buf[TILE_BUF_SIZE];	/* XMakeTiles buffer. */

    /*
     * Calloc the XMenu structure and the initial pane.
     */
    menu = (XMenu *)calloc(1, sizeof(XMenu));
    if (menu == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(NULL);
    }
    pane = (XMPane *)calloc(1, sizeof(XMPane));
    if (pane == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(NULL);
    }

    /*
     * Create the XAssocTable.
     */
    assoc_tab = (XAssocTable *)XCreateAssocTable(XASSOC_TABLE_SIZE);
    if (assoc_tab == NULL) {
	_XMErrorCode = XME_CREATE_ASSOC;
	return(NULL);
    }

    /*
     * Set up the default environment name.
     */
    if (def_env == NULL || *def_env == '\0') def_env = "XMenu";

    /*
     * Set up internal fail-safe defaults.
     */
    freeze = DEF_FREEZE;
    reverse = DEF_REVERSE;
    menu_style = DEF_MENU_STYLE;
    menu_mode = DEF_MENU_MODE;
    inact_pnum = DEF_INACT_PNUM;

    p_style = DEF_P_STYLE;
    p_spread = DEF_P_SPREAD;
    p_fnt_name = DEF_P_FNT_NAME;
    p_bdr_width = DEF_P_BDR_WIDTH;

    s_style = DEF_S_STYLE;
    s_spread = DEF_S_SPREAD;
    s_fnt_name = DEF_S_FNT_NAME;
    s_bdr_width = DEF_S_BDR_WIDTH;

    /*
     * Get default values from X.
     */
    def_val = XGetDefault(def_env, "MenuFreeze");
    if (def_val != NULL) {
	if (strcmp(def_val, "on") == 0) freeze = 1;
	else if (strcmp(def_val, "off") == 0) freeze = 0;
    }

    def_val = XGetDefault(def_env, "MenuReverseVideo");
    if (def_val != NULL) {
	if (strcmp(def_val, "on") == 0) reverse = 1;
	else if (strcmp(def_val, "off") == 0) reverse = 0;
    }

    def_val = XGetDefault(def_env, "MenuStyle");
    if (def_val != NULL) {
	if (strcmp(def_val, "right_hand") == 0) menu_style = RIGHT;
	else if (strcmp(def_val, "left_hand") == 0) menu_style = LEFT;
	else if (strcmp(def_val, "center") == 0) menu_style = CENTER;
    }

    def_val = XGetDefault(def_env, "MenuMode");
    if (def_val != NULL) {
	if (strcmp(def_val, "box") == 0) menu_mode = BOX;
	else if (strcmp(def_val, "invert") == 0) menu_mode = INVERT;
    }

    def_val = XGetDefault(def_env, "MenuMouse");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) mouse_color = color_def.pixel;
    else if (reverse) mouse_color = WhitePixel;
    else mouse_color = BlackPixel;

    def_val = XGetDefault(def_env, "MenuBackground");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) bkgnd_color = color_def.pixel;
    else if (reverse) bkgnd_color = BlackPixel;
    else bkgnd_color = WhitePixel;
    
    def_val = XGetDefault(def_env, "MenuInactivePattern");
    if (def_val != NULL) {
	if (strcmp(def_val, "dimple1") == 0) inact_pnum = 0;
	else if (strcmp(def_val, "dimple3") == 0) inact_pnum = 1;
	else if (strcmp(def_val, "gray1") == 0) inact_pnum = 2;
	else if (strcmp(def_val, "gray3") == 0) inact_pnum = 3;
	else if (strcmp(def_val, "cross_weave") == 0) inact_pnum = 4;
    }

    def_val = XGetDefault(def_env, "PaneStyle");
    if (def_val != NULL) {
	if (strcmp(def_val, "flush_left") == 0) p_style = LEFT;
	else if (strcmp(def_val, "flush_right") == 0) p_style = RIGHT;
	else if (strcmp(def_val, "center") == 0) p_style = CENTER;
    }

    def_val = XGetDefault(def_env, "PaneFont");
    if (def_val != NULL) p_fnt_name = def_val;

    def_val = XGetDefault(def_env, "PaneForeground");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) p_frg_color = color_def.pixel;
    else if (reverse) p_frg_color = WhitePixel;
    else p_frg_color = BlackPixel;

    def_val = XGetDefault(def_env, "PaneBorder");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) p_bdr_color = color_def.pixel;
    else if (reverse) p_bdr_color = WhitePixel;
    else p_bdr_color = BlackPixel;
    
    def_val = XGetDefault(def_env, "PaneBorderWidth");
    if (def_val != NULL) p_bdr_width = atoi(def_val);
    
    def_val = XGetDefault(def_env, "PaneSpread");
    if (def_val != NULL) p_spread = atof(def_val);

    def_val = XGetDefault(def_env, "SelectionStyle");
    if (def_val != NULL) {
	if (strcmp(def_val, "flush_left") == 0) s_style = LEFT;
	else if (strcmp(def_val, "flush_right") == 0) s_style = RIGHT;
	else if (strcmp(def_val, "center") == 0) s_style = CENTER;
    }

    def_val = XGetDefault(def_env, "SelectionFont");
    if (def_val != NULL) s_fnt_name = def_val;

    def_val = XGetDefault(def_env, "SelectionForeground");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) s_frg_color = color_def.pixel;
    else if (reverse) s_frg_color = WhitePixel;
    else s_frg_color = BlackPixel;

    def_val = XGetDefault(def_env, "SelectionBorder");
    if (
	def_val != NULL &&
	DisplayCells() > 2 &&
	XParseColor(def_val, &color_def) &&
	XGetHardwareColor(&color_def)
    ) s_bdr_color = color_def.pixel;
    else if (reverse) s_bdr_color = WhitePixel;
    else s_bdr_color = BlackPixel;

    def_val = XGetDefault(def_env, "SelectionBorderWidth");
    if (def_val != NULL) s_bdr_width = atoi(def_val);
    
    def_val = XGetDefault(def_env, "SelectionSpread");
    if (def_val != NULL) s_spread = atof(def_val);

    /*
     * Create and store the inactive pattern pixmap.
     */
    switch (inact_pnum) {
	case 0:
	    inact_bitmap = XStoreBitmap(16, 16, dimple1_bits);
	    break;
	case 1:
	    inact_bitmap = XStoreBitmap(16, 16, dimple3_bits);
	    break;
	case 2:
	    inact_bitmap = XStoreBitmap(16, 16, gray1_bits);
	    break;
	case 3:
	    inact_bitmap = XStoreBitmap(16, 16, gray3_bits);
	    break;
	case 4:
	    inact_bitmap = XStoreBitmap(16, 16, cross_weave_bits);
	    break;
    }
    if (inact_bitmap == _X_FAILURE) {
	_XMErrorCode = XME_STORE_BITMAP;
	return(NULL);
    }

    /*
     * Generate the pixmaps from the background and forground colors.
     */
    tile_buf[0].pixel = p_bdr_color;
    tile_buf[1].pixel = s_bdr_color;
    tile_buf[2].pixel = p_frg_color;
    tile_buf[3].pixel = s_frg_color;
    tile_buf[4].pixel = bkgnd_color;

    tile_count = XMakeTiles(tile_buf, TILE_BUF_SIZE);
    if (tile_count != TILE_BUF_SIZE) {
	_XMErrorCode = XME_MAKE_TILES;
	return(NULL);
    }

    p_bdr_pixmap = tile_buf[0].pixmap;
    s_bdr_pixmap = tile_buf[1].pixmap;
    p_frg_pixmap = tile_buf[2].pixmap;
    s_frg_pixmap = tile_buf[3].pixmap;
    bkgnd_pixmap = tile_buf[4].pixmap;

    /*
     * Generate the inactive pixmap.
     */
    inact_pixmap = XMakePixmap(inact_bitmap, p_frg_color, bkgnd_color);
    if (inact_pixmap == _X_FAILURE) {
	_XMErrorCode = XME_MAKE_PIXMAP;
	return(NULL);
    }

    /*
     * Free the inactive pattern bitmap since we no longer need it.
     */
    XFreeBitmap(inact_bitmap);

    /*
     * Load the mouse cursor.
     */
    switch (menu_style) {
	case LEFT:
	    mouse_cursor = XCreateCursor(
		right_ptr_width, right_ptr_height, 
		right_ptr_bits, right_ptr_mask_bits, 
		right_ptr_x_hot, right_ptr_y_hot,
		mouse_color, bkgnd_color,
		GXcopy
	    );
	    break;
	case RIGHT:
	    mouse_cursor = XCreateCursor(
		left_ptr_width, left_ptr_height, 
		left_ptr_bits, left_ptr_mask_bits, 
		left_ptr_x_hot, left_ptr_y_hot,
		mouse_color, bkgnd_color,
		GXcopy
	    );
	    break;
	case CENTER:
	    mouse_cursor = XCreateCursor(
		center_ptr_width, center_ptr_height, 
		center_ptr_bits, center_ptr_mask_bits, 
		center_ptr_x_hot, center_ptr_y_hot,
		mouse_color, bkgnd_color,
		GXcopy
	    );
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(NULL);
    }
    if (mouse_cursor == _X_FAILURE) {
	_XMErrorCode = XME_CREATE_CURSOR;
	return(NULL);
    }

    /*
     * Open the pane and selection fonts.
     */
    p_fnt_info = XOpenFont(p_fnt_name);
    if (p_fnt_info == NULL) {
	_XMErrorCode = XME_OPEN_FONT;
	return(NULL);
    }
    s_fnt_info = XOpenFont(s_fnt_name);
    if (s_fnt_info == NULL) {
	_XMErrorCode = XME_OPEN_FONT;
	return(NULL);
    }
     
    /*
     * Calculate the fixed padding value in pixels for each font.
     */
    p_fnt_pad = s_spread * p_fnt_info->height;
    s_fnt_pad = s_spread * s_fnt_info->height;

    /*
     * Calculate fixed height and offset requirements.
     */
    flag_height = p_fnt_info->height + (p_fnt_pad << 1);

    p_height = 0;
    p_y_off = flag_height + p_bdr_width;
    p_x_off = p_y_off * p_spread;

    s_height = s_fnt_info->height + (s_fnt_pad << 1) + (s_bdr_width << 1);
    s_y_off = s_height;
    s_x_off = p_x_off;

    /*
     * Set up the pane list header.
     */
    pane->next = pane;
    pane->prev = pane;
    pane->type = PL_HEADER;
    pane->serial = -1;

    /*
     * Initialize the internal pane and selection creation queues.
     */
    _XMWinQueInit();

    /*
     * Construct the XMenu object.
     */
    /* -------------------- Menu data -------------------- */
    menu->menu_style = menu_style;
    menu->menu_mode = menu_mode;
    menu->freeze = freeze;
    menu->aeq = 0;
    menu->recompute = 1;
    menu->parent = parent;
    menu->height = 0;
    menu->width = 0;
    menu->mouse_cursor = mouse_cursor;
    menu->assoc_tab = assoc_tab;
    menu->p_list = pane;
    /* -------------------- Pane window data -------------------- */
    menu->p_style = p_style;
    menu->p_events = DEF_P_EVENTS;
    menu->p_fnt_info = p_fnt_info;
    menu->p_fnt_pad = p_fnt_pad;
    menu->p_spread = p_spread;
    menu->p_bdr_width = p_bdr_width;
    menu->flag_height = flag_height;
    menu->p_width = 0;
    menu->p_height = p_height;
    menu->p_x_off = p_x_off;
    menu->p_y_off = p_y_off;
    menu->p_count = 0;
    /* -------------------- Selection window data -------------------- */
    menu->s_style = s_style;
    menu->s_events = DEF_S_EVENTS;
    menu->s_fnt_info = s_fnt_info;
    menu->s_fnt_pad = s_fnt_pad;
    menu->s_spread = s_spread;
    menu->s_bdr_width = s_bdr_width;
    menu->s_width = 0;
    menu->s_height = s_height;
    menu->s_x_off = s_x_off;
    menu->s_y_off = s_y_off;
    menu->s_count = 0;
    /* -------------------- Color data -------------------- */
    menu->p_bdr_color = p_bdr_color;
    menu->s_bdr_color = s_bdr_color;
    menu->p_frg_color = p_frg_color;
    menu->s_frg_color = s_frg_color;
    menu->bkgnd_color = bkgnd_color;
    /* -------------------- Pixmap data -------------------- */
    menu->p_bdr_pixmap = p_bdr_pixmap;
    menu->s_bdr_pixmap = s_bdr_pixmap;
    menu->p_frg_pixmap = p_frg_pixmap;
    menu->s_frg_pixmap = s_frg_pixmap;
    menu->bkgnd_pixmap = bkgnd_pixmap;
    menu->inact_pixmap = inact_pixmap;

    /*
     * Return the completed XMenu.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(menu);
}
