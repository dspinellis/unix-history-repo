#include <X/mit-copyright.h>

/* $Header: XMenuInternal.c,v 10.22 86/11/30 17:03:36 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuInternal.c - XMenu internal (not user visable) routines.
 *
 *	Author:		Tony Della Fera, DEC
 *			November, 1985
 *
 */

#include "XMenuInternal.h"

/*
 * Toggle color macro.
 */
#define toggle_color(x) \
	((x) == menu->bkgnd_color ? menu->s_frg_color : menu->bkgnd_color)

/*
 * Internal Window creation queue sizes.
 */
#define S_QUE_SIZE	300
#define P_QUE_SIZE	20
#define BUFFER_SIZE	(S_QUE_SIZE >= P_QUE_SIZE ? S_QUE_SIZE : P_QUE_SIZE)


/*
 * XMWinQue - Internal window creation queue datatype.
 */
typedef struct _xmwinquedef {
    int sq_size;
    XMSelect *sq[S_QUE_SIZE];
    XMSelect **sq_ptr;
    int pq_size;
    XMPane *pq[P_QUE_SIZE];
    XMPane **pq_ptr;
} XMWinQue;

/*
 * _XMWinQue - Internal static window creation queue.
 */
static Bool _XMWinQueIsInit = FALSE;
static XMWinQue _XMWinQue;

/*
 * _XMErrorCode - Global XMenu error code.
 */
int _XMErrorCode = XME_NO_ERROR;

/*
 * _XMErrorList - Global XMenu error code discription strings.
 */
char *
_XMErrorList[XME_CODE_COUNT] = {
    "No error",				/* XME_NO_ERROR */
    "Menu not initialized",		/* XME_NOT_INIT */
    "Argument out of bounds",		/* XME_ARG_BOUNDS */
    "Pane not found",			/* XME_P_NOT_FOUND */
    "Selection not found",		/* XME_S_NOT_FOUND */
    "Invalid menu style parameter",	/* XME_STYLE_PARAM */
    "Unable to grab mouse",		/* XME_GRAB_MOUSE */
    "Unable to interpret locator",	/* XME_INTERP_LOC */
    "Unable to calloc memory",		/* XME_CALLOC */
    "Unable to create XAssocTable",	/* XME_CREATE_ASSOC */
    "Unable to store bitmap",		/* XME_STORE_BITMAP */
    "Unable to make tile pixmaps",	/* XME_MAKE_TILES */
    "Unable to make pixmap",		/* XME_MAKE_PIXMAP */
    "Unable to create cursor",		/* XME_CREATE_CURSOR */
    "Unable to open font",		/* XME_OPEN_FONT */
    "Unable to create windows",		/* XME_CREATE_WINDOW */
    "Unable to create transparencies",	/* XME_CREATE_TRANSP */
};

/*
 * _XMEventHandler - Internal event handler variable.
 */
int (*_XMEventHandler)() = NULL;



/*
 * _XMWinQueInit - Internal routine to initialize the window
 *		   queue.
 */
_XMWinQueInit()
{
    /*
     * If the queue is not initialized initialize it.
     */
    if (!_XMWinQueIsInit) {
	/*
	 * Blank the queue structure.
	 */
	bzero(&_XMWinQue, sizeof(XMWinQue));

	/*
	 * Initialize the next free location pointers.
	 */
	_XMWinQue.sq_ptr = _XMWinQue.sq;
	_XMWinQue.pq_ptr = _XMWinQue.pq;
    }
}



/*
 * _XMWinQueAddPane - Internal routine to add a pane to the pane
 *		      window queue.
 */
int
_XMWinQueAddPane(menu, p_ptr)
    register XMenu *menu;	/* Menu being manipulated. */
    register XMPane *p_ptr;	/* XMPane being queued. */
{
    /*
     * If the queue is currently full then flush it.
     */
    if (_XMWinQue.pq_size == P_QUE_SIZE) {
	if (_XMWinQueFlush(menu) == _FAILURE) return(_FAILURE);
    }

    /*
     * Insert the new XMPane pointer and increment the queue pointer
     * and the queue size.
     */
    *_XMWinQue.pq_ptr = p_ptr;
    _XMWinQue.pq_ptr++;
    _XMWinQue.pq_size++;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMWinQueAddSelection - Internal routine to add a selection to
 *			   the selection window queue.
 */
int
_XMWinQueAddSelection(menu, s_ptr)
    register XMenu *menu;	/* Menu being manipulated. */
    register XMSelect *s_ptr;	/* XMSelection being queued. */
{
    /*
     * If this entry will overflow the queue then flush it.
     */
    if (_XMWinQue.sq_size == S_QUE_SIZE) {
	if (_XMWinQueFlush(menu) == _FAILURE) return(_FAILURE);
    }

    /*
     * Insert the new XMSelect pointer and increment the queue pointer
     * and the queue size.
     */
    *_XMWinQue.sq_ptr = s_ptr;
    _XMWinQue.sq_ptr++;
    _XMWinQue.sq_size++;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMWinQueFlush - Internal routine to flush the pane and
 *		    selection window queues.
 */
int
_XMWinQueFlush(menu)
    register XMenu *menu;	/* Menu being manipulated. */
{
    register int pq_index;	/* Pane queue index. */
    register int sq_index;	/* Selection queue index. */
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;   /* XMSelect pointer. */

    int win_count;			/* Count of created windows. */
    Window window;			/* Window id temporary variable. */
    BatchFrame batch_buf[BUFFER_SIZE];	/* Window batch creation buffer. */

    /*
     * If the pane window queue is not empty...
     */
    if (_XMWinQue.pq_size > 0) {
	/*
	 * Pack the window batch buffer.
	 */
	for (pq_index = 0; pq_index < _XMWinQue.pq_size; pq_index++) {
	    /*
	     * Retrieve the an XMPane pointer.
	     */
	    p_ptr = _XMWinQue.pq[pq_index];
	    
	    /*
	     * Pack the next buffer location with the data
	     * associated with the XMPane. 
	     */
	    batch_buf[pq_index].type = IsOpaque;
	    batch_buf[pq_index].parent = menu->parent;
	    batch_buf[pq_index].x = p_ptr->window_x;
	    batch_buf[pq_index].y = p_ptr->window_y;
	    batch_buf[pq_index].width = p_ptr->window_w;
	    batch_buf[pq_index].height = p_ptr->window_h;
	    batch_buf[pq_index].bdrwidth = menu->p_bdr_width;
	    batch_buf[pq_index].border = menu->p_bdr_pixmap;
	    batch_buf[pq_index].background = menu->inact_pixmap;
	}

	/*
	 * Transmit the pane window buffer.
	 */
	win_count = XCreateWindowBatch(batch_buf, _XMWinQue.pq_size);
	if (win_count != _XMWinQue.pq_size) {
	    _XMErrorCode = XME_CREATE_WINDOW;
	    return(_FAILURE);
	}

	/*
	 * Retrieve the resulting window id's.
	 */
	for (pq_index = 0; pq_index < _XMWinQue.pq_size; pq_index++) {
	    window = batch_buf[pq_index].self;
	    /*
	     * Store the window id with its XMPane structure.
	     */
	    p_ptr = _XMWinQue.pq[pq_index];
	    p_ptr->window = window;
	    /*
	     * Insert the new window id and its
	     * associated XMPane structure into the 
	     * assoction table.
	     */
	    XMakeAssoc(menu->assoc_tab, window, p_ptr);
	    /*
	     * Select input events on the new window.
	     */
	    XSelectInput(window, menu->p_events);
	}

	/*
	 * Reset the pane queue pointer and size.
	 */
	_XMWinQue.pq_size = 0;
	_XMWinQue.pq_ptr = _XMWinQue.pq;
    }

    /*
     * If the selection window queue is not empty...
     */
    if (_XMWinQue.sq_size > 0) {
	/*
	 * Pack the selection transparency buffer. 
	 */
	for (sq_index = 0; sq_index < _XMWinQue.sq_size; sq_index++) {
	    /*
	     * Retrieve the an XMSelect pointer.
	     */
	    s_ptr = _XMWinQue.sq[sq_index];
	    
	    /*
	     * Pack the next buffer location with the data
	     * associated with the XMPane. 
	     */
	    batch_buf[sq_index].type = IsTransparent;
	    batch_buf[sq_index].parent = s_ptr->parent_p->window;
	    batch_buf[sq_index].x = s_ptr->window_x;
	    batch_buf[sq_index].y = s_ptr->window_y;
	    batch_buf[sq_index].width = s_ptr->window_w;
	    batch_buf[sq_index].height = s_ptr->window_h;
	}
    
	/*
	 * Transmit the selection transparency buffer.
	 */
	win_count = XCreateWindowBatch(batch_buf, _XMWinQue.sq_size);
	if (win_count != _XMWinQue.sq_size) {
	    _XMErrorCode = XME_CREATE_TRANSP;
	    return(_FAILURE);
	}
    
	/*
	 * Retrieve the resulting window id's.
	 */
	for (sq_index = 0; sq_index < _XMWinQue.sq_size; sq_index++) {
	    window = batch_buf[sq_index].self;
	    /*
	     * Store the window id with its XMSelect structure.
	     */
	    s_ptr = _XMWinQue.sq[sq_index];
	    s_ptr->window = window;
	    /*
	     * Insert the new window id and its
	     * associated XMSelect structure into the 
	     * assoction table.
	     */
	    XMakeAssoc(menu->assoc_tab, window, s_ptr);
	    /*
	     * Select input events on the new window.
	     */
	    XSelectInput(window, menu->s_events);
	}

	/*
	 * Reset the selection queue pointer and size.
	 */
	_XMWinQue.sq_size = 0;
	_XMWinQue.sq_ptr = _XMWinQue.sq;
    }

    /*
     * Flush X's internal queues.
     */
    XFlush();

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMGetPanePtr - 	Given a menu pointer and a pane index number, return
 *			a pane pointer that points to the indexed pane.
 */
XMPane *
_XMGetPanePtr(menu, p_num)
    register XMenu *menu;	/* Menu to find the pane in. */
    register int p_num;		/* Index number of pane to find. */
{
    register XMPane *p_ptr;	/* Pane pointer to be returned. */
    register int i;		/* Loop counter. */

    /*
     * Is the pane number out of range?
     */
    if ((p_num < 0) || (p_num > (menu->p_count - 1))) {
	_XMErrorCode = XME_P_NOT_FOUND;
	return(NULL);
    }

    /*
     * Find the right pane.
     */
    p_ptr = menu->p_list->next;
    for (i = 0; i < p_num; i++) p_ptr = p_ptr->next;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_ptr);
}



/*
 * _XMGetSelectionPtr -	Given pane pointer and a selection index number,
 *			return a selection pointer that points to the
 *			indexed selection.
 */
XMSelect *
_XMGetSelectionPtr(p_ptr, s_num)
    register XMPane *p_ptr;	/* Pane to find the selection in. */
    register int s_num;		/* Index number of the selection to find. */
{
    register XMSelect *s_ptr;	/* Selection pointer to be returned. */
    register int i;		/* Loop counter. *./
    
    /*
     * Is the selection number out of range?
     */
    if ((s_num < 0) || (s_num > (p_ptr->s_count - 1))) {
	_XMErrorCode = XME_S_NOT_FOUND;
	return(NULL);
    }

    /*
     * Find the right selection.
     */
    s_ptr = p_ptr->s_list->next;
    for (i = 0; i < s_num; i++) s_ptr = s_ptr->next;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_ptr);
}



/*
 * _XMRecomputeGlobals - Internal subroutine to recompute menu wide
 *			 global values.
 */
_XMRecomputeGlobals(menu)
    register XMenu *menu;	/* Menu object to compute from. */
{
    register XMPane *p_ptr;	/* Pane pointer. */
    register XMSelect *s_ptr;	/* Selection pointer. */

    register int max_p_label = 0;	/* Maximum pane label width. */
    register int max_s_label = 0;	/* Maximum selection label width. */
    register int s_count = 0;		/* Maximum selection count. */

    int p_s_pad;		/* Pane <-> selection padding. */
    int p_s_diff;		/* Pane <-> selection seperation. */

    int p_height;		/* Pane window height. */
    int p_width;		/* Pane window width. */
    int s_width;		/* Selection window width. */

    int seperation;		/* Pane window, selection window seperation. */

    /*
     * For each pane...
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	/*
	 * Recompute maximum pane label width.
	 */
	max_p_label = max(max_p_label, p_ptr->label_width);

	/*
	 * Recompute maximum selection count. 
	 */
	s_count = max(s_count, p_ptr->s_count);

	/*
	 * For each selection in the current pane...
	 */
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_ptr->next
	){
	    /*
	     * Recompute maximum selection label width.
	     */
	    max_s_label = max(max_s_label, s_ptr->label_width);
	}
    }

    /*
     * Recompute pane height.
     */
    p_height = (menu->flag_height << 1) + (menu->s_y_off * s_count);

    /*
     * Recompute horizontal padding between the pane window and the
     * selection windows.
     */
    p_s_pad = menu->p_x_off << 1;

    /*
     * Recompute pane and selection window widths.
     * This is done by first computing the window sizes from the maximum
     * label widths.  If the spacing between the selection window and the
     * containing pane window is less than the pane selection padding value
     * (twice the pane X offset) then change the size of the pane to be
     * the size of the selection window plus the padding.  If, however the
     * spacing between the selection window and the containing pane window
     * is more than the pane selection padding value increase the size of
     * the selection to its maximum possible value (the pane width minus
     * the pane selection padding value).
     */
    p_width = max_p_label + p_s_pad;
    s_width = max_s_label + (menu->s_fnt_pad << 1) + (menu->s_bdr_width << 1);
    p_s_diff = p_width - s_width;
    if (p_s_diff < p_s_pad) {
	p_width = s_width + p_s_pad;
    }
    else if (p_s_diff > p_s_pad) {
	s_width = p_width - p_s_pad;
    }

    /*
     * Reset menu wide global values.
     */
    menu->s_count = s_count;
    menu->p_height = p_height;
    menu->p_width = p_width;
    menu->s_width = s_width;
}



/*
 * _XMRecomputePane - Internal subroutine to recompute pane
 *		      window dependencies.
 */
int
_XMRecomputePane(menu, p_ptr, p_num)
    register XMenu *menu;	/* Menu object being recomputed. */
    register XMPane *p_ptr;	/* Pane pointer. */
    register int p_num;		/* Pane sequence number. */
{
    register int window_x;	/* Recomputed window X coordinate. */
    register int window_y;	/* Recomputed window Y coordinate. */

    register Bool config_p = FALSE;	/* Reconfigure pane window? */

    /*
     * Update the pane serial number.
     */
    p_ptr->serial = p_num;

    /*
     * Recompute window X and Y coordinates.
     */
    switch (menu->menu_style) {
	case LEFT:
	    window_x = menu->p_x_off * ((menu->p_count - 1) - p_num);
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	case RIGHT:
	    window_x = menu->p_x_off * p_num;
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	case CENTER:
	    window_x = 0;
	    window_y = menu->p_y_off * ((menu->p_count - 1) - p_num);
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }

    /*
     * If the newly compute pane coordinates differ from the 
     * current coordinates, reset the current coordinates and
     * reconfigure the pane.
     */
    if (
	(window_x != p_ptr->window_x) ||
	(window_y != p_ptr->window_y)
    ){
	/*
	 * Reset the coordinates and schedule
	 * the pane for reconfiguration.
	 */
	p_ptr->window_x = window_x;
	p_ptr->window_y = window_y;
	config_p = TRUE;
    }

    /*
     * If the local pane width and height differs from the
     * menu pane width and height, reset the local values.
     */
    if (
	(p_ptr->window_w != menu->p_width) ||
	(p_ptr->window_h != menu->p_height)
    ){
	/*
	 * Reset window width and height and schedule
	 * the pane for reconfiguration.
	 */
	p_ptr->window_w = menu->p_width;
	p_ptr->window_h = menu->p_height;
	config_p = TRUE;
    }

    /*
     * If we need to reconfigure the pane window do it now.
     */
    if (config_p == TRUE) {
	/*
	 * If the pane window has already been created then
	 * reconfigure the existing window, otherwise queue
	 * it for creation with the new configuration.
	 */
	if (p_ptr->window) {
	    XConfigureWindow(
		p_ptr->window,
		p_ptr->window_x, p_ptr->window_y,
		p_ptr->window_w, p_ptr->window_h
	    );
	}
	else {
	    if (_XMWinQueAddPane(menu, p_ptr) == _FAILURE) {
		return(_FAILURE);
	    }
	}
    }
 
    /*
     * Recompute label X position.
     */
    switch (menu->p_style) {
	case LEFT:
	    p_ptr->label_x = menu->p_x_off + menu->p_fnt_pad;
	    break;
	case RIGHT:
	    p_ptr->label_x = menu->p_width -
		(p_ptr->label_width + menu->p_x_off + menu->p_fnt_pad);
	    break;
	case CENTER:
	    p_ptr->label_x = (menu->p_width - p_ptr->label_width) >> 1;
	    break;
	default:
	    /* Error! Invalid style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }
    /*
     * Recompute label Y positions.
     */
    p_ptr->label_uy = menu->p_fnt_pad;
    p_ptr->label_ly = (menu->p_height - menu->flag_height) + menu->p_fnt_pad;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMRecomputeSelection - Internal subroutine to recompute
 *			   selection window dependencies.
 */
int
_XMRecomputeSelection(menu, s_ptr, s_num)
    register XMenu *menu;	/* Menu object being recomputed. */
    register XMSelect *s_ptr;	/* Selection pointer. */
    register int s_num;		/* Selection sequence number. */
{
    register Bool config_s = FALSE;	/* Reconfigure selection window? */

    /*
     * If the selection serial numbers are out of order, begin
     * resequencing selections.  Recompute selection window coordinates
     * and serial number.
     *
     * When selections are created they are given a serial number of
     * -1, this causes this routine to give a new selection
     * its initial coordinates and serial number.
     */
    if (s_ptr->serial != s_num) {
	/*
	 * Fix the sequence number.
	 */
	s_ptr->serial = s_num;
	/*
	 * Recompute window X and Y coordinates.
	 */
	s_ptr->window_x = menu->s_x_off;
	s_ptr->window_y = menu->flag_height + (menu->s_y_off * s_num);
	/*
	 * We must reconfigure the window.
	 */
	config_s = TRUE;
    }

    /*
     * If the local selection width and height differs from the
     * menu selection width and height, reset the local values.
     */
    if (
	(s_ptr->window_w != menu->s_width) ||
	(s_ptr->window_h != menu->s_height)
    ){
	/*
	 * We must reconfigure the window.
	 */
	config_s = TRUE;
	/*
	 * Reset window width and height.
	 */
	s_ptr->window_w = menu->s_width;
	s_ptr->window_h = menu->s_height;
    }

    /*
     * If we need to reconfigure the selection window do it now.
     */
    if (config_s == TRUE) {
	/*
	 * If the selection window has already been created then
	 * reconfigure the existing window, otherwise queue it
	 * for creation with the new configuration.
	 */
	if (s_ptr->window) {
	    XConfigureWindow(
		s_ptr->window,
		s_ptr->window_x, s_ptr->window_y,
		s_ptr->window_w, s_ptr->window_h
	    );
	}
	else {
	    if (_XMWinQueAddSelection(menu, s_ptr) == _FAILURE) {
		return(_FAILURE);
	    }
	}
    }

    /*
     * Recompute label X position.
     */
    switch (menu->s_style) {
	case LEFT:
	    s_ptr->label_x = menu->s_bdr_width + menu->s_fnt_pad;
	    break;
	case RIGHT:
	    s_ptr->label_x = menu->s_width -
		(s_ptr->label_width + menu->s_bdr_width + menu->s_fnt_pad);
	    break;
	case CENTER:
	    s_ptr->label_x = (menu->s_width - s_ptr->label_width) >> 1;
	    break;
	default:
	    /* Error! Invaild style parameter. */
	    _XMErrorCode = XME_STYLE_PARAM;
	    return(_FAILURE);
    }
    /*
     * Recompute label Y position.
     */
    s_ptr->label_y = menu->s_fnt_pad + menu->s_bdr_width;

    /*
     * All went well, return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(_SUCCESS);
}



/*
 * _XMTransToOrigin - Internal subroutine to translate the point at
 *		      the center of the current pane and selection to the 
 *		      the menu origin.
 *
 *	WARNING! ******	Be certain that all menu depencies have been
 *			recomputed before calling this routine or
 *			unpredicable results will follow.
 */
_XMTransToOrigin(menu, p_ptr, s_ptr, x_pos, y_pos, orig_x, orig_y)
    register XMenu *menu;	/* Menu being computed against. */
    register XMPane *p_ptr;	/* Current pane pointer. */
    register XMSelect *s_ptr;	/* Current selection pointer. */
    int x_pos;			/* X coordinate of point to translate. */
    int y_pos;			/* Y coordinate of point to translate. */
    int *orig_x;		/* Return value X coord. of the menu origin. */
    int *orig_y;		/* Return value Y coord. of the menu origin. */
{
    register int l_orig_x;	/* Local X coordinate of the menu origin. */
    register int l_orig_y;	/* Local Y coordinate of the menu origin. */
    
    /*
     * Translate the menu origin such that the cursor hot point will be in the
     * center of the desired current selection and pane.
     * If the current selection pointer is NULL then assume that the hot point
     * will be in the center of the current pane flag.
     */

    if (s_ptr == NULL) {
	/*
	 * Translate from the center of the pane flag to the upper left
	 * of the current pane window.
	 */
	l_orig_x = x_pos - (menu->p_width >> 1) - menu->p_bdr_width;
	l_orig_y = y_pos - (menu->flag_height >> 1) - menu->p_bdr_width;
    }
    else {
	/*
	 * First translate from the center of the current selection
	 * to the upper left of the current selection window.
	 */
	l_orig_x = x_pos - (menu->s_width >> 1);
	l_orig_y = y_pos - (menu->s_height >> 1);

	/*
	 * Then translate to the upper left of the current pane window.
	 */
	l_orig_x -= (s_ptr->window_x + menu->p_bdr_width);
	l_orig_y -= (s_ptr->window_y + menu->p_bdr_width);
    }

    /*
     * Finally translate to the upper left of the menu.
     */
    l_orig_x -= p_ptr->window_x;
    l_orig_y -= p_ptr->window_y;

    /*
     * Set the return values.
     */
    *orig_x = l_orig_x;
    *orig_y = l_orig_y;
}



/*
 * _XMRefreshPaneLabels - Internal subroutine that refreshes
 *			  the labels in a menu pane.
 */
_XMRefreshPaneText(menu, pane)
    register XMenu *menu;
    register XMPane *pane;
{
    /*
     * First clear the pane. 
     */
    XClear(pane->window);
    /*
     * Then redraw the label text.
     */
    XTextMask(
	pane->window,
	pane->label_x, pane->label_uy,
	pane->label, pane->label_length,
	menu->p_fnt_info->id,
	menu->p_frg_color
    );
    XTextMask(
	pane->window,
	pane->label_x, pane->label_ly,
	pane->label, pane->label_length,
	menu->p_fnt_info->id,
	menu->p_frg_color
    );
}



/*
 * _XMRefreshPane - Internal subroutine to completely refresh
 *		    the contents of a pane.
 */
_XMRefreshPane(menu,pane)
    register XMenu *menu;
    register XMPane *pane;
{
    register XMSelect *s_list = pane->s_list;
    register XMSelect *s_ptr;

    /*
     * First clear the pane. 
     */
    XClear(pane->window);
    /*
     * Then redraw the label text.
     */
    XTextMask(
	pane->window,
	pane->label_x, pane->label_uy,
	pane->label, pane->label_length,
	menu->p_fnt_info->id,
	menu->p_frg_color
    );
    XTextMask(
	pane->window,
	pane->label_x, pane->label_ly,
	pane->label, pane->label_length,
	menu->p_fnt_info->id,
	menu->p_frg_color
    );
    /*
     * Finally refresh each selection.
     */
    for (s_ptr = s_list->next; s_ptr != s_list; s_ptr = s_ptr->next){
	_XMRefreshSelection(menu, s_ptr);
    }
}



/*
 * _XMRefreshSelection - Internal subroutine that refreshes 
 *			 a single selection window.
 */
_XMRefreshSelection(menu, select)
    register XMenu *menu;
    register XMSelect *select;
{
    register int width = select->window_w;
    register int height = select->window_h;
    register int bdr_width = menu->s_bdr_width;
    
    if (select->activated) {
	if (menu->menu_mode == INVERT) {
	    XPixSet(select->window, 0, 0, width, height, menu->s_frg_color);
	    XTextMask(
                select->window,
                select->label_x, select->label_y,
                select->label, select->label_length,
                menu->s_fnt_info->id,
		menu->bkgnd_color
            );
	}
        else {
            /*
             * Since most drawing routines with arbitrary width lines
	     * are slow compared to raster-ops lets use a raster-op to
	     * draw the boxes.
             */
	    XPixSet(select->window, 0, 0, width, height, menu->bkgnd_color);
            XPixSet(
                select->window,
                0, 0,
                (width - bdr_width), bdr_width,
		menu->s_bdr_color
            );
            XPixSet(
                select->window,
                (0 + (width - bdr_width)), 0,
                bdr_width, (height - bdr_width),
		menu->s_bdr_color
            );
            XPixSet(
                select->window,
                (0 + bdr_width), (0 + (height - bdr_width)),
                (width - bdr_width), bdr_width,
		menu->s_bdr_color
            );
            XPixSet(
                select->window,
                0, (0 + bdr_width),
                bdr_width, (height - bdr_width),
		menu->s_bdr_color
            );
	    XTextMask(
                select->window,
                select->label_x, select->label_y,
                select->label, select->label_length,
                menu->s_fnt_info->id,
		menu->s_frg_color
            );
        }
    }
    else {
	XClear(select->window);
        XTextMask(
            select->window,
            select->label_x, select->label_y,
            select->label, select->label_length,
            menu->s_fnt_info->id,
            menu->s_frg_color
        );
    }
}
