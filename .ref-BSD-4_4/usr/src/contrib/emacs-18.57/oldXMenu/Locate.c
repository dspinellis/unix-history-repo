#include <X11/copyright.h>

/* $Header: Locate.c,v 1.5 88/02/02 19:09:02 jim Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuLocate - Return data necessary to position and locate
 *		      a menu on the screen.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 11, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuLocate(display, menu, p_num, s_num, x_pos, y_pos, ul_x, ul_y, width, height)
    register Display *display;	/* Previously opened display. */
    register XMenu *menu;	/* Menu object being located. */
    int p_num;			/* Active pane number. */
    int s_num;			/* Active selection number. */
    int x_pos;			/* X coordinate of mouse active position. */
    int y_pos;			/* Y coordinate of mouse active position. */
    int *ul_x;			/* Returned upper left menu X coordinate. */
    int *ul_y;			/* Returned upper left menu Y coordinate. */
    int *width;			/* Returned menu width. */
    int *height;		/* Returned menu height. */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */
    
    /*
     * Are the position arguments positive?
     */
    if ((x_pos <= 0) || (y_pos <= 0)) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the right selection.
     */
      s_ptr = _XMGetSelectionPtr(p_ptr, s_num);

    /*
     * Check to see that the menu's dependencies have been
     * recomputed and are up to date.  If not, do it now.
     */
    if (menu->recompute) XMenuRecompute(display, menu);

    /*
     * Compute the new menu origin such that the active point lies
     * in the center of the desired active pane and selection.
     * This sets the values of ul_x and ul_y.
     */
    _XMTransToOrigin(display, menu, p_ptr, s_ptr, x_pos, y_pos, ul_x, ul_y);

    /*
     * Set remaining return argument values.
     */
    *width = menu->width;
    *height = menu->height;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(XM_SUCCESS);
}
