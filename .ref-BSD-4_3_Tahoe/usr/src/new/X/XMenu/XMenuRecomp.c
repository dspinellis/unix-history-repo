#include <X/mit-copyright.h>

/* $Header: XMenuRecomp.c,v 10.7 86/02/12 16:19:51 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuRecompute - Recompute XMenu object dependencies.
 *
 *	Author:		Tony Della Fera, DEC
 *			September, 1985
 *
 */

#include "XMenuInternal.h"

int
XMenuRecompute(menu)
    register XMenu *menu;	/* Menu object to be recomputed. */
{
    register XMPane *p_ptr;	/* Pane pointer. */
    register XMSelect *s_ptr;	/* Selection pointer. */

    register int p_num;		/* Pane serial number. */
    register int s_num;		/* Selection serial number. */

    /*
     * If no recompute is necessary, return.
     */
    if (!menu->recompute) {
	_XMErrorCode = XME_NO_ERROR;
	return(XM_SUCCESS);
    }

    /*
     * If there are no panes in the menu then return failure
     * beacuse the menu is not initialized.
     */
    if (menu->p_count == 0) {
	_XMErrorCode = XME_NOT_INIT;
	return(XM_FAILURE);
    }

    /*
     * Recompute menu wide global values: pane window size,
     * selection size and maximum selection count.
     */
    _XMRecomputeGlobals(menu);

    /*
     * For each pane in the menu...
     */
    p_num = 0;
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	/*
	 * Recompute pane dependencies.
	 */
	if (_XMRecomputePane(menu, p_ptr, p_num) == _FAILURE) {
	    return(XM_FAILURE);
	}
        p_num++;
	/*
	 * For each selection in the pane...
	 */
	s_num = 0;
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_ptr->next
	) {
	    /*
	     * Recompute selection dependencies.
	     */
	    if (_XMRecomputeSelection(menu, s_ptr, s_num) == _FAILURE) {
		return(XM_FAILURE);
	    }
	    s_num++;
	}
    }

    /*
     * Flush the window creation queue.
     * This batches all window creates since lazy evaluation
     * is more efficient than individual evaluation.
     * This routine also does an XFlush().
     */
    if (_XMWinQueFlush(menu) == _FAILURE) return(XM_FAILURE);

    /*
     * Make sure all selection windows are mapped.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	XMapSubwindows(p_ptr->window);
    }

    /*
     * Recompute menu size.
     */
    if (menu->menu_style == CENTER) {
	menu->width = menu->p_width + (menu->p_bdr_width << 1);
    }
    else {
	menu->width = menu->p_width + (menu->p_bdr_width << 1) +
	    ((menu->p_count - 1) * menu->p_x_off);
    }
    menu->height = menu->p_height + (menu->p_bdr_width << 1) +
	((menu->p_count - 1) * menu->p_y_off);

    /*
     * Reset the recompute flag.
     */
    menu->recompute = 0;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(XM_SUCCESS);
}
