#include <X/mit-copyright.h>

/* $Header: XMenuChgPane.c,v 10.8 86/02/12 16:18:39 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuChangePane - Change the label of a  menu pane.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include "XMenuInternal.h"

int
XMenuChangePane(menu, p_num, label)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    char *label;		/* Selection label. */
{
    register int i;		/* Loop counter. */
    register XMPane *p_ptr;	/* XMPane pointer. */

    int label_length;		/* Label lenght in characters. */
    int label_width;		/* Label width in pixels. */

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Determine label size.
     */
    label_width = XQueryWidth(label, menu->p_fnt_info->id);
    label_length = strlen(label);

    /*
     * Change the pane data.
     */
    p_ptr->label = label;
    p_ptr->label_width = label_width;
    p_ptr->label_length = label_length;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just changed.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}
