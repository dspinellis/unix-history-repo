#include <X/mit-copyright.h>

/* $Header: XMenuChgSel.c,v 10.9 86/02/12 16:18:42 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuChangeSelection - Change a menu selection.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include "XMenuInternal.h"

int
XMenuChangeSelection(menu, p_num, s_num, data, data_sw, label, label_sw)
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    register int s_num;		/* Selection number to modified. */
    char *data;			/* Data value. */
    int data_sw;		/* Change to new data value? */
    char *label;		/* Selection label. */
    int label_sw;		/* Change to new label? */
{
    register int i;		/* Loop counter. */
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */
    
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
     * Find the right selection.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, s_num);
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Reset the label?
     */
    if (label_sw) {
	/*
	 * Determine label size.
	 */
	label_length = strlen(label);
	label_width = XQueryWidth(label, menu->s_fnt_info->id);

	/*
	 * Change the selection data.
	 */
	s_ptr->label = label;
	s_ptr->label_width = label_width;
	s_ptr->label_length = label_length;

	/*
	 * Schedule a recompute.
	 */
	menu->recompute = 1;
    }

    /*
     * Reset the data?
     */
    if (data_sw) s_ptr->data = data;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}
