#include <X11/copyright.h>

/* $Header: AddSel.c,v 1.4 87/12/20 12:04:00 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuAddSelection - Adds a selection to an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuAddSelection(display, menu, p_num, data, label, active)
    Display *display;
    register XMenu *menu;	/* Menu object to be modified. */
    register int p_num;		/* Pane number to be modified. */
    char *data;			/* Data value. */
    char *label;		/* Selection label. */
    int active;			/* Make selection active? */
{
    register XMPane *pane;	/* Pane containing the new selection. */
    register XMSelect *select;	/* Newly created selection. */


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
    pane = _XMGetPanePtr(menu, p_num);
    if (pane == NULL) return(XM_FAILURE);

    /*
     * Calloc the XMSelect structure.
     */
    select = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (select == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }
    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->s_fnt_info, label, label_length);
    
    /*
     * Fill the XMSelect structure.
     */
    select->type = SELECTION;
    select->active = active;
    select->serial = -1;
    select->label = label;
    select->label_width = label_width;
    select->label_length = label_length;
    select->data = data;
    select->parent_p = pane;
    
    /*
     * Insert the selection at the end of the selection list.
     */
    insque(select, pane->s_list->prev);

    /*
     * Update the selection count.
     */
    pane->s_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return((pane->s_count - 1));
}
