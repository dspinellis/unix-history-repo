#include <X11/copyright.h>

/* $Header: AddPane.c,v 1.3 87/12/20 12:03:52 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuAddPane - Adds a pane to an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuAddPane(display, menu, label, active)
    Display *display;           
    register XMenu *menu;	/* Menu object to be modified. */
    register char *label;	/* Selection label. */
    int active;			/* Make selection active? */
{
    register XMPane *pane;	/* Newly created pane. */
    register XMSelect *select;	/* Initial selection for the new pane. */
        
    int label_length;		/* Label length in characters. */
    int label_width;		/* Label width in pixels. */

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Calloc the XMPane structure and the initial XMSelect.
     */
    pane = (XMPane *)calloc(1, sizeof(XMPane));
    if (pane == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }
    select = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (select == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }
    
    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->p_fnt_info,
			     label,
			     label_length);
    
    /*
     * Set up the initial selection.
     * Values not explicitly set are zeroed by calloc.
     */
    select->next = select;
    select->prev = select;
    select->type = SL_HEADER;
    select->serial = -1;
    select->parent_p = pane;

    /*
     * Fill the XMPane structure.
     * X and Y position are set to 0 since a recompute will follow.
     */
    pane->type = PANE;
    pane->active = active;
    pane->serial = -1;
    pane->label = label;
    pane->label_width = label_width;
    pane->label_length = label_length;
    pane->s_list = select;

    /*
     * Insert the pane at the end of the pane list.
     */
    insque(pane, menu->p_list->prev);

    /*
     * Update the pane count. 
     */
    menu->p_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return((menu->p_count - 1));
}
